package codegen

import (
	"language/compiler/icode"
	"language/compiler/lex"
	"language/compiler/parse"
	"errors"
	"fmt"
	"io"
	"strconv"
)

const (
	Heap LocType = iota
	Stack
	Global
)

type (
	LocType int

	Generator struct {
		symbolTable map[string]parse.Symbol
		icode       icode.Icode
	}

	location struct {
		locType LocType
		pos     *int // represents either the register number or the offset depending on locType
	}
)

func NewGenerator(st map[string]parse.Symbol, code icode.Icode) *Generator {
	return &Generator{
		symbolTable: st,
		icode:       code,
	}
}

func (g *Generator) Generate(out io.Writer) error {
	// this keeps track of how many byts of opcodes and data are written out
	bytsAllocated = 0

	for _, code := range g.icode {
		var err error
		q, ok := code.(icode.Quad)
		if !ok {
			return notIcodeInIcodeList
		}
		_, err = fmt.Fprintln(out, "-- "+q.Operator())
		if err != nil {
			return err
		}
		switch q.Operator() {
		case icode.ADD, icode.SUB, icode.MUL, icode.DIV, icode.ADDI:
			err = g.mathOp(q, out)
		case icode.LT, icode.GT, icode.NE, icode.EQ, icode.LE, icode.GE:
			err = g.cmp(q, out)
		case icode.AND, icode.OR:
			err = g.logic(q, out)
		case icode.BF, icode.BT:
			err = g.branch(q, out)
		case icode.JMP:
			i := op(q.Label(), JMP, q.Operand1(), "")
			err = write(out, i)
		case icode.PUSH:
			err = g.push(q, out)
		case icode.PEEK:
			err = g.peek(q, out)
		case icode.FRAME:
			err = g.frame(q, out)
		case icode.CALL:
			err = g.call(q, out)
		case icode.RTN, icode.RETURN:
			err = g.rtn(q, out)
		case icode.FUNC:
			err = g.function(q, out)
		case icode.NEWI, icode.NEW:
			err = g.heapAlloc(q, out)
		case icode.AEF:
			err = g.aef(q, out)
		case icode.REF:
			err = g.ref(q, out)
		case icode.MOV, icode.MOVI, icode.MOVC:
			err = g.mov(q, out)
		case icode.WRTC, icode.WRTI:
			err = g.write(q, out)
		case icode.RDC, icode.RDI:
			err = g.read(q, out)
		case icode.TERM:
			_, err = fmt.Fprintln(out, opnl(TRP, "0", ""))
		default:
			return unknownIcode(q.Operator())
		}
		if err != nil {
			return err
		}
	}

	// spit the constants onto the the asm file
	for _, sym := range g.symbolTable {
		if sym.Kind().IsLit() {
			var dirType Directive
			if sym.Kind() == parse.Character {
				dirType = BYT
			} else {
				dirType = INT
			}
			var value string
			if sym.Kind() == parse.Null || sym.Value() == string(lex.False) {
				value = "0"
			} else if sym.Value() == string(lex.True) {
				value = "1"
			} else {
				value = sym.Value()
			}
			directive := dir(sym.SymID(), dirType, value)
			err := write(out, directive)
			if err != nil {
				return err
			}
		}
	}

	// write the number of allocated byts
	free := writeFREE()
	return write(out, free)
}

func (g *Generator) logic(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	ni, err := g.loadSymIDIntoRegister("", R1, q.Operand2())
	if err != nil {
		return err
	}
	instructions = append(instructions, ni...)

	var op Instruction
	switch q.Operator() {
	case icode.AND:
		op = AND
	case icode.OR:
		op = OR
	default:
		return expectedLogical
	}

	i := opnl(op, R0, R1)
	instructions = append(instructions, i)

	loadInstructions, err := g.loadLocation(R3, q.Operand3())
	if err != nil {
		return err
	}
	instructions = append(instructions, loadInstructions...)

	i = opnl(STR, R0, R3)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) cmp(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	ni, err := g.loadSymIDIntoRegister("", R1, q.Operand2())
	if err != nil {
		return err
	}
	instructions = append(instructions, ni...)

	storeLabel := parse.IdGen.GenID()
	trueLabel := parse.IdGen.GenID()

	i := opnl(SUB, R2, R2)
	instructions = append(instructions, i)

	loadInstructions, err := g.loadLocation(R3, q.Operand3())
	if err != nil {
		return err
	}
	instructions = append(instructions, loadInstructions...)

	i = opnl(CMP, R0, R1)
	instructions = append(instructions, i)

	switch q.Operator() {
	case icode.LT:
		i1 := opnl(BLT, R0, trueLabel)
		i2 := opnl(JMP, storeLabel, "")
		instructions = append(instructions, i1, i2)
	case icode.GT:
		i1 := opnl(BGT, R0, trueLabel)
		i2 := opnl(JMP, storeLabel, "")
		instructions = append(instructions, i1, i2)
	case icode.NE:
		i1 := opnl(BNZ, R0, trueLabel)
		i2 := opnl(JMP, storeLabel, "")
		instructions = append(instructions, i1, i2)
	case icode.EQ:
		i1 := opnl(BRZ, R0, trueLabel)
		i2 := opnl(JMP, storeLabel, "")
		instructions = append(instructions, i1, i2)
	case icode.LE:
		i1 := opnl(BLT, R0, trueLabel)
		i2 := opnl(BRZ, R0, trueLabel)
		i3 := opnl(JMP, storeLabel, "")
		instructions = append(instructions, i1, i2, i3)
	case icode.GE:
		i1 := opnl(BGT, R0, trueLabel)
		i2 := opnl(BRZ, R0, trueLabel)
		i3 := opnl(JMP, storeLabel, "")
		instructions = append(instructions, i1, i2, i3)
	default:
		return expectedCmp
	}

	i = op(trueLabel, ADI, R2, one)
	instructions = append(instructions, i)

	i = op(storeLabel, STR, R2, R3)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) branch(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	if q.Operator() == icode.BT {
		i := opnl(ADI, R0, nOne)
		instructions = append(instructions, i)
	}

	i := opnl(BRZ, R0, q.Operand2())
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) mathOp(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	newInstructions, err := g.loadSymIDIntoRegister("", R1, q.Operand2())
	if err != nil {
		return err
	}
	instructions = append(instructions, newInstructions...)

	newInstructions, err = g.loadLocation(R2, q.Operand3())
	if err != nil {
		return err
	}
	instructions = append(instructions, newInstructions...)

	var op Instruction
	switch q.Operator() {
	case icode.ADD, icode.ADDI:
		op = ADD
	case icode.SUB:
		op = SUB
	case icode.MUL:
		op = MUL
	case icode.DIV:
		op = DIV
	default:
		return errors.New("expected math op not " + string(q.Operator()))
	}

	i := opnl(op, R0, R1)
	instructions = append(instructions, i)

	i = opnl(STR, R0, R2)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) ref(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	loc, err := g.getLocation(q.Operand2())
	if err != nil {
		return err
	}

	i := opnl(ADI, R0, strconv.Itoa(*loc.pos))
	instructions = append(instructions, i)

	i = opnl(MOV, R1, FP)
	instructions = append(instructions, i)

	loc, err = g.getLocation(q.Operand3())
	if err != nil {
		return err
	}

	i = opnl(ADI, R1, strconv.Itoa(*loc.pos))
	instructions = append(instructions, i)

	i = opnl(STR, R0, R1)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) aef(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	loadInstructions, err := g.loadSymIDIntoRegister("", R1, q.Operand2())
	if err != nil {
		return err
	}

	instructions = append(instructions, loadInstructions...)

	i := opnl(ADD, R0, R1)
	instructions = append(instructions, i)

	i = opnl(MOV, R1, FP)
	instructions = append(instructions, i)

	loc, err := g.getLocation(q.Operand3())
	if err != nil {
		return err
	}

	i = opnl(ADI, R1, strconv.Itoa(*loc.pos))
	instructions = append(instructions, i)

	i = opnl(STR, R0, R1)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) mov(q icode.Quad, out io.Writer) error {
	var instructions []string
	i := op(q.Label(), MOV, R1, FP)
	instructions = append(instructions, i)

	loadInstructions, err := g.loadLocation(R1, q.Operand2())
	if err != nil {
		return err
	}

	instructions = append(instructions, loadInstructions...)

	loadInstructions, err = g.loadSymIDIntoRegister("", R0, q.Operand1())
	if err != nil {
		return err
	}

	instructions = append(instructions, loadInstructions...)

	var op Instruction
	if g.symbolTable[q.Operand1()].Type() == string(lex.Character) {
		op = STB
	} else {
		op = STR
	}
	i = opnl(op, R0, R1)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) heapAlloc(q icode.Quad, out io.Writer) error {
	var instructions []string

	i := op(q.Label(), MOV, R1, FP)
	instructions = append(instructions, i)

	loc, err := g.getLocation(q.Operand2())
	if err != nil {
		return err
	}
	i = opnl(ADI, R1, strconv.Itoa(*loc.pos))
	instructions = append(instructions, i)

	i = opnl(LDR, R2, freeLABEL)
	instructions = append(instructions, i)

	i = opnl(STR, R2, R1)
	instructions = append(instructions, i)

	switch q.Operator() {
	case icode.NEW:
		loadInstructions, err := g.loadSymIDIntoRegister("", R0, q.Operand1())
		if err != nil {
			return err
		}
		instructions = append(instructions, loadInstructions...)

		i = opnl(ADD, R2, R0)
		instructions = append(instructions, i)
	case icode.NEWI:
		i = opnl(ADI, R2, q.Operand1())
		instructions = append(instructions, i)
	default:
		return expectedNew
	}

	i = opnl(STR, R2, freeLABEL)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) peek(q icode.Quad, out io.Writer) error {
	var instructions []string

	i := op(q.Label(), MOV, R0, SP)
	instructions = append(instructions, i)

	i = opnl(LDR, R0, R0)
	instructions = append(instructions, i)

	loadInstructions, err := g.loadLocation(R1, q.Operand1())
	if err != nil {
		return err
	}

	instructions = append(instructions, loadInstructions...)

	i = opnl(STR, R0, R1)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) read(q icode.Quad, out io.Writer) error {
	var instructions []string
	var code string
	switch q.Operator() {
	case icode.RDI:
		code = "2"
	case icode.RDC:
		code = "4"
	default:
		return expectedRead
	}

	i := op(q.Label(), TRP, code, "")
	instructions = append(instructions, i)

	loadInstructions, err := g.loadLocation(R0, q.Operand1())
	if err != nil {
		return err
	}

	instructions = append(instructions, loadInstructions...)

	i = opnl(STR, R3, R0)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) write(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R3, q.Operand1())
	if err != nil {
		return err
	}

	var i string
	switch q.Operator() {
	case icode.WRTI:
		i = opnl(TRP, "1", "")
	case icode.WRTC:
		i = opnl(TRP, "3", "")
	default:
		return expectedWrt
	}

	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) function(q icode.Quad, out io.Writer) error {
	funcSym := g.symbolTable[q.Operand1()]

	funcSize, ok := funcSym.LocTempCount()
	if !ok {
		return unknownFunctionSize(funcSym.SymID())
	}

	i := op(q.Label(), ADI, SP, "-"+funcSize)

	return write(out, i)
}

func (g *Generator) rtn(q icode.Quad, out io.Writer) error {
	var instructions []string
	var i string
	var err error

	if q.Operator() == icode.RETURN {
		instructions, err = g.loadSymIDIntoRegister(q.Label(), R3, q.Operand1())
		if err != nil {
			return err
		}
		i = opnl(MOV, R0, FP)
		instructions = append(instructions, i)
	} else {
		i = op(q.Label(), MOV, R0, FP)
		instructions = append(instructions, i)
	}

	i = opnl(LDR, R1, R0)
	instructions = append(instructions, i)

	i = opnl(ADI, R0, nFour)
	instructions = append(instructions, i)

	i = opnl(LDR, R2, R0)
	instructions = append(instructions, i)

	i = opnl(MOV, SP, FP)
	instructions = append(instructions, i)

	i = opnl(MOV, FP, R2)
	instructions = append(instructions, i)

	if q.Operator() == icode.RETURN {
		i := opnl(STR, R3, SP)
		instructions = append(instructions, i)
	}

	i = opnl(JMR, R1, "")
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) call(q icode.Quad, out io.Writer) error {
	var instructions []string

	funcSym := g.symbolTable[q.Operand1()]

	defaultSize, ok := funcSym.ParamDefaultCount()
	if !ok {
		return unknownFunctionSize(funcSym.SymID())
	}

	i := op(q.Label(), MOV, FP, SP)
	instructions = append(instructions, i)

	i = opnl(ADI, FP, defaultSize)
	instructions = append(instructions, i)

	i = opnl(MOV, R0, PC)
	instructions = append(instructions, i)

	i = opnl(ADI, R0, pThirtySix)
	instructions = append(instructions, i)

	i = opnl(STR, R0, FP)
	instructions = append(instructions, i)

	i = opnl(JMP, q.Operand1(), "")
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) frame(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R1, q.Operand2())
	if err != nil {
		return err
	}

	i := opnl(MOV, R0, FP)
	instructions = append(instructions, i)

	i = opnl(ADI, SP, nFour)
	instructions = append(instructions, i)

	i = opnl(STR, R0, SP)
	instructions = append(instructions, i)

	i = opnl(ADI, SP, nFour)
	instructions = append(instructions, i)

	i = opnl(STR, R1, SP)
	instructions = append(instructions, i)

	i = opnl(ADI, SP, nFour)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

// reg is the register to load before pushing onto the stack
func (g *Generator) push(q icode.Quad, out io.Writer) error {
	instructions, err := g.loadSymIDIntoRegister(q.Label(), R0, q.Operand1())
	if err != nil {
		return err
	}

	i := opnl(STR, R0, SP)
	instructions = append(instructions, i)

	i = opnl(ADI, SP, nFour)
	instructions = append(instructions, i)

	return write(out, instructions...)
}

func (g *Generator) loadLocation(reg string, symID string) ([]string, error) {
	var instructions []string
	i := opnl(MOV, reg, FP)
	instructions = append(instructions, i)

	loc, err := g.getLocation(symID)
	if err != nil {
		return nil, err
	}

	i = opnl(ADI, reg, strconv.Itoa(*loc.pos))
	instructions = append(instructions, i)

	if g.symbolTable[symID].IndirectAddr() {
		i = opnl(LDR, reg, reg)
		instructions = append(instructions, i)
	}

	return instructions, nil
}

func (g *Generator) getLocation(symID string) (*location, error) {
	sym := g.symbolTable[symID]
	switch {
	case sym.Kind().IsLit():
		return &location{
			locType: Global,
			pos:     nil,
		}, nil
	case sym.Kind().IsStackVar():
		offset, err := sym.GetOffset()
		if err != nil {
			return nil, err
		}

		offset *= -1

		return &location{
			locType: Stack,
			pos:     &offset,
		}, nil
	case sym.Kind().IsHeapVar():
		offset, err := sym.GetOffset()
		if err != nil {
			return nil, err
		}
		return &location{
			locType: Heap,
			pos:     &offset,
		}, nil
	default:
		return nil, parsingError(sym.Value())
	}
}

func (g *Generator) loadSymIDIntoRegister(label string, reg string, symID string) ([]string, error) {
	loc, err := g.getLocation(symID)
	if err != nil {
		return nil, err
	}

	var instructions []string
	var i string
	switch loc.locType {
	case Stack:
		i = op(label, MOV, reg, FP)
		instructions = append(instructions, i)

		i = opnl(ADI, reg, strconv.Itoa(*loc.pos))
		instructions = append(instructions, i)

		if g.symbolTable[symID].Type() == string(lex.Character) && !g.symbolTable[symID].IndirectAddr() {
			i = opnl(LDB, reg, reg)
			instructions = append(instructions, i)
		} else {
			i = opnl(LDR, reg, reg)
			instructions = append(instructions, i)
		}

		if g.symbolTable[symID].IndirectAddr() {
			var op Instruction
			if g.symbolTable[symID].Type() == string(lex.Character) {
				op = LDB
			} else {
				op = LDR
			}
			i = opnl(op, reg, reg)
			instructions = append(instructions, i)
		}
	case Global:
		var instruction Instruction
		if g.symbolTable[symID].Kind() == parse.Character {
			instruction = LDB
		} else {
			instruction = LDR
		}
		i := op(label, instruction, reg, symID)
		instructions = append(instructions, i)
	default:
		return nil, errors.New("I don't think we should get anything other than stack or global variables loaded into regesters because of the way we do refs")
	}

	return instructions, nil
}
