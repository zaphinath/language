package codegen

import (
	"fmt"
	"strconv"
)

type (
	Instruction string
	Directive   string
)

const (
	freeLABEL  = "FREE"
	nOne       = "-1"
	one        = "1"
	nFour      = "-4"
	four       = "4"
	pThirtySix = "36"

	JMP Instruction = "jmp"
	JMR Instruction = "jmr"
	BNZ Instruction = "bnz"
	BGT Instruction = "bgt"
	BLT Instruction = "blt"
	BRZ Instruction = "brz"
	MOV Instruction = "mov"
	LDA Instruction = "lda"
	STR Instruction = "str"
	LDR Instruction = "ldr"
	STB Instruction = "stb"
	LDB Instruction = "ldb"
	ADD Instruction = "add"
	ADI Instruction = "adi"
	SUB Instruction = "sub"
	MUL Instruction = "mul"
	DIV Instruction = "div"
	AND Instruction = "and"
	OR  Instruction = "or"
	CMP Instruction = "cmp"
	TRP Instruction = "trp"

	INT Directive = ".int"
	BYT Directive = ".byt"

	R0 = "R0"
	R1 = "R1"
	R2 = "R2"
	R3 = "R3"
	R4 = "R4"
	R5 = "R5"
	R6 = "R6"
	R7 = "R7"
	FP = "FP"
	SP = "SP"
	PC = "PC"
	SL = "SL"
)

// keeps track of how many bytes of opcodes and data was written out
var (
	bytsAllocated = 0
)

func op(label string, instruction Instruction, op1 string, op2 string) string {
	bytsAllocated += 12
	return fmt.Sprintf("%s %s %s %s", label, instruction, op1, op2)
}

func opnl(instruction Instruction, op1 string, op2 string) string {
	return op("", instruction, op1, op2)
}

func writeFREE() string {
	bytsAllocated += 4
	return fmt.Sprintf("%s %s %s", freeLABEL, INT, strconv.Itoa(bytsAllocated))
}

func dir(label string, dir Directive, val string) string {
	if dir == BYT {
		bytsAllocated += 1
	} else {
		bytsAllocated += 4
	}
	return fmt.Sprintf("%s %s %s", label, dir, val)
}

func dirln(direc Directive, val string) string {
	return dir("", direc, val)
}
