package parse

import (
	"language/compiler/icode"
	"language/compiler/lex"
	"fmt"
	"strconv"
	"strings"
)

const (
	Ivar             Kind = "ivar"
	Lvar             Kind = "lvar"
	Tvar             Kind = "tvar"
	Param            Kind = "param"
	Class            Kind = "class"
	Function         Kind = "function"
	Constructor      Kind = "constructor"
	StaticInitilizer Kind = "staticInitilizer"
	Int              Kind = "int"
	Character        Kind = "char"
	Bool             Kind = "bool"
	Null             Kind = "null"

	Public  AccessModifier = "public"
	Private AccessModifier = "private"

	AccessMod         = "accessMod"
	typeField         = "type"
	returnType        = "returnType"
	paramField        = "params"
	positionField     = "position"
	classSizeField    = "classSize"
	functionSizeField = "functionSize"

	staticInit = ":staticINIT"
)

type AccessModifier string

type Kind string

func (k Kind) IsVarOrLit() bool {
	return k.IsVar() || k.IsLit()
}

func (k Kind) IsVar() bool {
	return k == Ivar ||
		k == Lvar ||
		k == Tvar ||
		k == Param
}

func (k Kind) IsStackVar() bool {
	return k == Lvar ||
		k == Tvar ||
		k == Param
}

func (k Kind) IsHeapVar() bool {
	return k == Ivar
}

func (k Kind) IsLit() bool {
	return k == Int ||
		k == Character ||
		k == Bool ||
		k == Null
}

func (k Kind) IsFunctionOrConstructor() bool {
	return k == Constructor || k == Function
}

func (k Kind) IsFunc() bool {
	return k == Constructor ||
		k == Function ||
		k == StaticInitilizer
}

func (k Kind) KindEqual(kind Kind) bool {
	return (k.IsVar() && kind.IsVar()) ||
		(k == Function && kind == Function) ||
		(k == Constructor && kind == Constructor) ||
		(k == Class && kind == Class)
}

func typeIsEqual(t1 string, t2 string) bool {
	return t1 == t2 ||
		(isArray(t1) && t2 == string(lex.Null)) ||
		(t1 == string(lex.Null) && isArray(t2)) ||
		(isObject(t1) && t2 == string(lex.Null)) ||
		(t1 == string(lex.Null) && isObject(t2))
}

func isObject(t string) bool {
	return t != string(lex.Int) &&
		t != string(lex.Char) &&
		t != string(lex.Bool)
}

type Symbol struct {
	symID              string
	value              string
	scope              Scope
	kind               Kind
	data               map[string]string
	indirectAddressing bool
}

func NewSymbol(symID string, value string, scope Scope, kind Kind, indirectAddressing bool, data map[string]string) Symbol {
	return Symbol{
		symID:              symID,
		value:              value,
		scope:              scope,
		kind:               kind,
		data:               data,
		indirectAddressing: indirectAddressing,
	}
}

func (s Symbol) SymID() string {
	return s.symID
}

func (s Symbol) Value() string {
	return s.value
}

func (s Symbol) Scope() string {
	return string(s.scope)
}

func (s Symbol) Kind() Kind {
	return s.kind
}

func (s Symbol) Data() map[string]string {
	return s.data
}

func (s Symbol) Type() string {
	return s.data[typeField]
}

func (s Symbol) IndirectAddr() bool {
	return s.indirectAddressing
}

// the number returned is the number of temp/local variables
func (s Symbol) LocTempCount() (string, bool) {
	size, ok := s.data[functionSizeField]
	if !ok {
		return "", ok
	}

	i, err := strconv.Atoi(size)
	if err != nil {
		return "", false
	}

	d, ok := s.paramDefaultCount()
	if !ok {
		return "", ok
	}

	return strconv.Itoa(i - d), ok
}

func (s Symbol) paramDefaultCount() (int, bool) {
	params, ok := s.data[paramField]
	if !ok {
		return 0, ok
	}
	if strings.TrimSpace(params) == "" {
		return 12, true
	}
	return 4*strings.Count(params, ",") + 4 + 12, ok
}

// the number of parameters + this + pfp + ra
func (s Symbol) ParamDefaultCount() (string, bool) {
	c, ok := s.paramDefaultCount()
	return strconv.Itoa(c), ok
}

func (s Symbol) GetOffset() (int, error) {
	offsetStr, ok := s.data[positionField]
	if !ok {
		return 0, fmt.Errorf("symbol %s does not have an offset field", s.value)
	}
	return strconv.Atoi(offsetStr)
}

type Parser struct {
	symbolTable            map[string]Symbol
	currentScope           Scope
	lexer                  *lex.Lexer
	secondPass             bool
	opStack                operatorStack
	semanticActionStack    stack
	currentMethodSymID     string
	icode                  icode.Icode
	currentStaticInitSymID string
	staticInitIcode        icode.Icode
}

func NewParser(lexer *lex.Lexer) *Parser {
	return &Parser{
		symbolTable:  map[string]Symbol{},
		currentScope: DefaultScope(),
		lexer:        lexer,
		secondPass:   false,
		opStack: operatorStack{
			stack: stack([]interface{}{}),
		},
		semanticActionStack: stack([]interface{}{}),
		icode:               icode.NewIcode(),
	}
}

func (p *Parser) SetSecondPass(lexer *lex.Lexer) {
	p.secondPass = true
	p.lexer = lexer
}

func (p Parser) Icode() icode.Icode {
	return p.icode
}

func (p *Parser) Parse() error {
	p.lexer.LexToken()
	err := p.compilationUnit()
	if err != nil {
		return err
	}
	tok := p.lexer.NextToken()
	if tok.LexemeType() != lex.EOF {
		return foundExtraToken(tok.Lexeme())
	}
	return nil
}

func (p *Parser) SymbolTable() map[string]Symbol {
	return p.symbolTable
}

func (p *Parser) addSymbol(lexeme string, kind Kind, data map[string]string) string {
	symID := IdGen.GenID()
	p.symbolTable[symID] = NewSymbol(symID, lexeme, p.currentScope, kind, false, data)
	return symID
}

// compute location in stack frame proper location depends on the fact that params
// must be added to the symbol table before the local variables are
func (p *Parser) addParamToSymbolTable(lexeme string, pType string) error {
	loc, err := p.incFunctionSize()
	if err != nil {
		return err
	}
	p.addSymbol(lexeme, Param, map[string]string{
		typeField:     pType,
		AccessMod:     string(Private),
		positionField: loc,
	})
	return nil
}

// compute location in stack frame proper location depends on the fact that params
// must be added to the symbol table before the local variables are
func (p *Parser) addLocalVariableToSymbolTable(lexeme string, lType string) error {
	nextPos, err := p.incFunctionSize()
	if err != nil {
		return err
	}
	p.addSymbol(lexeme, Lvar, map[string]string{
		typeField:     lType,
		AccessMod:     string(Private),
		positionField: nextPos,
	})
	return nil
}

func (p *Parser) addThisToSymbolTable() string {
	return p.addSymbol(string(lex.This), Lvar, map[string]string{
		typeField:     p.currentScope.SplitScope()[1], // g.<class name>.<member that needs this>
		AccessMod:     string(Private),
		positionField: "8",
	})
}

func (p *Parser) getThis(scope Scope) (Symbol, bool) {
	for _, sym := range p.symbolTable {
		if sym.scope == scope && sym.kind == Lvar && sym.value == string(lex.This) {
			return sym, true
		}
	}
	return Symbol{}, false
}

func (p *Parser) classExists(className string) bool {
	for _, s := range p.symbolTable {
		if s.kind == Class && s.value == className && string(s.scope) == global {
			return true
		}
	}
	return false
}

// compute location in stack frame
func (p *Parser) addTemporaryVariableToSymbolTable(lType string) (string, error) {
	nextPos, err := p.incFunctionSize()
	if err != nil {
		return "", err
	}
	symID := IdGen.GenID()
	p.symbolTable[symID] = NewSymbol(symID, symID, p.currentScope, Tvar, false, map[string]string{
		typeField:     lType,
		AccessMod:     string(Private),
		positionField: nextPos,
	})
	return symID, nil
}

func (p *Parser) addTemporaryIndirectAddressingVariableToSymbolTable(lType string) (string, error) {
	nextPos, err := p.incFunctionSize()
	if err != nil {
		return "", err
	}
	symID := IdGen.GenID()
	p.symbolTable[symID] = NewSymbol(symID, symID, p.currentScope, Tvar, true, map[string]string{
		typeField:     lType,
		AccessMod:     string(Private),
		positionField: nextPos,
	})
	return symID, nil
}

func (p *Parser) addInstanceVariableToSymbolTable(lexeme string, iType string, accessMod AccessModifier) error {
	// find the position in the class. Its actual offset would be position * 4.
	// everything is 4 bytes.
	if !p.scopeIsInClass(p.currentScope) {
		return addInstanceVariableOutsideClass(iType, lexeme, accessMod)
	}
	scopeLevels := p.currentScope.SplitScope()
	classSymID, err := p.findClass(scopeLevels[1])
	if err != nil {
		return err
	}

	classSizeStr := p.symbolTable[classSymID].data[classSizeField]
	classSize, err := strconv.Atoi(classSizeStr)
	if err != nil {
		return err
	}

	if iType == string(lex.Char) {
		classSize++
	} else {
		classSize += 4
	}

	p.symbolTable[classSymID].data[classSizeField] = strconv.Itoa(classSize)

	p.addSymbol(lexeme, Ivar, map[string]string{
		typeField:     iType,
		AccessMod:     string(accessMod),
		positionField: classSizeStr,
	})
	return nil
}

// uses the current scope to get the function
func (p *Parser) incFunctionSize() (string, error) {
	sym, ok := p.symbolTable[p.currentMethodSymID]
	if !ok {
		return "", notInFunction
	}

	if !sym.kind.IsFunc() {
		return "", notInFunction
	}

	size, ok := sym.data[functionSizeField]
	if !ok {
		size = "0"
	}
	sizeInt, err := strconv.Atoi(size)
	if err != nil {
		return "", err
	}
	sizeInt += 4
	p.symbolTable[p.currentMethodSymID].data[functionSizeField] = strconv.Itoa(sizeInt)
	return size, nil
}

func (p *Parser) addMethodToSymbolTable(lexeme string, rt string, accessMod AccessModifier) string {
	return p.addSymbol(lexeme, Function, map[string]string{
		returnType:        rt,
		paramField:        "",
		functionSizeField: "12", // this is the number of temp/local variables
		AccessMod:         string(accessMod),
	})
}

func (p *Parser) addStaticInitilizerToSymbolTable(className string) string {
	symID := IdGen.GenID()
	s := p.currentScope
	fn := className + staticInit
	p.symbolTable[symID] = NewSymbol(symID, fn, s, StaticInitilizer, false, map[string]string{
		returnType:        string(lex.Void),
		paramField:        "",
		AccessMod:         string(Private),
		functionSizeField: "12",
	})
	return symID
}

func (p *Parser) addConstructorToSymbolTable(lexeme string) string {
	return p.addSymbol(lexeme, Constructor, map[string]string{
		returnType:        lexeme,
		AccessMod:         string(Public),
		functionSizeField: "12",
	})
}

func (p *Parser) findConstructorByClassName(class string) string {
	scope := NewScope(class)
	for _, val := range p.symbolTable {
		if val.scope == scope && val.value == class && val.kind == Constructor {
			return val.symID
		}
	}
	return ""
}

func (p *Parser) findClass(className string) (string, error) {
	var classSymID string
	for _, v := range p.symbolTable {
		if v.kind == Class && v.value == className {
			classSymID = v.symID
			break
		}
	}

	if classSymID == "" {
		return "", classNotFound(className)
	}
	return classSymID, nil
}

func (p *Parser) addClassToSymbolTable(lexeme string) {
	p.addSymbol(lexeme, Class, map[string]string{
		classSizeField: "0",
	})
}

// need to add error handling for if the wrong kind is returned
func (p *Parser) addLiteralToSymbolTable(lexeme string, kind Kind) string {
	if p.findLiteral(lexeme) != "" {
		return ""
	}

	symID := IdGen.GenID()
	p.symbolTable[symID] = NewSymbol(symID, lexeme, DefaultScope(), kind, false, map[string]string{
		typeField: string(kind),
		AccessMod: string(Public),
	})
	return symID
}

func (p *Parser) findLiteral(val string) string {
	// because "3" and "+3" are the same
	if val[0] == '+' {
		val = val[1:]
	}
	for _, v := range p.symbolTable {
		if v.value == val && v.scope == global {
			return v.symID
		}
	}
	return ""
}

func (p *Parser) findLocVar(id string, scope Scope, fixScope bool) *Symbol {
	for {
		for _, v := range p.symbolTable {
			if v.value == id && v.scope == scope && v.kind.IsVarOrLit() {
				return &v
			}
		}
		if scope.IsGlobalScope() || fixScope {
			break
		} else {
			scope.PopScope()
		}
	}
	return nil
}

// should only be called when you are parsing member variables
func (p *Parser) findStaticInitThisSymID() string {
	for _, sym := range p.symbolTable {
		if sym.scope == p.currentScope && sym.value == string(lex.This) {
			return sym.symID
		}
	}
	return ""
}

func (p *Parser) findFunction(name string, scope Scope, params []string) *Symbol {
	// this ensures that the proper scope is used when searching for a function
	// when doing an iExist then the scope where the function is found is used
	// when doing an rExist then the scope is specified by the type of the ref object
	if scope == DefaultScope() {
		if !(name == string(lex.Main) && params == nil) {
			return nil
		}
	} else if scope.Depth() != 2 {
		err := scope.PopScope()
		if err != nil {
			return nil
		}
	}
	return p.findFunctionOrConstructor(name, scope, params, Function)
}

func (p *Parser) findConstructor(name string, scope Scope, params []string) *Symbol {
	return p.findFunctionOrConstructor(name, scope, params, Constructor)
}

func (p *Parser) findFunctionOrConstructor(name string, scope Scope, params []string, kind Kind) *Symbol {
	// return early if bad kind is returned
	if !kind.IsFunctionOrConstructor() {
		return nil
	}
symbolTableLoop:
	for _, v := range p.symbolTable {
		// functions and constructors are the same sar
		if v.value == name && v.scope == scope && v.kind == kind {
			var paramSysIDs []string
			if v.data[paramField] == "" {
				paramSysIDs = []string{}
			} else {
				paramSysIDs = strings.Split(v.data[paramField], ",")
			}

			if len(paramSysIDs) != len(params) {
				continue symbolTableLoop
			}

			for i := 0; i < len(paramSysIDs); i++ {
				paramDeclType := p.symbolTable[paramSysIDs[i]].data[typeField]
				paramPassedType := p.symbolTable[params[i]].data[typeField]
				if !typeIsEqual(paramDeclType, paramPassedType) {
					continue symbolTableLoop
				}
			}

			return &v
		}
	}
	return nil
}

func (p *Parser) scopeIsInClass(scope Scope) bool {
	// the second level in the scope is guaranteed to be the class name because of the
	// grammar and the way scopes are represented
	// g.<class name>.<member>
	levels := scope.SplitScope()
	if len(levels) == 1 {
		return false
	}

	for _, s := range p.symbolTable {
		if s.kind == Class && s.value == levels[1] {
			return true
		}
	}
	return false
}

func (p *Parser) findStaticInitilizerSym(className string) (*Symbol, error) {
	scope := NewScope(className)
	for _, sym := range p.symbolTable {
		if sym.scope == scope && sym.kind == StaticInitilizer {
			return &sym, nil
		}
	}
	return nil, staticInitilizerNotFound(className)
}

func (p *Parser) addRef(refSymID string, symID string) (string, error) {
	sym := p.symbolTable[symID]
	tSymID, err := p.addTemporaryIndirectAddressingVariableToSymbolTable(sym.data[typeField])
	if err != nil {
		return "", err
	}
	quad := icode.NewQuad("", icode.REF, p.symbolTable[refSymID].symID, symID, tSymID, "")
	p.icode.AddNewQuad(quad)

	return tSymID, nil

}

func (p *Parser) addAef(indexSymID string, symID string) (string, error) {
	sym := p.symbolTable[symID]
	tSymID, err := p.addTemporaryIndirectAddressingVariableToSymbolTable(stripBrackets(sym.data[typeField]))
	if err != nil {
		return "", err
	}

	if stripBrackets(p.symbolTable[symID].data[typeField]) != string(lex.Char) {
		tIndexSymID, err := p.addTemporaryVariableToSymbolTable(string(lex.Int))
		if err != nil {
			return "", err
		}

		op, err := newOperator("*", 0)
		if err != nil {
			return "", err
		}

		fourLit := p.findLiteral("4")
		if fourLit == "" {
			fourLit = p.addLiteralToSymbolTable("4", Int)
			if err != nil {
				return "", err
			}
		}

		err = p.addOpQuad(op, fourLit, indexSymID, tIndexSymID)
		if err != nil {
			return "", err
		}
		indexSymID = tIndexSymID
	}

	quad := icode.NewQuad("", icode.AEF, symID, indexSymID, tSymID, "")
	p.icode.AddNewQuad(quad)

	return tSymID, nil
}

func (p *Parser) addMovQuad(lOperand interface{}, rOperand interface{}) error {
	lSymID, ok := getSymIDOfSAR(lOperand)
	if !ok {
		return invalidSarMOV
	}

	rSymID, ok := getSymIDOfSAR(rOperand)
	if !ok {
		return invalidSarMOV
	}

	var instruction icode.IInstruction
	k := p.symbolTable[rSymID].kind
	if k.IsLit() {
		if k == Character {
			instruction = icode.MOVC
		} else {
			instruction = icode.MOVI
		}
	} else {
		instruction = icode.MOV
	}

	c := icode.MovComment(p.symbolTable[rSymID].value, p.symbolTable[lSymID].value)
	quad := icode.NewQuad("", instruction, rSymID, lSymID, "", c)
	p.icode.AddNewQuad(quad)

	return nil
}

func (p *Parser) addOpQuad(op operator, lSymID string, rSymID string, target string) error {
	lSym := p.symbolTable[lSymID]
	rSym := p.symbolTable[rSymID]
	tSym := p.symbolTable[target]
	comment := icode.OpComment(op.op, lSym.value, rSym.value, tSym.value)

	// addi needs special care
	if op.op == "+" {
		if lSym.kind == Int {
			quad := icode.NewQuad("", icode.ADDI, rSymID, lSym.symID, target, comment)
			p.icode.AddNewQuad(quad)
			return nil
		} else if rSym.kind == Int {
			quad := icode.NewQuad("", icode.ADDI, lSymID, rSym.symID, target, comment)
			p.icode.AddNewQuad(quad)
			return nil
		}
	}

	var icodeOp icode.IInstruction
	switch op.op {
	case "+":
		icodeOp = icode.ADD
	case "-":
		icodeOp = icode.SUB
	case "/":
		icodeOp = icode.DIV
	case "*":
		icodeOp = icode.MUL
	case "==":
		icodeOp = icode.EQ
	case ">=":
		icodeOp = icode.GE
	case "<=":
		icodeOp = icode.LE
	case ">":
		icodeOp = icode.GT
	case "<":
		icodeOp = icode.LT
	case "!=":
		icodeOp = icode.NE
	case "and":
		icodeOp = icode.AND
	case "or":
		icodeOp = icode.OR
	default:
		return invalidOperator(op.lineNum, op.op)
	}
	quad := icode.NewQuad("", icodeOp, lSymID, rSymID, target, comment)
	p.icode.AddNewQuad(quad)
	return nil
}

func (p *Parser) addBranch(boolean string, jmpLabel string) {
	comment := icode.BranchComment(boolean, jmpLabel, false)
	quad := icode.NewQuad("", icode.BF, boolean, jmpLabel, "", comment)
	p.icode.AddNewQuad(quad)
}

func (p *Parser) addJmp(jmpLabel string) {
	quad := icode.NewQuad("", icode.JMP, jmpLabel, "", "", "") // I think that the icode is self explanatory
	p.icode.AddNewQuad(quad)
}

func (p *Parser) addFunctionCall(sym *Symbol, refSymID string, args []string, retSymID string) {
	c := icode.FrameComment(sym.value, refSymID)
	q := icode.NewQuad("", icode.FRAME, sym.symID, refSymID, "", c)
	p.icode.AddNewQuad(q)
	for _, symID := range args {
		c := icode.PushComment(p.symbolTable[symID].value)
		q = icode.NewQuad("", icode.PUSH, symID, "", "", c)
		p.icode.AddNewQuad(q)
	}
	c = icode.CallComment(sym.value)
	q = icode.NewQuad("", icode.CALL, sym.symID, "", "", c)
	p.icode.AddNewQuad(q)
	if sym.kind != StaticInitilizer {
		q = icode.NewQuad("", icode.PEEK, retSymID, "", "", "")
		p.icode.AddNewQuad(q)
	}
}

func (p *Parser) addNewObj(className string, constructorSym *Symbol, argList []string) error {
	classSymID, err := p.findClass(className)
	if err != nil {
		return err
	}
	size := p.symbolTable[classSymID].data[classSizeField]

	objRefSymID, err := p.addTemporaryVariableToSymbolTable(className)
	if err != nil {
		return err
	}

	c := icode.NewIComment(className, objRefSymID)
	q := icode.NewQuad("", icode.NEWI, size, objRefSymID, "", c)
	p.icode.AddNewQuad(q)

	retSymID, err := p.addTemporaryVariableToSymbolTable(constructorSym.data[returnType])
	if err != nil {
		return err
	}

	p.addFunctionCall(constructorSym, objRefSymID, argList, retSymID)

	p.semanticActionStack.push(identifierSAR{
		id:     retSymID,
		symID:  retSymID,
		scope:  p.currentScope,
		idType: className,
	})

	return nil
}
