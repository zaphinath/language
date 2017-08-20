package parse

import (
	"language/compiler/icode"
	"language/compiler/lex"
	"fmt"
)

type (
	identifierSAR struct {
		id      string
		symID   string
		scope   Scope
		idType  string
		lineNum uint64
	}

	newObjSAR struct {
		typeID    string
		arguments argumentList
		symID     string
	}

	newArraySAR struct {
		typeID    string
		sizeSymID string
		symID     string
	}

	arrayIndexSAR struct {
		id         string
		symID      string // symid of the array. it is local or in a object
		scope      Scope
		typeID     string
		lineNum    uint64
		indexSymID string // symid of the indexing variable
		outSymID   string // symid of the temp variable to write to
	}

	functionSAR struct {
		id         string
		symID      string // symid of the variable to return to
		returnType string
		scope      Scope
		lineNum    uint64
		arguments  argumentList
		outSymID   string // param to return to
	}

	// placeholder to put on the sar to indicate the beginning of an argument list.
	paramListMarker struct{}

	// order matters
	argumentList struct {
		argSysIDs []string
	}

	thisSAR struct {
		lineNum uint64
		scope   Scope
		symID   string
	}

	literalSAR struct {
		symID string
	}

	typeSAR struct {
		typeId  string
		lineNum uint64
	}
)

func (p *Parser) functionStringRep(f functionSAR) string {
	strRep := ""
	strRep += f.returnType
	strRep += " "
	strRep += f.id
	strRep += "("

	for i := 0; i < len(f.arguments.argSysIDs); i++ {
		sym := p.symbolTable[f.arguments.argSysIDs[i]]
		strRep += sym.data[typeField]
		strRep += " "
		strRep += sym.value
		if i != len(f.arguments.argSysIDs)-1 {
			strRep += ", "
		}
	}

	strRep += ")"

	return strRep
}

func (p *Parser) iPush(id string, scope Scope, lineNum uint64) {
	p.semanticActionStack.push(identifierSAR{
		id:      id,
		scope:   scope,
		lineNum: lineNum,
	})
}

func (p *Parser) pushParamListMarker() {
	p.semanticActionStack.push(paramListMarker{})
}

func (p *Parser) paramListAction() error {
	argList := argumentList{
		argSysIDs: []string{},
	}

	for {
		sar, ok := p.semanticActionStack.peekPop()
		if !ok {
			return noIdentifierOnStack
		}

		if _, ok := sar.(paramListMarker); ok {
			break
		}

		symID, ok := getSymIDOfSAR(sar)
		if !ok {
			return noIdentifierOnStack
		}
		argList.argSysIDs = append([]string{
			symID,
		}, argList.argSysIDs...)
	}

	p.semanticActionStack.push(argList)
	return nil
}

func (p *Parser) commaAction() error {
	for {
		op, ok := p.opStack.peek()
		if !ok {
			return expectedOperatorOnStack
		}
		if op.op == lex.OpenParen {
			break
		}
		err := p.evalOperator()
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *Parser) funcAction() error {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	argListSAR, ok := sar.(argumentList)
	if !ok {
		return expectedArgListSAR
	}

	sar, ok = p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	idSAR, ok := sar.(identifierSAR)
	if !ok {
		return expectedFuncSAR
	}

	p.semanticActionStack.push(functionSAR{
		id:        idSAR.id,
		scope:     idSAR.scope,
		lineNum:   idSAR.lineNum,
		arguments: argListSAR,
	})

	return nil
}

func (p *Parser) newObj() error {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	argListSAR, ok := sar.(argumentList)
	if !ok {
		return expectedArgListSAR
	}

	sar, ok = p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	tSAR, ok := sar.(typeSAR)
	if !ok {
		return expectedTypeSAR
	}

	scope := NewScope(tSAR.typeId)
	sym := p.findConstructor(tSAR.typeId, scope, argListSAR.argSysIDs)

	if sym == nil {
		return constructorNotFound(tSAR.lineNum, tSAR.typeId)
	}

	return p.addNewObj(tSAR.typeId, sym, argListSAR.argSysIDs)
}

func (p *Parser) newArray(ln uint64) error {
	var err error
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}

	symID, ok := getSymIDOfSAR(sar)
	if !ok {
		return sarDidNotHaveSymID
	}

	sym := p.symbolTable[symID]
	if sym.data[typeField] != string(lex.Int) {
		return expectedType(ln, string(lex.Int), string(sym.data[typeField]))
	}

	sar, ok = p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}

	typeSAR, ok := sar.(typeSAR)
	if !ok {
		return expectedTypeSAR
	}

	var sizeSymID string
	if p.secondPass {
		if typeSAR.typeId != string(lex.Char) {
			sizeSymID, err = p.addTemporaryVariableToSymbolTable(string(lex.Int))
			if err != nil {
				return err
			}
			fourSymID := p.findLiteral("4")
			if fourSymID == "" {
				fourSymID = p.addLiteralToSymbolTable("4", Int)
			}
			op, err := newOperator("*", ln)
			if err != nil {
				return err
			}

			err = p.addOpQuad(op, symID, fourSymID, sizeSymID)
			if err != nil {
				return err
			}
		} else {
			sizeSymID = symID
		}
	}

	arrType := makeArrayType(typeSAR.typeId)
	arrPtrSymID, err := p.addTemporaryVariableToSymbolTable(arrType)
	if err != nil {
		return err
	}

	c := icode.NewArrayComment(p.symbolTable[sizeSymID].value, typeSAR.typeId, arrPtrSymID)
	q := icode.NewQuad("", icode.NEW, sizeSymID, arrPtrSymID, "", c)
	p.icode.AddNewQuad(q)

	p.semanticActionStack.push(identifierSAR{
		id:     arrPtrSymID,
		symID:  arrPtrSymID,
		idType: arrType,
		scope:  p.currentScope,
	})

	return nil
}

func (p *Parser) cd(className string, ln uint64) error {
	scopes := p.currentScope.SplitScope()
	// meh we will panic because this should never be incorrect anyway.
	if scopes[1] != className {
		return constructorNotInClass(scopes[1], className, ln)
	}
	return nil
}

func (p *Parser) getThisAndRef(symbol *Symbol) (string, error) {
	thisSym, ok := p.getThis(p.currentScope)
	if !ok {
		return "", thisSymbolNotFound(p.currentScope)
	}
	tSymID, err := p.addRef(thisSym.symID, symbol.symID)
	if err != nil {
		return "", err
	}
	return tSymID, nil
}

func (p *Parser) iPushThis(scope Scope) {
	p.semanticActionStack.push(thisSAR{
		scope: scope,
	})
}

// function
// array
// variable
// this
func (p *Parser) iExists(ln uint64) error {
	top, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}

	if idSAR, ok := top.(identifierSAR); ok {
		symbol := p.findLocVar(idSAR.id, idSAR.scope, false)
		if symbol == nil {
			return variableNotDefined(idSAR.lineNum, idSAR.id)
		}

		if symbol.kind == Ivar {
			tSymID, err := p.getThisAndRef(symbol)
			if err != nil {
				return err
			}
			idSAR.symID = tSymID
		} else {
			idSAR.symID = symbol.symID
		}
		idSAR.idType = symbol.data[typeField]
		p.semanticActionStack.push(idSAR)
		return nil
	}

	if arrSAR, ok := top.(arrayIndexSAR); ok {
		symbol := p.findLocVar(arrSAR.id, arrSAR.scope, false)
		if symbol == nil || !isArray(symbol.data[typeField]) {
			return arrayNotDefined(arrSAR.lineNum, arrSAR.id)
		}

		if symbol.kind == Ivar {
			tSymID, err := p.getThisAndRef(symbol)
			if err != nil {
				return err
			}
			arrSAR.symID = tSymID
		} else {
			arrSAR.symID = symbol.symID
		}

		tSymID, err := p.addAef(arrSAR.indexSymID, arrSAR.symID)
		if err != nil {
			return err
		}

		p.semanticActionStack.push(identifierSAR{
			id:     tSymID,
			symID:  tSymID,
			scope:  p.currentScope,
			idType: stripBrackets(symbol.data[typeField]),
		})

		return nil
	}

	if funcSAR, ok := top.(functionSAR); ok {
		symbol := p.findFunction(funcSAR.id, funcSAR.scope, funcSAR.arguments.argSysIDs)
		if symbol == nil {
			types := make([]string, len(funcSAR.arguments.argSysIDs))
			for i, v := range funcSAR.arguments.argSysIDs {
				types[i] = p.symbolTable[v].data[typeField]
			}
			return functionNotDefined(funcSAR.lineNum, funcSAR.id, types)
		}

		funcSAR.returnType = symbol.data[returnType]
		// make temp var for function
		var err error
		funcSAR.outSymID, err = p.addTemporaryVariableToSymbolTable(funcSAR.returnType)
		if err != nil {
			return err
		}
		funcSAR.symID = symbol.symID
		thisSymbol, ok := p.getThis(p.currentScope)
		if !ok {
			return thisSymbolNotFound(p.currentScope)
		}
		p.addFunctionCall(symbol, thisSymbol.symID, funcSAR.arguments.argSysIDs, funcSAR.outSymID)
		p.semanticActionStack.push(funcSAR)
		return nil
	}

	if thisSAR, ok := top.(thisSAR); ok {
		if thisSAR.scope == mainScope {
			return thisNotDefinedInMain
		}

		sym, ok := p.getThis(thisSAR.scope)
		if !ok {
			return notInClassError(thisSAR.lineNum, string(lex.This))
		}
		thisSAR.symID = sym.symID
		p.semanticActionStack.push(thisSAR)
		return nil
	}

	return invalidTypeForIExist(ln)
}

func (p *Parser) lPush(value string) error {
	symId := p.findLiteral(value)
	if symId == "" {
		return litNotAddedToTable(value)
	}
	p.semanticActionStack.push(literalSAR{
		symID: symId,
	})
	return nil
}

func getSymIDOfSAR(sar interface{}) (string, bool) {
	var symID string
	ok := true
	switch assertedSAR := sar.(type) {
	case identifierSAR:
		symID = assertedSAR.symID
	case literalSAR:
		symID = assertedSAR.symID
	case functionSAR:
		symID = assertedSAR.outSymID
	case arrayIndexSAR:
		symID = assertedSAR.outSymID
	case newObjSAR:
		symID = assertedSAR.symID
	case newArraySAR:
		symID = assertedSAR.symID
	default:
		symID = ""
		ok = false
	}
	return symID, ok
}

func (p *Parser) rExist(ln uint64) error {
	refsar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}

	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	var scope Scope
	var refSymID string
	var thisRef bool
	if thisSAR, ok := sar.(thisSAR); ok {
		levels := thisSAR.scope.SplitScope()
		scope = NewScope(levels[1])
		thisRef = true
		refSymID = thisSAR.symID
	} else {
		objSymID, ok := getSymIDOfSAR(sar)
		if !ok {
			return invalidTypeForRef(ln, p.symbolTable[objSymID].data[typeField])
		}
		objSym := p.symbolTable[objSymID]
		if objSym.kind.IsVar() && p.classExists(objSym.data[typeField]) {
			thisRef = false
			refSymID = objSym.symID
			scope = NewScope(objSym.data[typeField])
		} else {
			return invalidTypeForRef(ln, objSym.data[typeField])
		}
	}

	// identifier
	// array
	// function
	switch assertedSAR := refsar.(type) {
	case identifierSAR:
		// make sure that arrays are handled properly
		sym := p.findLocVar(assertedSAR.id, scope, true)
		if sym == nil {
			return memberNotDefined(ln, assertedSAR.id, scope.SplitScope()[1])
		}
		if !thisRef && sym.data[AccessMod] == string(Private) {
			return memberNotPublic(ln, assertedSAR.id, scope.SplitScope()[1])
		}
		tSymID, err := p.addRef(refSymID, sym.symID)
		if err != nil {
			return err
		}
		p.semanticActionStack.push(identifierSAR{
			symID:  tSymID,
			idType: sym.data[typeField],
			scope:  assertedSAR.scope,
			id:     tSymID,
		})
	case functionSAR:
		sym := p.findFunction(assertedSAR.id, scope, assertedSAR.arguments.argSysIDs)
		if sym == nil {
			return memberNotDefined(ln, p.functionStringRep(assertedSAR), scope.SplitScope()[1])
		}

		if !thisRef && sym.data[AccessMod] == string(Private) {
			return memberNotPublic(ln, p.functionStringRep(assertedSAR), scope.SplitScope()[1])
		}

		assertedSAR.symID = sym.symID
		assertedSAR.returnType = sym.data[returnType]

		var err error
		assertedSAR.outSymID, err = p.addTemporaryVariableToSymbolTable(assertedSAR.returnType)
		if err != nil {
			return err
		}

		// call function
		p.addFunctionCall(sym, refSymID, assertedSAR.arguments.argSysIDs, assertedSAR.outSymID)

		p.semanticActionStack.push(assertedSAR)
	case arrayIndexSAR:
		sym := p.findLocVar(assertedSAR.id, scope, true)
		if sym == nil || !isArray(sym.data[typeField]) {
			return memberNotDefined(ln, assertedSAR.id, scope.SplitScope()[1])
		}

		if !thisRef && sym.data[AccessMod] == string(Private) {
			return memberNotPublic(ln, assertedSAR.id, scope.SplitScope()[1])
		}

		rSymID, err := p.addRef(refSymID, sym.symID)
		if err != nil {
			return err
		}

		tSymID, err := p.addAef(assertedSAR.indexSymID, rSymID)
		if err != nil {
			return err
		}

		p.semanticActionStack.push(identifierSAR{
			id:     tSymID,
			symID:  tSymID,
			scope:  p.currentScope,
			idType: stripBrackets(sym.data[typeField]),
		})
	default:
		return invalidMemberType(ln)
	}

	return nil
}

func (p *Parser) oPush(opStr string, ln uint64) error {
	op1, err := newOperator(opStr, ln)
	if err != nil {
		return err
	}

	if op1.op == lex.OpenParen || op1.op == lex.OpenBracket {
		p.opStack.push(op1)
	} else if op1.op == lex.CloseParen || op1.op == lex.CloseBracket {
		var close string
		if op1.op == lex.CloseParen {
			close = lex.OpenParen
		} else {
			close = lex.OpenBracket
		}
		for {
			op2, ok := p.opStack.peek()
			if !ok {
				return expectedOperator
			} else if op2.op == close {
				p.opStack.pop()
				return nil
			} else {
				err := p.evalOperator()
				if err != nil {
					return err
				}
			}
		}
	} else {
		for {
			op2, ok := p.opStack.peek()
			if !ok {
				break
			}
			if op1.precedenceVal <= op2.precedenceVal {
				err = p.evalOperator()
				if err != nil {
					return err
				}
			} else {
				break
			}
		}
		p.opStack.push(op1)
	}
	return nil
}

func (p *Parser) evalOperator() error {
	op, ok := p.opStack.peekPop()
	if !ok {
		return expectedOperator
	}

	rOperand, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedOperand
	}

	lOperand, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedOperand
	}

	rVal, rType, err := p.getDataFromSAR(rOperand)
	if err != nil {
		return err
	}
	rSymID, ok := getSymIDOfSAR(rOperand)
	if !ok {
		return sarDidNotHaveSymID
	}

	lVal, lType, err := p.getDataFromSAR(lOperand)
	if err != nil {
		return err
	}
	lSymID, ok := getSymIDOfSAR(lOperand)
	if !ok {
		return sarDidNotHaveSymID
	}

	switch op.op {
	case "=":
		if _, ok := lOperand.(literalSAR); ok {
			return cannotAssignToLiteral(op.lineNum, lVal, op.op, rVal)
		}

		// we can assign null to a class or array
		if !typeIsEqual(lType, rType) {
			return typeMismatchError(op.lineNum, lVal, op.op, rVal)
		}

		return p.addMovQuad(lOperand, rOperand)
	case "*", "+", "-", "/":
		if lType == string(lex.Int) && rType == string(lex.Int) {
			tempSymID, err := p.addTemporaryVariableToSymbolTable(lType)
			if err != nil {
				return err
			}
			symbol := p.symbolTable[tempSymID]
			p.semanticActionStack.push(identifierSAR{
				id:     symbol.value,
				symID:  symbol.symID,
				scope:  symbol.scope,
				idType: symbol.data[typeField],
			})

			return p.addOpQuad(op, lSymID, rSymID, tempSymID)
		}

		return typeMismatchError(op.lineNum, lVal, op.op, rVal)
	case "==", ">=", "<=", ">", "<", "!=":
		if lType == rType ||
			(rType == string(lex.Null) && (p.classExists(lType) || isArray(lType))) ||
			(lType == string(lex.Null) && (p.classExists(rType) || isArray(rType))) {
			tempSymID, err := p.addTemporaryVariableToSymbolTable(string(lex.Bool))
			if err != nil {
				return err
			}
			symbol := p.symbolTable[tempSymID]
			p.semanticActionStack.push(identifierSAR{
				id:     symbol.value,
				symID:  symbol.symID,
				scope:  symbol.scope,
				idType: string(lex.Bool),
			})
			return p.addOpQuad(op, lSymID, rSymID, tempSymID)
		}

		return typeMismatchError(op.lineNum, lVal, op.op, rVal)
	case "and", "or":
		if lType == string(lex.Bool) && rType == string(lex.Bool) {
			tempSymID, err := p.addTemporaryVariableToSymbolTable(lType)
			if err != nil {
				return err
			}
			symbol := p.symbolTable[tempSymID]
			p.semanticActionStack.push(identifierSAR{
				id:     symbol.value,
				symID:  symbol.symID,
				scope:  symbol.scope,
				idType: string(lex.Bool),
			})
			return p.addOpQuad(op, lSymID, rSymID, tempSymID)
		}

		return typeMismatchError(op.lineNum, lVal, op.op, rVal)
	default:
		return invalidOperator(op.lineNum, op.op)
	}
}

// value, type
func (p *Parser) getDataFromSAR(sar interface{}) (string, string, error) {
	switch assertedSAR := sar.(type) {
	case identifierSAR:
		val := assertedSAR.idType + " " + assertedSAR.id
		return val, assertedSAR.idType, nil
	case literalSAR:
		symbol := p.symbolTable[assertedSAR.symID]
		val := string(symbol.kind) + " " + symbol.value
		return val, string(symbol.kind), nil
	case functionSAR:
		typeStr := p.functionStringRep(assertedSAR)
		return typeStr, assertedSAR.returnType, nil
	case newObjSAR:
		return "new " + assertedSAR.typeID, assertedSAR.typeID, nil
	case newArraySAR:
		typeID := fmt.Sprintf("new %s[]", assertedSAR.typeID)
		asArray := makeArrayType(assertedSAR.typeID)
		return typeID, asArray, nil
	case arrayIndexSAR:
		val := fmt.Sprintf("%s %s[%s]", assertedSAR.typeID, assertedSAR.id, p.symbolTable[assertedSAR.indexSymID].value)
		return val, assertedSAR.typeID, nil
	default:
		return "", "", expectedIdLitOrFuncSAR
	}
}

func (p *Parser) isKind(ln uint64, statementKind string, statementType string) (string, error) {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return "", invalidExpressionForStatement(ln, statementKind)
	}
	_, typeStr, err := p.getDataFromSAR(sar)
	if err != nil {
		return "", err
	}

	if !typeIsEqual(typeStr, statementType) {
		if statementKind == string(lex.Return) {
			return "", invalidReturn(ln, statementType, typeStr)
		}
		return "", incorrectTypeForStatement(ln, statementKind, statementType, typeStr)
	}

	symID, ok := getSymIDOfSAR(sar)
	if !ok {
		return "", sarDidNotHaveSymID
	}

	return symID, nil
}

func (p *Parser) cin(ln uint64) error {
	err := p.evalOperators()
	if err != nil {
		return err
	}
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	symID, ok := getSymIDOfSAR(sar)
	if !ok {
		return invalidExpression
	}
	sym := p.symbolTable[symID]
	typeStr := sym.data[typeField]
	if sym.kind.IsVar() {
		if !(typeStr == string(lex.Int) || typeStr == string(lex.Char) || typeStr == string(lex.Bool)) {
			return invalidTypeForIO(ln, typeStr)
		}
	} else {
		return invalidKindForIO(ln, string(sym.kind))
	}

	symID, ok = getSymIDOfSAR(sar)
	if !ok {
		return sarDidNotHaveSymID
	}
	var q icode.Quad
	comment := fmt.Sprintf("read %s", p.symbolTable[symID].value)
	if typeStr == string(lex.Int) {
		q = icode.NewQuad("", icode.RDI, symID, "", "", comment)
	} else {
		q = icode.NewQuad("", icode.RDC, symID, "", "", comment)
	}
	p.icode.AddNewQuad(q)
	return nil
}

func (p *Parser) cout(ln uint64) error {
	err := p.evalOperators()
	if err != nil {
		return err
	}
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	_, typeStr, err := p.getDataFromSAR(sar)
	if err != nil {
		return err
	}
	if !(typeStr == string(lex.Int) || typeStr == string(lex.Char) || typeStr == string(lex.Bool)) {
		return invalidTypeForIO(ln, typeStr)
	}
	symID, ok := getSymIDOfSAR(sar)
	if !ok {
		return sarDidNotHaveSymID
	}
	var q icode.Quad
	comment := fmt.Sprintf("write %s", p.symbolTable[symID].value)
	if typeStr == string(lex.Int) {
		q = icode.NewQuad("", icode.WRTI, symID, "", "", comment)
	} else {
		q = icode.NewQuad("", icode.WRTC, symID, "", "", comment)
	}
	p.icode.AddNewQuad(q)
	return nil
}

func (p *Parser) ifAction(ln uint64) (string, error) {
	return p.isKind(ln, string(lex.If), string(lex.Bool))
}

func (p *Parser) whileAction(ln uint64) (string, error) {
	return p.isKind(ln, string(lex.While), string(lex.Bool))
}

func (p *Parser) returnAction(ln uint64, returnType string) error {
	err := p.evalOperators()
	if err != nil {
		return err
	}
	if p.semanticActionStack.isEmpty() {
		if returnType == string(lex.Void) {
			q := icode.NewQuad("", icode.RTN, "", "", "", "")
			p.icode.AddNewQuad(q)
			return nil
		}
		return invalidReturn(ln, returnType, string(lex.Void))
	}
	retSymID, err := p.isKind(ln, string(lex.Return), returnType)
	if err != nil {
		return err
	}
	q := icode.NewQuad("", icode.RETURN, retSymID, "", "", "return "+p.symbolTable[retSymID].value)
	p.icode.AddNewQuad(q)
	return err
}

func (p *Parser) atoi(ln uint64) error {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	_, typeStr, err := p.getDataFromSAR(sar)
	if err != nil {
		return err
	}
	if typeStr != string(lex.Char) {
		return invalidKindForAtoi(ln, typeStr)
	}
	symID, err := p.addTemporaryVariableToSymbolTable(string(lex.Int))
	if err != nil {
		return err
	}

	newIntSAR := identifierSAR{
		id:      symID,
		symID:   symID,
		scope:   p.currentScope,
		idType:  string(lex.Int),
		lineNum: ln,
	}

	p.semanticActionStack.push(newIntSAR)

	return p.addMovQuad(newIntSAR, sar)
}

func (p *Parser) itoa(ln uint64) error {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}
	_, typeStr, err := p.getDataFromSAR(sar)
	if err != nil {
		return err
	}
	if typeStr != string(lex.Int) {
		return invalidKindForItoa(ln, typeStr)
	}
	symID, err := p.addTemporaryVariableToSymbolTable(string(lex.Char))
	if err != nil {
		return err
	}
	newCharSar := identifierSAR{
		id:      symID,
		symID:   symID,
		scope:   p.currentScope,
		idType:  string(lex.Char),
		lineNum: ln,
	}
	p.semanticActionStack.push(newCharSar)
	return p.addMovQuad(newCharSar, sar)
}

func (p *Parser) tExists() error {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return noTypeOnStack
	}

	typeSAR, ok := sar.(typeSAR)
	if !ok {
		return noTypeOnStack
	}

	if lex.IsReservedWord(typeSAR.typeId, typeDeclaration) {
		return nil
	}

	for _, v := range p.symbolTable {
		if v.value == typeSAR.typeId && v.kind == Class {
			return nil
		}
	}
	return typeDoesNotExist(typeSAR.lineNum, typeSAR.typeId)
}

func (p *Parser) tPush(typeVal string, lineNum uint64) {
	p.semanticActionStack.push(typeSAR{
		typeId:  typeVal,
		lineNum: lineNum,
	})
}

// will check kind based on class, eg variable or class, or function, or constructor
func (p *Parser) dup(id string, ln uint64, kind Kind) (string, error) {
	exists := false
	var symID string
	for _, v := range p.symbolTable {
		if v.value == id && string(v.scope) == string(p.currentScope) && kind.KindEqual(v.kind) {
			if exists {
				var kStr string
				if kind.IsVar() {
					kStr = "variable"
				} else {
					kStr = string(kind)
				}
				return "", duplicateDeclError(ln, kStr, v.value)
			}
			symID = v.symID
			exists = true
		}
	}
	return symID, nil
}

// the symid should come from the call to dup(...)
func (p *Parser) vPush(symID string, id string, scope Scope, idType string) {
	p.semanticActionStack.push(identifierSAR{
		id:     id,
		scope:  scope,
		idType: idType,
		symID:  symID,
	})
}

func (p *Parser) eoePopAll() error {
	err := p.evalOperators()
	if err != nil {
		return err
	}
	p.semanticActionStack.empty()
	return nil
}

func (p *Parser) evalOperators() error {
	for !p.opStack.isEmpty() {
		err := p.evalOperator()
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *Parser) arr(ln uint64) error {
	sar, ok := p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}

	symID, ok := getSymIDOfSAR(sar)
	if !ok {
		return sarDidNotHaveSymID
	}

	_, typeID, err := p.getDataFromSAR(sar)
	if err != nil {
		return err
	}

	if typeID != string(lex.Int) {
		return expectedType(ln, string(lex.Int), typeID)
	}

	sar, ok = p.semanticActionStack.peekPop()
	if !ok {
		return expectedSAR
	}

	arridSAR, ok := sar.(identifierSAR)
	if !ok {
		return noIdentifierOnStack
	}

	p.semanticActionStack.push(arrayIndexSAR{
		id:         arridSAR.id,
		scope:      arridSAR.scope,
		lineNum:    ln,
		indexSymID: symID,
	})

	return nil
}
