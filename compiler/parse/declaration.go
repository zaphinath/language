package parse

import (
	"language/compiler/icode"
	"language/compiler/lex"
	"strings"
)

var (
	typeDeclaration = []lex.ReservedWord{
		lex.Int,
		lex.Char,
		lex.Bool,
		lex.Void,
		lex.Sym,
	}
)

func (p *Parser) argumentList() error {
	err := p.expression()
	if err != nil {
		return err
	}

	for {
		if p.lexer.NextToken().LexemeType() == lex.Comma {
			if p.secondPass {
				err := p.commaAction()
				if err != nil {
					return err
				}
			}
			p.lexer.LexToken()
			p.lexer.LexToken()
			err := p.expression()
			if err != nil {
				return err
			}
		} else {
			break
		}
	}

	if p.secondPass {
		err := p.commaAction()
		if err != nil {
			return err
		}
	}

	return nil
}

func (p *Parser) parameterList() ([]string, error) {
	paramCount := uint64(1)
	err := p.parameter()
	if err != nil {
		return []string{}, err
	}

	for {
		if p.lexer.NextToken().LexemeType() == lex.Comma {
			p.lexer.LexToken()
			p.lexer.LexToken()
			err := p.parameter()
			if err != nil {
				return []string{}, err
			}
			paramCount++
		} else {
			break
		}
	}
	return IdGen.GetLastNID(paramCount), nil
}

func (p *Parser) parameter() error {
	typeTok := p.lexer.CurrentToken()
	if !tokenIsType(typeTok) {
		return syntaxError(typeTok, "type")
	}

	if p.secondPass {
		p.tPush(typeTok.Lexeme(), typeTok.LineNumber())
		err := p.tExists()
		if err != nil {
			return err
		}
	}

	p.lexer.LexToken()

	idTok := p.lexer.CurrentToken()
	if idTok.LexemeType() != lex.Identifier {
		return syntaxError(idTok, string(lex.Identifier))
	}

	obTok := p.lexer.NextToken()
	if obTok.LexemeType() == lex.ArrayOpen {
		p.lexer.LexToken()
		cbTok := p.lexer.NextToken()
		if cbTok.LexemeType() != lex.ArrayClose {
			return syntaxError(cbTok, lex.CloseBracket)
		}
		if !p.secondPass {
			err := p.addParamToSymbolTable(idTok.Lexeme(), makeArrayType(typeTok.Lexeme()))
			if err != nil {
				return err
			}
		}
		p.lexer.LexToken()
	} else {
		if !p.secondPass {
			err := p.addParamToSymbolTable(idTok.Lexeme(), typeTok.Lexeme())
			if err != nil {
				return err
			}
		}
	}

	if p.secondPass {
		_, err := p.dup(idTok.Lexeme(), idTok.LineNumber(), Param)
		if err != nil {
			return err
		}
	}

	return nil
}

func (p *Parser) variableDeclaration() error {
	tok := p.lexer.CurrentToken()
	if !tokenIsType(tok) {
		return syntaxError(tok, "type")
	}
	tokType := tok.Lexeme()
	if p.secondPass {
		p.tPush(tok.Lexeme(), tok.LineNumber())
		err := p.tExists()
		if err != nil {
			return err
		}
	}
	p.lexer.LexToken()

	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.Identifier {
		return syntaxError(tok, string(lex.Identifier))
	}
	tokID := tok.Lexeme()
	p.lexer.LexToken()

	tok = p.lexer.CurrentToken()
	if tok.Lexeme() == lex.OpenBracket {
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.Lexeme() != lex.CloseBracket {
			return syntaxError(tok, lex.CloseBracket)
		}
		tokType = makeArrayType(tokType)
		p.lexer.LexToken()
	}

	if p.secondPass {
		symID, err := p.dup(tokID, tok.LineNumber(), Lvar)
		if err != nil {
			return err
		}

		p.vPush(symID, tokID, p.currentScope, tokType)
	} else {
		err := p.addLocalVariableToSymbolTable(tokID, tokType)
		if err != nil {
			return err
		}
	}

	tok = p.lexer.CurrentToken()
	if tok.LexemeType() == lex.Assingment {
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
		}

		p.lexer.LexToken()
		err := p.assingmentExpression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
	}

	tok = p.lexer.CurrentToken()
	if tok.Lexeme() != lex.SemicolonLit {
		return syntaxError(tok, lex.SemicolonLit)
	}

	if p.secondPass {
		err := p.eoePopAll()
		if err != nil {
			return err
		}
	}

	return nil
}

func (p *Parser) methodBody() error {
	tok := p.lexer.CurrentToken()
	if tok.LexemeType() != lex.BlockOpen {
		return syntaxError(tok, lex.OpenBrace)
	}

	for {
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tokenIsType(tok) && p.lexer.NextToken().LexemeType() == lex.Identifier {
			err := p.variableDeclaration()
			if err != nil {
				return err
			}
		} else {
			break
		}
	}

	for {
		if isStatement(tok) {
			err := p.statement()
			if err != nil {
				return err
			}
			p.lexer.LexToken()
			tok = p.lexer.CurrentToken()
		} else {
			break
		}
	}

	if tok.LexemeType() != lex.BlockClose {
		return syntaxError(tok, lex.CloseBrace)
	}

	return nil
}

func (p *Parser) constructorDeclaration() error {
	// generate icode for object here
	tok := p.lexer.CurrentToken()
	if tok.LexemeType() != lex.Identifier {
		return syntaxError(tok, string(lex.Identifier))
	}

	className := tok.Lexeme()

	if p.secondPass {
		_, err := p.dup(className, tok.LineNumber(), Constructor)
		if err != nil {
			return err
		}
		err = p.cd(className, tok.LineNumber())
		if err != nil {
			return err
		}
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.ParenOpen {
		return syntaxError(tok, lex.OpenParen)
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()

	var constructorSymID string
	if !p.secondPass {
		constructorSymID = p.addConstructorToSymbolTable(className)
	}

	var err error
	paramIDs := []string{}
	p.currentScope.AddScope(Scope(className))

	var thisSymID string
	if p.secondPass {
		thisSymID = p.addThisToSymbolTable()
		constructorSymID = p.findConstructorByClassName(className)
		if constructorSymID == "" {
			return constructorNotInSymbolTable(className)
		}
		c := icode.ConstructorComment(className)
		q := icode.NewQuad(constructorSymID, icode.FUNC, constructorSymID, "", "", c)
		p.icode.AddNewQuad(q)

		staticInitSym, err := p.findStaticInitilizerSym(className)
		if err != nil {
			return err
		}

		p.addFunctionCall(staticInitSym, thisSymID, []string{}, "")
	}

	// set the constructorSymID as the symid of the current method we are parsing
	p.currentMethodSymID = constructorSymID

	if tokenIsType(tok) {
		paramIDs, err = p.parameterList()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
	}

	if tok.LexemeType() != lex.ParenClose {
		return syntaxError(tok, lex.CloseParen)
	}

	p.lexer.LexToken()
	err = p.methodBody()
	if err != nil {
		return err
	}

	if p.secondPass {
		c := icode.ConstructorReturnComment(className)
		q := icode.NewQuad("", icode.RETURN, thisSymID, "", "", c)
		p.icode.AddNewQuad(q)
	}

	err = p.currentScope.PopScope()
	if err != nil {
		return err
	}
	// pop the scope so the scope of the constructor is correct when added to the symbol table
	if !p.secondPass {
		p.symbolTable[constructorSymID].data[paramField] = strings.Join(paramIDs, ",")
	}
	return nil
}

// "symID" is the symid of the identifier that was added to the symbol table
func (p *Parser) fieldDeclaration(symID string, id string, idType string) ([]string, error) {
	// ids of the symbols that are the parameters
	params := []string{}
	var err error
	tok := p.lexer.CurrentToken()

	if tok.LexemeType() == lex.ParenOpen {
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()

		if tokenIsType(tok) {
			params, err = p.parameterList()
			if err != nil {
				return params, err
			}
			p.lexer.LexToken()
			tok = p.lexer.CurrentToken()
		}

		if tok.LexemeType() != lex.ParenClose {
			return params, syntaxError(tok, lex.CloseParen)
		}

		if !p.secondPass {
			p.addThisToSymbolTable() // all methods have a this
		}

		if p.secondPass {
			q := icode.NewQuad(symID, icode.FUNC, symID, "", "", idType+" "+id)
			p.icode.AddNewQuad(q)
		}

		p.lexer.LexToken()
		err := p.methodBody()
		if err != nil {
			return params, err
		}

		if p.secondPass {
			q := icode.NewQuad("", icode.RTN, "", "", "", idType+" "+id)
			p.icode.AddNewQuad(q)
		}
	} else if tok.LexemeType() == lex.ArrayOpen || tok.LexemeType() == lex.Assingment || tok.LexemeType() == lex.Semicolon {
		if tok.LexemeType() == lex.ArrayOpen {
			p.lexer.LexToken()
			tok = p.lexer.CurrentToken()
			if tok.LexemeType() != lex.ArrayClose {
				return params, syntaxError(tok, lex.CloseBracket)
			}
			p.lexer.LexToken()
			tok = p.lexer.CurrentToken()
		}

		var tempIcode icode.Icode
		hasAssign := false
		if p.secondPass {
			// this way we don't need to
			// change the parsing methods when we are parsing static initilizers.
			tempIcode = p.icode
			p.icode = p.staticInitIcode
			p.currentStaticInitSymID, p.currentMethodSymID = p.currentMethodSymID, p.currentStaticInitSymID

			// only ref if next token is an assingment
			if tok.LexemeType() == lex.Assingment {
				hasAssign = true
				// to make sure all the temp stuff adds to the size of the static initilizer
				className := p.currentScope.SplitScope()[1]
				p.currentScope.AddScope(Scope(className + staticInit))

				thisSymID := p.findStaticInitThisSymID()
				if thisSymID == "" {
					return []string{}, thisSymbolNotFound(p.currentScope)
				}

				tSymID, err := p.addRef(thisSymID, symID)
				if err != nil {
					return []string{}, err
				}
				p.vPush(tSymID, id, p.currentScope, idType)
			} else {
				p.vPush(symID, id, p.currentScope, idType)
			}
		}

		if tok.LexemeType() == lex.Assingment {
			if p.secondPass {
				err := p.oPush(tok.Lexeme(), tok.LineNumber())
				if err != nil {
					return []string{}, err
				}
			}
			p.lexer.LexToken()
			err := p.assingmentExpression()
			if err != nil {
				return params, err
			}
			p.lexer.LexToken()
		}

		tok = p.lexer.CurrentToken()
		if tok.Lexeme() != lex.SemicolonLit {
			return params, syntaxError(tok, lex.SemicolonLit)
		}

		if p.secondPass {
			err = p.eoePopAll()
			if err != nil {
				return nil, err
			}
			if hasAssign {
				err = p.currentScope.PopScope()
				if err != nil {
					return nil, err
				}
			}

			p.staticInitIcode = p.icode
			p.icode = tempIcode
			p.currentMethodSymID, p.currentStaticInitSymID = p.currentStaticInitSymID, p.currentMethodSymID
		}
	} else {
		return params, syntaxError(tok, lex.OpenBracket+" or "+lex.Eq+" or "+lex.OpenParen)
	}

	return params, nil
}

func (p *Parser) compilationUnit() error {
	if p.secondPass {
		null := p.findLiteral(string(Null))
		if null == "" {
			null = p.addLiteralToSymbolTable(string(Null), Null)
		}

		main := p.findFunction(string(lex.Main), DefaultScope(), nil)
		if main == nil {
			return mainNotInSymbolTAble
		}

		// construct stack frame for main
		q := icode.NewQuad("", icode.FRAME, main.symID, null, "", "")
		p.icode.AddNewQuad(q)
		q = icode.NewQuad("", icode.CALL, main.symID, "", "", "")
		p.icode.AddNewQuad(q)
		q = icode.NewQuad("", icode.TERM, "", "", "", "good night")
		p.icode.AddNewQuad(q)
	}

	for {
		tok := p.lexer.CurrentToken()
		if tok.LexemeRW() == lex.Class {
			err := p.classDeclaration()
			if err != nil {
				return err
			}
			p.lexer.LexToken()
		} else {
			break
		}
	}

	tok := p.lexer.CurrentToken()
	if tok.LexemeRW() != lex.Void {
		return syntaxError(tok, string(lex.Void))
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeRW() != lex.Kxi2017 {
		return syntaxError(tok, string(lex.Kxi2017))
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeRW() != lex.Main {
		return syntaxError(tok, string(lex.Main))
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.ParenOpen {
		return syntaxError(tok, string(lex.OpenParen))
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.ParenClose {
		return syntaxError(tok, string(lex.CloseParen))
	}

	p.lexer.LexToken()

	if p.secondPass {
		main := p.findFunction(string(lex.Main), DefaultScope(), nil)
		p.currentMethodSymID = main.symID
		if main == nil {
			return mainNotInSymbolTAble
		}
		q := icode.NewQuad(main.symID, icode.FUNC, main.symID, "", "", "")
		p.icode.AddNewQuad(q)
	} else {
		p.currentMethodSymID = p.addMethodToSymbolTable(string(lex.Main), string(lex.Void), Public)
	}
	p.currentScope.AddScope(Scope(lex.Main))
	defer p.currentScope.PopScope()
	err := p.methodBody()
	if err != nil {
		return err
	}

	if p.secondPass {
		q := icode.NewQuad("", icode.RTN, "", "", "", "")
		p.icode.AddNewQuad(q)
	}
	return nil
}

func (p *Parser) classDeclaration() error {
	tok := p.lexer.CurrentToken()
	if tok.LexemeRW() != lex.Class {
		return syntaxError(tok, string(lex.Class))
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.Identifier {
		return syntaxError(tok, string(lex.Identifier))
	}

	// will be set on the second pass when we add the static initilizer to the symbol table
	var staticInitID string
	if p.secondPass {
		_, err := p.dup(tok.Lexeme(), tok.LineNumber(), Class)
		if err != nil {
			return err
		}
		p.currentScope.AddScope(Scope(tok.Lexeme()))
		defer p.currentScope.PopScope()

		staticInitSym, err := p.findStaticInitilizerSym(tok.Lexeme())
		if err != nil {
			return err
		}
		p.currentStaticInitSymID = staticInitSym.symID
		staticInitID = staticInitSym.symID
	} else {
		p.addClassToSymbolTable(tok.Lexeme())
		p.currentScope.AddScope(Scope(tok.Lexeme()))
		defer p.currentScope.PopScope()

		staticInitID = p.addStaticInitilizerToSymbolTable(tok.Lexeme())
		// add it as the currently parsed static initilizer
		p.currentStaticInitSymID = staticInitID
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.BlockOpen {
		return syntaxError(tok, lex.OpenBrace)
	}

	// always reset the static init code
	if p.secondPass {
		// add the object ref to the static initilizer right here
		class := p.currentScope.PeekScope()
		p.currentScope.AddScope(Scope(class + staticInit))
		thisSymID := p.addThisToSymbolTable()
		p.currentScope.PopScope()
		p.staticInitIcode = icode.NewIcode()
		q := icode.NewQuad(staticInitID, icode.FUNC, staticInitID, thisSymID, "", "call static initilizer for "+p.symbolTable[staticInitID].value)
		p.staticInitIcode.AddNewQuad(q)
	}
	p.lexer.LexToken()
	for {
		tok = p.lexer.CurrentToken()
		if tok.LexemeRW() == lex.Public || tok.LexemeRW() == lex.Private || tok.LexemeType() == lex.Identifier {
			err := p.classMemberDeclaration()
			if err != nil {
				return err
			}
			p.lexer.LexToken()
		} else {
			break
		}
	}
	if p.secondPass {
		q := icode.NewQuad("", icode.RTN, "", "", "", "return from static initilizer for "+p.symbolTable[staticInitID].value)
		p.staticInitIcode.AddNewQuad(q)
		p.icode.AppendIcode(p.staticInitIcode)
		p.staticInitIcode = icode.NewIcode() // reset
	}

	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.BlockClose {
		return syntaxError(tok, lex.CloseBrace)
	}

	return nil
}

func (p *Parser) classMemberDeclaration() error {
	tok := p.lexer.CurrentToken()
	if tok.LexemeRW() == lex.Private || tok.LexemeRW() == lex.Public {
		var accessMod AccessModifier
		switch tok.LexemeRW() {
		case lex.Private:
			accessMod = Private
		case lex.Public:
			accessMod = Public
		}

		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if !tokenIsType(tok) {
			return syntaxError(tok, "type identifier")
		}

		if p.secondPass {
			p.tPush(tok.Lexeme(), tok.LineNumber())
			err := p.tExists()
			if err != nil {
				return err
			}
		}

		memberType := tok.Lexeme()

		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Identifier {
			return syntaxError(tok, string(lex.Identifier))
		}
		memberId := tok.Lexeme()
		p.lexer.LexToken()
		tokenOfFieldDeclaration := p.lexer.CurrentToken()

		// just for making sure that the type of the symbol is correct
		//
		// if the current token is a "[" then we have an array type member variable
		//
		// if the next token is a "(" then we have a function as a field
		// declaration and we need to add it to the scope
		//
		// these are mutually exclusive
		var funcSymID string
		isMethod := false
		if tokenOfFieldDeclaration.LexemeType() == lex.ArrayOpen {
			memberType = makeArrayType(memberType)
		} else if tokenOfFieldDeclaration.LexemeType() == lex.ParenOpen {
			isMethod = true
			if !p.secondPass {
				funcSymID = p.addMethodToSymbolTable(memberId, memberType, accessMod)
				p.currentMethodSymID = funcSymID
			}
		}

		// set symid for call to vpush in fieldDeclaration
		var symID string
		if p.secondPass {
			var err error
			var k Kind
			if isMethod {
				k = Function
			} else {
				k = Ivar
			}
			symID, err = p.dup(memberId, tok.LineNumber(), k)
			if err != nil {
				return err
			}
			if isMethod {
				p.currentMethodSymID = symID
			}
		}

		if tokenOfFieldDeclaration.LexemeType() == lex.ParenOpen {
			p.currentScope.AddScope(Scope(memberId))
		}

		params, err := p.fieldDeclaration(symID, memberId, memberType)
		if err != nil {
			return err
		}
		if tokenOfFieldDeclaration.LexemeType() == lex.ParenOpen {
			if !p.secondPass {
				p.symbolTable[funcSymID].data[paramField] = strings.Join(params, ",")
			}
			p.currentScope.PopScope()
		} else {
			if !p.secondPass {
				err := p.addInstanceVariableToSymbolTable(memberId, memberType, accessMod)
				if err != nil {
					return err
				}
			}
		}
	} else if tok.LexemeType() == lex.Identifier {
		err := p.constructorDeclaration()
		if err != nil {
			return err
		}
	} else {
		return syntaxError(tok, string(lex.Identifier)+" or "+string(lex.Public)+" or "+string(lex.Private))
	}
	return nil
}

func tokenIsType(tok *lex.Token) bool {
	return lex.IsReservedWord(tok.Lexeme(), typeDeclaration) ||
		tok.LexemeType() == lex.Identifier
}
