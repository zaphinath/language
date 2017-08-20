package parse

import (
	"language/compiler/lex"
)

var (
	expressionZLT = []lex.LexemeType{
		lex.Relational,
		lex.Boolean,
		lex.Assingment,
		lex.Math,
		lex.LogicalConnective,
	}

	expressionRW = []lex.ReservedWord{
		lex.True,
		lex.False,
		lex.Null,
		lex.This,
	}

	expressionLT = []lex.LexemeType{
		lex.Integer,
		lex.Character,
		lex.Identifier,
		lex.ParenOpen,
		lex.Boolean,
	}
)

func (p *Parser) expression() error {
	current := p.lexer.CurrentToken()
	nextToken := p.lexer.NextToken()
	if current.LexemeType() == lex.ParenOpen {
		if p.secondPass {
			err := p.oPush(current.Lexeme(), current.LineNumber())
			if err != nil {
				return err
			}
		}

		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		current = p.lexer.CurrentToken()
		if current.LexemeType() != lex.ParenClose {
			return syntaxError(p.lexer.CurrentToken(), lex.CloseParen)
		}

		if p.secondPass {
			err = p.oPush(current.Lexeme(), current.LineNumber())
			if err != nil {
				return err
			}
		}

		if tokenIsExpressionZ(p.lexer.NextToken()) {
			p.lexer.LexToken()
			return p.expressionZ()
		}
	} else if current.LexemeRW() == lex.True {
		if !p.secondPass {
			p.addLiteralToSymbolTable(current.Lexeme(), Bool)
		} else {
			err := p.lPush(current.Lexeme())
			if err != nil {
				return err
			}
		}
		if tokenIsExpressionZ(nextToken) {
			p.lexer.LexToken()
			return p.expressionZ()
		}
	} else if current.LexemeRW() == lex.False {
		if !p.secondPass {
			p.addLiteralToSymbolTable(current.Lexeme(), Bool)
		} else {
			err := p.lPush(current.Lexeme())
			if err != nil {
				return err
			}
		}
		if tokenIsExpressionZ(nextToken) {
			p.lexer.LexToken()
			return p.expressionZ()
		}
	} else if current.LexemeRW() == lex.Null {
		if !p.secondPass {
			p.addLiteralToSymbolTable(current.Lexeme(), Null)
		} else {
			err := p.lPush(current.Lexeme())
			if err != nil {
				return err
			}
		}
		if tokenIsExpressionZ(nextToken) {
			p.lexer.LexToken()
			return p.expressionZ()
		}
	} else if current.LexemeRW() == lex.This {
		if p.secondPass {
			p.iPushThis(p.currentScope)
			err := p.iExists(current.LineNumber())
			if err != nil {
				return err
			}
		}
		p.lexer.LexToken()
		if p.lexer.CurrentToken().LexemeType() == lex.Period {
			err := p.memberRefz()
			if err != nil {
				return err
			}
		}

		nextToken = p.lexer.NextToken()
		if tokenIsExpressionZ(nextToken) {
			p.lexer.LexToken()
			err := p.expressionZ()
			if err != nil {
				return err
			}
		}
	} else if current.LexemeType() == lex.Integer ||
		current.LexemeRW() == lex.Plus ||
		current.LexemeRW() == lex.Minus {
		var sign string
		if current.LexemeRW() == lex.Plus || current.LexemeRW() == lex.Minus {
			sign = current.Lexeme()
			p.lexer.LexToken()
		}
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Integer {
			return syntaxError(tok, string(lex.Integer))
		}

		val := sign + tok.Lexeme()
		if !p.secondPass {
			p.addLiteralToSymbolTable(val, Int)
		} else {
			err := p.lPush(val)
			if err != nil {
				return err
			}
		}

		if tokenIsExpressionZ(p.lexer.NextToken()) {
			p.lexer.LexToken()
			return p.expressionZ()
		}
	} else if current.LexemeType() == lex.Character {
		if !p.secondPass {
			p.addLiteralToSymbolTable(current.Lexeme(), Character)
		} else {
			err := p.lPush(current.Lexeme())
			if err != nil {
				return err
			}
		}
		if tokenIsExpressionZ(nextToken) {
			p.lexer.LexToken()
			return p.expressionZ()
		}
	} else if current.LexemeType() == lex.Identifier {
		if p.secondPass {
			p.iPush(current.Lexeme(), p.currentScope, current.LineNumber())
		}
		if nextToken.LexemeType() == lex.ParenOpen || nextToken.LexemeType() == lex.ArrayOpen {
			p.lexer.LexToken()
			err := p.fnArrMember()
			if err != nil {
				return err
			}
		}

		if p.secondPass {
			err := p.iExists(p.lexer.CurrentToken().LineNumber())
			if err != nil {
				return err
			}
		}

		nextToken = p.lexer.NextToken()
		if nextToken.LexemeType() == lex.Period {
			p.lexer.LexToken()
			err := p.memberRefz()
			if err != nil {
				return err
			}
		}

		nextToken = p.lexer.NextToken()
		if tokenIsExpressionZ(nextToken) {
			p.lexer.LexToken()
			err := p.expressionZ()
			if err != nil {
				return err
			}
		}
	} else {
		return syntaxError(current, "expression")
	}
	return nil
}

func (p *Parser) expressionZ() error {
	lt := p.lexer.CurrentToken().LexemeType()
	if p.secondPass {
		err := p.oPush(p.lexer.CurrentToken().Lexeme(), p.lexer.CurrentToken().LineNumber())
		if err != nil {
			return err
		}
	}
	nextToken := p.lexer.NextToken()
	if lt == lex.Assingment {
		p.lexer.LexToken()
		return p.assingmentExpression()
	} else if lt == lex.LogicalConnective {
		p.lexer.LexToken()
		return p.expression()
	} else if lt == lex.Boolean {
		p.lexer.LexToken()
		return p.expression()
	} else if lt == lex.Math {
		p.lexer.LexToken()
		return p.expression()
	} else if lt == lex.Relational {
		p.lexer.LexToken()
		return p.expression()
	} else {
		return syntaxError(nextToken, "expressionZ")
	}
}

func (p *Parser) assingmentExpression() error {
	cTok := p.lexer.CurrentToken()
	if tokenIsExpression(cTok) {
		return p.expression()
	} else if cTok.LexemeRW() == lex.New {
		p.lexer.LexToken()
		cTok := p.lexer.CurrentToken()
		if !tokenIsType(cTok) {
			return syntaxError(cTok, "type declaration")
		}

		if p.secondPass {
			p.tPush(cTok.Lexeme(), cTok.LineNumber())
		}

		p.lexer.LexToken()
		err := p.newDeclaration()
		if err != nil {
			return err
		}
	} else if cTok.LexemeRW() == lex.Atoi {
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.ParenOpen {
			return syntaxError(tok, string(lex.ParenOpen))
		}
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
		}

		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.ParenClose {
			return syntaxError(tok, string(lex.ParenClose))
		}
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
			err = p.atoi(tok.LineNumber())
			if err != nil {
				return err
			}
		}
	} else if cTok.LexemeRW() == lex.Itoa {
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.ParenOpen {
			return syntaxError(tok, lex.OpenParen)
		}
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
		}
		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.ParenClose {
			return syntaxError(tok, lex.CloseParen)
		}
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
			err = p.itoa(tok.LineNumber())
			if err != nil {
				return err
			}
		}
	} else {
		return syntaxError(cTok, "assingment expression")
	}
	return nil
}

// I think welborn told me to do this just in case.
func (p *Parser) newDeclaration() error {
	tok := p.lexer.CurrentToken()
	if tok.LexemeType() == lex.ParenOpen {
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
			p.pushParamListMarker()
		}

		if tokenIsExpression(p.lexer.NextToken()) {
			p.lexer.LexToken()
			err := p.argumentList()
			if err != nil {
				return err
			}
		}
		p.lexer.LexToken()

		cTok := p.lexer.CurrentToken()
		if cTok.LexemeType() != lex.ParenClose {
			return syntaxError(cTok, lex.CloseParen)
		}

		if p.secondPass {
			err := p.oPush(cTok.Lexeme(), cTok.LineNumber())
			if err != nil {
				return err
			}

			err = p.paramListAction()
			if err != nil {
				return err
			}

			err = p.newObj()
			if err != nil {
				return err
			}
		}
	} else if tok.LexemeType() == lex.ArrayOpen {
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
		}

		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		cTok := p.lexer.CurrentToken()
		if cTok.LexemeType() != lex.ArrayClose {
			return syntaxError(cTok, lex.CloseBracket)
		}
		if p.secondPass {
			err := p.oPush(cTok.Lexeme(), cTok.LineNumber())
			if err != nil {
				return err
			}

			err = p.newArray(cTok.LineNumber())
			if err != nil {
				return err
			}
		}
	} else {
		return syntaxError(tok, lex.OpenParen+" or "+lex.OpenBracket)
	}
	return nil
}

func (p *Parser) fnArrMember() error {
	tok := p.lexer.CurrentToken()
	if tok.LexemeType() == lex.ParenOpen {
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
			p.pushParamListMarker()
		}

		if tokenIsExpression(p.lexer.NextToken()) {
			p.lexer.LexToken()
			err := p.argumentList()
			if err != nil {
				return err
			}
		}

		p.lexer.LexToken()

		cTok := p.lexer.CurrentToken()
		if cTok.LexemeType() != lex.ParenClose {
			return syntaxError(cTok, lex.CloseParen)
		}

		if p.secondPass {
			err := p.oPush(cTok.Lexeme(), cTok.LineNumber())
			if err != nil {
				return err
			}

			err = p.paramListAction()
			if err != nil {
				return err
			}
			err = p.funcAction()
			if err != nil {
				return err
			}
		}
	} else if tok.LexemeType() == lex.ArrayOpen {
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
		}

		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		cTok := p.lexer.CurrentToken()
		if cTok.LexemeType() != lex.ArrayClose {
			return syntaxError(cTok, lex.CloseBracket)
		}
		if p.secondPass {
			err := p.oPush(cTok.Lexeme(), cTok.LineNumber())
			if err != nil {
				return err
			}

			err = p.arr(cTok.LineNumber())
			if err != nil {
				return err
			}
		}
	} else {
		return syntaxError(tok, lex.OpenParen+" or "+lex.OpenBracket)
	}
	return nil
}

func (p *Parser) memberRefz() error {
	tok := p.lexer.CurrentToken()
	if tok.LexemeType() != lex.Period {
		return syntaxError(tok, lex.PeriodLit)
	}

	p.lexer.LexToken()
	tok = p.lexer.CurrentToken()
	if tok.LexemeType() != lex.Identifier {
		return syntaxError(tok, string(lex.Identifier))
	}

	if p.secondPass {
		p.iPush(tok.Lexeme(), p.currentScope, tok.LineNumber())
	}

	tok = p.lexer.NextToken()
	if tok.LexemeType() == lex.ParenOpen || tok.LexemeType() == lex.ArrayOpen {
		p.lexer.LexToken()
		err := p.fnArrMember()
		if err != nil {
			return err
		}
	}

	if p.secondPass {
		err := p.rExist(p.lexer.CurrentToken().LineNumber())
		if err != nil {
			return err
		}
	}

	tok = p.lexer.NextToken()
	if tok.LexemeType() == lex.Period {
		p.lexer.LexToken()
		err := p.memberRefz()
		if err != nil {
			return err
		}
	}
	return nil
}

func tokenIsExpressionZ(tok *lex.Token) bool {
	return lex.IsLexemeType(tok.LexemeType(), expressionZLT)
}

func tokenIsExpression(tok *lex.Token) bool {
	return lex.IsReservedWord(tok.Lexeme(), expressionRW) ||
		lex.IsLexemeType(tok.LexemeType(), expressionLT) ||
		tok.Lexeme() == lex.Minus ||
		tok.Lexeme() == lex.Plus
}
