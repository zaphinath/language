package parse

import (
	"language/compiler/lex"
)

var (
	statementRW = []lex.ReservedWord{
		lex.If,
		lex.While,
		lex.Return,
		lex.Cout,
		lex.Cin,
		lex.Spawn,
		lex.Block,
		lex.Lock,
		lex.Release,
	}

	statementLT = []lex.LexemeType{
		lex.BlockOpen,
	}
)

func (p *Parser) statement() error {
	cTok := p.lexer.CurrentToken()
	nTok := p.lexer.NextToken()
	if cTok.LexemeType() == lex.BlockOpen {
		for {
			if isStatement(p.lexer.NextToken()) {
				p.lexer.LexToken()
				err := p.statement()
				if err != nil {
					return err
				}
			} else {
				break
			}
		}
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.BlockClose {
			return syntaxError(tok, lex.CloseBrace)
		}
	} else if tokenIsExpression(cTok) {
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
		if p.secondPass {
			err = p.eoePopAll()
			if err != nil {
				return err
			}
		}
	} else if cTok.LexemeRW() == lex.If {
		p.lexer.LexToken()
		cTok := p.lexer.CurrentToken()
		if cTok.LexemeType() != lex.ParenOpen {
			return syntaxError(nTok, "(")
		} else if p.secondPass {
			err := p.oPush(cTok.Lexeme(), cTok.LineNumber())
			if err != nil {
				return err
			}
		}

		err := p.expression()
		if err != nil {
			return err
		}

		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.ParenClose {
			return syntaxError(tok, lex.CloseParen)
		}

		var bLabel string // this is the label that will be branched to if the conditional evaluates to false
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
			symID, err := p.ifAction(tok.LineNumber())
			if err != nil {
				return err
			}
			bLabel = IdGen.GenID()
			p.addBranch(symID, bLabel)
		}

		p.lexer.LexToken()
		err = p.statement()
		if err != nil {
			return err
		}

		// add the jmp after the code for the if block
		var exitIfID string
		if p.secondPass {
			if p.lexer.NextToken().LexemeRW() == lex.Else {
				// after the else statement we will backpatch for this if there is a label marker
				exitIfID = IdGen.GenID()
				p.addJmp(exitIfID)
			}

			if bLabel != "" {
				p.icode.AddLabelMarker(bLabel)
			}
		}

		if p.lexer.NextToken().LexemeRW() == lex.Else {
			p.lexer.LexToken()
			p.lexer.LexToken()
			err := p.statement()
			if err != nil {
				return err
			}
		}
		if p.secondPass && exitIfID != "" {
			if label, ok := p.icode.LabelMarker(); ok {
				p.icode.BackPatch(exitIfID, label)
			} else {
				p.icode.AddLabelMarker(exitIfID)
			}
		}
	} else if cTok.LexemeRW() == lex.While {
		p.lexer.LexToken()
		cTok := p.lexer.CurrentToken()
		if cTok.LexemeType() != lex.ParenOpen {
			return syntaxError(nTok, lex.OpenParen)
		} else if p.secondPass {
			err := p.oPush(cTok.Lexeme(), cTok.LineNumber())
			if err != nil {
				return err
			}
		}

		var beginID string
		var endID string
		if p.secondPass {
			beginID = IdGen.GenID()
			endID = IdGen.GenID()
			if label, ok := p.icode.LabelMarker(); ok {
				p.icode.BackPatch(beginID, label)
			} else {
				p.icode.AddLabelMarker(beginID)
			}
		}

		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}

		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.ParenClose {
			return syntaxError(tok, lex.CloseParen)
		}
		if p.secondPass {
			err := p.oPush(tok.Lexeme(), tok.LineNumber())
			if err != nil {
				return err
			}
			boolSymID, err := p.whileAction(tok.LineNumber())
			if err != nil {
				return err
			}
			p.addBranch(boolSymID, endID)
		}
		p.lexer.LexToken()
		err = p.statement()
		if err != nil {
			return err
		}

		if p.secondPass {
			p.addJmp(beginID)
			p.icode.AddLabelMarker(endID)
		}
	} else if cTok.LexemeRW() == lex.Return {
		if tokenIsExpression(nTok) {
			p.lexer.LexToken()
			err := p.expression()
			if err != nil {
				return err
			}
		}
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
		if p.secondPass {
			// get the return type of the function
			var rt string
			for _, sym := range p.symbolTable {
				if sym.kind == Function {
					funcScope := Scope(sym.Scope())
					funcScope.AddScope(Scope(sym.value))
					if funcScope == p.currentScope {
						rt = sym.data[returnType]
						break
					}
				}
			}
			if rt == "" {
				return returnNotInFunc(tok.LineNumber())
			}

			err := p.returnAction(tok.LineNumber(), rt)
			if err != nil {
				return err
			}
		}
	} else if cTok.LexemeRW() == lex.Cout {
		if nTok.LexemeType() != lex.PipeOut {
			return syntaxError(nTok, lex.ChevronOpen)
		}
		p.lexer.LexToken()
		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
		if p.secondPass {
			err := p.cout(tok.LineNumber())
			if err != nil {
				return err
			}
		}
	} else if cTok.LexemeRW() == lex.Cin {
		if nTok.LexemeType() != lex.PipeIn {
			return syntaxError(nTok, lex.ChevronClose)
		}
		p.lexer.LexToken()
		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
		if p.secondPass {
			err := p.cin(tok.LineNumber())
			if err != nil {
				return err
			}
		}
	} else if cTok.LexemeRW() == lex.Spawn {
		p.lexer.LexToken()
		err := p.expression()
		if err != nil {
			return err
		}
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.Lexeme() != "set" {
			return syntaxError(tok, "set")
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Identifier {
			return syntaxError(tok, string(lex.Identifier))
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
	} else if cTok.LexemeRW() == lex.Block {
		if nTok.LexemeType() != lex.Semicolon {
			return syntaxError(nTok, ";")
		}
		p.lexer.LexToken()
	} else if cTok.LexemeRW() == lex.Lock {
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Identifier {
			return syntaxError(tok, string(lex.Identifier))
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
	} else if cTok.LexemeRW() == lex.Release {
		p.lexer.LexToken()
		tok := p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Identifier {
			return syntaxError(tok, string(lex.Identifier))
		}
		p.lexer.LexToken()
		tok = p.lexer.CurrentToken()
		if tok.LexemeType() != lex.Semicolon {
			return syntaxError(tok, lex.SemicolonLit)
		}
	} else {
		return syntaxError(cTok, "statement")
	}
	return nil
}

func isStatement(cTok *lex.Token) bool {
	return lex.IsReservedWord(cTok.Lexeme(), statementRW) ||
		lex.IsLexemeType(cTok.LexemeType(), statementLT) ||
		tokenIsExpression(cTok)
}
