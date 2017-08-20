package lex

func (l *Lexer) lexCharacter(seed rune) *Token {
	lexeme := string(seed)
	lexemeType := Character
	nextRune, _, err := l.buffer.ReadRune()
	if err != nil {
		return NewToken(lexeme, Unknown, l.lineNumber)
	}
	lexeme += string(nextRune)
	if lexeme == "''" {
		return NewToken(lexeme, Unknown, l.lineNumber)
	}

	if nextRune == '\\' {
		escapedRune, _, err := l.buffer.ReadRune()
		if err != nil {
			return NewToken(lexeme, Unknown, l.lineNumber)
		} else {
			lexeme += string(escapedRune)
		}
	}

	finalQuote, _, err := l.buffer.ReadRune()
	if err != nil || finalQuote != '\'' {
		lexemeType = Unknown
	}
	if err == nil {
		lexeme += string(finalQuote)
	}
	return NewToken(lexeme, lexemeType, l.lineNumber)
}
