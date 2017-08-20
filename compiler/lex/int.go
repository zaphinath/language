package lex

import (
	"unicode"
)

func (l *Lexer) lexInt(seed rune) *Token {
	lexeme := string(seed)
	for {
		r, _, err := l.buffer.ReadRune()
		if err != nil {
			break
		}
		if unicode.IsDigit(r) {
			lexeme += string(r)
		} else {
			l.buffer.UnreadRune()
			break
		}
	}
	return NewToken(lexeme, Integer, l.lineNumber)
}
