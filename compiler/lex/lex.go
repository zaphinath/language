package lex

import (
	// "language/compiler/debug"
	"bufio"
	"bytes"
	// "fmt"
	"unicode"
)

type Lexer struct {
	currentToken *Token
	nextToken    *Token
	scanner      *bufio.Scanner
	buffer       *bytes.Buffer
	lineNumber   uint64
}

func NewLexer(scanner *bufio.Scanner) *Lexer {
	scanner.Scan()
	return &Lexer{
		currentToken: nil,
		nextToken:    nil,
		scanner:      scanner,
		buffer:       bytes.NewBufferString(scanner.Text()),
		lineNumber:   1,
	}
}

func (l *Lexer) CurrentToken() *Token {
	// callingFunction := debug.CallingFunction()
	// if l.currentToken == nil {
	// 	fmt.Println("current token: nil", callingFunction)
	// } else {
	// 	fmt.Println("current token:", l.currentToken.Lexeme(), l.currentToken.LexemeType(), l.currentToken.LineNumber(), callingFunction)
	// }
	return l.currentToken
}

func (l *Lexer) NextToken() *Token {
	// callingFunction := debug.CallingFunction()
	// if l.nextToken == nil {
	// 	fmt.Println("next token: nil", callingFunction)
	// } else {
	// 	fmt.Println("next token:", l.nextToken.Lexeme(), l.nextToken.LexemeType(), l.nextToken.LineNumber(), callingFunction)
	// }
	return l.nextToken
}

func (l *Lexer) LexToken() {
	if l.nextToken != nil {
		l.currentToken = l.nextToken
	} else {
		l.currentToken = l.lex()
	}
	l.nextToken = l.lex()
}

func (l *Lexer) newLine() bool {
	if l.scanner.Scan() {
		l.buffer.Reset()
		l.buffer.WriteString(l.scanner.Text())
		l.lineNumber++
		return true
	}
	return false
}

func (l *Lexer) lex() *Token {
	for {
		if l.buffer.Len() == 0 {
			isNewLine := l.newLine()
			if !isNewLine {
				return NewToken("", EOF, l.lineNumber)
			}
		}

		var tok *Token
		r, _, err := l.buffer.ReadRune()
		if err != nil {
			continue
		}
		switch true {
		case r > unicode.MaxASCII:
			tok = NewToken(string(r), Unknown, l.lineNumber)
		case r == '/':
			nextRune, _, err := l.buffer.ReadRune()
			if err != nil {
				tok = NewToken(string(r), Math, l.lineNumber)
			} else if nextRune == '/' {
				l.buffer.Reset()
				break
			} else {
				l.buffer.UnreadRune()
				tok = NewToken(string(r), Math, l.lineNumber)
			}
		case unicode.IsSpace(r):
			continue
		case unicode.IsDigit(r):
			tok = l.lexInt(r)
		case unicode.IsLetter(r):
			tok = l.lexIdOrKeyword(r)
		case checkTokenList(r, nonCharTokens):
			tok = l.lexNonLetterToken(r)
		case r == '\'':
			tok = l.lexCharacter(r)
		default:
			tok = NewToken(string(r), Unknown, l.lineNumber)
		}
		if tok != nil {
			return tok
		}
	}
}
