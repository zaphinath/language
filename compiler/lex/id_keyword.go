package lex

import (
	"unicode"
)

const (
	Atoi        ReservedWord = "atoi"
	Bool        ReservedWord = "bool"
	Class       ReservedWord = "class"
	Char        ReservedWord = "char"
	Cin         ReservedWord = "cin"
	Cout        ReservedWord = "cout"
	Else        ReservedWord = "else"
	False       ReservedWord = "false"
	If          ReservedWord = "if"
	Int         ReservedWord = "int"
	Itoa        ReservedWord = "itoa"
	Main        ReservedWord = "main"
	New         ReservedWord = "new"
	Null        ReservedWord = "null"
	Object      ReservedWord = "object"
	Public      ReservedWord = "public"
	Private     ReservedWord = "private"
	Return      ReservedWord = "return"
	String      ReservedWord = "string"
	This        ReservedWord = "this"
	True        ReservedWord = "true"
	Void        ReservedWord = "void"
	While       ReservedWord = "while"
	Spawn       ReservedWord = "spawn"
	Lock        ReservedWord = "lock"
	Release     ReservedWord = "release"
	Block       ReservedWord = "block"
	Sym         ReservedWord = "sym"
	Kxi2017     ReservedWord = "kxi2017"
	Protected   ReservedWord = "protected"
	Unprotected ReservedWord = "unprotected"
	And         ReservedWord = "and"
	Or          ReservedWord = "or"
)

var (
	keywords = []ReservedWord{
		Atoi,
		Bool,
		Class,
		Char,
		Cin,
		Cout,
		Else,
		If,
		Int,
		Itoa,
		Main,
		New,
		Null,
		Object,
		Public,
		Private,
		Return,
		String,
		This,
		Void,
		While,
		Spawn,
		Lock,
		Release,
		Block,
		Sym,
		Kxi2017,
		Protected,
		Unprotected,
	}

	logicalOperator = []ReservedWord{
		And,
		Or,
	}

	boolean = []ReservedWord{
		True,
		False,
	}
)

type ReservedWord string

func (l *Lexer) lexIdOrKeyword(seed rune) *Token {
	lexeme := string(seed)
	for {
		r, _, err := l.buffer.ReadRune()
		if err != nil {
			break
		}
		if !unicode.IsDigit(r) && !unicode.IsLetter(r) {
			l.buffer.UnreadRune()
			break
		}
		lexeme += string(r)
	}

	isKeyword := IsReservedWord(lexeme, keywords)
	isLogical := IsReservedWord(lexeme, logicalOperator)
	isBool := IsReservedWord(lexeme, boolean)
	var lexemeType LexemeType
	if !isKeyword && !isLogical && !isBool {
		lexemeType = Identifier
	} else if isLogical {
		lexemeType = LogicalConnective
	} else if isBool {
		lexemeType = Boolean
	} else {
		lexemeType = Keyword
	}
	return NewToken(lexeme, lexemeType, l.lineNumber)
}

func IsReservedWord(lexeme string, keywordList []ReservedWord) bool {
	for _, keyword := range keywordList {
		if keyword == ReservedWord(lexeme) {
			return true
		}
	}
	return false
}
