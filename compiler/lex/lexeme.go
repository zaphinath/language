package lex

const (
	Integer           LexemeType = "int"
	Character         LexemeType = "char"
	Boolean           LexemeType = "bool"
	Identifier        LexemeType = "ident"
	Period            LexemeType = "period"
	Comma             LexemeType = "comma"
	Semicolon         LexemeType = "semicolon"
	Keyword           LexemeType = "keyword"
	Math              LexemeType = "math"
	Relational        LexemeType = "relational"
	LogicalConnective LexemeType = "logical"
	Assingment        LexemeType = "assingment"
	ArrayOpen         LexemeType = "array open"
	ArrayClose        LexemeType = "array close"
	BlockOpen         LexemeType = "block open"
	BlockClose        LexemeType = "block close"
	ParenOpen         LexemeType = "paren open"
	ParenClose        LexemeType = "paren close"
	PipeIn            LexemeType = "pipe in"
	PipeOut           LexemeType = "pipe out"
	Unknown           LexemeType = "unknown"
	EOF               LexemeType = "eof"
)

func IsLexemeType(lt LexemeType, ltList []LexemeType) bool {
	for _, lexemeType := range ltList {
		if lt == lexemeType {
			return true
		}
	}
	return false
}

type LexemeType string

type Token struct {
	lexeme     string
	lexemeType LexemeType
	lineNumber uint64
}

func NewToken(lexeme string, lexemeType LexemeType, lineNumber uint64) *Token {
	return &Token{
		lexeme:     lexeme,
		lexemeType: lexemeType,
		lineNumber: lineNumber,
	}
}

func (t Token) Lexeme() string {
	return t.lexeme
}

func (t Token) LexemeRW() ReservedWord {
	return ReservedWord(t.Lexeme())
}

func (t Token) LexemeType() LexemeType {
	return t.lexemeType
}

func (t Token) LineNumber() uint64 {
	return t.lineNumber
}
