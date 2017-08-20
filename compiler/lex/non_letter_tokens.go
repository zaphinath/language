package lex

const (
	Equals             = "=="
	NotEquals          = "!="
	GreaterThanOrEqual = ">="
	LessThanOrEqual    = "<="
	ChevronClose       = ">>"
	ChevronOpen        = "<<"
)

const (
	nonCharTokens     = "-+/*=][(){},;.><!"
	validSecondTokens = "=<>"
	Plus              = "+"
	Minus             = "-"
	Div               = "/"
	Mul               = "*"
	Eq                = "="
	CloseBracket      = "]"
	OpenBracket       = "["
	CloseParen        = ")"
	OpenParen         = "("
	CloseBrace        = "}"
	OpenBrace         = "{"
	CommaLit          = ","
	PeriodLit         = "."
	SemicolonLit      = ";"
)

func checkTokenList(r rune, tokens string) bool {
	for _, token := range tokens {
		if r == token {
			return true
		}
	}
	return false
}

func (l *Lexer) lexNonLetterToken(seed rune) *Token {
	var lt LexemeType
	lexeme := string(seed)
	nextRune, _, err := l.buffer.ReadRune()
	if err == nil && checkTokenList(nextRune, validSecondTokens) {
		lexeme += string(nextRune)
	} else {
		l.buffer.UnreadRune()
	}

	switch lexeme {
	case Equals, NotEquals, LessThanOrEqual, GreaterThanOrEqual:
		lt = Relational
	case ChevronClose:
		lt = PipeIn
	case ChevronOpen:
		lt = PipeOut
	case "-", "+", "/", "*":
		lt = Math
	case "=":
		lt = Assingment
	case "[":
		lt = ArrayOpen
	case "]":
		lt = ArrayClose
	case "(":
		lt = ParenOpen
	case ")":
		lt = ParenClose
	case "{":
		lt = BlockOpen
	case "}":
		lt = BlockClose
	case ";":
		lt = Semicolon
	case ",":
		lt = Comma
	case ".":
		lt = Period
	case ">", "<":
		lt = Relational
	default:
		lt = Unknown
	}
	return NewToken(lexeme, lt, l.lineNumber)
}
