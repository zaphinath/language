package main

import (
	"bufio"
	"language/compiler/codegen"
	"language/compiler/icode"
	"language/compiler/lex"
	"language/compiler/parse"
	"fmt"
	"log"
	"os"
	"strconv"
)

var (
	outFileName = "a.asm"
)

func init() {
	log.SetFlags(0)
}

func main() {
	if len(os.Args) != 2 {
		log.Fatal("No source file provided")
	}

	lexer := newLexer(os.Args[1])
	parser := parse.NewParser(lexer)
	err := parser.Parse()
	if err != nil {
		symT := parser.SymbolTable()
		for i := 0; i < len(symT); i++ {
			k := "s" + strconv.Itoa(i)
			v := symT[k]
			fmt.Println(k, v.Value(), v.SymID(), v.Kind(), v.Data(), v.Value(), v.Scope(), v.IndirectAddr())
		}
		log.Fatal(err.Error())
	}

	lexer = newLexer(os.Args[1])
	parser.SetSecondPass(lexer)
	err = parser.Parse()
	symT := parser.SymbolTable()
	l := len(symT)
	for i := 0; i < l; i++ {
		k := "s" + strconv.Itoa(i)
		v, ok := symT[k]
		if ok {
			fmt.Println(k, v.Value(), v.SymID(), v.Kind(), v.Data(), v.Value(), v.Scope(), v.IndirectAddr())
		} else {
			l++
		}
	}

	for _, ic := range parser.Icode() {
		if q, ok := ic.(icode.Quad); ok {
			fmt.Println(q.String())
		} else {
			fmt.Println("LABEL MARKER FOUND", ic)
		}
	}

	if err != nil {
		log.Fatal(err.Error())
	}

	f, err := os.Create(outFileName)
	if err != nil {
		log.Fatal(err.Error())
	}
	generator := codegen.NewGenerator(parser.SymbolTable(), parser.Icode())
	err = generator.Generate(f)
	if err != nil {
		log.Fatal(err.Error())
	}
}

func newLexer(kxiFileName string) *lex.Lexer {
	kxiFile, err := os.Open(kxiFileName)
	if err != nil {
		log.Fatal(err.Error())
	}
	reader := bufio.NewReader(kxiFile)
	return lex.NewLexer(bufio.NewScanner(reader))
}
