package codegen

import (
	"language/compiler/icode"
	"errors"
	"fmt"
)

const (
	badArgToRtnFmtStr         = "rtn expects the RTN or RETURN icodes not %s"
	parsingErrorBadKindFmtStr = "kind %s is not a literal, ivar, lvar, tvar, or param"
	unknownIcodeFmtStr        = "unknown icode %s"
	unknownFunctionSizeFmtStr = "symbol has an unknown function size: %s"
	unknownSymbolFmtStr       = "unknown symid: %s"
)

var (
	notIcodeInIcodeList = errors.New("encountered type that was not icode")
	expectedWrt         = errors.New("expected wrtc or wrti icodes")
	expectedRead        = errors.New("expected rdc or rdi icodes")
	expectedNew         = errors.New("expected new or newi icodes")
	expectedCmp         = errors.New("expected comparison icodes")
	expectedLogical     = errors.New("expected logical icode")
)

func unknownSymbol(symID string) error {
	return fmt.Errorf(unknownSymbolFmtStr, symID)
}

func unknownFunctionSize(symid string) error {
	return fmt.Errorf(unknownFunctionSizeFmtStr, symid)
}

func parsingError(val string) error {
	return fmt.Errorf(parsingErrorBadKindFmtStr, val)
}

func unknownIcode(code icode.IInstruction) error {
	return fmt.Errorf(unknownIcodeFmtStr, code)
}

func badArtToRtn(arg string) error {
	return fmt.Errorf(badArgToRtnFmtStr, arg)
}
