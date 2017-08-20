package parse

import (
	"language/compiler/debug"
	"language/compiler/lex"
	"errors"
	"fmt"
	"strings"
)

const (
	variableNotDefinedFmtStr              = "%d: variable %s not defined"
	functionNotDefinedFmtStr              = "%d: function %s(%s) not defined"
	arrayNotDefinedFmtStr                 = "%d: array %s not defined"
	notInClassFmtStr                      = "%d: not in class definition %s"
	syntaxErrorFmtStr                     = "syntax error line: %d found: \"%s\" expected: %s"
	typeMismatchFmtStr                    = "%d: invalid operation %s %s %s"
	litNotAddedToTableFmtStr              = "literal %s not added to table"
	typeDoesNotExistFmtStr                = "%d: type %s does not exist"
	duplicateDeclErrorFmtStr              = "%d: duplicate %s %s"
	constructorNotInClassFmtStr           = "%d: cannot declare constructor %s in class %s"
	invalidOperatorFmtStr                 = "%d: invalid operator %s"
	foundExtraTokenFmtStr                 = "Found extra token %s\nExpecting EOF"
	constructorNotFoundFmtStr             = "%d: constructor for %s not found"
	expectedTypeFmtStr                    = "%d: expected %s found %s"
	nonArrayIndexFmtStr                   = "%d: cannot index variable that is not an array"
	invalidTypeForRefFmtStr               = "%d: cannot do reference off of type that is not an object %s"
	memberNotDefinedFmtStr                = "%d: member %s not defined in %s"
	memberNotPublicFmtStr                 = "%d: member %s not public in %s"
	badIExistFmtStr                       = "%d: compiler error invalid type for iexist"
	invalidMemberTypeFmtStr               = "%d: invalid member type"
	incorrectTypeForStatementFmtStr       = "%d: %s requires %s got %s"
	returnNotInFuncFmtStr                 = "%d: can only return from a function"
	invalidTypeForIOFmtStr                = "%d: got %s expected primitive type"
	invalidKindForIOFmtStr                = "%d: got %s expected identifier"
	invalidKindForAtoiFmtStr              = "%d: got %s expected char"
	invalidKindForItoaFmtStr              = "%d: got %s expected int"
	addInstanceVariableOutsideClassFmtStr = "cannot add instance variable %s %s %s ouside of class"
	cannotAssignToLiteralFmtStr           = "%d: cannot assign to a literal %s %s %s"
	thisSymbolNotFoundFmtStr              = "this symbol not found in %s"
	constructorNotInSymbolTableFmtStr     = "constructor for %s not found in symbol table"
	classNotFoundFmtStr                   = "class %s not found"
	staticInitilizerNotFoundFmtStr        = "static initilizer for class %s not found"
	invalidExpressionForStatementFmtStr   = "%d: invalid expression for %s statement"
	invalidReturnFmtStr                   = "%d: function requires %s returned %s"
)

var (
	thisNotDefinedInMain    = errors.New("this not defined in main")
	notInFunction           = errors.New("not in function")
	sarDidNotHaveSymID      = errors.New("SAR did not have a symid")
	noIdentifierOnStack     = errors.New("no identifier on stack")
	noTypeOnStack           = errors.New("no type found on stack")
	expectedSAR             = errors.New("expected sar on stack")
	expectedTypeSAR         = errors.New("expected type sar on stack")
	expectedOperatorOnStack = errors.New("no operator on stack")
	expectedOperator        = errors.New("expected operator in expression")
	expectedArgListSAR      = errors.New("expected arg list SAR")
	expectedFuncSAR         = errors.New("expected func SAR")
	expectedOperand         = errors.New("expected operand")
	invalidExpression       = errors.New("invalid expression")
	tooManyOperands         = errors.New("too many operands")
	tooManyOperators        = errors.New("too many operators")
	expectedIdLitOrFuncSAR  = errors.New("expected id, lit, or func sar")
	noRefSymID              = errors.New("no refSymID of sar when generating ref icode")
	invalidSarMOV           = errors.New("invalid sar when generating mov instruction")
	invalidSarMath          = errors.New("invalid sar when generating math icode")
	mainNotInSymbolTAble    = errors.New("main function was not loaded into symbol table")
)

func invalidReturn(ln uint64, rType string, aType string) error {
	return fmt.Errorf(invalidReturnFmtStr, ln, rType, aType)
}

func staticInitilizerNotFound(className string) error {
	return fmt.Errorf(staticInitilizerNotFoundFmtStr, className)
}

func invalidExpressionForStatement(ln uint64, statement string) error {
	return fmt.Errorf(invalidExpressionForStatementFmtStr, ln, statement)
}

func classNotFound(className string) error {
	return fmt.Errorf(classNotFoundFmtStr, classNotFound)
}

func constructorNotInSymbolTable(class string) error {
	return fmt.Errorf(constructorNotInSymbolTableFmtStr, class)
}

func thisSymbolNotFound(scope Scope) error {
	return fmt.Errorf(thisSymbolNotFoundFmtStr, string(scope))
}

func cannotAssignToLiteral(ln uint64, lVal string, op string, rVal string) error {
	return fmt.Errorf(cannotAssignToLiteralFmtStr, ln, lVal, op, rVal)
}

func addInstanceVariableOutsideClass(typeStr string, val string, accessMod AccessModifier) error {
	return fmt.Errorf(addInstanceVariableOutsideClassFmtStr, typeStr, val, string(accessMod))
}

func invalidKindForItoa(ln uint64, kind string) error {
	return fmt.Errorf(invalidKindForItoaFmtStr, ln, kind)
}

func invalidKindForAtoi(ln uint64, kind string) error {
	return fmt.Errorf(invalidKindForAtoiFmtStr, ln, kind)
}

func invalidKindForIO(ln uint64, kind string) error {
	return fmt.Errorf(invalidKindForIOFmtStr, ln, kind)
}

func invalidTypeForIO(ln uint64, st string) error {
	return fmt.Errorf(invalidTypeForIOFmtStr, ln, st)
}

func returnNotInFunc(ln uint64) error {
	return fmt.Errorf(returnNotInFuncFmtStr, ln)
}

func incorrectTypeForStatement(ln uint64, st string, reqType string, getType string) error {
	return fmt.Errorf(incorrectTypeForStatementFmtStr, ln, st, reqType, getType)
}

func invalidMemberType(ln uint64) error {
	return fmt.Errorf(invalidMemberTypeFmtStr, ln)
}

func memberNotDefined(ln uint64, member string, class string) error {
	return fmt.Errorf(memberNotDefinedFmtStr, ln, member, class)
}

func memberNotPublic(ln uint64, member string, class string) error {
	return fmt.Errorf(memberNotPublicFmtStr, ln, member, class)
}

func invalidTypeForIExist(ln uint64) error {
	return fmt.Errorf(badIExistFmtStr, ln)
}

func invalidTypeForRef(ln uint64, typeStr string) error {
	return fmt.Errorf(invalidTypeForRefFmtStr, ln, typeStr)
}

func nonArrayIndex(ln uint64) error {
	return fmt.Errorf(nonArrayIndexFmtStr, ln)
}

func expectedType(ln uint64, expected string, foundType string) error {
	fmt.Println(debug.CallingFunction())
	return fmt.Errorf(expectedTypeFmtStr, ln, expected, foundType)
}

func constructorNotFound(ln uint64, constructor string) error {
	return fmt.Errorf(constructorNotFoundFmtStr, ln, constructor)
}

func foundExtraToken(lexeme string) error {
	return fmt.Errorf(foundExtraTokenFmtStr, lexeme)
}

func invalidOperator(ln uint64, opStr string) error {
	return fmt.Errorf(invalidOperatorFmtStr, ln, opStr)
}

func constructorNotInClass(className string, constructorName string, ln uint64) error {
	return fmt.Errorf(constructorNotInClassFmtStr, ln, constructorName, className)
}

func duplicateDeclError(ln uint64, typeId string, id string) error {
	return fmt.Errorf(duplicateDeclErrorFmtStr, ln, typeId, id)
}

func typeDoesNotExist(ln uint64, typeId string) error {
	return fmt.Errorf(typeDoesNotExistFmtStr, ln, typeId)
}

func litNotAddedToTable(symbol string) error {
	return fmt.Errorf(litNotAddedToTableFmtStr, symbol)
}

func typeMismatchError(ln uint64, lVal, op, rVal string) error {
	return fmt.Errorf(typeMismatchFmtStr, ln, lVal, op, rVal)
}

func syntaxError(tok *lex.Token, expectedLexeme string) error {
	return fmt.Errorf(syntaxErrorFmtStr, tok.LineNumber(), tok.Lexeme(), expectedLexeme)
}

func variableNotDefined(ln uint64, varID string) error {
	return fmt.Errorf(variableNotDefinedFmtStr, ln, varID)
}

func arrayNotDefined(ln uint64, arrayID string) error {
	return fmt.Errorf(arrayNotDefinedFmtStr, ln, arrayID)
}

func functionNotDefined(ln uint64, funcID string, params []string) error {
	return fmt.Errorf(functionNotDefinedFmtStr, ln, funcID, strings.Join(params, ", "))
}

func notInClassError(ln uint64, scope string) error {
	return fmt.Errorf(notInClassFmtStr, ln, scope)
}
