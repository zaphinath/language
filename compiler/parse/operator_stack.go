package parse

import (
	"fmt"
)

func newOperator(opStr string, lineNum uint64) (operator, error) {
	var precedence int
	switch opStr {
	case "*", "/":
		precedence = 7
	case "+", "-":
		precedence = 6
	case ">", ">=", "<", "<=":
		precedence = 5
	case "==", "!=":
		precedence = 4
	case "and":
		precedence = 3
	case "or":
		precedence = 2
	case "=":
		precedence = 1
	case "(", ")", "[", "]":
		precedence = 0
	default:
		return operator{}, fmt.Errorf("invalid operator: %s", opStr)
	}
	return operator{
		op:            opStr,
		precedenceVal: precedence,
		lineNum:       lineNum,
	}, nil
}

type operator struct {
	op            string
	precedenceVal int
	lineNum       uint64
}

func (o operator) operator() string {
	return o.op
}

func (o operator) precedence() int {
	return o.precedenceVal
}

func (o operator) lineNumber() uint64 {
	return o.lineNum
}

type operatorStack struct {
	stack stack
}

func (o *operatorStack) pop() {
	o.stack.pop()
}

func (o *operatorStack) push(op operator) {
	o.stack.push(op)
}

func (o *operatorStack) peek() (operator, bool) {
	op, ok := o.stack.peek()
	if ok {
		return op.(operator), ok
	}
	return operator{}, ok
}

func (o *operatorStack) peekPop() (operator, bool) {
	op, ok := o.stack.peekPop()
	if ok {
		return op.(operator), ok
	}
	return operator{}, ok
}

func (o *operatorStack) isEmpty() bool {
	return o.stack.isEmpty()
}
