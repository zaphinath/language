package icode

import (
	"fmt"
)

const (
	ADD    IInstruction = "add"
	ADDI   IInstruction = "addi"
	SUB    IInstruction = "sub"
	MUL    IInstruction = "mul"
	DIV    IInstruction = "div"
	LT     IInstruction = "lt"
	GT     IInstruction = "gt"
	NE     IInstruction = "ne"
	EQ     IInstruction = "eq"
	LE     IInstruction = "le"
	GE     IInstruction = "ge"
	AND    IInstruction = "and"
	OR     IInstruction = "or"
	BF     IInstruction = "bf"
	BT     IInstruction = "bt"
	JMP    IInstruction = "jmp"
	PUSH   IInstruction = "push"
	PEEK   IInstruction = "peek"
	FRAME  IInstruction = "frame"
	CALL   IInstruction = "call"
	RTN    IInstruction = "rtn"
	RETURN IInstruction = "return"
	FUNC   IInstruction = "func"
	NEWI   IInstruction = "newi"
	NEW    IInstruction = "new"
	AEF    IInstruction = "aef"
	REF    IInstruction = "ref"
	MOV    IInstruction = "mov"
	MOVI   IInstruction = "movi"
	MOVC   IInstruction = "movc"
	WRTC   IInstruction = "wrtc"
	WRTI   IInstruction = "wrti"
	RDC    IInstruction = "rdc"
	RDI    IInstruction = "rdi"
	TERM   IInstruction = "term"
)

type (
	IInstruction string

	Quad struct {
		label    string
		operator IInstruction
		operand1 string
		operand2 string
		operand3 string
		comment  string
	}

	// I needed a way to add labels to the first statment in an expression for
	// making sure the labels are correct in the control markers. This is just a shim that says
	// when you put a new quad on the Icode then put this label on it.
	labelMarker struct {
		label string
	}

	Icode []interface{}
)

func (q Quad) Label() string {
	return q.label
}

func (q Quad) Operator() IInstruction {
	return q.operator
}

func (q Quad) Operand1() string {
	return q.operand1
}

func (q Quad) Operand2() string {
	return q.operand2
}

func (q Quad) Operand3() string {
	return q.operand3
}

func (q Quad) Comment() string {
	return q.comment
}

func (q Quad) String() string {
	return fmt.Sprintf("%s %s %s %s %s -- %s", q.label, q.operator, q.operand1, q.operand2, q.operand3, q.comment)
}

func NewQuad(label string, operator IInstruction, operand1, operand2, operand3, comment string) Quad {
	return Quad{
		label:    label,
		operator: operator,
		operand1: operand1,
		operand2: operand2,
		operand3: operand3,
		comment:  comment,
	}
}

func NewIcode() Icode {
	return Icode([]interface{}{})
}

func (i *Icode) AddNewQuad(q Quad) {
	if len(*i) == 0 {
		*i = append(*i, q)
	} else {
		top := (*i)[len(*i)-1]
		if lm, ok := top.(labelMarker); ok {
			q.label = lm.label
			(*i)[len(*i)-1] = q
		} else {
			*i = append(*i, q)
		}
	}
}

func (i *Icode) AppendIcode(ic Icode) {
	*i = append(*i, ic...)
}

func (i *Icode) AddLabelMarker(label string) {
	*i = append(*i, labelMarker{
		label: label,
	})
}

func (i Icode) BackPatch(new string, old string) {
	for idx, _ := range i {
		if lm, ok := i[idx].(labelMarker); ok {
			if lm.label == old {
				lm.label = new
				i[idx] = lm
			}
		} else {
			q := i[idx].(Quad)
			if q.label == old {
				q.label = new
			}
			if q.operand1 == old {
				q.operand1 = new
			}
			if q.operand2 == old {
				q.operand2 = new
			}
			if q.operand3 == old {
				q.operand3 = new
			}
			i[idx] = q
		}
	}
}

func (i Icode) LabelMarker() (string, bool) {
	if len(i) == 0 {
		return "", false
	}
	if lm, ok := i[len(i)-1].(labelMarker); ok {
		return lm.label, true
	}
	return "", false
}

func MovComment(src string, dst string) string {
	return fmt.Sprintf("%s → %s", src, dst)
}

func OpComment(op string, operand1 string, operand2 string, target string) string {
	return fmt.Sprintf("%s %s %s → %s", operand1, op, operand2, target)
}

func BranchComment(boolean string, targetLabel string, branchIf bool) string {
	return fmt.Sprintf("↷ %s if %s is %t", targetLabel, boolean, branchIf)
}

func PushComment(name string) string {
	return fmt.Sprintf("push %s onto the runtime stack", name)
}

func FrameComment(funcName string, obj string) string {
	return fmt.Sprintf("construct stack frame for function %s with object %s", funcName, obj)
}

func ConstructorComment(className string) string {
	return fmt.Sprintf("constructor declaration for %s", className)
}

func CallComment(funcName string) string {
	return fmt.Sprintf("call %s", funcName)
}

func NewIComment(className string, outID string) string {
	return fmt.Sprintf("malloc(sizeof(%s)) → %s", className, outID)
}

func ConstructorReturnComment(className string) string {
	return fmt.Sprintf("return from constructor for %s", className)
}

func NewArrayComment(sizeStr string, typeStr string, out string) string {
	return fmt.Sprintf("malloc(%s * sizeof(%s)) → %s", sizeStr, typeStr, out)
}
