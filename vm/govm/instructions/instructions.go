package instructions

type (
	Instruction int32
	TrpCode     int32
)

const (
	Term TrpCode = iota
	PutInt
	ReadInt
	PutChar
	ReadChar
)

const (
	JMP Instruction = iota + 1
	JMR
	BNZ
	BGT
	BLT
	BRZ
	MOV
	LDA
	STRrl
	LDRrl
	STBrl
	LDBrl
	ADD
	ADI
	SUB
	MUL
	DIV
	AND
	OR
	CMP
	TRP
	STRrr
	LDRrr
	STBrr
	LDBrr
	RUN
	END
	BLK
	LCK
	ULK
)
