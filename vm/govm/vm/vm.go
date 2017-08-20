package vm

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"language/vm/govm/common"
	"language/vm/govm/instructions"
)

const (
	MemSize               = 100000000
	ThreadStackSize       = 1000000
	HeapSize              = 1000000
	InstructionSize int32 = 12

	registerCount = 13
	r0            = 0
	r1            = 1
	r2            = 2
	r3            = 3
	r4            = 4
	r5            = 5
	r6            = 6
	r7            = 7
	pc            = 8
	sl            = 9
	sp            = 10
	fp            = 11
	sb            = 12
)

var (
	ThreadOverflow = errors.New("Thread limit exceeded.")
)

type VM struct {
	memory      [MemSize]byte
	registers   [registerCount]int32
	threadLimit int
	threadQueue []int
}

func NewVM(program []byte) *VM {
	const (
		stackStart = MemSize - 56
		stackLimit = MemSize - ThreadStackSize
	)
	threadLimit := (MemSize - HeapSize - len(program)) / ThreadStackSize
	memory := [MemSize]byte{}
	copy(memory[:], program)
	vm := &VM{
		memory: memory,
		registers: [registerCount]int32{
			0,
			0,
			0,
			0,
			0,
			0,
			0,
			0,
			0,
			stackLimit,
			stackStart,
			stackStart,
			stackStart,
		},
		threadQueue: []int{
			0,
		},

		threadLimit: threadLimit,
	}
	vm.storeContext()
	return vm
}

func (v *VM) storeContext() {
	v.storeContextAtThreadIndex(v.threadQueue[0])
}

func (v *VM) storeContextAtThreadIndex(ti int) {
	stackBase := MemSize - ti*ThreadStackSize - 4*registerCount
	for i, reg := range v.registers {
		byts := common.Int32ToBytes(reg)
		copy(v.memory[stackBase+i*4:stackBase+(i+1)*4], byts)
	}
}

func (v *VM) loadContext() {
	base := int32(MemSize - v.threadQueue[0]*ThreadStackSize - 4*registerCount)
	for i, _ := range v.registers {
		v.registers[i] = v.int32FromMemory(base)
		base += 4
	}
}

func (v *VM) int32FromMemory(addr int32) int32 {
	i, err := common.BytesToInt32(v.memory[addr : addr+4])
	if err != nil {
		panic(err)
	}
	return i
}

func (v *VM) storeValueToMemory(addr int32, value int32) {
	copy(v.memory[addr:addr+4], common.Int32ToBytes(value))
}

func (v *VM) newThread(reg int32, addr int32) error {
	threadIndex := 0
threadIndexLoop:
	for {
		if threadIndex > v.threadLimit {
			return ThreadOverflow
		}
		// search the thread queue
		for _, i := range v.threadQueue {
			if i == threadIndex {
				threadIndex++
				continue threadIndexLoop
			}
		}
		break
	}

	base := MemSize - threadIndex*ThreadStackSize
	stackStart := base - 4*(registerCount+1)
	v.threadQueue = append(v.threadQueue, threadIndex)

	currentContext := [registerCount]int32{}
	copy(currentContext[:], v.registers[:])

	v.registers[pc] = addr
	v.registers[sb] = int32(stackStart)
	v.registers[fp] = int32(stackStart)
	v.registers[sp] = int32(stackStart)
	v.registers[sl] = int32(base - ThreadStackSize)
	v.registers[reg] = int32(threadIndex)
	v.storeContextAtThreadIndex(threadIndex)

	v.registers = currentContext

	return nil
}

func (v *VM) Run() (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()

executionLoop:
	for {
		v.loadContext()
		curPC := v.registers[pc]
		op := v.int32FromMemory(curPC)
		operand1 := v.int32FromMemory(curPC + 4)
		operand2 := v.int32FromMemory(curPC + 8)
		v.registers[pc] += InstructionSize
		shouldStoreContext := true

		switch instructions.Instruction(op) {
		case instructions.JMP:
			v.registers[pc] = operand1
		case instructions.JMR:
			v.registers[pc] = v.registers[operand1]
		case instructions.BNZ:
			if v.registers[operand1] != 0 {
				v.registers[pc] = operand2
			}
		case instructions.BGT:
			if v.registers[operand1] > 0 {
				v.registers[pc] = operand2
			}
		case instructions.BLT:
			if v.registers[operand1] < 0 {
				v.registers[pc] = operand2
			}
		case instructions.BRZ:
			if v.registers[operand1] == 0 {
				v.registers[pc] = operand2
			}
		case instructions.MOV:
			v.registers[operand1] = v.registers[operand2]
		case instructions.LDA:
			v.registers[operand1] = operand2
		case instructions.STRrl:
			v.storeValueToMemory(operand2, v.registers[operand1])
		case instructions.LDRrl:
			v.registers[operand1] = v.int32FromMemory(operand2)
		case instructions.STBrl:
			v.memory[operand2] = common.Int32ToBytes(v.registers[operand1])[0]
		case instructions.LDBrl:
			v.registers[operand1] = int32(v.memory[operand2])
		case instructions.ADD:
			v.registers[operand1] += v.registers[operand2]
		case instructions.ADI:
			v.registers[operand1] += operand2
		case instructions.SUB:
			v.registers[operand1] -= v.registers[operand2]
		case instructions.MUL:
			v.registers[operand1] *= v.registers[operand2]
		case instructions.DIV:
			v.registers[operand1] /= v.registers[operand2]
		case instructions.AND:
			if v.registers[operand1] == 1 && v.registers[operand2] == 1 {
				v.registers[operand1] = 1
			} else {
				v.registers[operand1] = 0
			}
		case instructions.OR:
			if v.registers[operand1] == 1 || v.registers[operand2] == 1 {
				v.registers[operand1] = 1
			} else {
				v.registers[operand1] = 0
			}
		case instructions.CMP:
			v1 := v.registers[operand1]
			v2 := v.registers[operand2]
			if v1 == v2 {
				v.registers[operand1] = 0
			} else if v1 > v2 {
				v.registers[operand1] = 1
			} else {
				v.registers[operand1] = -1
			}
		case instructions.TRP:
			switch instructions.TrpCode(operand1) {
			case instructions.Term:
				break executionLoop
			case instructions.PutInt:
				fmt.Print(v.registers[r3])
			case instructions.ReadInt:
				l, readErr := bufio.NewReader(os.Stdin).ReadString('\n')
				if readErr != nil {
					err = readErr
					return
				}
				l = l[:len(l)-1] // remove the newline
				i, convErr := strconv.Atoi(l)
				if convErr != nil {
					err = convErr
					return
				}
				v.registers[r3] = int32(i)

			case instructions.PutChar:
				fmt.Printf("%c", common.Int32ToBytes(v.registers[r3])[0])
			case instructions.ReadChar:
				b := make([]byte, 1)
				os.Stdin.Read(b)
				v.registers[r3] = int32(b[0])
			default:
				err = fmt.Errorf("Invalid trp code %d", operand1)
				return
			}
		case instructions.STRrr:
			v.storeValueToMemory(v.registers[operand2], v.registers[operand1])
		case instructions.LDRrr:
			v.registers[operand1] = v.int32FromMemory(v.registers[operand2])
		case instructions.STBrr:
			v.memory[v.registers[operand2]] = common.Int32ToBytes(v.registers[operand1])[0]
		case instructions.LDBrr:
			v.registers[operand1] = int32(v.memory[v.registers[operand2]])
		case instructions.RUN:
			err = v.newThread(operand1, operand2)
			if err != nil {
				return
			}
		case instructions.END:
			if v.threadQueue[0] != 0 {
				v.threadQueue = v.threadQueue[1:]
				shouldStoreContext = false
			}
		case instructions.BLK:
			if v.threadQueue[0] == 0 && len(v.threadQueue) > 1 {
				v.registers[pc] -= InstructionSize
			}
		case instructions.LCK:
			cur := v.threadQueue[0]
			lockVal := v.int32FromMemory(operand1)
			if lockVal == int32(cur) || lockVal < 0 {
				v.storeValueToMemory(operand1, int32(cur))
			} else {
				v.registers[pc] -= InstructionSize
			}
		case instructions.ULK:
			cur := v.threadQueue[0]
			lockVal := v.int32FromMemory(operand1)
			if int32(cur) == lockVal {
				v.storeValueToMemory(operand1, -1)
			}
		default:
			err = fmt.Errorf("Unknown instruction encountered %d", op)
			return
		}

		if shouldStoreContext {
			v.storeContext()
			temp := v.threadQueue[0]
			v.threadQueue = append(v.threadQueue[1:], temp)
		}
	}
	err = nil
	return
}
