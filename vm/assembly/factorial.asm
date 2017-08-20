zero .int 0
nl .byt '\n'

stack_overflow_error_size .int 15
stack_overflow_error .byt 'S'
.byt 't'
.byt 'a'
.byt 'c'
.byt 'k'
.byt ' '
.byt 'o'
.byt 'v'
.byt 'e'
.byt 'r'
.byt 'f'
.byt 'l'
.byt 'o'
.byt 'w'
.byt '\n'

trp 2
mov FP SP
adi SP -4
ldr R0 zero
str R0 SP
adi SP -4
str R3 SP
mov R0 PC
adi R0 36
str R0 FP
jmp factorial

mov R3 R7
trp 1
ldb R3 nl
trp 3

end trp 0

-- activation record for factorial
-- n -8
-- pfp -4
-- return address
-- puts return value in R7
factorial mov R0 FP
adi R0 -8
ldr R1 R0
ldr R2 zero
cmp R2 R1

brz R2 return_one
jmp next_recursion

return_one ldr R7 zero
adi R7 1
jmp end_factorial

next_recursion ldr R0 zero
adi R0 -12
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov R0 FP
adi R0 -8
ldr R2 R0
adi R2 -1

mov R3 FP
adi SP -4
mov FP SP
adi SP -4
str R3 SP
adi SP -4
str R2 SP
mov R0 PC
adi R0 36
str R0 FP
jmp factorial

mov R0 FP
adi R0 -8
ldr R1 R0
mul R7 R1
jmp end_factorial

end_factorial mov R0 FP
ldr R1 R0 -- get return address
adi R0 -4
ldr R2 R0 -- get pfp
mov SP FP
mov FP R2
jmr R1

-- R0 size of new activation record
-- R1 return address
check_stack_overflow mov R2 SL
add R0 SP
cmp R0 R2
blt R0 stack_overflow
jmr R1
stack_overflow lda R0 stack_overflow_error
ldr R1 stack_overflow_error_size
stack_overflow_print ldb R3 R0
trp 3
adi R0 1
mov R2 R0
lda R3 stack_overflow_error
sub R2 R3
sub R2 R1
brz R2 end
jmp stack_overflow_print
