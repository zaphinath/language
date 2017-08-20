zero .int 0
one .int 1

thread_lock .int -1

factorial_word .byt 'F'
.byt 'a'
.byt 'c'
.byt 't'
.byt 'o'
.byt 'r'
.byt 'i'
.byt 'a'
.byt 'l'
.byt ' '

o .byt 'o'
f .byt 'f'

i .byt 'i'
s .byt 's'

space .byt ' '

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

array_lock .int -1
cnt .int 0
array .int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int
.int

-- PART 1 --
part_1 trp 2
ldr R0 zero
cmp R0 R3
brz R0 part_2
mov R6 R3
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

lda R0 array
ldr R1 cnt
add R0 R1
str R6 R0
ldr R3 R0
adi R0 4

ldr R3 SP
str R3 R0

adi R1 8
str R1 cnt

mov R4 R6
ldr R5 SP
adi SP 4
mov R6 PC
adi R6 24
jmp print_factorial

jmp part_1
-- END PART 1 --

-- PART 2 --
part_2 ldr R0 cnt
brz R0 part_3 -- handles the case where the user doesn't put in any numbers for part 1. Will Welborn test for this?
adi R0 -4
ldr R1 zero
ldr R2 zero
print_factorials mov R3 R0
cmp R3 R1
brz R3 term_part_2
brz R2 z
jmp nz

z lda R7 array
add R7 R1
ldr R3 R7
trp 1
ldb R3 space
trp 3
adi R1 4
ldr R2 one
jmp print_factorials

nz lda R7 array
add R7 R0
ldr R3 R7
trp 1
ldb R3 space
trp 3
adi R0 -4
ldr R2 zero
jmp print_factorials

term_part_2 lda R7 array
add R7 R1
ldr R3 R7
trp 1
ldb R3 nl
trp 3
jmp part_3
-- END PART 2 --

-- PART 3 --
part_3 ldr R0 zero
str R0 cnt
lck thread_lock

get_x_values trp 2
ldr R0 zero
cmp R0 R3
brz R0 run_threads
mov R6 R3
run R0 multithreaded_factorial
jmp get_x_values

multithreaded_factorial lck thread_lock
ulk thread_lock
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

mov R4 R6
ldr R5 SP
adi SP 4
mov R6 PC
adi R6 24
jmp print_factorial

lck array_lock
lda R0 array
ldr R1 cnt
add R0 R1
str R4 R0
ldr R3 R0
adi R0 4
str R5 R0
adi R1 8
str R1 cnt
ulk array_lock
end

run_threads ulk thread_lock
blk 
print_results ldr R0 cnt
brz R0 term
adi R0 -4
ldr R1 zero
ldr R2 zero
print_factorials_after_threads ldr R3 cnt
mov R3 R0
cmp R3 R1
brz R3 term_part_3
brz R2 z_part_3
jmp nz_part_3

z_part_3 lda R7 array
add R7 R1
ldr R3 R7
trp 1
ldb R3 space
trp 3
adi R1 4
ldr R2 one
jmp print_factorials_after_threads

nz_part_3 lda R7 array
add R7 R0
ldr R3 R7
trp 1
ldb R3 space
trp 3
adi R0 -4
ldr R2 zero
jmp print_factorials_after_threads

term_part_3 lda R7 array
add R7 R1
ldr R3 R7
trp 1
ldb R3 nl
trp 3

term trp 0
-- END PART 3 --

-- activation record for factorial
-- n -8
-- pfp -4
-- return address
-- puts return value on the stack after the frame is deallocated
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
ldr R7 SP
adi SP 4
mul R7 R1
jmp end_factorial

end_factorial mov R0 FP
ldr R1 R0 -- get return address
adi R0 -4
ldr R2 R0 -- get pfp
mov SP FP
mov FP R2
adi SP -4
str R7 SP
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
brz R2 term
jmp stack_overflow_print

-- R4 X
-- R5 Y
-- R6 return address
-- Too many registers were being used up in parts one and two if you used a loop without making a stack frame to store
-- temporary variables.
-- So I did this longform.
print_factorial lda R7 factorial_word
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3
adi R7 1
ldb R3 R7
trp 3

ldb R3 o
trp 3
ldb R3 f
trp 3
ldb R3 space
trp 3

mov R3 R4
trp 1

ldb R3 space
trp 3
ldb R3 i
trp 3
ldb R3 s
trp 3
ldb R3 space
trp 3

mov R3 R5
trp 1
ldb R3 nl
trp 3
jmr R6
