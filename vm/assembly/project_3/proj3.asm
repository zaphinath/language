zero .int 0
zero_c .byt '0'
one_c .byt '1'
two_c .byt '2'
three_c .byt '3'
four_c .byt '4'
five_c .byt '5'
six_c .byt '6'
seven_c .byt '7'
eight_c .byt '8'
nine_c .byt '9'
nl .byt '\n'
plus .byt '+'
minus .byt '-'
at .byt '@'
is_not_a_number_size .int 18
is_not_a_number .byt
.byt ' '
.byt 'i'
.byt 's'
.byt ' '
.byt 'n'
.byt 'o'
.byt 't'
.byt ' '
.byt 'a'
.byt ' '
.byt 'n'
.byt 'u'
.byt 'm'
.byt 'b'
.byt 'e'
.byt 'r'
.byt '\n'

number_to_big_size .int 15
number_to_big .byt 'N'
.byt 'u'
.byt 'm'
.byt 'b'
.byt 'e'
.byt 'r'
.byt ' '
.byt 't'
.byt 'o'
.byt 'o'
.byt ' '
.byt 'B'
.byt 'i'
.byt 'g'
.byt '\n'

operand_is_size .int 11
operand_is .byt 'O'
.byt 'p'
.byt 'e'
.byt 'r'
.byt 'a'
.byt 'n'
.byt 'd'
.byt ' '
.byt 'i'
.byt 's'
.byt ' '

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

cnt .int
tenth .int
data .int
flag .int
opdv .int

SIZE .int 7
c .byt
.byt
.byt
.byt
.byt
.byt
.byt

-- call reset(1, 0, 0, 0)
ldr R0 zero
adi R0 -24
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP -- set pfp to zero
adi SP -8 
str R0 SP -- set z to zero
adi SP -4
str R0 SP -- set y to zero
adi SP -4
str R0 SP -- set x to zero
adi SP -4
adi R0 1
str R0 SP -- set w to one
mov R1 PC
adi R1 36
str R1 FP
jmp reset

ldr R0 zero
adi R0 -4
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP
mov R0 PC
adi R0 36
str R0 FP
jmp getdata

while_not_at ldb R0 c
mov R3 R0

ldb R1 at
cmp R1 R0
brz R1 end

ldb R0 c
ldb R2 plus
cmp R2 R0
brz R2 call_get_data

ldb R2 minus
cmp R2 R0
brz R2 call_get_data
jmp else_not_plus_or_minus

call_get_data ldr R0 zero
adi R0 -4
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP
mov R0 PC
adi R0 36
str R0 FP
jmp getdata
jmp while_data

else_not_plus_or_minus ldb R0 c
lda R1 c
adi R1 1
stb R0 R1
ldb R1 plus
lda R0 c
stb R1 R0
ldr R1 cnt
adi R1 1
str R1 cnt

while_data ldr R0 data
brz R0 reset_and_get_data

lda R1 c
ldr R2 cnt
add R1 R2
adi R1 -1
ldb R2 R1
ldb R3 nl
cmp R3 R2
brz R3 process_data
jmp not_nl

process_data ldr R2 zero
str R2 data
adi R2 1
str R2 tenth
ldr R2 cnt
adi R2 -2
str R2 cnt
while_not_flag_and_cnt_not_zero ldr R2 flag
ldr R3 cnt
bnz R2 print_good_number
brz R3 print_good_number
-- set up function call for opd
ldr R0 zero
adi R0 -20
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP
adi SP -8
lda R0 c
ldr R1 cnt
add R0 R1
ldb R1 R0
stb R1 SP
adi SP -4
ldr R0 tenth
str R0 SP
adi SP -4
ldb R0 c
stb R0 SP
mov R0 PC
adi R0 36
str R0 FP
jmp opd
-- end function call for opd
ldr R2 cnt
adi R2 -1
str R2 cnt
ldr R2 zero
adi R2 10
ldr R3 tenth
mul R3 R2
str R3 tenth
jmp while_not_flag_and_cnt_not_zero

print_good_number ldr R0 flag
bnz R0 while_data
lda R0 operand_is
ldr R1 operand_is_size
lda R2 operand_is
ldb R3 R0
print_operand_is trp 3
adi R0 1
mov R3 R0
sub R3 R2
cmp R3 R1
brz R3 print_end_of_is_operand
ldb R3 R0
jmp print_operand_is
print_end_of_is_operand ldr R3 opdv
trp 1
ldb R3 nl
trp 3
jmp while_data
-- end print_good_number

not_nl ldr R0 zero
adi R0 -4
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP
mov R0 PC
adi R0 36
str R0 FP
jmp getdata
jmp while_data

reset_and_get_data ldr R0 zero
adi R0 -24
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP -- set pfp to zero
adi SP -8 
str R0 SP -- set z to zero
adi SP -4
str R0 SP -- set y to zero
adi SP -4
str R0 SP -- set x to zero
adi SP -4
adi R0 1
str R0 SP -- set w to one
mov R1 PC
adi R1 36
str R1 FP
jmp reset

ldr R0 zero
adi R0 -4
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov FP SP
adi SP -4
ldr R0 zero
str R0 SP
mov R0 PC
adi R0 36
str R0 FP
jmp getdata

jmp while_not_at

end trp 0

-- activation record for opd
-- parameters
-- char s -20
-- int k -16
-- char j -12

-- local variables
-- int t -8

-- previous frame pointer -4
-- return address 0
opd mov R0 FP
adi R0 -12
ldb R1 R0
adi R0 4 -- set to address of t for setting it after the if else block

ldb R2 zero_c
cmp R2 R1
ldr R3 zero
adi R3 0
brz R2 set

ldb R2 one_c
cmp R2 R1
ldr R3 zero
adi R3 1
brz R2 set

ldb R2 two_c
cmp R2 R1
ldr R3 zero
adi R3 2
brz R2 set

ldb R2 three_c
cmp R2 R1
ldr R3 zero
adi R3 3
brz R2 set

ldb R2 four_c
cmp R2 R1
ldr R3 zero
adi R3 4
brz R2 set

ldb R2 five_c
cmp R2 R1
ldr R3 zero
adi R3 5
brz R2 set

ldb R2 six_c
cmp R2 R1
ldr R3 zero
adi R3 6
brz R2 set

ldb R2 seven_c
cmp R2 R1
ldr R3 zero
adi R3 7
brz R2 set

ldb R2 eight_c
cmp R2 R1
ldr R3 zero
adi R3 8
brz R2 set

ldb R2 nine_c
cmp R2 R1
ldr R3 zero
adi R3 9
brz R2 set

jmp not_digit

-- the following set command assumes that R0 has been set to the address of t and that R3 is the value 
set str R3 R0
jmp flag_test

not_digit ldr R1 zero
adi R1 1
str R1 flag
-- write to stdout
mov R0 FP
adi R0 -12
ldb R3 R0

lda R1 is_not_a_number
stb R3 R1
ldb R3 R1
print_is_not_num trp 3
adi R1 1
mov R3 R1
lda R2 is_not_a_number
sub R3 R2
ldr R2 is_not_a_number_size
cmp R2 R3
brz R2 flag_test
ldb R3 R1
jmp print_is_not_num

flag_test ldr R1 flag
brz R1 flag_is_zero
jmp opd_end

flag_is_zero ldr R1 zero
ldb R1 plus
mov R0 FP
adi R0 -20
ldr R2 R0
cmp R2 R1
brz R2 is_plus
jmp not_plus

is_plus mov R0 FP
adi R0 -8 -- get t
ldr R1 R0
adi R0 -8 -- get k 
ldr R2 R0
mul R1 R2
adi R0 8
str R1 R0
jmp inc_opdv

not_plus mov R0 FP
adi R0 -8 -- get t
ldr R1 R0
adi R0 -8 -- get k 
ldr R2 R0
ldr R3 zero
adi R3 -1
mul R2 R3
mul R1 R2
adi R0 8
str R1 R0
jmp inc_opdv

inc_opdv ldr R0 opdv
add R0 R1
str R0 opdv

-- deallocates the frame
opd_end mov R0 FP
ldr R1 R0 -- return addr
adi R0 -4 -- pfp
ldr R2 R0
mov SP FP
bnz R2 set_pfp_opd
jmp term_opd
set_pfp_opd mov FP R2
term_opd jmr R1
-- end opd

-- activation record for flush
-- pfp -4
-- return address 0
flush ldr R0 zero
str R0 data
ldb R2 nl
lda R0 c
loop_until_nl trp 4
cmp R3 R2
bnz R3 loop_until_nl

-- deallocate the frame
mov R0 FP
ldr R1 R0
adi R0 -4
ldr R2 R0
mov SP FP
bnz R2 set_pfp_flush
jmp term_flush
set_pfp_flush mov FP R2
term_flush jmr R1

-- activation record for getdata
-- pfp -4
-- return address 0
getdata ldr R0 cnt
ldr R1 SIZE
cmp R1 R0
bgt R1 get_char_and_increment
jmp call_flush

get_char_and_increment trp 4
lda R0 c
ldr R1 cnt
add R0 R1
stb R3 R0
adi R1 1
str R1 cnt
jmp term_getdata

call_flush lda R0 number_to_big
ldr R1 number_to_big_size
lda R2 number_to_big
ldb R3 R0
print_number_to_big trp 3
adi R0 1
mov R3 R0
sub R3 R2
cmp R3 R1
brz R3 make_flush_call
ldb R3 R0
jmp print_number_to_big
-- calls the flush function when we are done printing number_to_big
make_flush_call ldr R0 zero
adi R0 -8
mov R1 PC
adi R1 24
jmp check_stack_overflow

mov R1 FP
adi SP -4
mov FP SP
adi SP -4
str R1 SP
mov R0 PC
adi R0 36
str R0 FP
jmp flush

term_getdata mov R0 FP
ldr R1 R0
adi R0 -4
ldr R2 R0
mov SP FP
bnz R2 set_pfp_getdata
jmr R1
set_pfp_getdata mov FP R2
jmr R1

-- activation record for reset
-- w -24
-- x -20
-- y -16
-- z -12
-- k -8
-- pfp -4
-- return address 0
reset mov R0 FP
adi R0 -8
ldr R1 zero
str R1 R0 -- zero out k
lda R2 c
ldr R3 zero

set_c_to_zero str R3 R2
adi R1 1
str R1 R0
add R2 R1
ldr R4 SIZE
cmp R4 R1
blt R4 set_c_to_zero

set_globals mov R0 FP
adi R0 -12
ldr R1 R0
str R1 flag

adi R0 -4
ldr R1 R0
str R1 cnt

adi R0 -4
ldr R1 R0
str R1 opdv

adi R0 -4
ldr R1 R0
str R1 data

-- clean activation record
mov R0 FP
ldr R1 R0
adi R0 -4
ldr R2 R0
mov SP FP
bnz R2 set_pfp_reset
jmr R1
set_pfp_reset mov FP R2
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
