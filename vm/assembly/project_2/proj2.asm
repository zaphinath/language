-- Part 1
SIZE .int 10
ARR .int 10
.int 2
.int 3
.int 4
.int 15
.int -6
.int 7
.int 8
.int 9
.int 10

offset .int 4
index .int 0
accum .int 0
result .int 0

two .int 2

is .byt 'i'
.byt 's'

even .byt 'e'
.byt 'v'
.byt 'e'
.byt 'n'

odd .byt 'o'
.byt 'd'

sum .byt 'S'
.byt 'u'
.byt 'm'

space .byt ' '
newline .byt '\n'

-- main loop
loop ldr R1 accum
lda R2 ARR
ldr R3 index
ldr R4 offset
mul R3 R4
add R2 R3 -- keep this R2 is now current index of array
ldr R3 R2
add R1 R3
str R1 accum

ldr R1 R2
mov R3 R1
mov R4 R1
ldr R2 two
div R1 R2
mul R1 R2
sub R4 R1
brz R4 printeven
jmp printodd

resume ldr R1 index
adi R1 1
str R1 index
ldr R2 SIZE
cmp R1 R2
brz R1 printsum
jmp loop
-- end main loop


printeven trp 1
ldb R3 space
trp 3

lda R4 is
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

ldr R3 space
trp 3

lda R4 even
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

ldb R3 newline
trp 3

jmp resume

printodd trp 1
ldr R3 space
trp 3

lda R4 is
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

ldb R3 space
trp 3

lda R4 odd
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3
trp 3

ldb R3 newline
trp 3

jmp resume

printsum lda R4 sum
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

ldb R3 space
trp 3

lda R4 is
ldb R3 R4
trp 3

adi R4 1
ldb R3 R4
trp 3

ldb R3 space
trp 3

ldr R3 accum
trp 1

ldb R3 newline
trp 3
trp 3

-- part 2
DAGS .byt 'D'
.byt 'A'
.byt 'G'
.byt 'S'

-- print dags
lda R1 DAGS
ldb R3 R1
trp 3

adi R1 1
ldb R3 R1
trp 3

adi R1 1
ldb R3 R1
trp 3

adi R1 1
ldb R3 R1
trp 3

ldb R3 space
trp 3

ldr R3 DAGS
trp 1

ldb R3 newline
trp 3

-- swap
ldr R7 DAGS
lda R1 DAGS
ldb R2 DAGS
adi R1 2
ldb R3 R1
stb R3 DAGS
stb R2 R1
ldr R6 DAGS

-- print gads
lda R1 DAGS
ldb R3 R1
trp 3

adi R1 1
ldb R3 R1
trp 3

adi R1 1
ldb R3 R1
trp 3

adi R1 1
ldb R3 R1
trp 3

ldb R3 space
trp 3

ldr R3 DAGS
trp 1

ldb R3 newline
trp 3

sub R7 R6
mov R3 R7
trp 1

ldb R3 newline
trp 3

trp 0
