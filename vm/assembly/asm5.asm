test .int -123
o .int 123
nl .byt '\n'

ldr R3 test
trp 1

ldr R3 nl
trp 3

ldr R3 test
ldr R4 o
add R3 R4

trp 1

ldr R3 nl
trp 3

ldr R3 o
adi R3 -100
adi R3 27
trp 1

ldr R3 nl
trp 3

ldr R4 test
ldr R3 test
cmp R3 R4

trp 1

ldr R3 nl
trp 3

one .int 1

ldr R3 one
ldr R4 zero
oR R3 R4
trp 1

ldr R3 nl
trp 3

trp 0

zero .int 0
ldr R3 test
brz R3 noexec

trp 0

n .byt 'n'
noexec ldr R3 n
trp 3

ldr R3 nl
trp 3

trp 0
