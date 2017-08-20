a1 .int 1
a2 .int 2
a3 .int 3
a4 .int 4
a5 .int 5
a6 .int 6

b1 .int 300
b2 .int 150 
b3 .int 50 
b4 .int 20
b5 .int 10
b6 .int 5

c1 .int 500
c2 .int 2
c3 .int 5 
c4 .int 10

J .BYT 'J'
O .byt 'o'
H .byt 'h'
N .byt 'n'
S .byt 's'
O2 .byt 'o'
N2 .byt 'n'
comma .byt ','
space .byt ' '
D .bYt 'D'
Y .Byt 'y'
L .byT 'l'
A .byt 'a'
N3 .byt 'n'

nl .byt '\n'

ldb R3 J
TRP 3
ldb R3 O
tRp 3
ldB R3 H
trp 3
ldb R3 N
trp 3
ldb R3 S
trp 3
ldb R3 O2
trp 3
ldb R3 N2
trp 3

ldb R3 comma 
trp 3

ldb R3 space 
trp 3

ldb R3 D
trp 3
ldb R3 Y
trp 3
ldb R3 L
trp 3
ldb R3 A
trp 3
ldb R3 N3
trp 3

ldb R3 nl 
trp 3
trp 3

ldr R1 b1
ldr R2 b2
add R2 R1
Mov R3 R2
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b3
add R2 R1
mov R3 R2
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b4
add R2 R1
mov R3 R2
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b5
add R2 R1
mov R3 R2
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b6
add R2 R1
mov R3 R2
trp 1
ldb R3 space 
trp 3
trp 3

ldb R3 nl 
trp 3
trp 3

ldr R4 a1
ldr R5 a2
mul R5 R4 
mov R3 R5 
trp 1
ldb R3 space
trp 3
trp 3

ldr R4 a3
mul R5 R4 
mov R3 R5 
trp 1
ldb R3 space
trp 3
trp 3

ldr R4 a4
mul R5 R4 
mov R3 R5 
trp 1
ldb R3 space
trp 3
trp 3

ldr R4 a5
mul R5 R4 
mov R3 R5 
trp 1
ldb R3 space
trp 3
trp 3

ldr R4 a6
mul R5 R4 
mov R3 R5 
trp 1
ldb R3 space
trp 3
trp 3

ldb R3 nl
trp 3
trp 3

ldr R1 b1
mov R3 R2
div R3 R1
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b2
mov R3 R2 
div R3 R1
trp 1
ldb R3 space 
trp 3 
trp 3

ldr R1 b3
mov R3 R2
div R3 R1
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b4
mov R3 R2
div R3 R1
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b5
mov R3 R2
div R3 R1
trp 1
ldb R3 space 
trp 3
trp 3

ldr R1 b6
mov R3 R2
div R3 R1
trp 1
ldb R3 space 
trp 3
trp 3

ldb R3 nl 
trp 3
trp 3

ldr R1 c1
mov R3 R5
sub R3 R1
trp 1
ldb R3 space
trp 3 
trp 3

ldr R1 c2
mov R3 R5
sub R3 R1
trp 1
ldb R3 space
trp 3 
trp 3

ldr R1 c3
mov R3 R5
sub R3 R1
trp 1
ldb R3 space
trp 3 
trp 3

ldr R1 c4
mov R3 R5
sub R3 R1
trp 1
ldb R3 space
trp 3 
trp 3

ldb R3 nl
trp 3

trp 0
