s .byt ' '

fifty .int 50
sixty .int 60

ldr R0 fifty

new R0 R3
mov R4 R3
trp 1
ldb R3 s
trp 3
del R4
mov R3 R4
trp 1
ldb R3 s
trp 3 
ldr R0 sixty
new R0 R3
trp 1

trp 0
