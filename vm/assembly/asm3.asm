count .INT 3
a .BYT 'a'
e .BYT 'q'
ldr R2 count
LDR R3 e
trp 1
JMP print
print ADD R3 R2
TRP 3
trp 1

trp 0
