a .byt 'a'
nl .byt '\n'
space .byt ' '

i .int 10
lock .int -1

ldb R3 a
trp 3
ldb R3 nl
trp 3

run R0 test
run R1 test
run R2 test
blk
die trp 0

test lck lock
mov R3 PC
str R3 FP
ldr R3 FP
trp 1
ldb R3 nl
trp 3
ldr R7 i
add R7 R7
str R7 i
mov R3 R7
trp 1
ldb R3 nl
trp 3
ulk lock
end

