
#function $bagel 0xa32cc
b .two
add r7, r6, r5
.two
mullw r7,r6,r5
sub r7,r6,r5
and r7,r6,r5
or r7,r6,r5
mr r7,r6
slw r7,r6,r5
sraw r7, r6, r5 % Shift right algebraic
srw r7,r6, r5
extsb r7, r6
extsh r7, r6
bl $debugfunc
cmpw r7,r6
beq .two
cmpwi r7,0x0708
bgt .two
cmplwi r7,0x0708
ble .two
lis r3,0x805C
subi r3,r3,0x3b90
addi r3,r1,0x0001
mulli r3,r1,0x0001
andi r3,r1,0x0001
ori r3,r1,0x0001
nop
rlwinm r3,r3,2,0,29
lbz r7, 0 (r6)
lhz r7, 0x0004 (r6)
lha r7, 0x0008 (r6)
lwz r7, 0x000C (r6)
stb r7, 0x0011 (r6)
sth r7, 0x001e ( r6 )
stw r7, -0x0020 (r6)
mflr r0
mtlr r0
nop

#function $debugfunc 0xa4208
nop
nop
nop
