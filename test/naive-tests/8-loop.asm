li $t1, 100
li $t2, 0x2000
li $t3, 10
li $t4, 10
loop:
sb $t1, 100($t2)
lb $t1, 100($t2)
beq $t3, $t4, loop
