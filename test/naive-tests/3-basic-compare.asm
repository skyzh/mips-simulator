li $a0, 1000000
li $a1, 2000000
li $a2, 20
li $a3, -100000
sll $t0, $a0, 10
sllv $t1, $a0, $a2
sra $t2, $a0, 10
srav $t3, $a0, $a2
srl $t4, $a0, 10
srlv $t5, $a0, $a2
slt $t6, $a3, $a0
sltu $t7, $a3, $a0
slti $s0, $a3, -10000
sltiu $s1, $a3, 10000
