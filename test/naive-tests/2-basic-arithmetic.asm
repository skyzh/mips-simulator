li $zero, 100
add $zero, $zero, -1000
li $at, 0xffff
lui $at, 0xffff
move $a0, $at
move $a1, $at
move $a2, $at
move $a3, $at
add $a0, $a0, $a0
addu $a1, $a1, $a1
addi $a2, $a2, 233
addiu $a2, $a2, 233
li $t0, 100
lui $t0, 233
li $t1, 233
lui $t1, 0xffff
or $t2, $t0, $t1
ori $t3, $t0, 0xfffe
and $t4, $t0, $t1
andi $t5, $t0, 0xfffe
sub $t6, $zero, $t0
subu $t7, $zero, $t0
subi $s1, $zero, 0xffff
subiu $s2, $zero, 0xffff
xor $s3, $t0, $t1
xori $s4, $t0, 0xffff
nor $s5, $t0, $t1
