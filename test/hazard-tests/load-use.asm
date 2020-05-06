li $a0, 100
li $a2, 0x2000
sb $a0, ($a2)
lb $a1, ($a2)
addi $a1, $a1, 2000
lb $a0, ($a2)
add $a0, $a0, $a1
