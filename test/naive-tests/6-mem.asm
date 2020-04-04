li $s1, 0x2000
li $s2, 0x0
li $s5, 0x200
loop:
add $s3, $s1, $s2
sb $s2, 5($s3)
add $s2, $s2, 1
ble $s2, $s5, loop

li $s1, 0x2000
li $s2, 0x0
li $s5, 0x200
li $s4, 0x0
li $s7, 0x0
loop2:
add $s3, $s1, $s2
lb $s4, 5($s3)
add $s2, $s2, 1
add $s7, $s4, $s7
ble $s2, $s5, loop2
