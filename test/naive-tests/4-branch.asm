li $v0, 100
li $v1, 50
li $a1, -50
beq $v0, $v0, beq_test_success
li $t0, 233
beq_test_success:
bne $v0, $v1, bne_test_success
li $t1, 233
bne_test_success:
bgez $zero, bgez_test_success_1
li $t2, 233
bgez_test_success_1:
bgez $v0, bgez_test_success_2
li $t2, 233
bgez_test_success_2:
bgtz $v0, bgtz_test_success
li $t3, 233
bgtz_test_success:
ble $v1, $v0, ble_test_success_1
li $t4, 233
ble_test_success_1:
ble $v1, $v1, ble_test_success
li $t4, 233
ble_test_success:
bltz $a1, bltz_test_success
li $t6, 233
bltz_test_success:
blez $zero, blez_test_success_1
li $t7, 233
blez_test_success_1:
blez $a1, blez_test_success
li $t7, 233
blez_test_success: