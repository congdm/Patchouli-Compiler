MAIN:
 MOV rcx, qword [rbx + 8]
 XOR r8d, r8d
 CMP rcx, 63
 JA END_SET_2
 MOV rax, -1
 SHR rax, cl
 SHL rax, cl
 OR r8, rax
END_SET_2:
 MOV rcx, qword [rbx + 8]
 CMP rcx, 63
 JA END_SET_11
 CMP cl, 0
 JB END_SET_11
 MOV rax
 SHL rax, cl
 NOT rax
 OR r8, rax
END_SET_11:
 MOV qword [rbx + 0], r8
 RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:
