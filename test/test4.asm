PROC_1:
 PUSH rbp
 MOV rbp, rsp
 MOV r8, qword [Global + 80]
 CMP r8, [rbp + 24]
 JAE RANGE_CHECK_TRAP
 MOV r9, [rbp + 16]
 LEA r8, [r9 + r8 * 8 + 0]
 MOV r9, qword [Global + 80]
 MOV qword [r8 + 0], r9
 LEAVE
 RET 16

PROC_14:
 PUSH rbp
 MOV rbp, rsp
 PUSH [rbp + 24]
 PUSH [rbp + 16]
 CALL PROC_1
 XOR r8d, r8d
 CMP r8, [rbp + 24]
 JAE RANGE_CHECK_TRAP
 MOV r9, [rbp + 16]
 LEA r8, [r9 + r8 * 8 + 0]
 MOV r9, [rbp + 24]
 MOV qword [r8 + 0], r9
 LEAVE
 RET 16

MAIN:
 PUSH 10
 LEA r8, [Global + 0]
 PUSH r8
 CALL PROC_1
 PUSH 10
 LEA r8, [Global + 0]
 PUSH r8
 CALL PROC_14
 MOV qword [Global + 8], 10
 RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
