PROC_1:
 PUSH rbp
 MOV rbp, rsp
 LEAVE
 RET 8

PROC_7:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 8
 MOV r8, qword [rbx + 20]
 SUB r8, 1
 JO INTEGER_OVERFLOW_TRAP
 MOV qword [rbp + -8], r8
WHILE_15:
 CMP qword [rbp + -8], 0
 JL WHILE_15_END
 MOV r8, qword [rbp + -8]
 CMP r8, 20
 JAE RANGE_CHECK_TRAP
 IMUL r8, r8, 1
 ADD r8, rbx
 MOVSX r8, byte [r8 + 0]
 PUSH r8
 CALL PROC_1
 MOV r8, qword [rbp + -8]
 SUB r8, 1
 JO INTEGER_OVERFLOW_TRAP
 MOV qword [rbp + -8], r8
 JMP WHILE_15
WHILE_15_END:
 LEAVE
 RET 0

PROC_35:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 16
 MOV qword [rbx + 20], 0
 CMP qword [rbp + 16], 0
 JLE IF_40_END
 MOV qword [rbp + -8], 0
WHILE_43:
 CMP qword [rbp + -8], 20
 JGE WHILE_43_END
 MOV r8, qword [rbp + -8]
 CMP r8, 20
 JAE RANGE_CHECK_TRAP
 IMUL r8, r8, 1
 ADD r8, rbx
 MOV byte [r8 + 0], 0
 MOV r8, 1
 ADD r8, qword [rbp + -8]
 JO INTEGER_OVERFLOW_TRAP
 MOV qword [rbp + -8], r8
 JMP WHILE_43
WHILE_43_END:
REPEAT_58:
 MOV r8, 10
 MOV rax, qword [rbp + 16]
 CQO
 IDIV r8
 MOV r8, rdx
 MOV qword [rbp + -16], r8
 MOV r8, 10
 MOV rax, qword [rbp + 16]
 CQO
 IDIV r8
 MOV r8, rax
 MOV qword [rbp + 16], r8
 MOV r8, qword [rbx + 20]
 CMP r8, 20
 JAE RANGE_CHECK_TRAP
 IMUL r8, r8, 1
 ADD r8, rbx
 MOV r9, 48
 ADD r9, qword [rbp + -16]
 JO INTEGER_OVERFLOW_TRAP
 CMP r9, -128
 JL INTEGER_OVERFLOW_TRAP
 CMP r9, 127
 JG INTEGER_OVERFLOW_TRAP
 MOV byte [r8 + 0], r9l
 MOV r8, 1
 ADD r8, qword [rbx + 20]
 JO INTEGER_OVERFLOW_TRAP
 MOV qword [rbx + 20], r8
 CMP qword [rbp + 16], 0
 JNE REPEAT_58
IF_40_END:
 LEAVE
 RET 8

PROC_94:
 PUSH rbp
 MOV rbp, rsp
 MOV r8, qword [rbp + 16]
 ADD r8, qword [rbp + 24]
 JO INTEGER_OVERFLOW_TRAP
 MOV qword [rbp + 24], r8
 MOV rax, qword [rbp + 24]
 LEAVE
 RET 16

MAIN:
 MOV r8, 45626473523
 PUSH r8
 CALL PROC_35
 CALL PROC_7
 RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:
