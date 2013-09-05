Hello_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, 1
	CMP r8, qword [rbp + 40]
	JAE RANGE_CHECK_TRAP
	MOV r9, [rbp + 32]
	LEA r8, [r9 + r8 * 8 + 0]
	MOV qword [r8 + 0], 1
	MOV r8, 2
	CMP r8, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	MOV r9, [rbp + 16]
	LEA r8, [r9 + r8 * 8 + 0]
	MOV qword [r8 + 0], 2
	LEAVE
	RET 32

Hello2_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, 1
	CMP r8, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	IMUL r8, qword [rbp + 32]
	MOV r9, [rbp + 16]
	LEA r8, [r9 + r8 * 8 + 0]
	MOV r9, 2
	CMP r9, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	LEA r8, [r8 + r9 * 8 + 0]
	MOV qword [r8 + 0], 10
	LEAVE
	RET 24

Hello3_:
	PUSH rbp
	MOV rbp, rsp
	LEAVE
	RET 16

MAIN:
	PUSH rbp
	MOV rbp, rsp
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
