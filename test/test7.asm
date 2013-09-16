Proc1_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	MOV r8, qword [r8 + 0]
	MOV qword [r8 + 0], 0
	MOV r8, qword [rbp + 16]
	CMP qword [r8 + 16], 1
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [r8 + 0]
	MOV qword [r8 + 8], 0
	MOV r8, qword [i_ + 0]
	MOV r9, qword [rbp + 16]
	CMP r8, qword [r9 + 16]
	JAE RANGE_CHECK_TRAP
	MOV r9, qword [r9 + 0]
	LEA r9, [r9 + r8 * 8 + 0]
	MOV qword [r9 + 0], 5
	LEAVE
	RET 8

Proc2_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	MOV r8, qword [r8 + 0]
	MOV r8, qword [r8 + 0]
	MOV qword [r8 + 0], 1
	MOV r8, qword [rbp + 16]
	CMP qword [r8 + 16], 2
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [r8 + 0]
	CMP qword [r8 + 64], 2
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [r8 + 48]
	MOV qword [r8 + 16], 9
	MOV r8, qword [i_ + 0]
	MOV r9, qword [rbp + 16]
	CMP r8, qword [r9 + 16]
	JAE RANGE_CHECK_TRAP
	IMUL r8, r8, 24
	MOV r9, qword [r9 + 0]
	LEA r9, [r9 + r8 * 1 + 0]
	MOV r8, qword [i_ + 0]
	CMP r8, qword [r9 + 16]
	JAE RANGE_CHECK_TRAP
	MOV r9, qword [r9 + 0]
	LEA r9, [r9 + r8 * 8 + 0]
	MOV qword [r9 + 0], 5
	LEAVE
	RET 8

Proc3_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	MOV qword [r8 + 0], 1
	CMP qword [rbp + 24], 10
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [rbp + 16]
	MOV qword [r8 + 80], 2
	MOV r8, qword [i_ + 0]
	CMP r8, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	MOV r9, qword [rbp + 16]
	LEA r9, [r9 + r8 * 8 + 0]
	MOV qword [r9 + 0], 5
	LEAVE
	RET 16

MAIN:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [x_ + 0]
	MOV qword [r8 + 0], 1
	CMP qword [x_ + 16], 1
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [x_ + 0]
	MOV qword [r8 + 8], 2
	MOV r8, qword [y_ + 0]
	MOV r8, qword [r8 + 0]
	MOV qword [r8 + 0], 0
	CMP qword [y_ + 16], 1
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [y_ + 0]
	CMP qword [r8 + 40], 1
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [r8 + 24]
	MOV qword [r8 + 8], 1
	LEA r8, qword [x_ + 0]
	PUSH r8
	CALL Proc1_
	LEA r8, qword [y_ + 0]
	PUSH r8
	CALL Proc2_
	PUSH qword [x_ + 16]
	PUSH qword [x_ + 0]
	CALL Proc3_
	MOV r8, qword [y_ + 0]
	LEA rdi, qword [r8 + 0]
	LEA rsi, qword [x_ + 0]
	MOV rcx, 3
	REP MOVSQ
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

TYPE_GUARD_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
x_	db 24 dup 0
y_	db 24 dup 0
i_	db 8 dup 0
