Hello_:
	PUSH rbp
	MOV rbp, rsp
	CMP qword [rbp + 40], 1
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [rbp + 32]
	MOV qword [r8 + 8], 1
	CMP qword [rbp + 24], 2
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [rbp + 16]
	MOV qword [r8 + 16], 2
	MOV r8, qword [rbp + 24]
	SUB r8, 1
	JO INTEGER_OVERFLOW_TRAP
	CMP r8, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	MOV r9, qword [rbp + 16]
	LEA r9, [r9 + r8 * 8 + 0]
	MOV qword [r9 + 0], 3
	LEAVE
	RET 32

Hello2_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 8
	MOV r8, 1
	CMP r8, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	IMUL r8, qword [rbp + 32]
	MOV r9, qword [rbp + 16]
	LEA r9, [r9 + r8 * 8 + 0]
	CMP qword [rbp + 32], 2
	JBE RANGE_CHECK_TRAP
	MOV qword [r9 + 16], 10
	MOV r8, qword [rbp + 24]
	MOV qword [rbp + -8], r8
	MOV r8, qword [rbp + 32]
	MOV qword [rbp + -8], r8
	LEAVE
	RET 24

Hello3_:
	PUSH rbp
	MOV rbp, rsp
	PUSH qword [rbp + 32]
	PUSH qword [rbp + 24]
	PUSH qword [rbp + 16]
	CALL Hello2_
	LEAVE
	RET 24

MAIN:
	PUSH rbp
	MOV rbp, rsp
	PUSH 20
	LEA r8, qword [x_ + 0]
	PUSH r8
	PUSH 20
	LEA r8, qword [x_ + 0]
	PUSH r8
	CALL Hello_
	PUSH 5
	PUSH 5
	LEA r8, qword [y_ + 0]
	PUSH r8
	CALL Hello2_
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

TYPE_GUARD_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
x_	db 160 dup 0
y_	db 200 dup 0
