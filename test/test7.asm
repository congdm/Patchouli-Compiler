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
	LEAVE
	RET 8

MAIN:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [x_ + 0]
	MOV qword [r8 + 0], 1
	CMP qword [x_ + 16], 1
	JBE RANGE_CHECK_TRAP
	MOV r8, qword [x_ + 0]
	MOV qword [r8 + 8], 2
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
x_	db 24 dup 0
y_	db 24 dup 0
