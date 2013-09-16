Proc1_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [b_ + 0]
	CMP r8, qword [rbp + 24]
	JAE RANGE_CHECK_TRAP
	MOV r9, qword [rbp + 16]
	LEA r9, [r9 + r8 * 8 + 0]
	MOV r8, qword [b_ + 0]
	MOV qword [r9 + 0], r8
	LEAVE
	RET 16

Proc2_:
	PUSH rbp
	MOV rbp, rsp
	PUSH qword [rbp + 24]
	PUSH qword [rbp + 16]
	CALL Proc1_
	MOV r8, qword [rbp + 24]
	MOV r9, qword [rbp + 16]
	MOV qword [r9 + 0], r8
	LEAVE
	RET 16

MAIN:
	PUSH rbp
	MOV rbp, rsp
	PUSH 10
	LEA r8, qword [a_ + 0]
	PUSH r8
	CALL Proc1_
	PUSH 10
	LEA r8, qword [a_ + 0]
	PUSH r8
	CALL Proc2_
	MOV qword [a_ + 8], 10
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

TYPE_GUARD_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
a_	db 80 dup 0
b_	db 8 dup 0
