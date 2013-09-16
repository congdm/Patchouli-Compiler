Add_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	ADD r8, qword [rbp + 16]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [rbp + 16], r8
	MOV rax, qword [rbp + 16]
	LEAVE
	RET 8

Atk_:
	PUSH rbp
	MOV rbp, rsp
	CALL qword [rbp + 24]
	PUSH qword [i_ + 0]
	CALL qword [rbp + 16]
	LEAVE
	RET 16

Hello_:
	PUSH rbp
	MOV rbp, rsp
	LEA rax, [Hello_]
	PUSH rax
	LEA rax, [Add_]
	PUSH rax
	CALL Atk_
	LEAVE
	RET 0

MAIN:
	PUSH rbp
	MOV rbp, rsp
	MOV qword [proc_ + 0], 0
	LEA rax, [Hello_]
	MOV qword [proc_ + 0], rax
	CALL qword [proc_ + 0]
	PUSH qword [i_ + 0]
	CALL qword [add_ + 0]
	MOV r8, rax
	ADD r8, qword [i_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [i_ + 0], r8
	LEA rax, [Add_]
	MOV qword [add_ + 0], rax
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

TYPE_GUARD_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
proc_	db 8 dup 0
add_	db 8 dup 0
i_	db 8 dup 0
