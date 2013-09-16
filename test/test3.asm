MAIN:
	PUSH rbp
	MOV rbp, rsp
	MOV rcx, qword [y_ + 0]
	XOR r8d, r8d
	CMP rcx, 63
	JA END_SET_4
	BTS r8, rcx
END_SET_4:
	MOV rcx, qword [z_ + 0]
	CMP rcx, 63
	JA END_SET_10
	BTS r8, rcx
END_SET_10:
	MOV qword [x_ + 0], r8
	MOV r8, 24
	XOR r8, qword [x_ + 0]
	MOV qword [x_ + 0], r8
	MOVSX r8, byte [a_ + 0]
	ADD r8, qword [y_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [y_ + 0], r8
	MOVSX r8, byte [a_ + 0]
	MOV r9, qword [x_ + 0]
	CMP r8, 63
	JA IF_23_END
	BT r9, r8
	JNC IF_23_END
	MOV r8, qword [x_ + 0]
	MOV qword [y_ + 0], r8
IF_23_END:
	MOV r8, qword [x_ + 0]
	MOV r9, qword [x_ + 0]
	NOT r9
	AND r8, r9
	JNE IF_32_END
	MOV r8, 2
	OR r8, qword [x_ + 0]
	MOV qword [x_ + 0], r8
IF_32_END:
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

TYPE_GUARD_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
x_	db 8 dup 0
y_	db 8 dup 0
z_	db 8 dup 0
a_	db 1 dup 0
b_	db 1 dup 0
