MAIN:
	PUSH rbp
	MOV rbp, rsp
	MOV rcx, qword [Global + 8]
	XOR r8d, r8d
	CMP rcx, 63
	JA END_SET_4
	BTS r8, ecx
END_SET_4:
	MOV r9, qword [Global + 16]
	CMP r9, 63
	JA END_SET_10
	BTS r8, r9d
END_SET_10:
	MOV qword [Global + 0], r8
	MOV r8, 24
	XOR r8, qword [Global + 0]
	MOV qword [Global + 0], r8
	MOVSX r8, byte [Global + 24]
	ADD r8, qword [Global + 8]
	MOV qword [Global + 8], r8
	MOVSX r8, byte [Global + 24]
	MOV r9, qword [Global + 0]
	CMP r8, 63
	JA IF_22_END
	BT r9, r8
	JNC IF_22_END
	MOV r8, qword [Global + 0]
	MOV qword [Global + 8], r8
IF_22_END:
	MOV r8, qword [Global + 0]
	MOV r9, qword [Global + 0]
	NOT r9
	AND r8, r9
	JNE IF_31_END
	MOV r8, 2
	OR r8, qword [Global + 0]
	MOV qword [Global + 0], r8
IF_31_END:
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
