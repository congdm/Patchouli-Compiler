MAIN:
	PUSH rbp
	MOV rbp, rsp
	CMP qword [Global + 0], 0
	JNE ELSIF_9
	MOV r8, qword [Global + 0]
	MOV qword [Global + 8], r8
	JMP IF_4_END
ELSIF_9:
	CMP qword [Global + 0], 0
	JLE ELSE_15
	MOV r8, qword [Global + 0]
	MOV qword [Global + 16], r8
	JMP IF_4_END
ELSE_15:
	MOV r8, qword [Global + 16]
	MOV qword [Global + 8], r8
IF_4_END:
WHILE_19:
	CMP qword [Global + 8], 0
	JLE WHILE_ELSIF_26
	MOV r8, qword [Global + 0]
	SUB r8, qword [Global + 8]
	MOV qword [Global + 8], r8
	JMP WHILE_19
WHILE_ELSIF_26:
	CMP qword [Global + 8], 0
	JGE WHILE_19_END
	MOV r8, qword [Global + 8]
	MOV qword [Global + 16], r8
	MOV qword [Global + 8], 0
	JMP WHILE_19
WHILE_19_END:
REPEAT_34:
	MOV r8, qword [Global + 8]
	ADD r8, qword [Global + 16]
	ADD r8, qword [Global + 0]
	MOV qword [Global + 16], r8
	MOV r8, qword [Global + 0]
	SUB r8, 1
	MOV qword [Global + 0], r8
	CMP qword [Global + 0], 0
	JG REPEAT_34
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
