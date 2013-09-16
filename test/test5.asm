MAIN:
	PUSH rbp
	MOV rbp, rsp
	CMP qword [x_ + 0], 0
	JNE ELSIF_9
	MOV r8, qword [x_ + 0]
	MOV qword [y_ + 0], r8
	JMP IF_4_END
ELSIF_9:
	CMP qword [x_ + 0], 0
	JLE ELSE_15
	MOV r8, qword [x_ + 0]
	MOV qword [z_ + 0], r8
	JMP IF_4_END
ELSE_15:
	MOV r8, qword [z_ + 0]
	MOV qword [y_ + 0], r8
IF_4_END:
WHILE_19:
	CMP qword [y_ + 0], 0
	JLE WHILE_ELSIF_27
	MOV r8, qword [x_ + 0]
	SUB r8, qword [y_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [y_ + 0], r8
	JMP WHILE_19
WHILE_ELSIF_27:
	CMP qword [y_ + 0], 0
	JGE WHILE_19_END
	MOV r8, qword [y_ + 0]
	MOV qword [z_ + 0], r8
	MOV qword [y_ + 0], 0
	JMP WHILE_19
WHILE_19_END:
REPEAT_35:
	MOV r8, qword [y_ + 0]
	ADD r8, qword [z_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	ADD r8, qword [x_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [z_ + 0], r8
	MOV r8, qword [x_ + 0]
	SUB r8, 1
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [x_ + 0], r8
	CMP qword [x_ + 0], 0
	JG REPEAT_35
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
