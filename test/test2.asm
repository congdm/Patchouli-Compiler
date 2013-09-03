WriteChar_:
	PUSH rbp
	MOV rbp, rsp
	LEAVE
	RET 8

WriteStr_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 8
	MOV r8, qword [Global + 20]
	SUB r8, 1
	MOV qword [rbp + -8], r8
WHILE_14:
	CMP qword [rbp + -8], 0
	JL WHILE_14_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, [Global + 0]
	LEA r8, [rax + r8 * 1 + 0]
	MOVSX r8, byte [r8 + 0]
	PUSH r8
	CALL WriteChar_
	MOV r8, qword [rbp + -8]
	SUB r8, 1
	MOV qword [rbp + -8], r8
	JMP WHILE_14
WHILE_14_END:
	LEAVE
	RET 0

IntToStr_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 16
	MOV qword [Global + 20], 0
	CMP qword [rbp + 16], 0
	JLE IF_38_END
	MOV qword [rbp + -8], 0
WHILE_41:
	CMP qword [rbp + -8], 20
	JGE WHILE_41_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, [Global + 0]
	LEA r8, [rax + r8 * 1 + 0]
	MOV byte [r8 + 0], 0
	MOV r8, 1
	ADD r8, qword [rbp + -8]
	MOV qword [rbp + -8], r8
	JMP WHILE_41
WHILE_41_END:
REPEAT_55:
	MOV r8, 10
	TEST r8, r8
	JLE NOT_POSITIVE_DIVISOR_TRAP
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_56
	IDIV r8
	JMP END_DIVISION_56
NEGATIVE_DIVIDEND_56:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_56
	ADD rdx, r8
END_DIVISION_56:
	MOV r8, rdx
	MOV qword [rbp + -16], r8
	MOV r8, 10
	TEST r8, r8
	JLE NOT_POSITIVE_DIVISOR_TRAP
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_73
	IDIV r8
	JMP END_DIVISION_73
NEGATIVE_DIVIDEND_73:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_73
	SUB rax, 1
END_DIVISION_73:
	MOV r8, rax
	MOV qword [rbp + 16], r8
	MOV r8, qword [Global + 20]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, [Global + 0]
	LEA r8, [rax + r8 * 1 + 0]
	MOV r9, 48
	ADD r9, qword [rbp + -16]
	MOV byte [r8 + 0], r9l
	MOV r8, 1
	ADD r8, qword [Global + 20]
	MOV qword [Global + 20], r8
	CMP qword [rbp + 16], 0
	JNE REPEAT_55
IF_38_END:
	LEAVE
	RET 8

Add_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	ADD r8, qword [rbp + 24]
	MOV qword [rbp + 24], r8
	MOV rax, qword [rbp + 24]
	LEAVE
	RET 16

MAIN:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, 45626473523
	PUSH r8
	CALL IntToStr_
	CALL WriteStr_
	PUSH qword [Global + 20]
	PUSH 2
	CALL Add_
	MOV r8, rax
	MOV qword [Global + 20], r8
	PUSH qword [Global + 20]
	PUSH 1
	CALL Add_
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
