WriteChar_:
	PUSH rbp
	MOV rbp, rsp
	LEAVE
	RET 8

WriteStr_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 8
	MOV r8, qword [Global + 160]
	SUB r8, 1
	MOV qword [rbp + -8], r8
WHILE_14:
	CMP qword [rbp + -8], 0
	JL WHILE_14_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, [Global + 0]
	LEA r8, [rax + r8 * 8 + 0]
	PUSH qword [r8 + 0]
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
	MOV qword [Global + 160], 0
	CMP qword [rbp + 16], 0
	JLE IF_37_END
	MOV qword [rbp + -8], 0
WHILE_40:
	CMP qword [rbp + -8], 20
	JGE WHILE_40_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, [Global + 0]
	LEA r8, [rax + r8 * 8 + 0]
	MOV qword [r8 + 0], 0
	MOV r8, 1
	ADD r8, qword [rbp + -8]
	MOV qword [rbp + -8], r8
	JMP WHILE_40
WHILE_40_END:
REPEAT_54:
	MOV r8, 10
	TEST r8, r8
	JLE NOT_POSITIVE_DIVISOR_TRAP
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_55
	IDIV r8
	JMP END_DIVISION_55
NEGATIVE_DIVIDEND_55:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_55
	ADD rdx, r8
END_DIVISION_55:
	MOV r8, rdx
	MOV qword [rbp + -16], r8
	MOV r8, 10
	TEST r8, r8
	JLE NOT_POSITIVE_DIVISOR_TRAP
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_72
	IDIV r8
	JMP END_DIVISION_72
NEGATIVE_DIVIDEND_72:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_72
	SUB rax, 1
END_DIVISION_72:
	MOV r8, rax
	MOV qword [rbp + 16], r8
	MOV r8, qword [Global + 160]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, [Global + 0]
	LEA r8, [rax + r8 * 8 + 0]
	MOV r9, 48
	ADD r9, qword [rbp + -16]
	MOV qword [r8 + 0], r9
	MOV r8, 1
	ADD r8, qword [Global + 160]
	MOV qword [Global + 160], r8
	CMP qword [rbp + 16], 0
	JNE REPEAT_54
IF_37_END:
	LEAVE
	RET 8

Add_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	MOV r9, qword [rbp + 24]
	ADD r8, qword [r9 + 0]
	MOV r9, qword [rbp + 24]
	MOV qword [r9 + 0], r8
	LEAVE
	RET 16

MAIN:
	PUSH rbp
	MOV rbp, rsp
	PUSH 45
	CALL IntToStr_
	CALL WriteStr_
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
