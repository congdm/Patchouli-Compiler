WriteChar_:
	PUSH rbp
	MOV rbp, rsp
	LEAVE
	RET 8

WriteStr_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 8
	MOV r8, qword [len_ + 0]
	SUB r8, 1
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [rbp + -8], r8
WHILE_15:
	CMP qword [rbp + -8], 0
	JL WHILE_15_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 1 + 0]
	MOVSX r8, byte [r8 + 0]
	PUSH r8
	CALL WriteChar_
	MOV r8, qword [rbp + -8]
	SUB r8, 1
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [rbp + -8], r8
	JMP WHILE_15
WHILE_15_END:
	LEAVE
	RET 0

IntToStr_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 16
	MOV qword [len_ + 0], 0
	CMP qword [rbp + 16], 0
	JLE IF_40_END
	MOV qword [rbp + -8], 0
WHILE_43:
	CMP qword [rbp + -8], 20
	JGE WHILE_43_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 1 + 0]
	MOV byte [r8 + 0], 0
	MOV r8, 1
	ADD r8, qword [rbp + -8]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [rbp + -8], r8
	JMP WHILE_43
WHILE_43_END:
REPEAT_58:
	MOV r8, 10
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_59
	IDIV r8
	JMP END_DIVISION_59
NEGATIVE_DIVIDEND_59:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_59
	ADD rdx, r8
END_DIVISION_59:
	MOV r8, rdx
	MOV qword [rbp + -16], r8
	MOV r8, 10
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_74
	IDIV r8
	JMP END_DIVISION_74
NEGATIVE_DIVIDEND_74:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_74
	SUB rax, 1
END_DIVISION_74:
	MOV r8, rax
	MOV qword [rbp + 16], r8
	MOV r8, qword [len_ + 0]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 1 + 0]
	MOV r9, 48
	ADD r9, qword [rbp + -16]
	JO INTEGER_OVERFLOW_TRAP
	CMP r9, -128
	JL INTEGER_OVERFLOW_TRAP
	CMP r9, 127
	JG INTEGER_OVERFLOW_TRAP
	MOV byte [r8 + 0], r9l
	MOV r8, 1
	ADD r8, qword [len_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [len_ + 0], r8
	CMP qword [rbp + 16], 0
	JNE REPEAT_58
IF_40_END:
	LEAVE
	RET 8

Add_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	ADD r8, qword [rbp + 24]
	JO INTEGER_OVERFLOW_TRAP
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
	PUSH qword [len_ + 0]
	PUSH 2
	CALL Add_
	MOV r8, rax
	MOV qword [len_ + 0], r8
	PUSH qword [len_ + 0]
	PUSH 1
	CALL Add_
	LEAVE
	RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:

TYPE_GUARD_TRAP:

; GLOBAL VARIABLES DECLARATION
Global:
s_	db 20 dup 0
len_	db 8 dup 0
