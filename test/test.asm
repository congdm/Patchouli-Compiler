format PE64 GUI
entry start

section '.text' code readable executable

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
	ADD WHILE_15_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 8 + 0]
	PUSH qword [r8 + 0]
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
	SUB IF_39_END
	MOV qword [rbp + -8], 0
WHILE_42:
	CMP qword [rbp + -8], 20
	SUB WHILE_42_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 8 + 0]
	MOV qword [r8 + 0], 0
	MOV r8, 1
	ADD r8, qword [rbp + -8]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [rbp + -8], r8
	JMP WHILE_42
WHILE_42_END:
REPEAT_57:
	MOV r8, 10
	MOV rax, qword [rbp + 16]
	CQO
	TEST rax, rax
	JLE NEGATIVE_DIVIDEND_58
	IDIV r8
	JMP END_DIVISION_58
NEGATIVE_DIVIDEND_58:
	IDIV r8
	TEST rdx, rdx
	JE END_DIVISION_58
	ADD rdx, r8
END_DIVISION_58:
	MOV r8, rdx
	MOV qword [rbp + -16], r8
	MOV r8, 10
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
	MOV r8, qword [len_ + 0]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 8 + 0]
	MOV r9, 48
	ADD r9, qword [rbp + -16]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [r8 + 0], r9
	MOV r8, 1
	ADD r8, qword [len_ + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV qword [len_ + 0], r8
	CMP qword [rbp + 16], 0
	SUB REPEAT_57
IF_39_END:
	LEAVE
	RET 8

Add_:
	PUSH rbp
	MOV rbp, rsp
	MOV r8, qword [rbp + 16]
	MOV r9, qword [rbp + 24]
	ADD r8, qword [r9 + 0]
	JO INTEGER_OVERFLOW_TRAP
	MOV r9, qword [rbp + 24]
	MOV qword [r9 + 0], r8
	LEAVE
	RET 16

INIT_MODULE:
	PUSH rbp
	MOV rbp, rsp
	PUSH 45
	CALL IntToStr_
	CALL WriteStr_
	LEAVE
	RET

start:
	PUSH  RBX
	PUSH  RSI
	PUSH  RDI
	PUSH  R12
	PUSH  R13
	PUSH  R14
	PUSH  R15

	CALL INIT_MODULE

	POP  R15
	POP  R14
	POP  R13
	POP  R12
	POP  RDI
	POP  RSI
	POP  RBX
	RET

section '.bss' data readable writeable
; TYPE DESCRIPTORS SECTION
; GLOBAL VARIABLES SECTION
Global:
s_	rb 160 dup ?
len_	rb 8 dup ?

section '.OBERON' data readable writeable
	db	9

