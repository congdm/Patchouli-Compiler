
; Example of 64-bit PE program

format PE64 console
entry start

section '.text' code readable executable

start:
	sub	rsp,8*5 	; reserve stack for API use and make stack dqword aligned
	lea	rbx, [Global]

	mov	ecx, -11
	call	[GetStdHandle]
	mov	[Console_handle], rax

	call	MAIN

	mov	ecx,eax
	call	[ExitProcess]

WriteChar_:
	PUSH rbp
	MOV rbp, rsp
	
	MOV rax, -16
	AND rsp, rax
	sub rsp, 48

	MOV al, [rbp + 16]
	MOV [Buffer], al

	mov rcx, [Console_handle]
	lea rdx, [Buffer]
	mov r8, 1
	lea r9, [Byte_written]
	mov qword [rsp + 8], 0
	mov qword [rsp], 0
	call [WriteFile]
	
	LEAVE
	RET 8

WriteStr_:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 8

	MOV r8, qword [len_ + 0]
	SUB r8, 1
	MOV qword [rbp + -8], r8

WHILE_14:
	CMP qword [rbp + -8], 0
	JL WHILE_14_END

	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP

	LEA rax, qword [s_ + 0]
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
	MOV qword [len_ + 0], 0
	CMP qword [rbp + 16], 0
	JLE IF_37_END
	MOV qword [rbp + -8], 0
WHILE_40:
	CMP qword [rbp + -8], 20
	JGE WHILE_40_END
	MOV r8, qword [rbp + -8]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
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
	MOV r8, qword [len_ + 0]
	CMP r8, 20
	JAE RANGE_CHECK_TRAP
	LEA rax, qword [s_ + 0]
	LEA r8, [rax + r8 * 8 + 0]
	MOV r9, 48
	ADD r9, qword [rbp + -16]
	MOV qword [r8 + 0], r9
	MOV r8, 1
	ADD r8, qword [len_ + 0]
	MOV qword [len_ + 0], r8
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
	
NOT_POSITIVE_DIVISOR_TRAP:
	mov	ecx,eax
	call	[ExitProcess]

RANGE_CHECK_TRAP:
	mov	ecx,eax
	call	[ExitProcess]

INTEGER_OVERFLOW_TRAP:
	mov	ecx,eax
	call	[ExitProcess]

section '.data' data readable writeable

Console_handle dq 0
Buffer db 0, 0
Byte_written dq 0

; GLOBAL VARIABLES DECLARATION
Global:
s_	db 160 dup 0
len_	dq 0

section '.idata' import data readable writeable

  dd 0,0,0,RVA kernel_name,RVA kernel_table
  dd 0,0,0,RVA user_name,RVA user_table
  dd 0,0,0,0,0

  kernel_table:
    GetStdHandle dq RVA _GetStdHandle
    WriteFile dq RVA _WriteFile
    ExitProcess dq RVA _ExitProcess
    dq 0
  user_table:
    MessageBoxA dq RVA _MessageBoxA
    dq 0

  kernel_name db 'KERNEL32.DLL',0
  user_name db 'USER32.DLL',0

  _GetStdHandle dw 0
    db 'GetStdHandle',0
  _WriteFile dw 0
    db 'WriteFile',0
  _ExitProcess dw 0
    db 'ExitProcess',0
  _MessageBoxA dw 0
    db 'MessageBoxA',0
