format PE64 GUI
entry start

section '.text' code readable executable

Add_:
	PUSH	rbp
	MOV	rbp, rsp
	MOV	r8, qword [rbp + 16]
	OR	r8, qword [rbp + 16]
	MOV	qword [rbp + 16], r8
	MOV	rax, qword [rbp + 16]
	LEAVE
	RET	8

Atk_:
	PUSH	rbp
	MOV	rbp, rsp
	CALL	qword [rbp + 24]
	PUSH	qword [i_ + 0]
	CALL	qword [rbp + 16]
	LEAVE
	RET	16

Hello_:
	PUSH	rbp
	MOV	rbp, rsp
	LEA	r8, [Hello_]
	PUSH	r8
	LEA	r8, [Add_]
	PUSH	r8
	CALL	Atk_
	LEAVE
	RET	0

INIT_MODULE:
	PUSH	rbp
	MOV	rbp, rsp
	MOV	qword [proc_ + 0], 0
	LEA	rax, [Hello_]
	MOV	qword [proc_ + 0], rax
	CALL	qword [proc_ + 0]
	PUSH	qword [i_ + 0]
	CALL	qword [add_ + 0]
	MOV	r8, rax
	OR	r8, qword [i_ + 0]
	MOV	qword [i_ + 0], r8
	LEA	rax, [Add_]
	MOV	qword [add_ + 0], rax
	LEAVE
	RET

start:
	SUB	rsp, 8	; Makes stack 16 bytes aligned

	CALL	INIT_MODULE

	MOV	ecx, eax
	CALL	[ExitProcess]

section '.data' data readable writeable

; TYPE DESCRIPTORS SECTION
; GLOBAL VARIABLES DECLARATION
Global:
proc_	rb 8 dup 0
add_	rb 8 dup 0
i_	rb 8 dup 0

section '.idata' import data readable writeable

section '.edata' export data readable writeable

