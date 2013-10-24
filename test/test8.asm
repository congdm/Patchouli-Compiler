format PE64 GUI
entry start

section '.text' code readable executable

New1_:
	PUSH	rbp
	MOV	rbp, rsp
	MOV	qword [l_ + 0], 0
	CMP	qword [l_ + 0], 0
	JNE	IF_5_END
IF_5_END:
	LEAVE
	RET	0

Proc2_:
	PUSH	rbp
	MOV	rbp, rsp
	LEA	rax, qword [TYPE_DESC.Node2Desc_3 + 0]
	CMP	rax, qword [rbp + 32]
	JNE	TYPE_GUARD_TRAP
	MOV	r8, qword [rbp + 24]
	MOV	qword [r8 + 16], 0
	MOV	rcx, qword [rbp + 16]
	LEA	rax, qword [TYPE_DESC.Node2Desc_3 + 0]
	CMP	rax, qword [rcx + -16]
	JNE	TYPE_GUARD_TRAP
	MOV	r8, qword [rbp + 16]
	MOV	qword [r8 + 16], 1
	LEA	rax, qword [TYPE_DESC.Node2Desc_3 + 0]
	CMP	rax, qword [rbp + 32]
	JNE	ELSIF_29
	JMP	IF_25_END
ELSIF_29:
	MOV	r8, qword [rbp + 16]
	LEA	rax, qword [TYPE_DESC.Node2Desc_3 + 0]
	CMP	rax, qword [r8 + -16]
	JNE	IF_25_END
IF_25_END:
	LEAVE
	RET	24

INIT_MODULE:
	PUSH	rbp
	MOV	rbp, rsp
	MOV	r8, qword [n_ + 0]
	MOV	qword [r8 + 0], 0
	MOV	r8, qword [l_ + 0]
	MOV	r8, qword [r8 + 0]
	MOV	qword [r8 + 0], 0
	MOV	r8, qword [n2_ + 0]
	MOV	qword [r8 + 16], 1
	MOV	r8, qword [n2_ + 0]
	MOV	qword [r8 + 0], 2
	MOV	r8, qword [l_ + 0]
	MOV	rcx, qword [r8 + 0]
	LEA	rax, qword [TYPE_DESC.Node2Desc_3 + 0]
	CMP	rax, qword [rcx + -16]
	JNE	TYPE_GUARD_TRAP
	MOV	r8, qword [r8 + 0]
	MOV	qword [r8 + 16], 6
	LEAVE
	RET

start:
	SUB rsp, 8	; Makes stack 16 bytes aligned
	; Clear global variables
	MOV ecx, 24
	LEA rdi, [Global]
	XOR eax, eax
	REP STOSB

	CALL INIT_MODULE

	MOV ecx, eax
	CALL [ExitProcess]

section '.data' data readable writeable

; TYPE DESCRIPTORS SECTION
TYPE_DESC.NodeDesc_1	db 32 dup 0
TYPE_DESC.List_2	db 32 dup 0
TYPE_DESC.Node2Desc_3	db 32 dup 0
TYPE_DESC.Rec_4	db 32 dup 0
; GLOBAL VARIABLES DECLARATION
Global:
n_	rb 8 dup 0
l_	rb 8 dup 0
n2_	rb 8 dup 0

section '.idata' import data readable writeable

section '.edata' export data readable writeable
	dd	0, 0, 0, RVA DLL_NAME, 1, 0, 0, RVA EXPORT_TABLE, 0, 0
EXPORT_TABLE:
DLL_NAME:
	db	'TEST8.EXE', 0
