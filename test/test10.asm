format PE64 GUI
entry start

section '.text' code readable executable

INIT_MODULE:
	PUSH rbp
	MOV rbp, rsp
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

section '.data' data readable writeable
; TYPE DESCRIPTORS SECTION
TYPE_DESC.NodeDesc_1	db 32 dup 0
; GLOBAL VARIABLES DECLARATION
Global:
i_	rb 8 dup 0
n_	rb 8 dup 0

section '.OBERON' data readable writeable
	db	'Const	max	100000	Int	'
	db	'Type	MyInt	Int	'
	db	'Type	Node	Pointer	-1	'
	db	'Type	NodeDesc	Record	17	NoBase	'
	db	'Field	val	1	Int	'
	db	'Field	next	9	0	'
	db	'EndRecord	'
	db	'Type	TwoDim	Array	10	-1	'
	db	'Type	0	Array	10	Int32	'
	db	'Var	i	Int	'
	db	'Var	n	0	'

section '.idata' import data readable writeable

section '.edata' export data readable writeable
	dd	0, 0, 0, RVA DLL_NAME, 1, 3, 0, RVA EXPORT_TABLE, 0, 0
EXPORT_TABLE:
	dd	RVA TYPE_DESC.NodeDesc_1
	dd	RVA i_
	dd	RVA n_
DLL_NAME:
	db	'TEST10.DLL', 0
