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

section '.bss' data readable writeable
; TYPE DESCRIPTORS SECTION
TYPE1	rb 0 dup ?
; GLOBAL VARIABLES SECTION
Global:
i_	rb 8 dup ?
n_	rb 8 dup ?

section '.OBERON' data readable writeable
	db	0
	dd	3
	db	'max'
	dq	100000
	dd	-2
	db	7
	dd	5
	db	'MyInt'
	dd	-2
	db	1
	dd	4
	db	'Node'
	db	12
	dd	-1
	db	1
	dd	8
	db	'NodeDesc'
	db	11
	dq	17
	dd	-8
	db	4
	dd	3
	db	'val'
	dq	1
	dd	-2
	db	4
	dd	4
	db	'next'
	dq	9
	dd	0
	db	9
	db	1
	dd	6
	db	'TwoDim'
	db	10
	dq	10
	dd	-1
	db	1
	dd	0
	db	10
	dq	10
	dd	-5
	db	2	dd	1
	db	'i'
	dd	-2
	db	2	dd	1
	db	'n'
	dd	0
	db	9

