
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

PROC_1:
 PUSH rbp
 MOV rbp, rsp

 MOV rax, $FFFFFFFFFFFFFFF0
 AND rsp, rax

 MOV al, [rbp + 16]
 MOV [Buffer], al

 mov rcx, [Console_handle]
 lea rdx, [Buffer]
 mov r8, 1
 lea r9, [Byte_written]
 push qword 0
 push qword 0
 call [WriteFile]

 LEAVE
 RET 8

PROC_7:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 8
 MOV rcx, qword [rbx + 20]
 SUB rcx, 1
 MOV qword [rbp + -8], rcx
WHILE_14:
 CMP qword [rbp + -8], 0
 JL WHILE_14_END
 MOV rcx, qword [rbp + -8]
 IMUL rcx, rcx, 1
 ADD rcx, rbx
 MOVSX rcx, byte [rcx + 0]
 PUSH rcx
 CALL PROC_1
 MOV rcx, qword [rbp + -8]
 SUB rcx, 1
 MOV qword [rbp + -8], rcx
 JMP WHILE_14
WHILE_14_END:
 LEAVE
 RET 0

PROC_31:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 16
 MOV qword [rbx + 20], 0
 CMP qword [rbp + 16], 0
 JLE IF_36_END
 MOV qword [rbp + -8], 0
WHILE_39:
 CMP qword [rbp + -8], 20
 JGE WHILE_39_END
 MOV rcx, qword [rbp + -8]
 IMUL rcx, rcx, 1
 ADD rcx, rbx
 MOV byte [rcx + 0], 0
 MOV rcx, 1
 ADD rcx, qword [rbp + -8]
 MOV qword [rbp + -8], rcx
 JMP WHILE_39
WHILE_39_END:
REPEAT_51:
 MOV rcx, 10
 MOV rax, qword [rbp + 16]
 CQO
 IDIV rcx
 MOV rcx, rdx
 MOV qword [rbp + -16], rcx
 MOV rcx, 10
 MOV rax, qword [rbp + 16]
 CQO
 IDIV rcx
 MOV rcx, rax
 MOV qword [rbp + 16], rcx
 MOV rcx, qword [rbx + 20]
 IMUL rcx, rcx, 1
 ADD rcx, rbx
 MOV rsi, 48
 ADD rsi, qword [rbp + -16]
 CMP rsi, -128
 JL INTEGER_OVERFLOW_TRAP
 CMP rsi, 127
 JG INTEGER_OVERFLOW_TRAP
 MOV byte [rcx + 0], sil
 MOV rcx, 1
 ADD rcx, qword [rbx + 20]
 MOV qword [rbx + 20], rcx
 CMP qword [rbp + 16], 0
 JNE REPEAT_51
IF_36_END:
 LEAVE
 RET 8

MAIN:
 PUSH 45
 CALL PROC_31
 CALL PROC_7
 RET

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

Global:
	repeat 21
	dq 0
	end repeat

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
