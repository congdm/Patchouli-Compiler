
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

RANGE_CHECK_TRAP:
	mov	ecx,eax
	call	[ExitProcess]

PROC_1:
 PUSH rbp
 MOV rbp, rsp

 MOV rax, $FFFFFFFFFFFFFFF0
 AND rsp, rax

 MOV rax, [rbp + 16]
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
PROC_6:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 8
 MOV rcx, [rbx + 160]
 SUB rcx, 1
 MOV [rbp + -8], rcx
WHILE_13:
 CMP qword [rbp + -8], 0
 JL WHILE_13_END
 MOV rcx, [rbp + -8]
 CMP rcx, 20
 JAE RANGE_CHECK_TRAP
 IMUL rcx, rcx, 8
 ADD rcx, rbx
 PUSH qword [rcx + 0]
 CALL PROC_1
 MOV rcx, [rbp + -8]
 SUB rcx, 1
 MOV [rbp + -8], rcx
 JMP WHILE_13
WHILE_13_END:
 LEAVE
 RET 0
PROC_30:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 16
 MOV qword [rbx + 160], 0
 CMP qword [rbp + 16], 0
 JLE IF_35_END
 MOV qword [rbp + -8], 0
WHILE_38:
 CMP qword [rbp + -8], 20
 JGE WHILE_38_END
 MOV rcx, [rbp + -8]
 CMP rcx, 20
 JAE RANGE_CHECK_TRAP
 IMUL rcx, rcx, 8
 ADD rcx, rbx
 MOV qword [rcx + 0], 0
 MOV rcx, 1
 ADD rcx, [rbp + -8]
 MOV [rbp + -8], rcx
 JMP WHILE_38
WHILE_38_END:
REPEAT_52:
 XOR rdx, rdx
 MOV rcx, 10
 MOV rax, [rbp + 16]
 IDIV rcx
 MOV rcx, rdx
 MOV [rbp + -16], rcx
 XOR rdx, rdx
 MOV rcx, 10
 MOV rax, [rbp + 16]
 IDIV rcx
 MOV rcx, rax
 MOV [rbp + 16], rcx
 MOV rcx, [rbx + 160]
 CMP rcx, 20
 JAE RANGE_CHECK_TRAP
 IMUL rcx, rcx, 8
 ADD rcx, rbx
 MOV rsi, 48
 ADD rsi, [rbp + -16]
 MOV [rcx + 0], rsi
 MOV rcx, 1
 ADD rcx, [rbx + 160]
 MOV [rbx + 160], rcx
 CMP qword [rbp + 16], 0
 JNE REPEAT_52
IF_35_END:
 LEAVE
 RET 8
PROC_81:
 PUSH rbp
 MOV rbp, rsp
 MOV rcx, [rbp + 24]
 MOV rcx, [rcx + 0]
 ADD rcx, [rbp + 16]
 MOV rsi, [rbp + 24]
 MOV [rsi + 0], rcx
 LEAVE
 RET 16
MAIN:
 PUSH 45
 CALL PROC_30
 CALL PROC_6
 RET

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
