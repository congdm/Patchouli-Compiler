format PE64
entry New@@INIT

section '.text' code readable executable

New@@INIT:
push	rbp
mov	rbp, rsp
sub	rsp, 48
sub	rsp, 32
lea	r10, [New@@STRING + 0]
mov	rcx, r10
call	[@LoadLibrary]
add	rsp, 32
mov	qword [New@@VAR + 8], rax
sub	rsp, 32
mov	r10, qword [New@@VAR + 8]
mov	rcx, r10
lea	r11, [New@@STRING + 11]
mov	rdx, r11
call	[@GetProcAddress]
add	rsp, 32
mov	qword [New@@VAR + 0], rax
sub	rsp, 32
mov	rcx, 0
lea	rsi, [New@@STRING + 23]
lea	rdi, [rbp + -20]
push	rcx
mov	rcx, 6
rep movsb
pop	rcx
lea	rdx, [rbp + -20]
lea	rsi, [New@@STRING + 29]
lea	rdi, [rbp + -40]
push	rcx
mov	rcx, 6
rep movsb
pop	rcx
lea	r8, [rbp + -40]
mov	r9, 0
call	qword [New@@VAR + 0]
add	rsp, 32
sub	rsp, 32
mov	ecx, 0
call	[@ExitProcess]

section '.data' data readable writable
New@@STRING db 85,83,69,82,51,50,46,68,76,76,0,77,101,115,115,97,103,101,66,111,120,65,0,72,101,108,108,111,0,72,101,108,108,111,0
New@@VAR db 16 dup ?

section '.idata' import data readable writeable
dd 0,0,0,RVA @kernel_name,RVA @kernel_table
dd 0,0,0,0,0
@kernel_table:
@ExitProcess dq RVA @nameofExitProcess
@LoadLibrary dq RVA @nameofLoadLibrary
@GetProcAddress dq RVA @nameofGetProcAddress
dq 0
@kernel_name db 'KERNEL32.DLL',0
@nameofExitProcess dw 0
db 'ExitProcess',0
@nameofLoadLibrary dw 0
db 'LoadLibraryA',0
@nameofGetProcAddress dw 0
db 'GetProcAddress',0
