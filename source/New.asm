format PE64
entry New@@INIT

section '.text' code readable executable

New@Test:
push	rbp
mov	rbp, rsp
sub	rsp, 32
mov	r10, qword [rbp + -8]
or	r10, qword [rbp + -16]
mov	qword [rbp + -8], r10
mov	r10, qword [rbp + -8]
mov	r11, qword [rbp + -16]
not	r11
and	r10, r11
mov	qword [rbp + -8], r10
mov	r10, qword [rbp + -8]
and	r10, qword [rbp + -16]
mov	qword [rbp + -8], r10
mov	r10, qword [rbp + -8]
xor	r10, qword [rbp + -16]
mov	qword [rbp + -8], r10
leave
ret

New@@INIT:
push	rbp
mov	rbp, rsp
sub	rsp, 48
lea	rax, [New@@TYPETAG + 0]
mov	[New@@TYPETAG + 8], rax
lea	rax, [New@@TYPETAG + 0]
mov	[New@@TYPETAG + 88], rax
lea	rax, [New@@TYPETAG + 80]
mov	[New@@TYPETAG + 96], rax
lea	rax, [New@@TYPETAG + 160]
mov	[New@@TYPETAG + 168], rax
lea	rax, [New@@TYPETAG + 240]
mov	[New@@TYPETAG + 248], rax
sub	rsp, 32
lea	r10, [New@@STRING + 0]
mov	rcx, r10
call	[@LoadLibrary]
add	rsp, 32
mov	qword [New@@VAR + 24], rax
sub	rsp, 32
mov	r10, qword [New@@VAR + 24]
mov	rcx, r10
lea	r10, [New@@STRING + 11]
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [New@@VAR + 32], rax
mov	qword [New@@VAR + 40], 2147483647
mov	qword [New@@VAR + 48], 2147483647
mov	r10, qword [New@@VAR + 40]
imul	r10, qword [New@@VAR + 48]
jo	INTEGER_OVERFLOW_TRAP
shl	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [New@@VAR + 40], r10
sub	rsp, 32
mov	rcx, 0
lea	rsi, [New@@STRING + 23]
lea	rdi, [rbp + 0]
push	rcx
mov	rcx, 12
rep movsb
pop	rcx
lea	rdx, [rbp + 0]
lea	rsi, [New@@STRING + 35]
lea	rdi, [rbp + 20]
push	rcx
mov	rcx, 5
rep movsb
pop	rcx
lea	r8, [rbp + 20]
mov	r9, 0
call	qword [New@@VAR + 32]
add	rsp, 32
sub	rsp, 32
mov	ecx, 0
call	[@ExitProcess]

INVALID_ARRAY_INDEX_TRAP:
AND	rsp, -16
SUB	rsp, 32
MOV	ecx, -1
CALL	[@ExitProcess]
INTEGER_OVERFLOW_TRAP:
AND	rsp, -16
SUB	rsp, 32
MOV	ecx, -2
CALL	[@ExitProcess]
INVALID_DIVISOR_TRAP:
AND	rsp, -16
SUB	rsp, 32
MOV	ecx, -3
CALL	[@ExitProcess]
TYPE_CHECK_TRAP:
AND	rsp, -16
SUB	rsp, 32
MOV	ecx, -4
CALL	[@ExitProcess]

section '.data' data readable writable
New@@TYPETAG dq 24,0,0,0,0,0,0,0,0,-1,27,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,-1,0,0,0,0,0,0,0,0,0,-1
New@@STRING db 85,83,69,82,51,50,46,68,76,76,0,77,101,115,115,97,103,101,66,111,120,65,0,72,101,108,108,111,32,87,111,114,108,100,0,84,101,115,116,0
New@@VAR db 56 dup ?

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
