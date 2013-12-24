format PE64
entry New@@INIT

section '.text' code readable executable

New@@INIT:
push	rbp
mov	rbp, rsp
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
New@@VAR db 24 dup ?

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
