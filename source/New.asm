format PE64
entry New@@INIT

section '.text' code readable executable

New@Ttt:
push	rbp
mov	rbp, rsp
sub	rsp, 40
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
push	r12
mov	r10, [rbp + 16]
cmp	[rbp + 24], 0
jbe	INVALID_ARRAY_INDEX_TRAP
movzx	r10, byte [r10 + 0]
mov	byte [rbp + -20], r10l
mov	r10, qword [rbp + -28]
cmp	r10, 20
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, [r12 + r10 * 1 + -20]
mov	r11, [rbp + 16]
mov	r12, qword [rbp + -28]
cmp	r12, [rbp + 24]
jae	INVALID_ARRAY_INDEX_TRAP
lea	r11, [r11 + r12 * 1 + 0]
movzx	r11, byte [r11 + 0]
mov	byte [r10 + 0], r11l
pop	r12
leave
ret

New@@INIT:
push	rbp
mov	rbp, rsp
sub	rsp, 32
lea	rcx, [New@@STRING + 0]
mov	rdx, 6
call	New@Ttt
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
New@@STRING db 72,101,108,108,111,0

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
