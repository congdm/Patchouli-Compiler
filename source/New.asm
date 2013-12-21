format PE64
entry New@@INIT

section '.text' code readable executable

New@Ttt:
push	rbp
mov	rbp, rsp
sub	rsp, 32
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	r10, [rbp + 16]
mov	r11, 0
cmp	r11, qword [rbp + -16]
jae	INVALID_ARRAY_INDEX_TRAP
imul	r11, r11, 1
add	r10, r11
movzx	r10, byte [r10 + 0]
mov	byte [rbp + -1], r10l
mov	r10, [rbp + 16]
mov	r11, 1
cmp	r11, qword [rbp + -16]
jae	INVALID_ARRAY_INDEX_TRAP
imul	r11, r11, 1
add	r10, r11
movzx	r10, byte [r10 + 0]
mov	byte [rbp + -2], r10l
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
