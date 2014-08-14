format PE64
entry Test2@@INIT

section '.text' code readable executable

Test2@MakeAnsiStr:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	[rbp + 32], r8
mov	rcx, qword [rbp + 32]
mov	qword [rbp + -16], rcx
mov	rcx, qword [rbp + -16]
cmp	rcx, 256
jle	Test2@MakeAnsiStr@29
mov	qword [rbp + -16], 256
Test2@MakeAnsiStr@29:
mov	qword [rbp + -8], 0
Test2@MakeAnsiStr@30:
mov	rcx, qword [rbp + -8]
cmp	rcx, qword [rbp + -16]
jge	Test2@MakeAnsiStr@50
mov	rcx, [rbp + 16]
mov	rdx, qword [rbp + -8]
cmp	rdx, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, [rcx + rdx * 1 + 0]
mov	rdx, qword [rbp + -8]
mov	r8, [rbp + 24]
cmp	rdx, qword [rbp + 32]
jae	INVALID_ARRAY_INDEX_TRAP
lea	rdx, [r8 + rdx * 2 + 0]
movzx	edx, word [rdx + 0]
mov	byte [rcx + 0], dl
mov	rcx, qword [rbp + -8]
add	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], rcx
jmp	Test2@MakeAnsiStr@30
Test2@MakeAnsiStr@50:
mov	rcx, qword [rbp + -16]
sub	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	rdx, [rbp + 16]
cmp	rcx, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, [rdx + rcx * 1 + 0]
movzx	ecx, byte [rcx + 0]
cmp	rcx, 0
je	Test2@MakeAnsiStr@68
mov	rcx, qword [rbp + -16]
sub	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	rdx, [rbp + 16]
cmp	rcx, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, [rdx + rcx * 1 + 0]
mov	byte [rcx + 0], 0
Test2@MakeAnsiStr@68:
leave
ret

Test2@NullStringLen:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	qword [rbp + -8], 0
mov	byte [rbp + -9], 1
Test2@NullStringLen@25:
mov	rcx, qword [rbp + -8]
cmp	rcx, qword [rbp + 24]
jge	Test2@NullStringLen@45
cmp	byte [rbp + -9], 0
je	Test2@NullStringLen@45
mov	rcx, qword [rbp + -8]
mov	rdx, [rbp + 16]
cmp	rcx, qword [rbp + 24]
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, qword [rdx + rcx * 1 + 0]
movzx	ecx, byte [rcx + 0]
cmp	rcx, 0
je	Test2@NullStringLen@43
mov	rcx, qword [rbp + -8]
add	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], rcx
jmp	Test2@NullStringLen@44
Test2@NullStringLen@43:
mov	byte [rbp + -9], 0
Test2@NullStringLen@44:
jmp	Test2@NullStringLen@25
Test2@NullStringLen@45:
mov	rax, qword [rbp + -8]
leave
ret

Test2@ZeroClearRecord:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	qword [rbp + -8], 0
Test2@ZeroClearRecord@24:
mov	rcx, qword [rbp + -8]
cmp	rcx, qword [rbp + 24]
jge	Test2@ZeroClearRecord@36
mov	rcx, qword [rbp + 16]
add	rcx, qword [rbp + -8]
jo	INTEGER_OVERFLOW_TRAP
mov	byte [rcx + 0], 0
mov	rcx, qword [rbp + -8]
add	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], rcx
jmp	Test2@ZeroClearRecord@24
Test2@ZeroClearRecord@36:
leave
ret

Test2@InitLibrary:
push	rbp
mov	rbp, rsp
sub	rsp, 272
sub	rsp, 32
lea	rcx, [Test2@@STRING + 0]
call	[@LoadLibrary]
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -264], rcx
sub	rsp, 32
lea	rcx, [Test2@@STRING + 26]
call	[@LoadLibrary]
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -272], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 48]
mov	r8d, 17
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -264]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 0], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 82]
mov	r8d, 15
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 8], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 112]
mov	r8d, 16
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 16], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 144]
mov	r8d, 11
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 24], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 166]
mov	r8d, 12
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 32], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 190]
mov	r8d, 17
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 40], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 224]
mov	r8d, 17
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 48], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 258]
mov	r8d, 15
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 56], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 288]
mov	r8d, 14
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 64], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 316]
mov	r8d, 16
call	Test2@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -272]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test2@@VAR + 72], rcx
leave
ret

Test2@WndProc:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	[rbp + 32], r8
mov	[rbp + 40], r9
mov	qword [rbp + -8], 0
mov	rcx, qword [rbp + 24]
cmp	rcx, 16
jne	Test2@WndProc@37
mov	rax, qword [Test2@@VAR + 64]
sub	rsp, 32
push	rax
mov	rcx, qword [rbp + 16]
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -16], rcx
jmp	Test2@WndProc@60
Test2@WndProc@37:
mov	rcx, qword [rbp + 24]
cmp	rcx, 2
jne	Test2@WndProc@48
mov	rax, qword [Test2@@VAR + 72]
sub	rsp, 32
push	rax
xor	ecx, ecx
pop	rax
call	rax
add	rsp, 32
jmp	Test2@WndProc@60
Test2@WndProc@48:
mov	rax, qword [Test2@@VAR + 56]
sub	rsp, 32
push	rax
mov	rcx, qword [rbp + 16]
mov	rdx, qword [rbp + 24]
mov	r8, qword [rbp + 32]
mov	r9, qword [rbp + 40]
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -8], rcx
Test2@WndProc@60:
mov	rax, qword [rbp + -8]
leave
ret

Test2@Main:
push	rbp
mov	rbp, rsp
sub	rsp, 160
mov	rax, qword [Test2@@VAR + 0]
sub	rsp, 32
push	rax
xor	ecx, ecx
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -8], rcx
sub	rsp, 32
lea	rcx, [rbp + -112]
mov	edx, 72
call	Test2@ZeroClearRecord
add	rsp, 32
lea	rcx, [Test2@@STRING + 348]
mov	qword [rbp + -16], rcx
lea	rcx, qword [Test2@WndProc]
mov	qword [rbp + -104], rcx
mov	rcx, qword [rbp + -8]
mov	qword [rbp + -88], rcx
mov	rcx, qword [rbp + -16]
mov	qword [rbp + -48], rcx
mov	qword [rbp + -64], 1
mov	rax, qword [Test2@@VAR + 8]
sub	rsp, 32
push	rax
lea	rcx, [rbp + -112]
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -24], rcx
mov	rcx, qword [rbp + -24]
and	rcx, 65535
mov	qword [rbp + -24], rcx
mov	rcx, qword [rbp + -24]
cmp	rcx, 0
je	Test2@Main@91
mov	rax, qword [Test2@@VAR + 16]
sub	rsp, 96
push	rax
xor	ecx, ecx
mov	rdx, qword [rbp + -16]
lea	r8, [Test2@@STRING + 364]
xor	r9d, r9d
xor	r10d, r10d
mov	qword [rsp + 40], r10
xor	r10d, r10d
mov	qword [rsp + 48], r10
mov	r10d, 640
mov	qword [rsp + 56], r10
mov	r10d, 480
mov	qword [rsp + 64], r10
xor	r10d, r10d
mov	qword [rsp + 72], r10
xor	r10d, r10d
mov	qword [rsp + 80], r10
mov	r10, qword [rbp + -8]
mov	qword [rsp + 88], r10
xor	r10d, r10d
mov	qword [rsp + 96], r10
pop	rax
call	rax
add	rsp, 96
mov	rcx, rax
mov	qword [rbp + -40], rcx
mov	rcx, qword [rbp + -40]
mov	qword [rbp + -24], rcx
Test2@Main@91:
mov	rcx, qword [rbp + -24]
cmp	rcx, 0
je	Test2@Main@144
mov	rax, qword [Test2@@VAR + 24]
sub	rsp, 32
push	rax
mov	rcx, qword [rbp + -40]
mov	edx, 1
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -24], rcx
Test2@Main@104:
mov	rax, qword [Test2@@VAR + 32]
sub	rsp, 32
push	rax
lea	rcx, [rbp + -160]
xor	edx, edx
xor	r8d, r8d
xor	r9d, r9d
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -24], rcx
mov	rcx, qword [rbp + -24]
cmp	rcx, 0
jle	Test2@Main@138
mov	rax, qword [Test2@@VAR + 40]
sub	rsp, 32
push	rax
lea	rcx, [rbp + -160]
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -32], rcx
mov	rax, qword [Test2@@VAR + 48]
sub	rsp, 32
push	rax
lea	rcx, [rbp + -160]
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -32], rcx
jmp	Test2@Main@141
Test2@Main@138:
mov	rcx, qword [rbp + -24]
cmp	rcx, 0
jge	Test2@Main@141
Test2@Main@141:
mov	rcx, qword [rbp + -24]
cmp	rcx, 0
jg	Test2@Main@104
Test2@Main@144:
leave
ret

Test2@@INIT:
push	rbp
mov	rbp, rsp
sub	rsp, 32
call	Test2@InitLibrary
add	rsp, 32
sub	rsp, 32
call	Test2@Main
add	rsp, 32
sub	rsp, 32
mov	ecx, 0
call	[@ExitProcess]

INVALID_ARRAY_INDEX_TRAP:
and	rsp, -16
sub	rsp, 32
mov	ecx, -1
call	[@ExitProcess]
INTEGER_OVERFLOW_TRAP:
and	rsp, -16
sub	rsp, 32
mov	ecx, -2
call	[@ExitProcess]
INVALID_DIVISOR_TRAP:
and	rsp, -16
sub	rsp, 32
mov	ecx, -3
call	[@ExitProcess]
TYPE_CHECK_TRAP:
and	rsp, -16
sub	rsp, 32
mov	ecx, -4
call	[@ExitProcess]

section '.data' data readable writable
Test2@@TYPEDESC:
	dq 8,0,0,0,0,0,0,0,0,-1
	dq 48,0,0,0,0,0,0,0,0,-1
	dq 72,0,0,0,0,0,0,0,0,-1
Test2@@STRING dw 107,101,114,110,101,108,51,50,46,100,108,108,0,117,115,101,114,51,50,46,100,108,108,0,71,101,116,77,111,100,117,108,101,72,97,110,100,108,101,87,0,82,101,103,105,115,116,101,114,67,108,97,115,115,87,0,67,114,101,97,116,101,87,105,110,100,111,119,69,120,87,0,83,104,111,119,87,105,110,100,111,119,0,71,101,116,77,101,115,115,97,103,101,87,0,84,114,97,110,115,108,97,116,101,77,101,115,115,97,103,101,0,68,105,115,112,97,116,99,104,77,101,115,115,97,103,101,87,0,68,101,102,87,105,110,100,111,119,80,114,111,99,87,0,68,101,115,116,114,111,121,87,105,110,100,111,119,0,80,111,115,116,81,117,105,116,77,101,115,115,97,103,101,0,77,121,67,108,97,115,115,0,77,121,87,105,110,100,111,119,0
Test2@@VAR db 80 dup ?

section '.idata' import data readable writeable
	dd 0,0,0,RVA @kernel32_name,RVA @kernel32_table
	dd 0,0,0,RVA @rtl_name,RVA @rtl_table
	dd 0,0,0,0,0
	
	@kernel32_table:
	@ExitProcess dq RVA @nameofExitProcess
	@LoadLibrary dq RVA @nameofLoadLibrary
	@GetProcAddress dq RVA @nameofGetProcAddress
	dq 0
	
	@rtl_table:
	@NEW dq RVA @nameofNEW
	;@RegisterModule dq RVA @nameofRegisterModule
	@DISPOSE dq RVA @nameofDISPOSE
	dq 0
	
	@kernel32_name db 'KERNEL32.DLL',0
	@rtl_name db 'RTL.DLL',0
	
	@nameofExitProcess dw 0
	db 'ExitProcess',0
	@nameofLoadLibrary dw 0
	db 'LoadLibraryW',0
	@nameofGetProcAddress dw 0
	db 'GetProcAddress',0
	@nameofNEW dw 0
	db 'New',0
	@nameofRegisterModule dw 0
	db 'RegisterModule',0
	@nameofDISPOSE dw 0
	db 'Dispose',0