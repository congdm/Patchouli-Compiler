format PE64
entry Test@@INIT

section '.text' code readable executable

Test@MakeAnsiStr:
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
jle	Test@MakeAnsiStr@29
mov	qword [rbp + -16], 256
Test@MakeAnsiStr@29:
mov	qword [rbp + -8], 0
Test@MakeAnsiStr@30:
mov	rcx, qword [rbp + -8]
cmp	rcx, qword [rbp + -16]
jge	Test@MakeAnsiStr@50
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
jmp	Test@MakeAnsiStr@30
Test@MakeAnsiStr@50:
mov	rcx, qword [rbp + -16]
sub	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	rdx, [rbp + 16]
cmp	rcx, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, [rdx + rcx * 1 + 0]
movzx	ecx, byte [rcx + 0]
cmp	rcx, 0
je	Test@MakeAnsiStr@68
mov	rcx, qword [rbp + -16]
sub	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	rdx, [rbp + 16]
cmp	rcx, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, [rdx + rcx * 1 + 0]
mov	byte [rcx + 0], 0
Test@MakeAnsiStr@68:
leave
ret

Test@NullStringLen:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	qword [rbp + -8], 0
mov	byte [rbp + -9], 1
Test@NullStringLen@25:
mov	rcx, qword [rbp + -8]
cmp	rcx, qword [rbp + 24]
jge	Test@NullStringLen@45
cmp	byte [rbp + -9], 0
je	Test@NullStringLen@45
mov	rcx, qword [rbp + -8]
mov	rdx, [rbp + 16]
cmp	rcx, qword [rbp + 24]
jae	INVALID_ARRAY_INDEX_TRAP
lea	rcx, qword [rdx + rcx * 1 + 0]
movzx	ecx, byte [rcx + 0]
cmp	rcx, 0
je	Test@NullStringLen@43
mov	rcx, qword [rbp + -8]
add	rcx, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], rcx
jmp	Test@NullStringLen@44
Test@NullStringLen@43:
mov	byte [rbp + -9], 0
Test@NullStringLen@44:
jmp	Test@NullStringLen@25
Test@NullStringLen@45:
mov	rax, qword [rbp + -8]
leave
ret

Test@InitLibrary:
push	rbp
mov	rbp, rsp
sub	rsp, 272
sub	rsp, 32
lea	rcx, [Test@@STRING + 0]
call	[@LoadLibrary]
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -264], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 26]
mov	r8d, 13
call	Test@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -264]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test@@VAR + 0], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 52]
mov	r8d, 13
call	Test@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -264]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test@@VAR + 8], rcx
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 78]
mov	r8d, 10
call	Test@MakeAnsiStr
add	rsp, 32
mov	rcx, qword [rbp + -264]
lea	rdx, [rbp + -256]
sub	rsp, 32
call	[@GetProcAddress]
add	rsp, 32
mov	rcx, rax
mov	qword [Test@@VAR + 16], rcx
leave
ret

Test@InitConsole:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	rax, qword [Test@@VAR + 0]
sub	rsp, 32
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [rbp + -8], rcx
mov	rax, qword [Test@@VAR + 8]
sub	rsp, 32
push	rax
mov	rcx, -11
pop	rax
call	rax
add	rsp, 32
mov	rcx, rax
mov	qword [Test@@VAR + 24], rcx
leave
ret

Test@WriteAnsiString:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	rax, qword [Test@@VAR + 16]
sub	rsp, 48
push	rax
mov	rcx, qword [Test@@VAR + 24]
mov	rdx, [rbp + 16]
push	rcx
push	rdx
sub	rsp, 40
mov	rcx, [rbp + 16]
mov	rdx, qword [rbp + 24]
call	Test@NullStringLen
add	rsp, 40
pop	rdx
pop	rcx
mov	r8, rax
lea	r9, qword [rbp + -16]
xor	r10d, r10d
mov	qword [rsp + 40], r10
pop	rax
call	rax
add	rsp, 48
mov	rcx, rax
mov	qword [rbp + -8], rcx
leave
ret

Test@Main:
push	rbp
mov	rbp, rsp
sub	rsp, 256
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 98]
mov	r8d, 14
call	Test@MakeAnsiStr
add	rsp, 32
sub	rsp, 32
lea	rcx, [rbp + -256]
mov	edx, 256
call	Test@WriteAnsiString
add	rsp, 32
leave
ret

Test@@INIT:
push	rbp
mov	rbp, rsp
sub	rsp, 32
call	Test@InitLibrary
add	rsp, 32
sub	rsp, 32
call	Test@InitConsole
add	rsp, 32
sub	rsp, 32
call	Test@Main
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
Test@@STRING dw 107,101,114,110,101,108,51,50,46,100,108,108,0,65,108,108,111,99,67,111,110,115,111,108,101,0,71,101,116,83,116,100,72,97,110,100,108,101,0,87,114,105,116,101,70,105,108,101,0,72,101,108,108,111,44,32,119,111,114,108,100,33,0
Test@@VAR db 32 dup ?

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