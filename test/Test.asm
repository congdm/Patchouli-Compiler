format PE64
entry Test@@INIT

section '.text' code readable executable

Test@MakeAnsiStr:
push	rbp
mov	rbp, rsp
sub	rsp, 24
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
mov	[rbp + 32], r8
push	r12
mov	r10, qword [rbp + 32]
mov	qword [rbp + -16], r10
mov	r10, qword [rbp + -16]
cmp	r10, 256
jle	Test@MakeAnsiStr@19
mov	qword [rbp + -16], 256
Test@MakeAnsiStr@19:
mov	qword [rbp + -8], 0
Test@MakeAnsiStr@20:
mov	r10, qword [rbp + -8]
cmp	r10, qword [rbp + -16]
jge	Test@MakeAnsiStr@40
mov	r10, [rbp + 16]
mov	r11, qword [rbp + -8]
cmp	r11, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, [r10 + r11 * 1 + 0]
mov	r11, qword [rbp + -8]
mov	r12, [rbp + 24]
cmp	r11, qword [rbp + 32]
jae	INVALID_ARRAY_INDEX_TRAP
lea	r11, [r12 + r11 * 2 + 0]
movzx	r11, word [r11 + 0]
mov	byte [r10 + 0], r11l
mov	r10, qword [rbp + -8]
add	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], r10
jmp	Test@MakeAnsiStr@20
Test@MakeAnsiStr@40:
mov	r10, qword [rbp + -16]
sub	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	r11, [rbp + 16]
cmp	r10, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, [r11 + r10 * 1 + 0]
movzx	r10, byte [r10 + 0]
cmp	r10, 0
je	Test@MakeAnsiStr@58
mov	r10, qword [rbp + -16]
sub	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	r11, [rbp + 16]
cmp	r10, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, [r11 + r10 * 1 + 0]
mov	byte [r10 + 0], 0
Test@MakeAnsiStr@58:
pop	r12
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
Test@NullStringLen@15:
mov	r10, qword [rbp + -8]
cmp	r10, qword [rbp + 24]
jge	Test@NullStringLen@35
cmp	byte [rbp + -9], 0
je	Test@NullStringLen@35
mov	r10, qword [rbp + -8]
mov	r11, [rbp + 16]
cmp	r10, qword [rbp + 24]
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, qword [r11 + r10 * 1 + 0]
movzx	r10, byte [r10 + 0]
cmp	r10, 0
je	Test@NullStringLen@33
mov	r10, qword [rbp + -8]
add	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], r10
jmp	Test@NullStringLen@34
Test@NullStringLen@33:
mov	byte [rbp + -9], 0
Test@NullStringLen@34:
jmp	Test@NullStringLen@15
Test@NullStringLen@35:
mov	rax, qword [rbp + -8]
leave
ret

Test@InitLibrary:
push	rbp
mov	rbp, rsp
sub	rsp, 272
sub	rsp, 32
lea	r10, [Test@@STRING + 0]
mov	rcx, r10
call	[@LoadLibrary]
add	rsp, 32
mov	qword [rbp + -264], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 26]
mov	r8, 13
call	Test@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -264]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test@@VAR + 0], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 52]
mov	r8, 13
call	Test@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -264]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test@@VAR + 8], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 78]
mov	r8, 10
call	Test@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -264]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test@@VAR + 16], rax
leave
ret

Test@InitConsole:
push	rbp
mov	rbp, rsp
sub	rsp, 16
sub	rsp, 32
call	qword [Test@@VAR + 0]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -8], r10
sub	rsp, 32
mov	rcx, -11
call	qword [Test@@VAR + 8]
add	rsp, 32
mov	r10, rax
mov	qword [Test@@VAR + 24], r10
leave
ret

Test@WriteAnsiString:
push	rbp
mov	rbp, rsp
sub	rsp, 16
mov	[rbp + 16], rcx
mov	[rbp + 24], rdx
sub	rsp, 48
mov	rcx, qword [Test@@VAR + 24]
mov	r10, [rbp + 16]
mov	rdx, r10
push	rcx
push	rdx
sub	rsp, 32
mov	rcx, qword [rbp + 16]
mov	rdx, qword [rbp + 24]
call	Test@NullStringLen
add	rsp, 32
pop	rdx
pop	rcx
mov	r10, rax
mov	r8, r10
lea	r10, qword [rbp + -16]
mov	r9, r10
xor	r10d, r10d
mov	[rsp + 32], r10
call	qword [Test@@VAR + 16]
add	rsp, 48
mov	r10, rax
mov	qword [rbp + -8], r10
leave
ret

Test@Main:
push	rbp
mov	rbp, rsp
sub	rsp, 256
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test@@STRING + 98]
mov	r8, 14
call	Test@MakeAnsiStr
add	rsp, 32
sub	rsp, 32
lea	rcx, [rbp + -256]
mov	rdx, 256
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