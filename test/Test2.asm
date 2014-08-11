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
mov	r10, qword [rbp + 32]
mov	qword [rbp + -16], r10
mov	r10, qword [rbp + -16]
cmp	r10, 256
jle	Test2@MakeAnsiStr@19
mov	qword [rbp + -16], 256
Test2@MakeAnsiStr@19:
mov	qword [rbp + -8], 0
Test2@MakeAnsiStr@20:
mov	r10, qword [rbp + -8]
cmp	r10, qword [rbp + -16]
jge	Test2@MakeAnsiStr@40
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
jmp	Test2@MakeAnsiStr@20
Test2@MakeAnsiStr@40:
mov	r10, qword [rbp + -16]
sub	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	r11, [rbp + 16]
cmp	r10, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, [r11 + r10 * 1 + 0]
movzx	r10, byte [r10 + 0]
cmp	r10, 0
je	Test2@MakeAnsiStr@58
mov	r10, qword [rbp + -16]
sub	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	r11, [rbp + 16]
cmp	r10, 256
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, [r11 + r10 * 1 + 0]
mov	byte [r10 + 0], 0
Test2@MakeAnsiStr@58:
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
Test2@NullStringLen@15:
mov	r10, qword [rbp + -8]
cmp	r10, qword [rbp + 24]
jge	Test2@NullStringLen@35
cmp	byte [rbp + -9], 0
je	Test2@NullStringLen@35
mov	r10, qword [rbp + -8]
mov	r11, [rbp + 16]
cmp	r10, qword [rbp + 24]
jae	INVALID_ARRAY_INDEX_TRAP
lea	r10, qword [r11 + r10 * 1 + 0]
movzx	r10, byte [r10 + 0]
cmp	r10, 0
je	Test2@NullStringLen@33
mov	r10, qword [rbp + -8]
add	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], r10
jmp	Test2@NullStringLen@34
Test2@NullStringLen@33:
mov	byte [rbp + -9], 0
Test2@NullStringLen@34:
jmp	Test2@NullStringLen@15
Test2@NullStringLen@35:
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
Test2@ZeroClearRecord@14:
mov	r10, qword [rbp + -8]
cmp	r10, qword [rbp + 24]
jge	Test2@ZeroClearRecord@26
mov	r10, qword [rbp + 16]
add	r10, qword [rbp + -8]
jo	INTEGER_OVERFLOW_TRAP
mov	byte [r10 + 0], 0
mov	r10, qword [rbp + -8]
add	r10, 1
jo	INTEGER_OVERFLOW_TRAP
mov	qword [rbp + -8], r10
jmp	Test2@ZeroClearRecord@14
Test2@ZeroClearRecord@26:
leave
ret

Test2@InitLibrary:
push	rbp
mov	rbp, rsp
sub	rsp, 272
sub	rsp, 32
lea	r10, [Test2@@STRING + 0]
mov	rcx, r10
call	[@LoadLibrary]
add	rsp, 32
mov	qword [rbp + -264], rax
sub	rsp, 32
lea	r10, [Test2@@STRING + 26]
mov	rcx, r10
call	[@LoadLibrary]
add	rsp, 32
mov	qword [rbp + -272], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 48]
mov	r8, 17
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -264]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 0], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 82]
mov	r8, 15
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 8], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 112]
mov	r8, 16
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 16], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 144]
mov	r8, 11
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 24], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 166]
mov	r8, 12
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 32], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 190]
mov	r8, 17
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 40], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 224]
mov	r8, 17
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 48], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 258]
mov	r8, 15
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 56], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 288]
mov	r8, 14
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 64], rax
sub	rsp, 32
lea	rcx, [rbp + -256]
lea	rdx, [Test2@@STRING + 316]
mov	r8, 16
call	Test2@MakeAnsiStr
add	rsp, 32
lea	r10, [rbp + -256]
sub	rsp, 32
mov	r11, qword [rbp + -272]
mov	rcx, r11
mov	rdx, r10
call	[@GetProcAddress]
add	rsp, 32
mov	qword [Test2@@VAR + 72], rax
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
mov	r10, qword [rbp + 24]
cmp	r10, 16
jne	Test2@WndProc@24
sub	rsp, 32
mov	rcx, qword [rbp + 16]
call	qword [Test2@@VAR + 64]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -16], r10
jmp	Test2@WndProc@41
Test2@WndProc@24:
mov	r10, qword [rbp + 24]
cmp	r10, 2
jne	Test2@WndProc@32
sub	rsp, 32
mov	rcx, 0
call	qword [Test2@@VAR + 72]
add	rsp, 32
jmp	Test2@WndProc@41
Test2@WndProc@32:
sub	rsp, 32
mov	rcx, qword [rbp + 16]
mov	rdx, qword [rbp + 24]
mov	r8, qword [rbp + 32]
mov	r9, qword [rbp + 40]
call	qword [Test2@@VAR + 56]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -8], r10
Test2@WndProc@41:
mov	rax, qword [rbp + -8]
leave
ret

Test2@Main:
push	rbp
mov	rbp, rsp
sub	rsp, 160
sub	rsp, 32
mov	rcx, 0
call	qword [Test2@@VAR + 0]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -8], r10
sub	rsp, 32
lea	r10, [rbp + -112]
mov	rcx, r10
mov	rdx, 72
call	Test2@ZeroClearRecord
add	rsp, 32
lea	r10, [Test2@@STRING + 348]
mov	qword [rbp + -16], r10
lea	r10, qword [Test2@WndProc]
mov	qword [rbp + -104], r10
mov	r10, qword [rbp + -8]
mov	qword [rbp + -88], r10
mov	r10, qword [rbp + -16]
mov	qword [rbp + -48], r10
mov	qword [rbp + -64], 1
sub	rsp, 32
lea	r10, [rbp + -112]
mov	rcx, r10
call	qword [Test2@@VAR + 8]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -24], r10
mov	r10, qword [rbp + -24]
and	r10, 65535
mov	qword [rbp + -24], r10
mov	r10, qword [rbp + -24]
cmp	r10, 0
je	Test2@Main@75
sub	rsp, 96
mov	rcx, 0
mov	rdx, qword [rbp + -16]
lea	r10, [Test2@@STRING + 364]
mov	r8, r10
mov	r9, 0
xor	r10d, r10d
mov	qword [rsp + 32], r10
xor	r10d, r10d
mov	qword [rsp + 40], r10
mov	r10, 640
mov	qword [rsp + 48], r10
mov	r10, 480
mov	qword [rsp + 56], r10
xor	r10d, r10d
mov	qword [rsp + 64], r10
xor	r10d, r10d
mov	qword [rsp + 72], r10
mov	r10, qword [rbp + -8]
mov	qword [rsp + 80], r10
xor	r10d, r10d
mov	qword [rsp + 88], r10
call	qword [Test2@@VAR + 16]
add	rsp, 96
mov	r10, rax
mov	qword [rbp + -40], r10
mov	r10, qword [rbp + -40]
mov	qword [rbp + -24], r10
Test2@Main@75:
mov	r10, qword [rbp + -24]
cmp	r10, 0
je	Test2@Main@119
sub	rsp, 32
mov	rcx, qword [rbp + -40]
mov	rdx, 1
call	qword [Test2@@VAR + 24]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -24], r10
Test2@Main@85:
sub	rsp, 32
lea	r10, [rbp + -160]
mov	rcx, r10
mov	rdx, 0
mov	r8, 0
mov	r9, 0
call	qword [Test2@@VAR + 32]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -24], r10
mov	r10, qword [rbp + -24]
cmp	r10, 0
jle	Test2@Main@113
sub	rsp, 32
lea	r10, [rbp + -160]
mov	rcx, r10
call	qword [Test2@@VAR + 40]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -32], r10
sub	rsp, 32
lea	r10, [rbp + -160]
mov	rcx, r10
call	qword [Test2@@VAR + 48]
add	rsp, 32
mov	r10, rax
mov	qword [rbp + -32], r10
jmp	Test2@Main@116
Test2@Main@113:
mov	r10, qword [rbp + -24]
cmp	r10, 0
jge	Test2@Main@116
Test2@Main@116:
mov	r10, qword [rbp + -24]
cmp	r10, 0
jg	Test2@Main@85
Test2@Main@119:
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
Test2@@TYPEDESC dq 8,0,0,0,0,0,0,0,0,-1,48,0,0,0,0,0,0,0,0,-1,72,0,0,0,0,0,0,0,0,-1
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