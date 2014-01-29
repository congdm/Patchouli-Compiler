format PE64
entry Test3@@INIT

section '.text' code readable executable

Test3@New:
push	rbp
mov	rbp, rsp
lea	rcx, qword [Test3@@VAR + 0]
lea	rdx, qword [Test3@@TYPEDESC + 0]
call	[@NEW]
mov	rax, qword [Test3@@VAR + 0]
leave
ret

Test3@@INIT:
push	rbp
mov	rbp, rsp
lea	rax, qword [Test3@@TYPEDESC + 0]
mov	qword [Test3@@TYPEDESC + 8], rax
sub	rsp, 32
call	Test3@New
add	rsp, 32
mov	r10, rax
mov	qword [Test3@@VAR + 0], r10
sub	rsp, 32
call	Test3@New
add	rsp, 32
mov	r10, rax
mov	qword [Test3@@VAR + 0], r10
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
Test3@@TYPEDESC dq 16,0,0,0,0,0,0,0,0,8,-1
Test3@@VAR db 8 dup ?

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