format PE64 DLL GUI
entry DLL_Entry

section '.text' code readable executable

DLL_Entry:
	push rbp
	mov rbp, rsp
	sub rsp, 32

	cmp dl, 1
	je .Initialize
	cmp dl, 0
	je .Finalize
	jmp .Return

	.Initialize:
	mov [Global.DLL_Handle], rcx
	call [GetProcessHeap]
	mov [Global.Heap_handle], rax

	jmp .Return

	.Finalize:

	.Return:
	mov eax, 1
	leave
	ret

New:
	push rbp
	mov rbp, rsp
	sub rsp, 16 + 32
	
	mov [rbp - 8], rcx
	mov [rbp - 16], rdx
	mov r10, rdx

	mov rcx, [Global.Heap_handle]
	mov rdx, 8 ; HEAP_ZERO_MEMORY
	mov r8, [r10]
	add r8, 15
	and r8, -16
	add r8, 16
	call [HeapAlloc]
	
	mov rdx, [rbp - 16]
	mov rcx, [rbp - 8]
	mov [rax + 8], rdx 
	add rax, 16
	mov [rcx], rax

	.Return:
	leave
	ret

Out_of_memory:
	sub rsp, 32
	and rsp, -16
	mov ecx, 1
	call [ExitProcess]

RegisterModule:
	ret
	
Dispose:
	push rbp
	mov rbp, rsp
	sub rsp, 32 + 16
	
	mov r8, [rcx]
	mov qword [rcx], 0
	sub r8, 16
	mov rcx, [Global.Heap_handle]
	xor edx, edx
	call [HeapFree]
	
	.Return:
	leave
	ret

section '.data' data readable writeable

Global:
	.DLL_Handle dq 0
	.Heap_handle dq 0

section '.idata' import data readable writeable

  dd 0,0,0,RVA kernel_name,RVA kernel_table
  dd 0,0,0,0,0

  kernel_table:
    GetProcessHeap dq RVA _GetProcessHeap
    HeapAlloc dq RVA _HeapAlloc
    HeapFree dq RVA _HeapFree
    VirtualAlloc dq RVA _VirtualAlloc
    ExitProcess dq RVA _ExitProcess
    dq 0

  kernel_name db 'KERNEL32.DLL',0
  _GetProcessHeap dw 0
    db 'GetProcessHeap',0
  _HeapAlloc dw 0
    db 'HeapAlloc',0
  _HeapFree dw 0
    db 'HeapFree',0
  _VirtualAlloc dw 0
    db 'VirtualAlloc',0
  _ExitProcess dw 0
    db 'ExitProcess',0

section '.edata' export data readable
	dd 0
	dd %t
	dw 0
	dw 0
	dd RVA DLL_Name
	dd 1
	dd 2
	dd 2
	dd RVA _func_array
	dd RVA _name_array
	dd RVA _index_array

_func_array:
	dd RVA Dispose
	dd RVA New
	;dd RVA RegisterModule
_name_array:
	dd RVA Dispose_name
	dd RVA New_name
	;dd RVA RegisterModule_name
_index_array:
	dw 0, 1

DLL_Name db 'RTL.DLL',0
New_name db 'New',0
RegisterModule_name db 'RegisterModule',0
Dispose_name db 'Dispose',0

data fixups
end data