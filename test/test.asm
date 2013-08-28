PROC_1:
 PUSH rbp
 MOV rbp, rsp
 LEAVE
 RET 8
PROC_6:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 8
 MOV rcx, qword [rbx + 160]
 SUB rcx, 1
 MOV qword [rbp + -8], rcx
WHILE_13:
 CMP qword [rbp + -8], 0
 JL WHILE_13_END
 MOV rcx, qword [rbp + -8]
 IMUL rcx, rcx, 8
 ADD rcx, rbx
 PUSH qword [rcx + 0]
 CALL PROC_1
 MOV rcx, qword [rbp + -8]
 SUB rcx, 1
 MOV qword [rbp + -8], rcx
 JMP WHILE_13
WHILE_13_END:
 LEAVE
 RET 0
PROC_28:
 PUSH rbp
 MOV rbp, rsp
 SUB rsp, 16
 MOV qword [rbx + 160], 0
 CMP qword [rbp + 16], 0
 JLE IF_33_END
 MOV qword [rbp + -8], 0
WHILE_36:
 CMP qword [rbp + -8], 20
 JGE WHILE_36_END
 MOV rcx, qword [rbp + -8]
 IMUL rcx, rcx, 8
 ADD rcx, rbx
 MOV qword [rcx + 0], 0
 MOV rcx, 1
 ADD rcx, qword [rbp + -8]
 MOV qword [rbp + -8], rcx
 JMP WHILE_36
WHILE_36_END:
REPEAT_48:
 MOV rcx, 10
 MOV rax, qword [rbp + 16]
 CQO
 IDIV rcx
 MOV rcx, rdx
 MOV qword [rbp + -16], rcx
 MOV rcx, 10
 MOV rax, qword [rbp + 16]
 CQO
 IDIV rcx
 MOV rcx, rax
 MOV qword [rbp + 16], rcx
 MOV rcx, qword [rbx + 160]
 IMUL rcx, rcx, 8
 ADD rcx, rbx
 MOV rsi, 48
 ADD rsi, qword [rbp + -16]
 MOV qword [rcx + 0], rsi
 MOV rcx, 1
 ADD rcx, qword [rbx + 160]
 MOV qword [rbx + 160], rcx
 CMP qword [rbp + 16], 0
 JNE REPEAT_48
IF_33_END:
 LEAVE
 RET 8
PROC_75:
 PUSH rbp
 MOV rbp, rsp
 MOV rcx, qword [rbp + 16]
 MOV rsi, qword [rbp + 24]
 ADD rcx, qword [rsi + 0]
 MOV rsi, qword [rbp + 24]
 MOV qword [rsi + 0], rcx
 LEAVE
 RET 16
MAIN:
 PUSH 45
 CALL PROC_28
 CALL PROC_6
 RET
