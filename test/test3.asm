MAIN:
 MOV rcx, qword [Global + 8]
 XOR r8d, r8d
 CMP rcx, 63
 JA END_SET_2
 BTS r8, ecx
END_SET_2:
 MOV r9, qword [Global + 16]
 CMP r9, 63
 JA END_SET_8
 BTS r8, r9d
END_SET_8:
 MOV qword [Global + 0], r8
 MOV r8, 24
 XOR r8, qword [Global + 0]
 MOV qword [Global + 0], r8
 MOVSX r8, byte [Global + 24]
 ADD r8, qword [Global + 8]
 JO INTEGER_OVERFLOW_TRAP
 MOV qword [Global + 8], r8
 MOVSX r8, byte [Global + 24]
 MOV r9, qword [Global + 0]
 CMP r8, 63
 JA IF_21_END
 BT r9, r8
 JNC IF_21_END
IF_21_END:
 RET

RANGE_CHECK_TRAP:

INTEGER_OVERFLOW_TRAP:

NOT_POSITIVE_DIVISOR_TRAP:
