MODULE Generator3;

IMPORT
	SYSTEM, Sys, Base, Scanner;

CONST
	MIN_INT8 = -128;
	MAX_INT8 = 127;
	MAX_UINT32 = 4294967295;
	MAX_INT32 = 2147483647;
	MIN_INT32 = -2147483648;
	MAX_INT64 = 9223372036854775807;
	MIN_INT64 = (-MAX_INT64) - 1;
	
	max_staticsize = 512 * 1024;
	tempOutputName = 'output.temp_';
	tempFixupName = 'fixup.temp_';
	tempSymName = 'sym.temp_';
	
	overflow_trap = 0;
	array_trap = 1;
	type_trap = 2;
	
	mode_reg = Base.mode_reg;
	mode_imm = Base.class_const;
	mode_regI = Base.mode_regI;
	mode_cond = Base.mode_cond;
	mode_mem = {Base.class_var, Base.class_ref, mode_regI};
	
	w_bit = 1; s_bit = 2; w_bit_1byte = 8;

	reg_A = 0; reg_C = 1; reg_D = 2; reg_B = 3;
	reg_SP = 4; reg_BP = 5; reg_SI = 6; reg_DI = 7;
	reg_R8 = 8; reg_R9 = 9; reg_R10 = 10; reg_R11 = 11;
	reg_R12 = 12; reg_R13 = 13; reg_R14 = 14; reg_R15 = 15;
	
	ccO = 0; ccNO = 1; ccB = 2; ccAE = 3; ccZ = 4; ccNZ = 5; ccBE = 6; ccA = 7;
	ccS = 8; ccNS = 9; ccP = 10; ccNP = 11; ccL = 12; ccGE = 13; ccLE = 14;
	ccG = 15; ccAlways = 16; ccNever = 17;
	
	MOVr = 88H; XCHGr = 86H; LEAr = 8DH; MOVZXr = 0B60FH;
	ADDr = 00H; SUBr = 28H; IMULr = 0AF0FH; IDIVr = 7F7H; NEGr = 3F6H;
	SHLcl = 4D2H; SHRcl = 5D2H; SARcl = 7D2H; RORcl = 1D0H;
	ANDr = 20H; ORr = 08H; XORr = 30H; NOTr = 2F6H;
	TESTr = 84H; CMPr = 38H;
	BTr = 0A30FH; BTSr = 0AB0FH;
	
	ADDi = 80H; SUBi = 580H; IMULi = 69H; NEGi = NEGr;
	SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H; RORi = 1C0H;
	ANDi = 480H; ORi = 180H; XORi = 680H;
	TESTi = 0F6H; CMPi = 780H;
	BTi = 4BA0FH; BTSi = 5BA0FH;
	
	MOVSrep = 0A4H;
	
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H;
	
TYPE
	InstructionInfo = RECORD
		relfixup, absfixup : BOOLEAN;
		size : UBYTE;
		ip : INTEGER
	END;
	
	RMOperand = RECORD
		mod, rm, bas, idx, scl : UBYTE;
		disp : INTEGER
	END;

	RegStack = RECORD
		ord : ARRAY 16 OF UBYTE;
		i, n : UBYTE;
	END;
	
	ProcInfo* = RECORD
		oldi : UBYTE; oldrs : INTEGER; oldcurRegs : SET;
		parblksize*, memstack, paradr : INTEGER;
		rtype* : Base.Type;
	END;
	
VAR
	code : ARRAY 100000H OF UBYTE;
	ip, pc : INTEGER;
	
	(* Global variables for Emit procedures *)
	Emit : RECORD
		i : INTEGER
	END;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)
	
PROCEDURE Emit_REX_prefix (reg, rsize : UBYTE; mem : RMOperand);
	CONST W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1;
	VAR rex : UBYTE;
BEGIN
	rex := 40H;
	IF rsize = 8 THEN rex := rex + W_bit END;
	IF reg >= reg_R8 THEN rex := rex + R_bit END;
	IF (mem.rm >= reg_R8) OR (mem.mod # 3) & (mem.rm = reg_SP)
		& (mem.bas >= reg_R8)
	THEN rex := rex + B_bit
	END;
	IF (mem.mod # 3) & (mem.rm = reg_SP) & (mem.idx >= reg_R8) THEN
		rex := rex + X_bit
	END;
	IF (rex # 40H) OR (rsize = 1) & ((reg IN {reg_SP .. reg_DI})
		OR (mem.mod = 3) & (mem.rm IN {reg_SP .. reg_DI}))
	THEN code [Emit.i] := rex; INC (Emit.i)
	END
END Emit_REX_prefix;

PROCEDURE Emit_16bit_prefix (rsize : UBYTE);
BEGIN
	IF rsize = 2 THEN code [Emit.i] := 66H; INC (Emit.i) END
END Emit_16bit_prefix;

PROCEDURE Emit_opcodeR (op : INTEGER; rsize : UBYTE);
BEGIN
	IF op MOD 256 = 0FH THEN
		code [Emit.i] := 0FH; op := op DIV 256; INC (Emit.i)
	END;
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
	code [Emit.i] := USHORT (op); INC (Emit.i)
END Emit_opcodeR;

PROCEDURE Emit_ModRM (reg : UBYTE; mem : RMOperand);
BEGIN
	code [Emit.i] := USHORT (mem.mod * 16 + reg * 8 + mem.rm); INC (Emit.i)
	IF mod # 3 THEN
		IF mem.rm = reg_SP THEN
			code [Emit.i] := USHORT (mem.scl * 16 + mem.idx * 8 + mem.bas);
			INC (Emit.i)
		END;
		IF (mem.mod = 0) & (mem.rm = reg_BP) OR (mem.mod = 2) THEN
			SYSTEM.PUT (SYSTEM.ADR (code [Emit.i]), mem.disp);
			Emit.i := Emit.i + 4
		ELSIF mem.mod = 1 THEN
			code [Emit.i] := USHORT (mem.disp); INC (Emit.i)
		END
	END
END Emit_ModRM;
	
PROCEDURE EmitRegRm (op : INTEGER; reg, rsize : UBYTE; mem : RMOperand);
BEGIN
	IF op # MOVZX THEN Emit_16bit_prefix (rsize) END;
	Emit_REX_prefix (reg, rsize, mem);
	Emit_opcodeR (op, rsize); Emit_ModRM (reg, mem)
END EmitRegRm;

END Generator3.