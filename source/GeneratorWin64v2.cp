MODULE GeneratorWin64v2;

IMPORT
	CPmain, SYSTEM, Sys;

CONST
	MIN_INT8 = -128;
	MAX_INT8 = 127;

	if_skipped = 0; if_REX = 1; if_16bit_prefix = 2; if_disp = 3;
	if_32bit_disp = 4;

	W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1; (* REX prefix *)
	d_bit = 2; w_bit = 1;

	reg_A = 0; reg_C = 1; reg_D = 2; reg_B = 3;
	reg_SP = 4; reg_BP = 5; reg_SI = 6; reg_DI = 7;
	reg_R8 = 8; reg_R9 = 9; reg_R10 = 10; reg_R11 = 11;
	reg_R12 = 12; reg_R13 = 13; reg_R14 = 14; reg_R15 = 15;
	
	MOVr = 88H;

TYPE
	InstructionInfo = RECORD
		flag : SET;
		ip : INTEGER;
		size : UBYTE
	END;

	Instruction = ARRAY 15 OF UBYTE;
	
VAR
	code : ARRAY 65536 OF Instruction;
	codeinfo : ARRAY 65536 OF InstructionInfo;
	
	pc : INTEGER;
	
PROCEDURE EmitRR* (op, reg, rm, rsize : UBYTE);
	VAR
		temp : INTEGER;
		i : UBYTE;
		iflag : SET;
BEGIN
	iflag := {}; i := 0;
	
	IF (rsize = 8) OR ({reg, rm} * {reg_R8 .. reg_R15} # {})
	OR (rsize = 1) & ({reg, rm} * {reg_SP..reg_DI} # {}) THEN
		code [pc, i] := 40H; (* REX prefix *)
		IF reg >= reg_R8 THEN INC (code [pc, 0], R_bit)
		END;
		IF rm >= reg_R8 THEN INC (code [pc, 0], B_bit)
		END;
		INCL (iflag, if_REX); INC (i)
	END;

	IF rsize = 2 THEN
		INC (op, w_bit); code [pc, i] := 66H;
		INCL (iflag, if_16bit_prefix); INC (i)
	ELSIF rsize = 4 THEN INC (op, w_bit)
	ELSIF rsize = 8 THEN INC (op, w_bit); INC (code [pc, 0], W_bit)
	END;
	
	INC (op, d_bit); code [pc, i] := op; INC (i);
	
	temp := 0C0H + ASH(reg MOD 8, 3) + rm MOD 8;	(* ModR/M byte *)
	SYSTEM.GET (SYSTEM.ADR (temp), code [pc, i]);
	INC (i);
	
	codeinfo [pc].size := i; codeinfo [pc].flag := iflag; INC (pc)
END EmitRR;

PROCEDURE EmitRMs* (d, op, reg, rsize, bas, idx, scl : UBYTE; disp : INTEGER);
	VAR
		temp : INTEGER;
		i : UBYTE;
		iflag : SET;
BEGIN
	iflag := {}; i := 0;
		
	IF (rsize = 8) OR ({reg, bas, idx} * {reg_R8 .. reg_R15} # {})
	OR (rsize = 1) & (reg IN {reg_SP..reg_DI}) THEN
		code [pc, i] := 40H; (* REX prefix *)
		IF reg >= reg_R8 THEN INC (code [pc, 0], R_bit)
		END;
		IF bas >= reg_R8 THEN INC (code [pc, 0], B_bit)
		END;
		IF idx >= reg_R8 THEN INC (code [pc, 0], X_bit)
		END;
		INCL (iflag, if_REX); INC (i)
	END;
	
	IF rsize = 2 THEN
		INC (op, w_bit); code [pc, i] := 66H;
		INCL (iflag, if_16bit_prefix); INC (i)
	ELSIF rsize = 4 THEN INC (op, w_bit)
	ELSIF rsize = 8 THEN INC (op, w_bit); INC (code [pc, 0], W_bit)
	END;
	
	INC (op, d * d_bit); code [pc, i] := op; INC (i);
	
	temp := ASH(reg MOD 8, 3) + 4;	(* ModR/M byte *)
	SYSTEM.GET (SYSTEM.ADR (temp), code [pc, i]);
	IF disp = 0 THEN
		IF bas IN {reg_BP, reg_R13} THEN
			INCL (iflag, if_disp);
			INC (code [pc, i], 40H)	(* mod = 01b *)
		END
	ELSIF (disp >= MIN_INT8) & (disp <= MAX_INT8) THEN
		INCL (iflag, if_disp);
		INC (code [pc, i], 40H) (* mod = 01b *)
	ELSE
		iflag := iflag + {if_disp, if_32bit_disp};
		INC (code [pc, i], 80H) (* mod = 10b *)
	END;
	INC (i);
	
	temp := ASH(scl, 6) + ASH(idx MOD 8, 3) + bas MOD 8;	(* SIB byte *)
	SYSTEM.GET (SYSTEM.ADR (temp), code [pc, i]);
	
	IF if_disp IN iflag THEN
		IF if_32bit_disp IN iflag THEN
			SYSTEM.PUT (SYSTEM.ADR (code [pc, i]), disp);
			INC (i, 4)
		ELSE
			SYSTEM.GET (SYSTEM.ADR (disp), code [pc, i]);
			INC (i)
		END
	END;
	
	codeinfo [pc].size := i; codeinfo [pc].flag := iflag; INC (pc)
END EmitRMs;

PROCEDURE EmitRM* (d, op, reg, rm, rsize : UBYTE; disp : INTEGER);
	VAR
		temp : INTEGER;
		i : UBYTE;
		iflag : SET;
BEGIN
	IF rm IN {reg_R12, reg_SP} THEN
		EmitRMs (d, op, reg, rsize, rm, reg_SP, 0, disp)
	ELSE
		iflag := {}; i := 0;
		
		IF (rsize = 8) OR ({reg, rm} * {reg_R8 .. reg_R15} # {})
		OR (rsize = 1) & (reg IN {reg_SP..reg_DI}) THEN
			code [pc, i] := 40H; (* REX prefix *)
			IF reg >= reg_R8 THEN INC (code [pc, 0], R_bit)
			END;
			IF rm >= reg_R8 THEN INC (code [pc, 0], B_bit)
			END;
			INCL (iflag, if_REX); INC (i)
		END;
		
		IF rsize = 2 THEN
			INC (op, w_bit); code [pc, i] := 66H;
			INCL (iflag, if_16bit_prefix); INC (i)
		ELSIF rsize = 4 THEN INC (op, w_bit)
		ELSIF rsize = 8 THEN INC (op, w_bit); INC (code [pc, 0], W_bit)
		END;
		
		INC (op, d * d_bit); code [pc, i] := op; INC (i);
		
		temp := ASH(reg MOD 8, 3) + rm MOD 8;	(* ModR/M byte *)
		SYSTEM.GET (SYSTEM.ADR (temp), code [pc, i]);
		IF disp = 0 THEN
			IF rm IN {reg_BP, reg_R13} THEN
				INCL (iflag, if_disp);
				INC (code [pc, i], 40H)	(* mod = 01b *)
			END
		ELSIF (disp >= MIN_INT8) & (disp <= MAX_INT8) THEN
			INCL (iflag, if_disp);
			INC (code [pc, i], 40H) (* mod = 01b *)
		ELSE
			iflag := iflag + {if_disp, if_32bit_disp};
			INC (code [pc, i], 80H) (* mod = 01b *)
		END;
		INC (i);
		
		IF if_disp IN iflag THEN
			IF if_32bit_disp IN iflag THEN
				SYSTEM.PUT (SYSTEM.ADR (code [pc, i]), disp);
				INC (i, 4)
			ELSE
				SYSTEM.GET (SYSTEM.ADR (disp), code [pc, i]);
				INC (i)
			END
		END;
		
		codeinfo [pc].size := i; codeinfo [pc].flag := iflag; INC (pc)
	END
END EmitRM;

PROCEDURE Write_to_file*;
	VAR
		file : Sys.FileHandle;
		i, j : INTEGER;
BEGIN
	Sys.Rewrite (file, 'output.bin');
	FOR i := 0 TO pc - 1 DO
		FOR j := 0 TO codeinfo [i].size - 1 DO
			Sys.Write_byte (file, code [i, j])
		END
	END;
	Sys.Close (file);
END Write_to_file;

BEGIN
	pc := 0;
	EmitRR (MOVr, reg_A, reg_R8, 8);
	EmitRR (MOVr, reg_A, reg_R8, 4);
	EmitRR (MOVr, reg_A, reg_R8, 2);
	EmitRR (MOVr, reg_A, reg_R8, 1);
	Write_to_file
END GeneratorWin64v2.