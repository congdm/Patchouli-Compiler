MODULE GeneratorWin64v2;

IMPORT
	CPmain, SYSTEM, Sys, Base, Scanner;

CONST
	MIN_INT8 = -128;
	MAX_INT8 = 127;
	MAX_UINT32 = 4294967295;
	MAX_INT32 = 2147483647;
	MIN_INT32 = -2147483648;
	
	integer_check = 0;
	
	mode_reg = Base.mode_reg; mode_imm = Base.class_const;
	mode_mem = {Base.class_var, Base.class_ref, Base.mode_regI};
	
	if_skipped = 0; if_REX = 1; if_16bit_prefix = 2; if_disp = 3;
	if_32bit_disp = 4; if_global_var = 5; if_static_var = 6;

	W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1; (* REX prefix *)
	d_bit = 2; w_bit = 1; s_bit = 2; w_bit_1byte = 8;

	reg_A = 0; reg_C = 1; reg_D = 2; reg_B = 3;
	reg_SP = 4; reg_BP = 5; reg_SI = 6; reg_DI = 7;
	reg_R8 = 8; reg_R9 = 9; reg_R10 = 10; reg_R11 = 11;
	reg_R12 = 12; reg_R13 = 13; reg_R14 = 14; reg_R15 = 15;
	
	ccO = 0; ccNO = 1; ccB = 2; ccAE = 3; ccZ = 4; ccNZ = 5; ccBE = 6; ccA = 7;
	ccS = 8; ccNS = 9; ccP = 10; ccNP = 11; ccL = 12; ccGE = 13; ccLE = 14;
	ccG = 15; ccAlways = 16; ccNever = 17;
	
	MOVr = 88H; XCHGr = 86H;
	ADDr = 00H; SUBr = 28H; IMULr = 0AF0FH; IDIVr = 7F7H; NEGr = 3F6H;
	SHLcl = 4D2H; SHRcl = 5D2H; SARcl = 7D2H;
	ANDr = 20H; ORr = 08H; XORr = 30H; NOTr = 2F6H;
	TESTr = 84H; CMPr = 38H;
	BTr = 0A30FH; BTSr = 0AB0FH;
	
	MOVZX = 0B60FH;
	
	ADDi = 80H; SUBi = 580H; IMULi = 69H; NEGi = NEGr;
	SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H;
	ANDi = 480H; ORi = 180H; XORi = 680H;
	TESTi = 0F6H; CMPi = 780H;
	BTi = 4BA0FH; BTSi = 5BA0FH;
	
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H;

TYPE
	InstructionInfo = RECORD
		flag : SET;
		ip : INTEGER;
		size : UBYTE
	END;

	Instruction = ARRAY 15 OF UBYTE;
	
	RegStack = RECORD
		ord : ARRAY 16 OF UBYTE;
		i, n : UBYTE;
	END;
	
	Item* = RECORD
		r : UBYTE; readonly, varparam : BOOLEAN;
		mode, lev : INTEGER;
		type : Base.Type;
		b, c : INTEGER;
		a : LONGINT
	END;
	
VAR
	compiler_flag : SET;
	
	out : Sys.FileHandle;

	code : ARRAY 65536 OF Instruction;
	codeinfo : ARRAY 65536 OF InstructionInfo;
	pc, ip, varbase, staticbase, varsize* : INTEGER;
	
	(* Reg stack *)
	reg_stacks : ARRAY 1 OF RegStack;
	curRegs : SET;
	crs : INTEGER;
	
	(* Global variables for Emit procedures *)
	Emit : RECORD
		i : UBYTE;
		iflag : SET;
	END;
	
	ProcState : RECORD
		adr, prolog_size, locblksize : INTEGER;
		parlist : Base.Object;
		usedRegs : SET
	END;

PROCEDURE Emit_REX_prefix (rsize, reg, r2, bas, idx : UBYTE);
	VAR n : UBYTE;
BEGIN
	IF (rsize = 8) OR ({reg, r2, bas, idx} * {reg_R8 .. reg_R15} # {})
	OR (rsize = 1) & ({reg, r2} * {reg_SP..reg_DI} # {}) THEN
		n := 40H;
		IF reg >= reg_R8 THEN INC (n, R_bit) END;
		IF {r2, bas} * {reg_R8 .. reg_R15} # {} THEN INC (n, B_bit) END;
		IF idx >= reg_R8 THEN INC (n, X_bit) END;
		IF rsize = 8 THEN INC (n, W_bit) END;
		code [pc, Emit.i] := n; INCL (Emit.iflag, if_REX); INC (Emit.i)
	END
END Emit_REX_prefix;

PROCEDURE Emit_16bit_prefix (rsize : UBYTE);
BEGIN
	IF rsize = 2 THEN INCL (Emit.iflag, if_16bit_prefix);
		code [pc, Emit.i] := 66H; INC (Emit.i)
	END
END Emit_16bit_prefix;

PROCEDURE Emit_opcodeR (op : INTEGER; rsize, d : UBYTE);
BEGIN
	IF op MOD 256 = 0FH THEN
		code [pc, Emit.i] := 0FH; op := op DIV 256;
		INC (Emit.i)
	END;
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
	IF (d > 0) & (BITS(op) * BITS(d_bit) = {}) THEN INC (op, d_bit) END;
	code [pc, Emit.i] := USHORT (op);
	INC (Emit.i)
END Emit_opcodeR;

PROCEDURE Emit_ModRM (reg, rm, bas : UBYTE; disp : INTEGER);
	VAR n : INTEGER;
BEGIN
	n := reg MOD 8 + rm;
	IF disp = 0 THEN
		IF (rm IN {reg_BP, reg_R13})
		OR (rm = reg_SP) & (bas IN {reg_BP, reg_R13}) THEN
			INCL (Emit.iflag, if_disp); INC (n, 40H)	(* mod = 01b *)
		END
	ELSIF (disp >= MIN_INT8) & (disp <= MAX_INT8) THEN
		INCL (Emit.iflag, if_disp); INC (n, 40H) (* mod = 01b *)
	ELSE
		Emit.iflag := Emit.iflag + {if_disp, if_32bit_disp};
		INC (n, 80H) (* mod = 10b *)
	END;
	code [pc, Emit.i] := USHORT (n);
	INC (Emit.i)
END Emit_ModRM;

PROCEDURE Emit_disp (disp : INTEGER);
BEGIN
	IF if_disp IN Emit.iflag THEN
		SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp);
		IF if_32bit_disp IN Emit.iflag THEN INC (Emit.i, 4)
		ELSE INC (Emit.i)
		END
	END
END Emit_disp;

PROCEDURE Next_inst;
BEGIN
	codeinfo [pc].ip := ip; codeinfo [pc].size := Emit.i;
	codeinfo [pc].flag := Emit.iflag; INC (pc); INC (ip, Emit.i)
END Next_inst;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitBare (op : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	WHILE op > 0 DO
		code [pc, Emit.i] := USHORT (op MOD 256); INC (Emit.i);
		op := op DIV 256
	END;
	Next_inst
END EmitBare;

PROCEDURE EmitRR (op : INTEGER; reg, rsize, rm : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, rm, 0, 0); Emit_16bit_prefix (rsize);
	Emit_opcodeR (op, rsize, 1);
	
	(* ModR/M byte *)
	code [pc, Emit.i] := USHORT (0C0H + reg MOD 8 * 8 + rm MOD 8);
	INC (Emit.i);
	
	Next_inst
END EmitRR;

PROCEDURE EmitRRnd (op : INTEGER; reg, rsize, rm : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, rm, 0, 0); Emit_16bit_prefix (rsize);
	Emit_opcodeR (op, rsize, 0);
	
	(* ModR/M byte *)
	code [pc, Emit.i] := USHORT (0C0H + reg MOD 8 * 8 + rm MOD 8);
	INC (Emit.i);
	
	Next_inst
END EmitRRnd;

PROCEDURE EmitRMs
(d : UBYTE; op : INTEGER; reg, rsize, bas, idx, scl : UBYTE; disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, 0, bas, idx); Emit_16bit_prefix (rsize);
	Emit_opcodeR (op, rsize, d); Emit_ModRM (reg, reg_SP, bas, disp);
	
	(* SIB byte *)
	code [pc, Emit.i] := USHORT (scl * 64 + idx MOD 8 * 8 + bas MOD 8);
	INC (Emit.i);
	
	Emit_disp (disp);
	Next_inst
END EmitRMs;

PROCEDURE EmitRM
(d : UBYTE; op : INTEGER; reg, rsize, rm : UBYTE; disp : INTEGER);
BEGIN
	IF ~ (rm IN {reg_R12, reg_SP}) THEN
		Emit.iflag := {}; Emit.i := 0;
		Emit_REX_prefix (rsize, reg, 0, rm, 0); Emit_16bit_prefix (rsize);
		Emit_opcodeR (op, rsize, d); Emit_ModRM (reg, rm, 0, disp);
		Emit_disp (disp);
		Next_inst
	ELSE
		EmitRMs (d, op, reg, rsize, rm, reg_SP, 0, disp)
	END
END EmitRM;

PROCEDURE EmitRR2 (op : INTEGER; reg, rsize, rm, rmsize : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, rm, 0, 0); Emit_opcodeR (op, rmsize, 1);
	
	(* ModR/M byte *)
	code [pc, Emit.i] := USHORT (0C0H + reg MOD 8 * 8 + rm MOD 8);
	INC (Emit.i);
	
	Next_inst
END EmitRR2;

PROCEDURE EmitRMs2
(op : INTEGER; reg, rsize, bas, idx, scl : UBYTE; disp : INTEGER; rmsize : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, 0, bas, idx); Emit_opcodeR (op, rmsize, 1);
	Emit_ModRM (reg, reg_SP, bas, disp);
	
	(* SIB byte *)
	code [pc, Emit.i] := USHORT (scl * 64 + idx MOD 8 * 8 + bas MOD 8);
	INC (Emit.i);
	
	Emit_disp (disp);
	Next_inst
END EmitRMs2;

PROCEDURE EmitRM2
(op : INTEGER; reg, rsize, rm : UBYTE; disp : INTEGER; rmsize : UBYTE);
BEGIN
	IF rm IN {reg_R12, reg_SP} THEN
		EmitRMs2 (op, reg, rsize, rm, reg_SP, 0, disp, rmsize)
	ELSE
		Emit.iflag := {}; Emit.i := 0;
		Emit_REX_prefix (rsize, reg, 0, rm, 0); Emit_opcodeR (op, rmsize, 1);
		Emit_ModRM (reg, rm, 0, disp); Emit_disp (disp);
		Next_inst
	END
END EmitRM2;

PROCEDURE EmitRMip
(d : UBYTE; op : INTEGER; reg, rsize : UBYTE; disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, 0, 0, 0); Emit_16bit_prefix (rsize);
	Emit_opcodeR (op, rsize, d);
	
	code [pc, Emit.i] := USHORT (reg MOD 8 * 8 + reg_BP); INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i, 4);
	
	Next_inst
END EmitRMip;

PROCEDURE MoveRI (rm, rsize : UBYTE; imm : LONGINT);
	VAR op : UBYTE;
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, 0, rm, 0, 0); Emit_16bit_prefix (rsize);
	
	op := USHORT (0B0H + rm);
	IF rsize > 1 THEN INC (op, w_bit_1byte) END;
	code [pc, Emit.i] := op;	(* Opcode byte *)
	INC (Emit.i);
	
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), imm);
	INC (Emit.i, rsize);
	
	Next_inst
END MoveRI;

PROCEDURE EmitRRI (op : INTEGER; reg, rsize, rm : UBYTE; imm : LONGINT);
	VAR isize : UBYTE;
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, rm, 0, 0); Emit_16bit_prefix (rsize);
	
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
	
	isize := rsize;
	IF (isize > 1) & (imm >= MIN_INT8) & (imm <= MAX_INT8) THEN
		INC (op, s_bit); isize := 1
	ELSIF isize = 8 THEN isize := 4
	END;
	
	code [pc, Emit.i] := USHORT (op);	(* Opcode byte *)
	INC (Emit.i);
	
	(* ModR/M byte *)
	code [pc, Emit.i] := USHORT (0C0H + reg MOD 8 * 8 + rm MOD 8);
	INC (Emit.i);
	
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), imm);
	INC (Emit.i, isize);
	
	Next_inst
END EmitRRI;

PROCEDURE EmitRI (op : INTEGER; rm, rsize : UBYTE; imm : LONGINT);
	VAR op3bit, isize : UBYTE;
BEGIN
	IF op # IMULi THEN
		Emit.iflag := {}; Emit.i := 0;
		Emit_REX_prefix (rsize, 0, rm, 0, 0); Emit_16bit_prefix (rsize);
		
		op3bit := USHORT (op DIV 256);
		op := op MOD 256;
		
		IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
		
		isize := rsize;
		IF (rsize > 1) & (imm >= MIN_INT8) & (imm <= MAX_INT8) THEN
			INC (op, s_bit); isize := 1
		ELSIF isize = 8 THEN isize := 4
		END;
		
		code [pc, Emit.i] := USHORT (op);	(* Opcode byte *)
		INC (Emit.i);
		
		(* ModR/M byte *)
		code [pc, Emit.i] := USHORT (0C0H + op3bit * 8 + rm MOD 8);
		INC (Emit.i);
		
		SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), imm);
		INC (Emit.i, isize);
		
		Next_inst
	ELSE
		EmitRRI (IMULi, rm, rsize, rm, imm)
	END
END EmitRI;

PROCEDURE EmitRI8 (op : INTEGER; rm, rsize : UBYTE; imm8 : LONGINT);
	VAR op3bit : UBYTE;
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, 0, rm, 0, 0); Emit_16bit_prefix (rsize);
	
	op3bit := USHORT (op DIV 256);
	op := op MOD 256;
	
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
	
	code [pc, Emit.i] := USHORT (op);	(* Opcode byte *)
	INC (Emit.i);
	
	(* ModR/M byte *)
	code [pc, Emit.i] := USHORT (0C0H + op3bit * 8 + rm MOD 8);
	INC (Emit.i);
	
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), imm8);
	INC (Emit.i);
	
	Next_inst
END EmitRI8;

PROCEDURE EmitR (op : INTEGER; rm, rsize : UBYTE);
	VAR op3bit : UBYTE;
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, 0, rm, 0, 0); Emit_16bit_prefix (rsize);
	
	op3bit := USHORT (op DIV 256);
	op := op MOD 256;
	
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
	
	code [pc, Emit.i] := USHORT (op);	(* Opcode byte *)
	INC (Emit.i);
		
	(* ModR/M byte *)
	code [pc, Emit.i] := USHORT (0C0H + op3bit * 8 + rm MOD 8);
	INC (Emit.i);
	
	Next_inst
END EmitR;

PROCEDURE PushR (rm : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0; Emit_REX_prefix (8, 0, rm, 0, 0);
	code [pc, Emit.i] := USHORT (50H + rm MOD 8); INC (Emit.i); Next_inst
END PushR;

PROCEDURE PopR (rm : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0; Emit_REX_prefix (8, 0, rm, 0, 0);
	code [pc, Emit.i] := USHORT (58H + rm MOD 8); INC (Emit.i); Next_inst
END PopR;

PROCEDURE EmitRGv (d : UBYTE; op : INTEGER; reg, rsize : UBYTE; disp : INTEGER);
BEGIN
	EmitRMip (d, op, reg, rsize, varbase + disp);
	INCL (codeinfo [pc - 1].flag, if_global_var)
END EmitRGv;

PROCEDURE EmitRSv (d : UBYTE; op : INTEGER; reg, rsize : UBYTE; disp : INTEGER);
BEGIN
	EmitRMip (d, op, reg, rsize, staticbase + disp);
	INCL (codeinfo [pc - 1].flag, if_static_var)
END EmitRSv;

PROCEDURE Branch (disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	code [pc, Emit.i] := 0E9H; INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i, 4);
	Next_inst
END Branch;

PROCEDURE BranchS (disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	code [pc, Emit.i] := 0EBH; INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i);
	Next_inst
END BranchS;

PROCEDURE CondBranch (cond, disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	code [pc, Emit.i] := 0FH; INC (Emit.i);
	code [pc, Emit.i] := USHORT (80H + cond); INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i, 4);
	Next_inst
END CondBranch;

PROCEDURE CondBranchS (cond, disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	code [pc, Emit.i] := USHORT (70H + cond); INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i);
	Next_inst
END CondBranchS;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Fix_link* (L : INTEGER);
	VAR L1, i, size : INTEGER;
BEGIN
	WHILE L # 0 DO
		size := codeinfo [L].size;
		IF code [L, 0] # 0FH THEN i := 1 ELSE i := 2 END;
		SYSTEM.GET (SYSTEM.ADR (code [L, i]), L1);
		SYSTEM.PUT (SYSTEM.ADR (code [L, i]), ip - codeinfo [L].ip - size);
		L := L1
	END
END Fix_link;

PROCEDURE Fix_link_with (L : INTEGER; dst : INTEGER);
	VAR L1, i, size : INTEGER;
BEGIN
	WHILE L # 0 DO
		i := 1;
		size := codeinfo [L].size;
		IF size = 6 THEN INC (i) END;
		SYSTEM.GET (SYSTEM.ADR (code [L, i]), L1);
		SYSTEM.PUT (SYSTEM.ADR (code [L, i]), dst - codeinfo [L].ip - size);
		L := L1
	END
END Fix_link_with;

PROCEDURE merged (L0, L1: INTEGER) : INTEGER;
	VAR L2, L3, size, i : INTEGER;
BEGIN 
	IF L0 # 0 THEN
		L3 := L0;
		REPEAT
			L2 := L3;
			i := 1;
			size := codeinfo [L2].size;
			IF size = 6 THEN INC (i) END;
			SYSTEM.GET (SYSTEM.ADR (code [L2, i]), L3)
		UNTIL L3 = 0;
		SYSTEM.PUT (SYSTEM.ADR (code [L2, i]), L1);
		L1 := L0
	END;
    RETURN L1
END merged;

PROCEDURE Set_cond (VAR x : Item; n : INTEGER);
BEGIN
	x.mode := Base.mode_cond;
	x.a := 0; x.b := 0; x.c := n
END Set_cond;

PROCEDURE negated (cond : INTEGER) : INTEGER;
BEGIN
	IF ODD(cond) THEN DEC (cond) ELSE INC (cond) END;
	RETURN cond
END negated;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Reg_stack (offset : INTEGER) : UBYTE;
	VAR i : INTEGER;
BEGIN
	i := reg_stacks [crs].i;
	RETURN reg_stacks [crs].ord [i + offset]
END Reg_stack;

PROCEDURE Alloc_reg() : UBYTE;
	VAR i, r : UBYTE;
BEGIN
	i := reg_stacks [crs].i;
	IF i < reg_stacks [crs].n THEN INC (reg_stacks [crs].i)
	ELSE Scanner.Mark ('Compiler limit: Register stack overflow')
	END;
	r := reg_stacks [crs].ord [i];
	INCL (curRegs, r); INCL (ProcState.usedRegs, r)
	RETURN r
END Alloc_reg;

PROCEDURE Free_reg;
	VAR i, r : UBYTE;
BEGIN
	i := reg_stacks [crs].i;
	IF i > 0 THEN
		DEC (reg_stacks [crs].i); r := reg_stacks [crs].ord [i - 1];
		EXCL (curRegs, r)
	ELSE Scanner.Mark ('Fault: Register stack overflow')
	END
END Free_reg;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE load* (VAR x : Item);
	VAR
		rsize : UBYTE;
BEGIN
	IF x.mode # mode_reg THEN
		rsize := USHORT (x.type.size);
		IF x.mode = Base.class_var THEN
			x.r := Alloc_reg();
			IF x.lev > 0 THEN
				IF x.type.form # Base.type_string THEN
					EmitRM (1, MOVr, x.r, rsize, reg_BP, SHORT(x.a))
				ELSE EmitRSv (1, MOVr, x.r, rsize, SHORT(x.a))
				END
			ELSIF x.lev = 0 THEN
				EmitRGv (1, MOVr, x.r, rsize, SHORT(x.a))
			END
		ELSIF x.mode = Base.class_ref THEN
			x.r := Alloc_reg();
			IF x.lev > 0 THEN
				EmitRM (1, MOVr, x.r, 8, reg_BP, SHORT(x.a));
				EmitRM (1, MOVr, x.r, rsize, x.r, x.b);
				x.a := 0
			END
		ELSIF x.mode = Base.mode_regI THEN
			EmitRM (1, MOVr, x.r, rsize, x.r, SHORT(x.a))
		ELSIF x.mode = mode_imm THEN
			x.r := Alloc_reg();
			IF (rsize = 8) & (x.a >= 0) & (x.a <= MAX_UINT32) THEN
				MoveRI (x.r, 4, x.a)
			ELSE MoveRI (x.r, rsize, x.a)
			END
		END;
		x.mode := mode_reg
	END
END load;

PROCEDURE ConvertToInt64 (VAR x : Item);
BEGIN
	IF x.type # Base.byte_type THEN (* Do nothing *)
	ELSE load (x); EmitRR2 (MOVZX, x.r, 4, x.r, 1); x.type := Base.int_type
	END
END ConvertToInt64;

PROCEDURE Load_cond (VAR x : Item);
BEGIN
	IF x.mode = mode_imm THEN Set_cond (x, ccNever - SHORT (x.a))
	ELSE load (x); EmitRRnd (TESTr, x.r, 8, x.r); Free_reg; Set_cond (x, ccNZ)
	END
END Load_cond;

PROCEDURE Small_const (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN (x.mode = mode_imm) & (x.a >= MIN_INT32) & (x.a <= MAX_INT32)
END Small_const;

PROCEDURE Trap (cond, traptype : UBYTE);
BEGIN
END Trap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x : Item);
	VAR r : UBYTE;
BEGIN
	IF x.mode = mode_reg THEN
		IF x.a # 0 THEN
			IF (x.a >= MIN_INT32) & (x.a <= MAX_INT32) THEN
				EmitRI (ORi, x.r, 8, x.a)
			ELSE
				r := Alloc_reg(); MoveRI (r, 8, x.a);
				EmitRR (ORr, x.r, 8, r); Free_reg
			END
		END
	END
END Set1;
	
PROCEDURE Set2* (VAR x, y : Item);
	VAR
		s : ARRAY 2 OF SET;
		r : UBYTE;
BEGIN
	IF y.mode = mode_imm THEN
		(* Delay code generation *)
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s), x.a);
		IF y.a > 31 THEN INCL (s[0], y.a) ELSE INCL (s[1], y.a - 32) END;
		SYSTEM.GET (SYSTEM.ADR (s), x.a)
	ELSIF x.mode = mode_imm THEN
		r := Alloc_reg(); EmitRR (XORr, r, 4, r);
		load (y); EmitRR (BTSr, y.r, 8, r);
		IF r # Reg_stack(-2) THEN EmitRR (MOVr, r, 8, y.r); r := y.r END;
		Free_reg;
		x.mode := mode_reg; x.r := r
	ELSIF x.mode = mode_reg THEN
		load (y); EmitRR (BTSr, y.r, 8, x.r); Free_reg
	END
END Set2;
	
PROCEDURE Set3* (VAR x, y, z : Item);
	VAR
		s : ARRAY 2 OF SET;
		imm : LONGINT;
		r : UBYTE;
BEGIN
	IF (y.mode = mode_imm) & (z.mode = mode_imm) THEN
		IF y.a <= z.a THEN
			(* Delay code generation *)
			s[0] := {}; s[1] := {};
			SYSTEM.PUT (SYSTEM.ADR (s), x.a);
			IF (y.a < 32) & (z.a < 32) THEN
				s[0] := s[0] + {y.a .. z.a}
			ELSIF (y.a > 31) & (z.a > 31) THEN
				s[1] := s[1] + {y.a - 32 .. z.a - 32}
			ELSE
				s[0] := s[0] + {y.a .. 31};
				s[1] := s[1] + {32 .. z.a - 32}
			END;
			SYSTEM.GET (SYSTEM.ADR (s), x.a)
		ELSE Scanner.Mark ('The first element is greater than last element')
		END
	ELSE
		IF x.mode = mode_imm THEN r := Alloc_reg() ELSE r := x.r END;
		load (y); load (z);
		
		IF y.r = reg_C THEN
			MoveRI (r, 8, -1); EmitR (SHLcl, r, 8);
			EmitRR (MOVr, reg_C, 1, z.r); MoveRI (z.r, 8, -2);
			EmitR (SHLcl, z.r, 8); EmitRR (XORr, r, 8, z.r)
		ELSIF z.r = reg_C THEN
			MoveRI (r, 8, -2); EmitR (SHLcl, r, 8);
			EmitRR (MOVr, reg_C, 1, y.r); MoveRI (y.r, 8, -1);
			EmitR (SHLcl, y.r, 8); EmitRR (XORr, r, 8, y.r)
		ELSE
			MoveRI (r, 8, -1); EmitRR (XCHGr, reg_C, 1, y.r);
			EmitR (SHLcl, r, 8); EmitRR (MOVr, reg_C, 1, z.r);
			MoveRI (z.r, 8, -2); EmitR (SHLcl, z.r, 8);
			EmitRR (XORr, r, 8, z.r); EmitRR (XCHGr, reg_C, 1, y.r)
		END;
		
		x.r := Reg_stack(-3); x.mode := mode_reg;
		IF r = x.r THEN (* Do nothing *) ELSE EmitRR (MOVr, x.r, 8, r) END;
		Free_reg; Free_reg
	END
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Op1* (op : INTEGER; VAR x : Item);
	VAR
		s : ARRAY 2 OF SET;
		t, cond : INTEGER;
BEGIN
	IF op = Base.sym_not THEN
		IF x.mode = mode_imm THEN
			x.a := (x.a + 1) MOD 2
		ELSE
			IF x.mode # Base.mode_cond THEN Load_cond (x) END;
			x.c := negated (x.c);
			t := SHORT (x.a); x.a := x.b; x.b := t
		END
	ELSIF op = Base.sym_minus THEN
		IF x.mode = mode_imm THEN
			IF x.type.form = Base.type_integer THEN
				IF x.a = Base.MIN_INT THEN
					Scanner.Mark ('Integer overflow detected')
				ELSE x.a := -x.a
				END
			ELSIF x.type = Base.set_type THEN
				s[0] := {}; s[1] := {}; SYSTEM.PUT (SYSTEM.ADR (s), x.a);
				s[0] := -s[0]; s[1] := -s[1]; SYSTEM.GET (SYSTEM.ADR (s), x.a)
			END
		ELSE
			load (x);
			IF x.type = Base.int_type THEN EmitR (NEGr, x.r, 8);
				IF integer_check IN compiler_flag THEN Trap (ccO, integer_check)
				END
			ELSIF x.type = Base.byte_type THEN
				EmitRR2 (MOVZX, x.r, 8, x.r, 1); EmitR (NEGr, x.r, 8)
			ELSIF x.type = Base.set_type THEN EmitR (NOTr, x.r, 8)
			END
		END
	ELSIF op = Base.sym_and THEN
		IF x.mode # Base.mode_cond THEN Load_cond (x) END;
		cond := negated (x.c);
		IF cond # ccNever THEN CondBranch (cond, SHORT (x.a));
			x.a := pc - 1
		END;
		Fix_link (x.b); x.b := 0
	ELSIF op = Base.sym_or THEN
		IF x.mode # Base.mode_cond THEN Load_cond (x) END;
		IF x.c # ccNever THEN CondBranch (x.c, x.b);
			x.b := pc - 1
		END;
		Fix_link (SHORT (x.a)); x.a := 0
	END
END Op1;

PROCEDURE IntegerOp (opR, opI : INTEGER; VAR x, y : Item);
BEGIN
	IF Small_const (y) THEN EmitRI (opI, x.r, 8, y.a)
	ELSIF ~ Small_const (x) THEN
		load (x); load (y); x.r := Reg_stack(-2);
		EmitRR (opR, x.r, 8, Reg_stack(-1)); Free_reg
	ELSE
		load (y); EmitRI (opI, y.r, 8, x.a);
		x.mode := mode_reg; x.r := y.r
	END
END IntegerOp;

PROCEDURE Subtract (VAR x, y : Item);
BEGIN
	IF x.mode # mode_imm THEN
		IF Small_const (y) THEN EmitRI (SUBi, x.r, 8, y.a)
		ELSE load (y); EmitRR (SUBr, x.r, 8, y.r); Free_reg
		END
	ELSE
		load (x); load (y); x.r := Reg_stack(-2);
		EmitRR (SUBr, x.r, 8, Reg_stack(-1)); Free_reg
	END
END Subtract;

PROCEDURE Divide (op : INTEGER; VAR x, y : Item);
	VAR dst, r, r2 : UBYTE; saveA, saveD : BOOLEAN;
		L : INTEGER;
BEGIN
	IF (y.mode # mode_imm) OR (y.a > 0) & (Base.log2(y.a) < 0) THEN
		r := 0; r2 := 0;

		load (x); load (y);
		IF integer_check IN compiler_flag THEN
			EmitRRnd (TESTr, y.r, 8, y.r); Trap (ccLE, integer_check)
		END;
		
		dst := Reg_stack(-2);
		
		IF (reg_A IN curRegs) & (x.r # reg_A) THEN
			r := Alloc_reg(); EmitRR (MOVr, r, 8, reg_A); saveA := TRUE
		ELSE saveA := FALSE
		END;
		IF (reg_D IN curRegs) & (x.r # reg_D) THEN
			r2 := Alloc_reg(); EmitRR (MOVr, r2, 8, reg_D); saveD := TRUE
		ELSE saveD := FALSE
		END;
		
		IF x.r # reg_A THEN EmitRR (MOVr, reg_A, 8, x.r) END;
		EmitBare (CQO);
		
		EmitRRnd (TESTr, reg_A, 8, reg_A);
		L := pc; CondBranchS (ccS, 0);
		
		IF ~ (y.r IN {reg_A, reg_D}) THEN EmitR (IDIVr, y.r, 8)
		ELSIF y.r = reg_D THEN EmitR (IDIVr, r2, 8)
		ELSE EmitR (IDIVr, r, 8)
		END;
		
		BranchS (0); Fix_link (L); L := pc - 1; 
		
		IF ~ (y.r IN {reg_A, reg_D}) THEN EmitR (IDIVr, y.r, 8)
		ELSIF y.r = reg_D THEN EmitR (IDIVr, r2, 8)
		ELSE EmitR (IDIVr, r, 8)
		END;
		
		EmitRRnd (TESTr, reg_D, 8, reg_D);
		CondBranchS (ccZ, L); L := pc - 1;
		
		IF op = Base.sym_div THEN EmitRI (SUBi, reg_A, 8, 1)
		ELSIF ~ (y.r IN {reg_A, reg_D}) THEN EmitRR (ADDr, reg_D, 8, y.r)
		ELSIF y.r = reg_D THEN EmitRR (ADDr, reg_D, 8, r2)
		ELSE EmitRR (ADDr, reg_D, 8, r)
		END;
		
		Fix_link (L);
		
		IF op = Base.sym_div THEN
			IF dst # reg_A THEN EmitRR (MOVr, dst, 8, reg_A) END
		ELSIF dst # reg_D THEN EmitRR (MOVr, dst, 8, reg_D)
		END;
		
		IF saveD THEN Free_reg;
			IF y.r # reg_D THEN EmitRR (MOVr, reg_D, 8, r2) END
		END;
		IF saveA THEN Free_reg;
			IF y.r # reg_A THEN EmitRR (MOVr, reg_A, 8, r) END
		END;
		
		x.r := dst; Free_reg
	ELSE
		IF y.a > 1 THEN
			IF op = Base.sym_div THEN EmitRI8 (SARi, x.r, 8, Base.log2(y.a))
			ELSE DEC (y.a);
				IF y.a <= MAX_INT32 THEN EmitRI (ANDi, x.r, 8, y.a)
				ELSE load (y); EmitRR (ANDr, x.r, 8, y.r); Free_reg
				END
			END
		ELSIF y.a = 1 THEN
			IF op = Base.sym_mod THEN EmitRR (XORr, x.r, 4, x.r)
			END
		ELSE Scanner.Mark ('Division by non-positive number')
		END
	END
END Divide;

PROCEDURE BitwiseOp (op : INTEGER; VAR x, y : Item);
	VAR dst, src : UBYTE;
BEGIN
	load (x); load (y); dst := Reg_stack(-2); src := Reg_stack(-1);
	EmitRR (op, dst, 8, src); Free_reg; x.r := dst
END BitwiseOp;

PROCEDURE Difference (VAR x, y : Item);
	VAR s : ARRAY 2 OF SET;
BEGIN
	IF y.mode # mode_imm THEN
		load (y); EmitR (NOTr, y.r, 8); BitwiseOp (ANDr, x, y)
	ELSE
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s), y.a); s[0] := -s[0]; s[1] := -s[1];
		SYSTEM.GET (SYSTEM.ADR (s), y.a);
		load (y); EmitRR (ANDr, x.r, 8, y.r); Free_reg
	END
END Difference;

PROCEDURE IntConstOp (op : INTEGER; x, y : LONGINT) : LONGINT;
BEGIN
	CASE op OF
		Base.sym_plus:
		IF Base.Safe_to_add (x, y) THEN INC (x, y)
		ELSE Scanner.Mark ('Integer overflow detected')
		END |
		
		Base.sym_minus:
		IF Base.Safe_to_subtract (x, y) THEN DEC (x, y)
		ELSE Scanner.Mark ('Integer overflow detected')
		END |
		
		Base.sym_times:
		IF Base.Safe_to_multiply (x, y) THEN x := x * y
		ELSE Scanner.Mark ('Integer overflow detected')
		END |
		
		Base.sym_div:
		IF y > 0 THEN x := x DIV y
		ELSE Scanner.Mark ('Division by non-positive number')
		END |
		
		Base.sym_mod:
		IF y > 0 THEN x := x MOD y
		ELSE Scanner.Mark ('Division by non-positive number')
		END
	END;
	RETURN x
END IntConstOp;

PROCEDURE SetConstOp (op : INTEGER; x, y : LONGINT) : LONGINT;
	VAR dst, src : ARRAY 2 OF SET;
BEGIN
	dst[0] := {}; dst[1] := {}; src[0] := {}; src[1] := {};
	SYSTEM.PUT (SYSTEM.ADR (dst), x); SYSTEM.PUT (SYSTEM.ADR (src), y);
	
	CASE op OF
		Base.sym_plus: dst[0] := dst[0] + src[0]; dst[1] := dst[1] + src[1] |
		Base.sym_minus: dst[0] := dst[0] - src[0]; dst[1] := dst[1] - src[1] |
		Base.sym_times: dst[0] := dst[0] * src[0]; dst[1] := dst[1] * src[1] |
		Base.sym_slash: dst[0] := dst[0] / src[0]; dst[1] := dst[1] / src[1]
	END;
	
	SYSTEM.GET (SYSTEM.ADR (dst), x);
	RETURN x
END SetConstOp;
	
PROCEDURE Op2* (op : INTEGER; VAR x, y : Item);
BEGIN
	IF (x.mode = Base.class_const) & (y.mode = Base.class_const) THEN
		IF x.type = Base.int_type THEN x.a := IntConstOp (op, x.a, y.a)
		ELSIF x.type = Base.set_type THEN x.a := SetConstOp (op, x.a, y.a)
		END
	ELSE
		IF x.type.form = Base.type_integer THEN
			ConvertToInt64 (x); ConvertToInt64 (y);
			CASE op OF
				Base.sym_plus: IntegerOp (ADDr, ADDi, x, y) |
				Base.sym_minus: Subtract (x, y) |
				Base.sym_times: IntegerOp (IMULr, IMULi, x, y) |
				Base.sym_div: Divide (Base.sym_div, x, y) |
				Base.sym_mod: Divide (Base.sym_mod, x, y)
			END
		ELSIF x.type = Base.set_type THEN
			CASE op OF
				Base.sym_plus: BitwiseOp (ORr, x, y) |
				Base.sym_minus: Difference (x, y) |
				Base.sym_times: BitwiseOp (ANDr, x, y) |
				Base.sym_slash: BitwiseOp (XORr, x, y) |
			END
		ELSIF x.type = Base.bool_type THEN
			IF y.mode # Base.mode_cond THEN Load_cond (y) END;
			IF op = Base.sym_and THEN
				x.a := merged (SHORT (y.a), SHORT (x.a));
				x.b := y.b
			ELSIF op = Base.sym_or THEN
				x.b := merged (y.b, x.b);
				x.a := y.a
			END;
			x.c := y.c
		END
	END
END Op2;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FJump* (VAR L : INTEGER);
BEGIN
	Branch (L);
	L := pc - 1
END FJump;

PROCEDURE CFJump* (VAR x : Item);
	VAR cond : INTEGER;
BEGIN
	IF x.mode # Base.mode_cond THEN Load_cond (x) END;
    cond := negated (x.c);
	IF cond < 16 THEN CondBranch (cond, SHORT (x.a)); x.a := pc - 1
	ELSIF cond = ccAlways THEN Branch (SHORT (x.a)); x.a := pc - 1
	END;
	Fix_link (x.b)
END CFJump;
	
PROCEDURE CBJump* (VAR x : Item; L : INTEGER);
	VAR cond : INTEGER;
BEGIN
	IF x.mode # Base.mode_cond THEN Load_cond (x) END;
    cond := negated (x.c);
	IF cond < 16 THEN CondBranch (cond, codeinfo [L].ip - ip - 6)
	ELSIF cond = ccAlways THEN Branch (codeinfo [L].ip - ip - 5)
	END;
	Fix_link (x.b); Fix_link_with (SHORT (x.a), codeinfo [L].ip)
END CBJump;
  
PROCEDURE BJump* (L : INTEGER);
BEGIN
	Branch (codeinfo [L].ip - ip - 5)
END BJump;

PROCEDURE Fixup* (x : Item);
BEGIN
	Fix_link (SHORT (x.a))
END Fixup;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Write_to_file* (from, to : INTEGER);
	VAR	file : Sys.FileHandle; i, j : INTEGER;
		
	PROCEDURE Fixup_disp (p : INTEGER);
		VAR i, disp : INTEGER;
	BEGIN
		disp := 0;
		i := codeinfo [p].size - 4;
		SYSTEM.GET (SYSTEM.ADR (code [p, i]), disp);
		DEC (disp, codeinfo [p].ip + codeinfo [p].size + ProcState.prolog_size);
		SYSTEM.PUT (SYSTEM.ADR (code [p, i]), disp)
	END Fixup_disp;
	
BEGIN (* Write_to_file *)
	ASSERT ((from > 0) & (to > 0) & (from <= pc) & (to <= pc));
	FOR i := from TO to DO
		IF {if_global_var, if_static_var} * codeinfo [i].flag # {} THEN
			Fixup_disp (i)
		END;
		FOR j := 0 TO codeinfo [i].size - 1 DO
			Sys.Write_byte (out, code [i, j])
		END
	END
END Write_to_file;

PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
BEGIN
	IF proc # NIL THEN
		ProcState.locblksize := locblksize;
		ProcState.parlist := proc.dsc;
		proc.val := ip
	ELSE
		ProcState.locblksize := 0;
		ProcState.parlist := NIL
	END;
	ProcState.usedRegs := {};
	ProcState.adr := ip;
	Reset_reg_stack; pc := 1
END Enter;
	
PROCEDURE Return* (proc : Base.Object; locblksize : INTEGER);
	VAR i, k, nRegs, endPC, endIP : INTEGER;
		paramRegs : ARRAY 4 OF UBYTE; obj : Base.Object;
		
	PROCEDURE Param_size (obj : Base.Object) : INTEGER;
		VAR result : INTEGER;
	BEGIN
		IF (obj.type.form = Base.type_array) & (obj.type.len < 0) THEN
			result := obj.type.size
		ELSIF ~ obj.readonly & (obj.type.form = Base.type_record) THEN
			result := 16
		ELSE result := 8
		END;
		RETURN result
	END Param_size;
		
BEGIN (* Return *)
	nRegs := 0;
	IF reg_R15 IN ProcState.usedRegs THEN PopR (reg_R15); INC (nRegs) END;
	IF reg_R14 IN ProcState.usedRegs THEN PopR (reg_R14); INC (nRegs) END;
	IF reg_R13 IN ProcState.usedRegs THEN PopR (reg_R13); INC (nRegs) END;
	IF reg_R12 IN ProcState.usedRegs THEN PopR (reg_R12); INC (nRegs) END;
	IF reg_SI IN ProcState.usedRegs THEN PopR (reg_SI); INC (nRegs) END;
	IF reg_DI IN ProcState.usedRegs THEN PopR (reg_DI); INC (nRegs) END;
	
	i := (ProcState.locblksize + nRegs * 8) MOD 16;
	IF i > 0 THEN INC (ProcState.locblksize, 16 - i) END;
	EmitBare (LEAVE); EmitBare (RET);
	endPC := pc; endIP := ip;
	
	PushR (reg_BP); EmitRR (MOVr, reg_BP, 8, reg_SP);
	IF locblksize > 0 THEN EmitRI (SUBi, reg_SP, 8, locblksize) END;
	
	IF reg_DI IN ProcState.usedRegs THEN PushR (reg_DI); INC (nRegs) END;
	IF reg_SI IN ProcState.usedRegs THEN PushR (reg_SI); INC (nRegs) END;
	IF reg_R12 IN ProcState.usedRegs THEN PushR (reg_R12); INC (nRegs) END;
	IF reg_R13 IN ProcState.usedRegs THEN PushR (reg_R13); INC (nRegs) END;
	IF reg_R14 IN ProcState.usedRegs THEN PushR (reg_R14); INC (nRegs) END;
	IF reg_R15 IN ProcState.usedRegs THEN PushR (reg_R15); INC (nRegs) END;
	
	IF ProcState.parlist # NIL THEN
		paramRegs [0] := reg_C; paramRegs [1] := reg_D;
		paramRegs [2] := reg_R8; paramRegs [3] := reg_R9;
		obj := parlist; i := 0; k := 0;
		WHILE obj # Base.guard DO
			IF k = 0 THEN k := Param_size (obj) END;
			EmitRM (0, MOVr, paramRegs [i], 8, reg_BP, 16 + i * 8);
			INC (i); DEC (k, 8);
			IF k = 0 THEN obj := obj.next END
		END
	END;
	
	ProcState.prolog_size := ip - endIP;
	Write_to_file (endPC, pc - 1);
	Write_to_file (1, endPC - 1);
	IF ip MOD 16 # 0 THEN INC (ip, 16 - ip MOD 16) END
END Return;

PROCEDURE Init* (modname : Base.String);
BEGIN
	ip := 0; varsize := 0; varbase := 0; staticbase := 0;
	Sys.Rewrite (out, 'output.bin')
END Init;

PROCEDURE Finish*;
BEGIN
	Sys.Close (out)
END Finish;

BEGIN
	reg_stacks [0].i := 0;
	reg_stacks [0].n := 11;
	reg_stacks [0].ord [0] := reg_A;
	reg_stacks [0].ord [1] := reg_C;
	reg_stacks [0].ord [2] := reg_D;
	reg_stacks [0].ord [3] := reg_R8;
	reg_stacks [0].ord [4] := reg_R9;
	reg_stacks [0].ord [5] := reg_R10;
	reg_stacks [0].ord [6] := reg_R11;
	reg_stacks [0].ord [7] := reg_R12;
	reg_stacks [0].ord [8] := reg_R13;
	reg_stacks [0].ord [9] := reg_R14;
	reg_stacks [0].ord [10] := reg_R15
END GeneratorWin64v2.