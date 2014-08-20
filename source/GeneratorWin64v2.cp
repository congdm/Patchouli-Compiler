MODULE GeneratorWin64v2;

IMPORT
	SYSTEM, Sys, Base, Scanner;

CONST
	MIN_INT8 = -128;
	MAX_INT8 = 127;
	MAX_UINT32 = 4294967295;
	MAX_INT32 = 2147483647;
	MIN_INT32 = -2147483648;
	
	integer_check = Base.integer_overflow_check;
	array_check = Base.array_bound_check;
	
	mode_reg = Base.mode_reg; mode_imm = Base.class_const;
	mode_regI = Base.mode_regI;
	mode_mem = {Base.class_var, Base.class_ref, mode_regI};
	
	if_skipped = 0; if_REX = 1; if_16bit_prefix = 2; if_disp = 3;
	if_32bit_disp = 4; if_fix1 = 5;

	W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1; (* REX prefix *)
	d_bit = 2; w_bit = 1; s_bit = 2; w_bit_1byte = 8;

	reg_A = 0; reg_C = 1; reg_D = 2; reg_B = 3;
	reg_SP = 4; reg_BP = 5; reg_SI = 6; reg_DI = 7;
	reg_R8 = 8; reg_R9 = 9; reg_R10 = 10; reg_R11 = 11;
	reg_R12 = 12; reg_R13 = 13; reg_R14 = 14; reg_R15 = 15;
	
	ccO = 0; ccNO = 1; ccB = 2; ccAE = 3; ccZ = 4; ccNZ = 5; ccBE = 6; ccA = 7;
	ccS = 8; ccNS = 9; ccP = 10; ccNP = 11; ccL = 12; ccGE = 13; ccLE = 14;
	ccG = 15; ccAlways = 16; ccNever = 17;
	
	MOVr = 88H; XCHGr = 86H; LEAr = 8DH;
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
	
	ProcInfo* = RECORD
		has_retval, procvar : BOOLEAN;
		oldi : UBYTE; oldrs : INTEGER;
		memstack, parblksize, paradr : INTEGER
	END;
	
VAR
	out : Sys.FileHandle;

	code : ARRAY 65536 OF Instruction;
	codeinfo : ARRAY 65536 OF InstructionInfo;
	pc, ip, entry : INTEGER;
	
	staticdata : ARRAY 100000H OF UBYTE;
	varbase, staticbase, varsize, staticsize : INTEGER;
	
	(* Reg stack *)
	reg_stacks : ARRAY 2 OF RegStack;
	curRegs : SET;
	crs : INTEGER;
	
	(* Global variables for Emit procedures *)
	Emit : RECORD
		i : UBYTE;
		iflag : SET;
	END;
	
	ProcState : RECORD
		adr, prolog_size, locblksize, memstack : INTEGER;
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
	n := reg MOD 8 * 8 + rm MOD 8;
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
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (rsize, reg, 0, rm, 0); Emit_16bit_prefix (rsize);
	Emit_opcodeR (op, rsize, d); Emit_ModRM (reg, rm, 0, disp);
	
	IF rm IN {reg_R12, reg_SP} THEN
		code [pc, Emit.i] := 24H; INC (Emit.i); (* SIB byte *)
	END;
	
	Emit_disp (disp);
	Next_inst
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

(* -------------------------------------------------------------------------- *)

PROCEDURE PushR (rm : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0; Emit_REX_prefix (8, 0, rm, 0, 0);
	code [pc, Emit.i] := USHORT (50H + rm MOD 8); INC (Emit.i);
	INC (ProcState.memstack, 8);
	Next_inst
END PushR;

PROCEDURE PopR (rm : UBYTE);
BEGIN
	Emit.iflag := {}; Emit.i := 0; Emit_REX_prefix (8, 0, rm, 0, 0);
	code [pc, Emit.i] := USHORT (58H + rm MOD 8); INC (Emit.i);
	DEC (ProcState.memstack, 8);
	Next_inst
END PopR;

PROCEDURE EmitRGv (d : UBYTE; op : INTEGER; reg, rsize : UBYTE; disp : INTEGER);
BEGIN
	EmitRMip (d, op, reg, rsize, varbase + disp);
	INCL (codeinfo [pc - 1].flag, if_fix1)
END EmitRGv;

PROCEDURE EmitRSv (d : UBYTE; op : INTEGER; reg, rsize : UBYTE; disp : INTEGER);
BEGIN
	EmitRMip (d, op, reg, rsize, staticbase + disp);
	INCL (codeinfo [pc - 1].flag, if_fix1)
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

PROCEDURE EmitCall (disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	code [pc, Emit.i] := 0E8H; INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i, 4);
	INCL (Emit.iflag, if_fix1);
	Next_inst
END EmitCall;

PROCEDURE EmitCallM (rm : UBYTE; disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	Emit_REX_prefix (8, 0, rm, 0, 0);
	code [pc, Emit.i] := 0FFH; INC (Emit.i); Emit_ModRM (2, rm, 0, disp);
	
	IF rm IN {reg_R12, reg_SP} THEN
		code [pc, Emit.i] := 24H; INC (Emit.i); (* SIB byte *)
	END;
	
	Emit_disp (disp);
	Next_inst
END EmitCallM;

PROCEDURE EmitCallMip (disp : INTEGER);
BEGIN
	Emit.iflag := {}; Emit.i := 0;
	code [pc, Emit.i] := 0FFH; INC (Emit.i);
	
	code [pc, Emit.i] := USHORT (2 * 8 + reg_BP); INC (Emit.i);
	SYSTEM.PUT (SYSTEM.ADR (code [pc, Emit.i]), disp); INC (Emit.i, 4);
	
	Next_inst
END EmitCallMip;

PROCEDURE EmitCallSv (disp : INTEGER);
BEGIN
	EmitCallMip (staticbase + disp);
	INCL (codeinfo [pc - 1].flag, if_fix1)
END EmitCallSv;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Make_const* (VAR x : Base.Item; typ : Base.Type; val : LONGINT);
BEGIN
	x.readonly := FALSE;
	x.param := FALSE;
	x.mode := Base.class_const;
	x.lev := Base.cur_lev;
	x.type := typ;
	x.a := val;
	x.b := 0;
	x.c := 0
END Make_const;
	
PROCEDURE Make_item* (VAR x : Base.Item; obj : Base.Object);
BEGIN
	x.readonly := obj.readonly;
	x.param := obj.param;
	x.mode := obj.class;
	x.lev := obj.lev;
	x.type := obj.type;
	x.obj := obj;
	x.a := obj.val;
	x.b := obj.val2;
	x.c := 0;
	
	IF x.mode # Base.class_const THEN (* Do nothing *)
	ELSIF x.type.form = Base.type_string THEN
		x.mode := Base.class_var
	END
END Make_item;
	
PROCEDURE Make_string* (VAR x : Base.Item; str : Base.String);
	VAR
		bufoffset, strlen, i, charsize : INTEGER; b : UBYTE;
BEGIN
	x.readonly := TRUE; x.param := FALSE;
	x.mode := Base.class_var;
	x.lev := -1;
	
	charsize := Base.char_type.size;
	strlen := str.len + 1;
	
	Base.New_typ (x.type, Base.type_string);
	x.type.len := strlen;
	x.type.size := strlen * charsize;
	x.type.base := Base.char_type;
	
	INC (staticsize, strlen * charsize);
	i := -staticsize; x.a := i;
	
	bufoffset := i + LEN(staticdata); i := 0;
	IF charsize = 1 THEN
		WHILE i < strlen DO
			b := USHORT (ORD (str.content [i]) MOD 256);
			staticdata [bufoffset] := b;
			INC (i); INC (bufoffset)
		END;
	ELSIF charsize = 2 THEN
		WHILE i < strlen DO
			b := USHORT (ORD (str.content [i]) MOD 256);
			staticdata [bufoffset] := b;
			b := USHORT (ORD (str.content [i]) DIV 256 MOD 256);
			staticdata [bufoffset + 1] := b;
			INC (i); INC (bufoffset, 2)
		END;
	ELSE Scanner.Mark ('Fault: Unsupported char size')
	END
END Make_string;

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

PROCEDURE Set_cond (VAR x : Base.Item; n : INTEGER);
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

PROCEDURE Reset_reg_stack;
BEGIN
	reg_stacks [0].i := 0;
	crs := 0
END Reset_reg_stack;

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
	INCL (curRegs, r); INCL (ProcState.usedRegs, r);
	RETURN r
END Alloc_reg;

PROCEDURE Free_reg;
	VAR i, r : UBYTE;
BEGIN
	i := reg_stacks [crs].i;
	IF i > 0 THEN
		DEC (reg_stacks [crs].i); r := reg_stacks [crs].ord [i - 1];
		EXCL (curRegs, r)
	ELSE Scanner.Mark ('Fault: Register stack underflow')
	END
END Free_reg;

PROCEDURE Free_item* (VAR x : Base.Item);
BEGIN
	IF x.mode IN {mode_regI, mode_reg} THEN Free_reg END
END Free_item;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Ref_to_regI (VAR x : Base.Item);
BEGIN
	x.r := Alloc_reg();
	IF x.lev > 0 THEN EmitRM (1, MOVr, x.r, 8, reg_BP, SHORT(x.a)) END;
	x.a := x.c; x.mode := mode_regI
END Ref_to_regI;

PROCEDURE load* (VAR x : Base.Item);
	VAR
		rsize : UBYTE;
BEGIN
	IF x.mode # mode_reg THEN
		rsize := USHORT (x.type.size);
		IF x.mode = Base.class_var THEN
			x.r := Alloc_reg();
			IF x.lev > 0 THEN EmitRM (1, MOVr, x.r, rsize, reg_BP, SHORT(x.a))
			ELSIF x.lev = 0 THEN EmitRGv (1, MOVr, x.r, rsize, SHORT(x.a))
			ELSIF x.lev < 0 THEN EmitRSv (1, MOVr, x.r, rsize, SHORT(x.a)) 
			END
		ELSIF x.mode = Base.class_ref THEN
			Ref_to_regI (x);
			EmitRM (1, MOVr, x.r, rsize, x.r, SHORT(x.a))
		ELSIF x.mode = mode_regI THEN
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

PROCEDURE ConvertToInt64 (VAR x : Base.Item);
BEGIN
	IF x.type # Base.byte_type THEN (* Do nothing *)
	ELSE load (x); EmitRR2 (MOVZX, x.r, 4, x.r, 1); x.type := Base.int_type
	END
END ConvertToInt64;

PROCEDURE Load_adr (VAR x : Base.Item);
BEGIN
	IF x.mode = Base.class_var THEN
		x.r := Alloc_reg();
		IF x.lev > 0 THEN EmitRM (0, LEAr, x.r, 8, reg_BP, SHORT(x.a))
		ELSIF x.lev = 0 THEN EmitRGv (0, LEAr, x.r, 8, SHORT(x.a))
		ELSIF x.lev < 0 THEN EmitRSv (0, LEAr, x.r, 8, SHORT(x.a))
		END
	ELSIF x.mode = Base.class_ref THEN
		Ref_to_regI (x);
		IF x.a # 0 THEN EmitRI (ADDi, x.r, 8, x.a) END
	ELSIF x.mode = mode_regI THEN
		IF x.a # 0 THEN EmitRI (ADDi, x.r, 8, x.a) END
	END;
	x.mode := mode_reg
END Load_adr;

PROCEDURE Load_cond (VAR x : Base.Item);
BEGIN
	IF x.mode = mode_imm THEN Set_cond (x, ccNever - SHORT (x.a))
	ELSE load (x); EmitRRnd (TESTr, x.r, 8, x.r); Free_reg; Set_cond (x, ccNZ)
	END
END Load_cond;

PROCEDURE Store* (VAR x, y : Base.Item);
	VAR rsize : UBYTE;
BEGIN
	IF x.type.form IN Base.types_Scalar THEN
		load (y); rsize := USHORT (x.type.size);
		IF x.mode = Base.class_var THEN
			IF x.lev > 0 THEN EmitRM (0, MOVr, y.r, rsize, reg_BP, SHORT(x.a))
			ELSIF x.lev = 0 THEN EmitRGv (0, MOVr, y.r, rsize, SHORT(x.a))
			ELSIF x.lev < 0 THEN EmitRSv (0, MOVr, y.r, rsize, SHORT(x.a))
			END
		ELSIF x.mode = Base.class_ref THEN
			Ref_to_regI (x); EmitRM (1, MOVr, y.r, rsize, x.r, SHORT(x.a));
			Free_reg
		ELSIF x.mode = mode_regI THEN
			EmitRM (1, MOVr, y.r, rsize, x.r, SHORT(x.a)); Free_reg
		END;
		Free_reg
	ELSIF x.type.form IN {Base.type_array, Base.type_record} THEN

	END
END Store;

PROCEDURE Small_const (VAR x : Base.Item) : BOOLEAN;
BEGIN
	RETURN (x.mode = mode_imm) & (x.a >= MIN_INT32) & (x.a <= MAX_INT32)
END Small_const;

PROCEDURE Trap (cond, traptype : UBYTE);
BEGIN
END Trap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x : Base.Item);
	VAR r : UBYTE;
BEGIN
	IF x.mode = mode_reg THEN
		IF x.a # 0 THEN
			IF (x.a > 0) & (x.a <= MAX_INT32) THEN EmitRI (ORi, x.r, 8, x.a)
			ELSE
				r := Alloc_reg(); MoveRI (r, 8, x.a);
				EmitRR (ORr, x.r, 8, r); Free_reg
			END
		END
	END
END Set1;
	
PROCEDURE Set2* (VAR x, y : Base.Item);
	VAR
		s : ARRAY 2 OF SET;
		r : UBYTE;
BEGIN
	IF y.mode = mode_imm THEN
		(* Delay code generation *)
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR(s[0]), x.a);
		IF y.a > 31 THEN INCL (s[0], y.a) ELSE INCL (s[1], y.a - 32) END;
		SYSTEM.GET (SYSTEM.ADR(s[0]), x.a)
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
	
PROCEDURE Set3* (VAR x, y, z : Base.Item);
	VAR
		s : ARRAY 2 OF SET;
		imm : LONGINT;
		r : UBYTE;
BEGIN
	IF (y.mode = mode_imm) & (z.mode = mode_imm) THEN
		IF y.a <= z.a THEN
			(* Delay code generation *)
			s[0] := {}; s[1] := {};
			SYSTEM.PUT (SYSTEM.ADR(s[0]), x.a);
			IF (y.a < 32) & (z.a < 32) THEN
				s[0] := s[0] + {y.a .. z.a}
			ELSIF (y.a > 31) & (z.a > 31) THEN
				s[1] := s[1] + {y.a - 32 .. z.a - 32}
			ELSE
				s[0] := s[0] + {y.a .. 31};
				s[1] := s[1] + {32 .. z.a - 32}
			END;
			SYSTEM.GET (SYSTEM.ADR(s[0]), x.a)
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

PROCEDURE Op1* (op : INTEGER; VAR x : Base.Item);
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
				IF integer_check IN Base.compiler_flag THEN Trap (ccO, integer_check)
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

PROCEDURE IntegerOp (opR, opI : INTEGER; VAR x, y : Base.Item);
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

PROCEDURE Subtract (VAR x, y : Base.Item);
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

PROCEDURE Divide (op : INTEGER; VAR x, y : Base.Item);
	VAR dst, r, r2 : UBYTE; saveA, saveD : BOOLEAN;
		L : INTEGER;
BEGIN
	IF (y.mode # mode_imm) OR (y.a > 0) & (Base.log2(y.a) < 0) THEN
		r := 0; r2 := 0;

		load (x); load (y);
		IF integer_check IN Base.compiler_flag THEN
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

PROCEDURE BitwiseOp (op : INTEGER; VAR x, y : Base.Item);
	VAR dst, src : UBYTE;
BEGIN
	load (x); load (y); dst := Reg_stack(-2); src := Reg_stack(-1);
	EmitRR (op, dst, 8, src); Free_reg; x.r := dst
END BitwiseOp;

PROCEDURE Difference (VAR x, y : Base.Item);
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
	SYSTEM.PUT (SYSTEM.ADR(dst[0]), x); SYSTEM.PUT (SYSTEM.ADR(src[0]), y);
	
	CASE op OF
		Base.sym_plus: dst[0] := dst[0] + src[0]; dst[1] := dst[1] + src[1] |
		Base.sym_minus: dst[0] := dst[0] - src[0]; dst[1] := dst[1] - src[1] |
		Base.sym_times: dst[0] := dst[0] * src[0]; dst[1] := dst[1] * src[1] |
		Base.sym_slash: dst[0] := dst[0] / src[0]; dst[1] := dst[1] / src[1]
	END;
	
	SYSTEM.GET (SYSTEM.ADR(dst[0]), x);
	RETURN x
END SetConstOp;
	
PROCEDURE Op2* (op : INTEGER; VAR x, y : Base.Item);
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
(* Selector operation *)
	
PROCEDURE Field* (VAR x : Base.Item; field : Base.Object);
BEGIN
	IF x.mode = Base.class_ref THEN INC (x.c, SHORT(field.val))
	ELSE INC (x.a, field.val)
	END;
	x.param := FALSE; x.type := field.type
END Field;
	
PROCEDURE Deref* (VAR x : Base.Item);
BEGIN
	IF x.mode = Base.class_ref THEN
		x.r := Alloc_reg();
		IF x.lev > 0 THEN EmitRM (1, MOVr, x.r, 8, reg_BP, SHORT(x.a)) END;
		x.a := x.c; x.mode := mode_regI
	ELSIF x.mode = Base.class_var THEN
		load (x); x.a := 0; x.mode := mode_regI
	ELSIF x.mode = mode_regI THEN
		EmitRM (1, MOVr, x.r, 8, x.r, SHORT(x.a)); x.a := 0
	ELSIF x.mode = mode_reg THEN
		x.mode := mode_regI; x.a := 0
	END;
	x.readonly := FALSE; x.type := x.type.base
END Deref;
	
PROCEDURE Open_array_index (VAR x, y : Base.Item);
	VAR	flag : BOOLEAN;
		size, e : INTEGER;

	PROCEDURE Calculate_offset (r : UBYTE; typ : Base.Type; loff : INTEGER);
		VAR size, e : INTEGER;
			len : UBYTE;
	BEGIN
		len := Alloc_reg();
		EmitRM (1, MOVr, len, 8, reg_BP, loff);
		EmitRR (IMULr, r, 8, len);
		Free_reg;
		
		size := typ.base.size;
		IF size > 0 THEN
			e := Base.log2 (size);
			IF e > 0 THEN EmitRI8 (SHLi, r, 8, e)
			ELSIF e < 0 THEN EmitRI (IMULi, r, 8, size)
			END
		ELSIF typ.base.form = Base.type_array THEN
			Calculate_offset (r, typ.base, loff + 8)
		END
	END Calculate_offset;
		
	PROCEDURE Check_array_index (VAR x, y : Base.Item);
		VAR len : UBYTE;
	BEGIN
		IF array_check IN Base.compiler_flag THEN
			len := Alloc_reg();
			EmitRM (1, MOVr, len, 8, reg_BP, x.b);
			IF y.mode = mode_imm THEN EmitRI (CMPi, len, 8, y.a)
			ELSIF y.mode = mode_reg THEN EmitRR (CMPr, len, 8, y.r)
			END;
			Trap (ccBE, array_check); Free_reg
		END
	END Check_array_index;

BEGIN (* Open_array_index *)
	flag := FALSE;
	IF y.mode # mode_imm THEN load (y); flag := TRUE
	ELSIF y.a < 0 THEN Scanner.Mark ('Array index out of bound')
	ELSIF y.a > 0 THEN flag := TRUE
	END;
	
	IF flag THEN
		Check_array_index (x, y); size := x.type.base.size;
		IF size > 0 THEN
			IF y.mode = mode_imm THEN
				IF x.mode = Base.class_ref THEN INC (x.c, SHORT(y.a) * size)
				ELSIF x.mode = mode_regI THEN INC (x.a, y.a * size)
				END
			ELSIF y.mode = mode_reg THEN
				e := Base.log2 (size);
				IF e > 0 THEN EmitRI8 (SHLi, y.r, 8, e)
				ELSIF e < 0 THEN EmitRI (IMULi, y.r, 8, size)
				END;
				
				IF x.mode = Base.class_ref THEN
					Ref_to_regI (x); EmitRR (ADDr, y.r, 8, x.r); x.r := y.r
				ELSIF x.mode = Base.mode_regI THEN EmitRR (ADDr, x.r, 8, y.r)
				END;
				Free_reg
			END
		ELSIF x.type.base.form = Base.type_array THEN
			load (y); Calculate_offset (y.r, x.type.base, x.b + 8);
			IF x.mode = Base.class_ref THEN
				Ref_to_regI (x); EmitRR (ADDr, y.r, 8, x.r); x.r := y.r
			ELSIF x.mode = mode_regI THEN EmitRR (ADDr, x.r, 8, y.r)
			END;
			Free_reg
		END
	END;
	INC (x.b, 8)
END Open_array_index;
	
PROCEDURE Index* (VAR x, y : Base.Item);
	VAR e, size : INTEGER;
BEGIN
	IF x.type.len < 0 THEN
		IF x.b = 0 THEN x.b := SHORT (x.a) + 8 END;
		Open_array_index (x, y)
	ELSE
		size := x.type.base.size;
		IF y.mode = Base.class_const THEN
			IF (y.a >= 0) & (y.a < x.type.len) THEN
				IF x.mode IN {Base.class_var, mode_regI} THEN
					INC (x.a, y.a * size)
				ELSIF x.mode = Base.class_ref THEN INC (x.c, SHORT(y.a) * size)
				END
			ELSE Scanner.Mark ('Array index out of bound')
			END
		ELSE
			load (y);
			IF array_check IN Base.compiler_flag THEN
				EmitRI (CMPi, y.r, 8, x.type.len);
				Trap (ccAE, array_check)
			END;
				
			e := Base.log2 (size);
			IF e > 0 THEN EmitRI8 (SHLi, y.r, 8, e)
			ELSIF e < 0 THEN EmitRI (IMULi, y.r, 8, size)
			END;
			
			IF x.mode = Base.class_var THEN
				Load_adr (x); EmitRR (ADDr, y.r, 8, x.r);
				x.r := y.r; x.a := 0; x.mode := mode_regI
			ELSIF x.mode = Base.class_ref THEN
				Ref_to_regI (x); EmitRR (ADDr, y.r, 8, x.r); x.r := y.r
			ELSIF x.mode = Base.mode_regI THEN
				EmitRR (ADDr, x.r, 8, y.r)
			END;
			Free_reg
		END
	END;
	x.type := x.type.base
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Save_reg_stacks (VAR pinfo : ProcInfo);
	VAR i : INTEGER;
BEGIN
	pinfo.oldrs := crs;
	pinfo.oldi := reg_stacks [crs].i;
	FOR i := 0 TO reg_stacks [crs].i - 1 DO
		PushR (reg_stacks [crs].ord [i])
	END;
	crs := 1; reg_stacks [1].i := 0
END Save_reg_stacks;

PROCEDURE Restore_reg_stacks (VAR pinfo : ProcInfo);
	VAR r : UBYTE; i : INTEGER;
BEGIN
	crs := pinfo.oldrs;
	reg_stacks [crs].i := pinfo.oldi;
	FOR i := reg_stacks [crs].i - 1 TO 0 BY -1 DO
		r := reg_stacks [crs].ord [i];
		IF (r = reg_A) & pinfo.has_retval THEN
			r := Alloc_reg();
			EmitRR (MOVr, r, 8, reg_A)
		END;
		PopR (r)
	END
END Restore_reg_stacks;
	
PROCEDURE Prepare_to_call* (VAR x : Base.Item; VAR pinfo : ProcInfo);
BEGIN
	IF x.mode = Base.class_proc THEN
		pinfo.has_retval := x.type # NIL;
		pinfo.parblksize := x.obj.parblksize;
		pinfo.procvar := FALSE
	ELSE
		load (x); PushR (x.r); Free_reg;
		pinfo.parblksize := x.type.len;
		pinfo.has_retval := x.type.base # NIL;
		pinfo.procvar := TRUE
	END;
	
	Save_reg_stacks (pinfo);
	
	IF pinfo.parblksize < 32 THEN pinfo.parblksize := 32 END;
	INC (ProcState.memstack, pinfo.parblksize);
	IF ProcState.memstack MOD 16 # 0 THEN
		INC (pinfo.parblksize, 8); INC (ProcState.memstack, 8)
	END;
	EmitRI (SUBi, reg_SP, 8, pinfo.parblksize);
	
	pinfo.paradr := 0
END Prepare_to_call;
	
PROCEDURE Call* (VAR x : Base.Item; VAR pinfo : ProcInfo);
	VAR n : INTEGER;
BEGIN
	IF ~ pinfo.procvar THEN EmitCall (SHORT(x.a)); n := 0
	ELSE EmitCallM (reg_SP, pinfo.parblksize); n := 8
	END;
	
	(* Release the stack area used for parameters and proc address *)
	EmitRI (ADDi, reg_SP, 8, pinfo.parblksize + n);
	DEC (pinfo.memstack, pinfo.parblksize + n);
	
	Restore_reg_stacks (pinfo);
	
	(* Return value *)
	IF pinfo.has_retval THEN
		IF pinfo.procvar THEN x.type := x.type.base END;
		x.r := Reg_stack(-1); x.mode := mode_reg
	END
END Call;
	
PROCEDURE Normal_parameter* (VAR x : Base.Item; VAR pinfo : ProcInfo);
BEGIN
	load (x);
	IF pinfo.paradr >= 32 THEN EmitRM (0, MOVr, x.r, 8, reg_SP, pinfo.paradr)
	END;
	INC (pinfo.paradr, 8)
END Normal_parameter;
	
PROCEDURE Reference_parameter* (VAR x : Base.Item; VAR pinfo : ProcInfo);
BEGIN
	Load_adr (x);
	IF pinfo.paradr >= 32 THEN EmitRM (0, MOVr, x.r, 8, reg_SP, pinfo.paradr)
	END;
	INC (pinfo.paradr, 8)
END Reference_parameter;
	
PROCEDURE Record_var_parameter* (VAR x : Base.Item; VAR pinfo : ProcInfo);
	VAR tag : Base.Item;
BEGIN
	IF x.param & ~ x.readonly THEN
		tag.mode := Base.class_ref; tag.lev := x.lev; tag.a := x.a + 8
	ELSE
		(* Make_type_desc_item (tag, x.type) *)
	END;
	Reference_parameter (x, pinfo);
	Reference_parameter (tag, pinfo)
END Record_var_parameter;
	
PROCEDURE Open_array_parameter*
(VAR x : Base.Item; VAR pinfo : ProcInfo; ftype : Base.Type);
	VAR
		array, len : Base.Item;
		tp : Base.Type;
BEGIN
	array := x; Reference_parameter (array, pinfo);
	
	tp := x.type;
	
	IF x.b = 0 THEN x.b := SHORT(x.a) + 8 END; (* For open array *)
	
	WHILE (ftype.form = Base.type_array) & (ftype.len < 0) DO
		IF tp.len < 0 THEN
			len.mode := Base.class_var;
			len.lev := x.lev;
			len.a := x.b;
			INC (x.b, 8)
		ELSE Make_const (len, Base.int_type, tp.len)
		END;
		Normal_parameter (len, pinfo);		
		ftype := ftype.base; tp := tp.base
	END
END Open_array_parameter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FJump* (VAR L : INTEGER);
BEGIN
	Branch (L);
	L := pc - 1
END FJump;

PROCEDURE CFJump* (VAR x : Base.Item);
	VAR cond : INTEGER;
BEGIN
	IF x.mode # Base.mode_cond THEN Load_cond (x) END;
    cond := negated (x.c);
	IF cond < 16 THEN CondBranch (cond, SHORT (x.a)); x.a := pc - 1
	ELSIF cond = ccAlways THEN Branch (SHORT (x.a)); x.a := pc - 1
	END;
	Fix_link (x.b)
END CFJump;
	
PROCEDURE CBJump* (VAR x : Base.Item; L : INTEGER);
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

PROCEDURE Fixup* (x : Base.Item);
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
		IF if_fix1 IN codeinfo [i].flag THEN Fixup_disp (i) END;
		FOR j := 0 TO codeinfo [i].size - 1 DO
			Sys.Write_byte (out, code [i, j])
		END
	END
END Write_to_file;

PROCEDURE Write_padding (n : INTEGER);
	VAR i : INTEGER;
BEGIN
	FOR i := 0 TO n-1 DO Sys.Write_byte (out, 90H) END
END Write_padding;

PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
	VAR k : INTEGER;
BEGIN
	k := ip MOD 16; IF k # 0 THEN INC (ip, 16 - k); Write_padding (16 - k) END;
	
	IF proc # NIL THEN
		ProcState.locblksize := locblksize;
		ProcState.parlist := proc.dsc;
		proc.val := ip
	ELSE
		entry := ip;
		ProcState.locblksize := 0;
		ProcState.parlist := NIL
	END;
	ProcState.memstack := 0;
	ProcState.usedRegs := {};
	ProcState.adr := ip;
	Reset_reg_stack; pc := 1
END Enter;
	
PROCEDURE Return*;
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
	IF ProcState.locblksize > 0 THEN
		EmitRI (SUBi, reg_SP, 8, ProcState.locblksize)
	END;
	
	IF reg_DI IN ProcState.usedRegs THEN PushR (reg_DI); INC (nRegs) END;
	IF reg_SI IN ProcState.usedRegs THEN PushR (reg_SI); INC (nRegs) END;
	IF reg_R12 IN ProcState.usedRegs THEN PushR (reg_R12); INC (nRegs) END;
	IF reg_R13 IN ProcState.usedRegs THEN PushR (reg_R13); INC (nRegs) END;
	IF reg_R14 IN ProcState.usedRegs THEN PushR (reg_R14); INC (nRegs) END;
	IF reg_R15 IN ProcState.usedRegs THEN PushR (reg_R15); INC (nRegs) END;
	
	IF ProcState.parlist # NIL THEN
		paramRegs [0] := reg_C; paramRegs [1] := reg_D;
		paramRegs [2] := reg_R8; paramRegs [3] := reg_R9;
		obj := ProcState.parlist; i := 0; k := 0;
		WHILE obj # Base.guard DO
			IF k = 0 THEN k := Param_size (obj) END;
			EmitRM (0, MOVr, paramRegs [i], 8, reg_BP, 16 + i * 8);
			INC (i); DEC (k, 8);
			IF k = 0 THEN obj := obj.next END
		END
	END;
	
	ProcState.prolog_size := ip - endIP;
	Write_to_file (endPC, pc - 1);
	Write_to_file (1, endPC - 1)
END Return;

PROCEDURE Module_exit*;
BEGIN
	EmitRI (SUBi, reg_SP, 8, 32);
	EmitRR (XORr, reg_C, 4, reg_C);
	EmitCallSv (-56);
END Module_exit;

PROCEDURE Set_varsize* (n : INTEGER);
BEGIN
	varsize := n;
	staticbase := -varsize
END Set_varsize;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Linker *)

PROCEDURE Init* (modname : Base.String);
BEGIN
	ip := 0; varbase := 0; staticsize := 56;
	Sys.Rewrite (out, 'output.exe');
	Sys.Seek (out, 400H)
END Init;

PROCEDURE Write_idata_section (
	idata_fadr, idata_rva, data_rva, data_size : INTEGER
);
	CONST
		table_len = 7;
		table_size = table_len * 8;
		table_rva = 40;
		name_rva = table_rva + table_size;
		hint_rva = name_rva + 16;
		
	VAR i, j : INTEGER; b : UBYTE;
		kernel32table : ARRAY table_len OF LONGINT;
BEGIN
	Sys.Seek (out, idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Sys.Write_4bytes (out, idata_rva + table_rva);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, idata_rva + name_rva);
	Sys.Write_4bytes (out, data_rva + data_size + staticbase - table_size);

	Sys.Seek (out, idata_fadr + table_rva);
	FOR i := 0 TO 5 DO
		kernel32table [i] := idata_rva + hint_rva + 32 * i
	END;
	kernel32table [6] := 0;
	i := 0; j := LEN(staticdata) - table_size;
	WHILE i < table_size DO
		SYSTEM.GET (SYSTEM.ADR(kernel32table[0]) + i, b);
		staticdata [j] := b; Sys.Write_byte (out, b); INC (i); INC (j)
	END;
	
	Sys.Seek (out, idata_fadr + name_rva);
	Sys.Write_ansi_str (out, 'KERNEL32.DLL');
	
	Sys.Seek (out, idata_fadr + hint_rva + 2);
	Sys.Write_ansi_str (out, 'ExitProcess');
	Sys.Seek (out, idata_fadr + hint_rva + (32 + 2));
	Sys.Write_ansi_str (out, 'LoadLibraryW');
	Sys.Seek (out, idata_fadr + hint_rva + (64 + 2));
	Sys.Write_ansi_str (out, 'GetProcAddress');
	Sys.Seek (out, idata_fadr + hint_rva + (96 + 2));
	Sys.Write_ansi_str (out, 'GetProcessHeap');
	Sys.Seek (out, idata_fadr + hint_rva + (128 + 2));
	Sys.Write_ansi_str (out, 'HeapAlloc');
	Sys.Seek (out, idata_fadr + hint_rva + (160 + 2));
	Sys.Write_ansi_str (out, 'HeapFree')
END Write_idata_section;


PROCEDURE Write_reloc_section (data_rva : INTEGER);
BEGIN
	Sys.Write_4bytes (out, 4);
	Sys.Write_4bytes (out, 12);
	Sys.Write_2bytes (out, 0);
	Sys.Write_2bytes (out, 0)
END Write_reloc_section;


PROCEDURE Write_SectionHeader (
	name : ARRAY OF CHAR;
	chr, rva, rawsize, size, fileadr : INTEGER
);	
	VAR
		b : UBYTE; i : INTEGER;	
BEGIN
	FOR i := 0 TO 7 DO
		b := 0; IF i < LEN(name) THEN b := USHORT (ORD (name[i])) END;
		Sys.Write_byte (out, b)
	END;
	Sys.Write_4bytes (out, size);
	Sys.Write_4bytes (out, rva);
	Sys.Write_4bytes (out, rawsize);
	Sys.Write_4bytes (out, fileadr);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, chr)
END Write_SectionHeader;

PROCEDURE Write_PEHeader (
	imagebase : LONGINT;
	code_rva, code_size, code_rawsize, code_fadr,
	data_rva, data_size, data_rawsize, data_fadr,
	idata_rva, idata_fadr : INTEGER
);
	VAR k : INTEGER;
BEGIN
	Sys.Seek (out, 0);
	Sys.Write_2bytes (out, 5A4DH);
	Sys.Seek (out, 60);
	Sys.Write_4bytes (out, 128);
	Sys.Seek (out, 128);
	Sys.Write_4bytes (out, 4550H);
	
	Sys.Write_2bytes (out, 8664H); (* Machine = AMD64/Intel 64 *)
	Sys.Write_2bytes (out, 3); (* NumberOfSections *)
	Sys.SeekRel (out, 4 * 3);
	Sys.Write_2bytes (out, 240);
	Sys.Write_2bytes (out, 20H + 2 + 1);
	
	Sys.Write_2bytes (out, 20BH);
	Sys.SeekRel (out, 2);
	Sys.Write_4bytes (out, code_rawsize);
	Sys.Write_4bytes (out, data_rawsize + 200H);
	Sys.SeekRel (out, 4);
	Sys.Write_4bytes (out, code_rva + entry);
	Sys.Write_4bytes (out, code_rva);
	
	Sys.Write_8bytes (out, imagebase);
	Sys.Write_4bytes (out, 4096);
	Sys.Write_4bytes (out, 512);
	Sys.Write_2bytes (out, 1); (* MajorOSVer *)
	Sys.SeekRel (out, 2 * 3);
	Sys.Write_2bytes (out, 5);
	Sys.SeekRel (out, 2 + 4);
	k := (4096 - code_size) MOD 4096 + code_size + data_size + 4096 * 2;
	Sys.Write_4bytes (out, k);
	Sys.Write_4bytes (out, 400H);
	Sys.SeekRel (out, 4);
	Sys.Write_2bytes (out, 2); (* Subsys = GUI *)
	Sys.SeekRel (out, 2);
	Sys.Write_8bytes (out, 1000H);
	Sys.Write_8bytes (out, 1000H);
	Sys.Write_8bytes (out, 10000H);
	Sys.SeekRel (out, 8 + 4);
	Sys.Write_4bytes (out, 16);
	
	Sys.SeekRel (out, 8);
	Sys.Write_4bytes (out, idata_rva);
	Sys.Write_4bytes (out, 130H);
	Sys.SeekRel (out, 8 * 14);
	
	Write_SectionHeader
		('.data', -1073741760, data_rva, data_rawsize, data_size, data_fadr);
	Write_SectionHeader
		('.text', 60000020H, code_rva, code_rawsize, code_size, code_fadr);
	Write_SectionHeader
		('.idata', -1073741760, idata_rva, 200H, 130H, idata_fadr);
	(*
	Write_SectionHeader
		('.reloc', 42000040H, idata_rva + 4096, 200H, 4096, idata_fadr + 200H)
	*)
END Write_PEHeader;

PROCEDURE Finish*;
	VAR code_rawsize, data_rawsize, idata_rawsize : INTEGER;
		code_size, data_size, idata_size : INTEGER;
		code_rva, data_rva, idata_rva : INTEGER;
		code_fadr, data_fadr, idata_fadr, reloc_fadr : INTEGER;
		i, padding : INTEGER; k, imagebase : LONGINT;
BEGIN
	imagebase := 400000H;

	code_rawsize := (512 - ip) MOD 512 + ip;
	code_size := ip;
	
	data_size := staticsize + varsize;
	data_size := (4096 - data_size) MOD 4096 + data_size;
	padding := data_size - staticsize - varsize;
	data_rawsize := padding + staticsize;
	data_rawsize := (512 - data_rawsize) MOD 512 + data_rawsize;
	
	data_rva := 1000H;
	code_rva := data_rva + (4096 - data_size) MOD 4096 + data_size;
	idata_rva := code_rva + (4096 - code_size) MOD 4096 + code_size;
	
	code_fadr := 400H;
	data_fadr := code_fadr + code_rawsize;
	idata_fadr := data_fadr + data_rawsize;
	reloc_fadr := idata_fadr + 200H;
	
	Write_idata_section (idata_fadr, idata_rva, data_rva, data_size);
	
	Sys.Seek (out, data_fadr + padding);
	i := LEN(staticdata) - staticsize;
	WHILE i < LEN(staticdata) DO
		Sys.Write_byte (out, staticdata [i]); INC (i)
	END;
	
	Sys.Seek (out, reloc_fadr);
	Write_reloc_section (data_rva);
	Sys.Seek (out, reloc_fadr + 200H);
	
	Write_PEHeader (imagebase,
		code_rva, code_size, code_rawsize, code_fadr,
		data_rva, data_size, data_rawsize, data_fadr,
		idata_rva, idata_fadr
	);

	Sys.Close (out);
	
	Sys.Console_WriteString ('Code size: ');
	Sys.Console_WriteInt (code_size); Sys.Console_WriteLn;
	Sys.Console_WriteString ('Global variables size: ');
	Sys.Console_WriteInt (varsize); Sys.Console_WriteLn;
	Sys.Console_WriteString ('Static data size: ');
	Sys.Console_WriteInt (staticsize); Sys.Console_WriteLn
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
	reg_stacks [0].ord [10] := reg_R15;
	
	reg_stacks [1].i := 0;
	reg_stacks [1].n := 11;
	reg_stacks [1].ord [0] := reg_C;
	reg_stacks [1].ord [1] := reg_D;
	reg_stacks [1].ord [2] := reg_R8;
	reg_stacks [1].ord [3] := reg_R9;
	reg_stacks [1].ord [4] := reg_A;
	reg_stacks [1].ord [5] := reg_R10;
	reg_stacks [1].ord [6] := reg_R11;
	reg_stacks [1].ord [7] := reg_R12;
	reg_stacks [1].ord [8] := reg_R13;
	reg_stacks [1].ord [9] := reg_R14;
	reg_stacks [1].ord [10] := reg_R15
END GeneratorWin64v2.