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

	reg_A = 0; reg_C = 1; reg_D = 2; reg_B = 3;
	reg_SP = 4; reg_BP = 5; reg_SI = 6; reg_DI = 7;
	reg_R8 = 8; reg_R9 = 9; reg_R10 = 10; reg_R11 = 11;
	reg_R12 = 12; reg_R13 = 13; reg_R14 = 14; reg_R15 = 15;
	
	ccO = 0; ccNO = 1; ccB = 2; ccAE = 3; ccZ = 4; ccNZ = 5; ccBE = 6; ccA = 7;
	ccS = 8; ccNS = 9; ccP = 10; ccNP = 11; ccL = 12; ccGE = 13; ccLE = 14;
	ccG = 15; ccAlways = 16; ccNever = 17;
	
	(* Opcodes used with EmitRegRm *)
	ADD = 00H; ADDd = 02H; AND = 20H; ANDd = 22H; XOR = 30H; XORd = 32H;
	TEST = 84H; XCHG = 86H; 
	_OR = 08H; ORd = 0AH; SUB = 28H; SUBd = 2AH; CMP = 38H; CMPd = 3AH;
	MOV = 88H; MOVd = 8AH; LEA = 8DH;
	BT = 0A30FH; BTR = 0B30FH; MOVZX = 0B60FH;
	BTS = 0AB0FH; IMUL = 0AF0FH;
	
	(* Opcodes used with EmitRm *)
	POP = 8FH; ROR1 = 1D0H; RORcl = 1D2H; SHL1 = 4D0H; SHLcl = 4D2H;
	SHR1 = 5D0H; SHRcl = 5D2H; SAR1 = 7D0H; SARcl = 7D2H;
	NOT = 2F6H; NEG = 3F6H; IDIVa = 7F7H; _INC = 0FEH; _DEC = 1FEH;
	CALL = 2FFH; JMP = 4FFH; PUSH = 6FFH;
	
	(* Opcodes used with EmitRmImm *)
	ADDi = 80H; ORi = 180H; ANDi = 480H; SUBi = 580H; XORi = 680H; CMPi = 780H;
	RORi = 1C0H; SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H; MOVi = 0C6H; TESTi = 76H;
	BTi = 4BA0FH; BTSi = 5BA0FH; BTRi = 6BA0FH;
	IMULi = 69H (* Special case *);
	
	(* Opcodes used with EmitBare *)
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H;
	
	(* REP instructions *)
	MOVSrep = 0A4H;
	
TYPE
	InstructionInfo = RECORD
		relfixup : BOOLEAN;
		size, dispPos : UBYTE;
		ip : INTEGER
	END;

	RegStack = RECORD
		ord : ARRAY 16 OF INTEGER;
		i, n : INTEGER
	END;
	
	ProcInfo* = RECORD
		oldi, oldrs : INTEGER; oldcurRegs : SET;
		parblksize*, memstack, paradr : INTEGER;
		rtype* : Base.Type;
	END;
	
VAR
	modid : Base.String;
	out, fixupFile : Sys.FileHandle;

	code : ARRAY 100000H OF UBYTE;
	codeinfo : ARRAY 20000H OF InstructionInfo;
	ip*, pc* : INTEGER;
	
	(* Global variables for code emitter *)
	Emit : RECORD
		i, oldi : INTEGER;
		mem : RECORD
			mod : INTEGER;
			rm, bas, idx, scl : INTEGER;
			disp : INTEGER
		END
	END;
	
	staticlist : Base.Type;
	varbase, staticbase, varsize, staticsize : INTEGER;
	
	(* Reg stack *)
	reg_stacks : ARRAY 2 OF RegStack;
	curRegs : SET; crs : INTEGER;
	
	ProcState : RECORD
		adr, prolog_size, locblksize, memstack : INTEGER;
		parlist : Base.Object;
		usedRegs : SET
	END;
	
	(* Global variables for linker *)
	Linker : RECORD
		imagebase : LONGINT; entry : INTEGER;
		code_rawsize, data_rawsize, edata_rawsize : INTEGER;
		code_size, data_size, edata_size : INTEGER;
		code_rva, data_rva, idata_rva, reloc_rva, edata_rva : INTEGER;
		code_fadr, data_fadr, idata_fadr, reloc_fadr, edata_fadr : INTEGER
	END;
	Kernel32Table : ARRAY 7 OF LONGINT;

	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)

PROCEDURE Reset_code_buffer;
BEGIN
	Emit.i := 0; Emit.oldi := 0; pc := 1; codeinfo [pc].relfixup := FALSE
END Reset_code_buffer;

PROCEDURE Put_byte (n : INTEGER);
BEGIN
	code [Emit.i] := USHORT (n MOD 256); INC (Emit.i)
END Put_byte;

PROCEDURE Put_2bytes (n : INTEGER);
BEGIN
	code [Emit.i] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 1] := USHORT (n MOD 256); INC (Emit.i, 2)
END Put_2bytes;

PROCEDURE Put_4bytes (n : INTEGER);
BEGIN
	code [Emit.i] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 1] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 2] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 3] := USHORT (n MOD 256); INC (Emit.i, 4)
END Put_4bytes;

PROCEDURE Put_8bytes (n : LONGINT);
	VAR k : INTEGER;
BEGIN
	code [Emit.i] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 1] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 2] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 3] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 4] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 5] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 6] := USHORT (n MOD 256); n := n DIV 256;
	code [Emit.i + 7] := USHORT (n MOD 256); INC (Emit.i, 8)
END Put_8bytes;
	
PROCEDURE Emit_REX_prefix (reg, rsize : INTEGER);
	CONST W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1;
	VAR rex : INTEGER;
BEGIN
	rex := 40H;
	IF rsize = 8 THEN rex := rex + W_bit END;
	IF reg >= reg_R8 THEN rex := rex + R_bit END;
	IF (Emit.mem.rm >= reg_R8) OR (Emit.mem.mod # 3) & (Emit.mem.rm = reg_SP)
		& (Emit.mem.bas >= reg_R8)
	THEN rex := rex + B_bit
	END;
	IF (Emit.mem.mod # 3) & (Emit.mem.rm = reg_SP) & (Emit.mem.idx >= reg_R8) THEN
		rex := rex + X_bit
	END;
	IF (rex # 40H) OR (rsize = 1) & ((reg IN {reg_SP .. reg_DI})
		OR (Emit.mem.mod = 3) & (Emit.mem.rm IN {reg_SP .. reg_DI}))
	THEN Put_byte (rex)
	END
END Emit_REX_prefix;

PROCEDURE Emit_16bit_prefix (rsize : INTEGER);
BEGIN
	IF rsize = 2 THEN Put_byte (66H) END
END Emit_16bit_prefix;

PROCEDURE Handle_multibytes_opcode (VAR op : INTEGER);
BEGIN
	IF op MOD 256 # 0FH THEN (* One byte opcode *)
	ELSE Put_byte (0FH); op := op DIV 256;
		IF (op MOD 256 = 038H) OR (op MOD 256 = 03AH) THEN
			Put_byte (op); op := op DIV 256
		END
	END
END Handle_multibytes_opcode;

PROCEDURE Emit_ModRM (reg : INTEGER);
	VAR b : INTEGER;
BEGIN
	Put_byte (Emit.mem.mod * 64 + reg MOD 8 * 8 + Emit.mem.rm MOD 8);
	IF Emit.mem.mod # 3 THEN
		IF Emit.mem.rm IN {reg_SP, reg_R12} THEN
			Put_byte (Emit.mem.scl * 64 + Emit.mem.idx MOD 8 * 8 + Emit.mem.bas MOD 8)
		END;
		codeinfo [pc].dispPos := USHORT (Emit.i - Emit.oldi);
		IF (Emit.mem.mod = 0) & (Emit.mem.rm IN {reg_BP, reg_R13}) OR (Emit.mem.mod = 2)
		THEN Put_4bytes (Emit.mem.disp)
		ELSIF Emit.mem.mod = 1 THEN Put_byte (Emit.mem.disp)
		END
	END
END Emit_ModRM;

PROCEDURE Next_inst;
BEGIN
	codeinfo [pc].ip := ip; codeinfo [pc].size := USHORT (Emit.i - Emit.oldi);
	ip := ip + Emit.i - Emit.oldi; INC (pc);
	Emit.oldi := Emit.i; codeinfo [pc].relfixup := FALSE
END Next_inst;

(* -------------------------------------------------------------------------- *)
	
PROCEDURE EmitRegRm (op, reg, rsize : INTEGER);
	CONST w_bit = 1;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (reg, rsize);
	Handle_multibytes_opcode (op);
	
	IF (rsize > 1) & ((op < LEA) OR (op = MOVZX) OR (op = IMUL)) THEN
		op := op + w_bit
	END;
	Put_byte (op); Emit_ModRM (reg);
	
	Next_inst
END EmitRegRm;

PROCEDURE EmitRm (op, rsize : INTEGER);
	CONST w_bit = 1;
	VAR op3bits : INTEGER;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	Handle_multibytes_opcode (op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN
		op := op + w_bit
	END;
	Put_byte (op); Emit_ModRM (op3bits);
	
	Next_inst
END EmitRm;

PROCEDURE EmitRmImm (op, rsize, imm : INTEGER);
	CONST w_bit = 1; s_bit = 2;
	VAR op3bits : INTEGER;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	Handle_multibytes_opcode (op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF rsize > 1 THEN
		IF (op = 0C0H) OR (op = 0BAH) THEN rsize := 1
		ELSIF (imm >= MIN_INT8) & (imm <= MAX_INT8) & (op = 80H) THEN
			op := op + s_bit; rsize := 1
		END;
		IF BITS(op) * BITS(w_bit) = {} THEN op := op + w_bit END
	END;
	Put_byte (op); Emit_ModRM (op3bits);
	
	IF rsize = 1 THEN Put_byte (imm) ELSIF rsize = 2 THEN Put_2bytes (imm)
	ELSE Put_4bytes (imm)
	END;
	
	Next_inst
END EmitRmImm;

PROCEDURE EmitBare (op : INTEGER);
BEGIN
	WHILE op > 0 DO	Put_byte (op); op := op DIV 256 END;
	Next_inst
END EmitBare;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetRmOperand_reg (reg : INTEGER);
BEGIN
	Emit.mem.rm := reg; Emit.mem.mod := 3
END SetRmOperand_reg;

PROCEDURE SetRmOperand_regI (reg, disp : INTEGER);
BEGIN
	Emit.mem.rm := reg; Emit.mem.disp := disp;
	IF (disp >= MIN_INT8) & (disp <= MAX_INT8) THEN
		IF (disp = 0) & ~ (reg IN {reg_BP, reg_R13}) THEN Emit.mem.mod := 0
		ELSE Emit.mem.mod := 1
		END
	ELSE Emit.mem.mod := 2
	END;
	IF reg IN {reg_SP, reg_R12} THEN
		Emit.mem.bas := reg_SP; Emit.mem.idx := reg_SP; Emit.mem.scl := 0
	END
END SetRmOperand_regI;

PROCEDURE SetRmOperand_staticvar (adr : INTEGER);
BEGIN
	Emit.mem.rm := reg_BP; Emit.mem.disp := adr + staticbase;
	Emit.mem.mod := 0; codeinfo [pc].relfixup := TRUE
END SetRmOperand_staticvar;

PROCEDURE SetRmOperand (x : Base.Item);
BEGIN
	IF x.mode IN {Base.class_var, Base.class_ref} THEN
		Emit.mem.rm := reg_BP; Emit.mem.disp := SHORT (x.a);
		IF x.lev > 0 THEN
			IF (x.a >= MIN_INT8) OR (x.a <= MAX_INT8) THEN Emit.mem.mod := 1
			ELSE Emit.mem.mod := 2
			END
		ELSE
			Emit.mem.mod := 0; codeinfo [pc].relfixup := TRUE;
			IF x.lev = 0 THEN Emit.mem.disp := Emit.mem.disp + varbase
			ELSE Emit.mem.disp := Emit.mem.disp + staticbase
			END
		END
	ELSIF x.mode = mode_regI THEN SetRmOperand_regI (x.r, SHORT (x.a))
	ELSIF x.mode = mode_reg THEN SetRmOperand_reg (x.r)
	ELSIF x.mode = Base.class_proc THEN
		Emit.mem.rm := reg_BP; Emit.mem.disp := SHORT (x.a);
		Emit.mem.mod := 0; codeinfo [pc].relfixup := TRUE;
		IF x.lev < 0 THEN Emit.mem.disp := Emit.mem.disp + staticbase END
	END
END SetRmOperand;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRR (op, reg, rsize, rm : INTEGER);
BEGIN
	SetRmOperand_reg (rm); EmitRegRm (op, reg, rsize)
END EmitRR;

PROCEDURE EmitRI (op, rm, rsize : INTEGER; imm : LONGINT);
BEGIN
	SetRmOperand_reg (rm);
	IF op # IMULi THEN (* nothing *) ELSE op := op + rm * 256 END;
	EmitRmImm (op, rsize, SHORT (imm))
END EmitRI;

PROCEDURE EmitR (op, rm, rsize : INTEGER);
BEGIN
	SetRmOperand_reg (rm); EmitRm (op, rsize)
END EmitR;

(* -------------------------------------------------------------------------- *)

PROCEDURE MoveRI (rm, rsize : INTEGER; imm : LONGINT);
	CONST w_bit = 8;
	VAR op : INTEGER;
BEGIN
	SetRmOperand_reg (rm); Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	op := 0B0H + rm MOD 8; IF rsize > 1 THEN op := op + w_bit END; Put_byte (op);
	IF rsize = 1 THEN Put_byte (SHORT(imm))
	ELSIF rsize = 2 THEN Put_2bytes (SHORT(imm))
	ELSIF rsize = 4 THEN Put_4bytes (SHORT(imm))
	ELSE Put_8bytes (imm)
	END;
	Next_inst
END MoveRI;

PROCEDURE PushR (rm : INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (50H + rm MOD 8);
	INC (ProcState.memstack, 8);
	Next_inst
END PushR;

PROCEDURE PopR (rm : INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (58H + rm MOD 8);
	INC (ProcState.memstack, 8);
	Next_inst
END PopR;

PROCEDURE Branch (disp : INTEGER);
BEGIN
	Put_byte (0E9H); codeinfo [pc].dispPos := USHORT (Emit.i - Emit.oldi);
	Put_4bytes (disp);
	Next_inst
END Branch;

PROCEDURE CallNear (disp : INTEGER);
BEGIN
	Put_byte (0E8H); codeinfo [pc].dispPos := USHORT (Emit.i - Emit.oldi);
	Put_4bytes (disp); codeinfo [pc].relfixup := TRUE;
	Next_inst
END CallNear;

PROCEDURE CondBranch (cond, disp : INTEGER);
BEGIN
	Put_byte (0FH); Put_byte (80H + cond);
	codeinfo [pc].dispPos := USHORT (Emit.i - Emit.oldi);
	Put_4bytes (disp);
	Next_inst
END CondBranch;

PROCEDURE SetccRm (cond : INTEGER);
BEGIN
	Emit_REX_prefix (0, 1);
	Put_byte (0FH); Put_byte (90H + cond); Emit_ModRM (0);
	Next_inst
END SetccRm;

PROCEDURE EmitREPop (op, rsize, z : INTEGER);
	CONST w_bit = 1;
BEGIN
	Put_byte (0F2H + z); (* REP prefix *)
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	IF (rsize > 1) & (BITS(op) * BITS(w_bit) = {}) THEN INC (op, w_bit) END;
	Put_byte (op);
	Next_inst
END EmitREPop;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Item constructor *)

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

PROCEDURE Alloc_static_data (size, alignment : INTEGER);
BEGIN
	IF staticsize + size <= max_staticsize THEN
		staticsize := staticsize + size;
		Base.Adjust_alignment (staticsize, alignment)
	ELSE Scanner.Mark ('The limit for static data is reached.')
	END
END Alloc_static_data;
	
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
	
	IF x.lev # -2 THEN (* Not imported object *)
	ELSE
		IF x.mode = Base.class_var THEN x.mode := Base.class_ref END;
		IF x.a # 0 THEN (* Already inited *)
		ELSE
			Alloc_static_data (8, 8); x.a := -staticsize; obj.val := x.a;
			IF (x.mode = Base.class_type)
			& (x.type.form = Base.type_record) THEN
				x.type.tdAdr := SHORT (x.a);
				x.type.import := TRUE
			END
		END
	END
END Make_item;
	
PROCEDURE Make_string* (VAR x : Base.Item);
	VAR strlen, i : INTEGER;
BEGIN
	x.readonly := TRUE; x.param := FALSE;
	x.mode := Base.class_var; x.lev := -1;
	x.b := ORD (Scanner.str[0]);
	
	strlen := Base.Str_len (Scanner.str) + 1;
	Base.New_typ (x.type, Base.type_string);
	x.type.len := strlen;
	x.type.size := strlen * Base.Char_size;
	x.type.base := Base.char_type;
	
	Alloc_static_data (strlen * Base.Char_size, Base.Char_size);
	x.type.tdAdr := -staticsize; x.a := -staticsize;
	
	i := Base.Alloc_string (Scanner.str, strlen);
	IF i >= 0 THEN x.type.ref := i;
	ELSE Scanner.Mark ('Compiler buffer for string is overflowed!');
		x.type.ref := 0
	END;
	
	IF staticlist # NIL THEN x.type.next := staticlist END;
	staticlist := x.type
END Make_string;

PROCEDURE Alloc_typedesc* (type : Base.Type);
	VAR tdsize, i, n : INTEGER;
BEGIN
	tdsize := 8 + 8 * (Base.max_extension - 1) + 4 * (type.num_ptr + 1);
	Base.Adjust_alignment (tdsize, 8);
	Alloc_static_data (staticsize, tdsize);
	type.tdAdr := -staticsize
END Alloc_typedesc;

PROCEDURE Get_typedesc (VAR x : Base.Item; type : Base.Type);
BEGIN
	ASSERT (type.tdAdr # 0);
	IF ~ type.import THEN x.mode := Base.class_var; x.lev := -1
	ELSE x.mode := Base.class_ref; x.lev := -2; x.c := 0
	END;
	x.type := Base.int_type; x.a := type.tdAdr
END Get_typedesc;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Branch Fixup *)

PROCEDURE Fix_link_with (L : INTEGER; dst : INTEGER);
	VAR i, dispPos, size, n : INTEGER;
BEGIN
	WHILE L # 0 DO
		size := codeinfo [L].size; dispPos := codeinfo [L].dispPos;
		i := codeinfo [L].ip;
		n := dst - i - size;
		i := i - ProcState.adr + dispPos;
		SYSTEM.GET (SYSTEM.ADR (code [i]), L);
		SYSTEM.PUT (SYSTEM.ADR (code [i]), n)
	END
END Fix_link_with;

PROCEDURE Fix_link* (L : INTEGER);
BEGIN
	Fix_link_with (L, ip)
END Fix_link;

PROCEDURE merged (L0, L1: INTEGER) : INTEGER;
	VAR L2, L3, size, i, dispPos : INTEGER;
BEGIN 
	IF L0 # 0 THEN
		L3 := L0;
		REPEAT
			L2 := L3;
			size := codeinfo [L2].size; dispPos := codeinfo [L2].dispPos;
			i := codeinfo [L2].ip - ProcState.adr + dispPos;
			SYSTEM.GET (SYSTEM.ADR (code [i]), L3)
		UNTIL L3 = 0;
		SYSTEM.PUT (SYSTEM.ADR (code [i]), L1);
		L1 := L0
	END;
    RETURN L1
END merged;

PROCEDURE ccCodeOf (op : INTEGER) : INTEGER;
BEGIN
	IF op = Scanner.equal THEN op := ccZ
	ELSIF op = Scanner.not_equal THEN op := ccNZ
	ELSIF op = Scanner.less THEN op := ccL
	ELSIF op = Scanner.greater THEN op := ccG
	ELSIF op = Scanner.less_equal THEN op := ccLE
	ELSE op := ccGE
	END;
	RETURN op
END ccCodeOf;

PROCEDURE Set_cond (VAR x : Base.Item; n : INTEGER);
BEGIN
	x.mode := mode_cond;
	x.a := 0; x.b := 0; x.c := n
END Set_cond;

PROCEDURE negated (cond : INTEGER) : INTEGER;
BEGIN
	IF ODD(cond) THEN DEC (cond) ELSE INC (cond) END;
	RETURN cond
END negated;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Register stack *)

PROCEDURE Reset_reg_stack;
BEGIN
	reg_stacks [0].i := 0; crs := 0
END Reset_reg_stack;

PROCEDURE Reg_stack (offset : INTEGER) : INTEGER;
	VAR i : INTEGER;
BEGIN
	i := reg_stacks [crs].i;
	RETURN reg_stacks [crs].ord [i + offset]
END Reg_stack;

PROCEDURE Alloc_reg() : INTEGER;
	VAR i, r : INTEGER;
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
	VAR i, r : INTEGER;
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
(* Trap *)

(* Need to be replaced *)
PROCEDURE Trap (cond : INTEGER; trapno : UBYTE);
	VAR L : INTEGER;
BEGIN
	(*
	ASSERT (trapno < 16);
	L := pc; CondBranch (cond, 0); Branch (0); Fix_link (L); L := pc - 1;
	SetRmOperand_regI (reg_B, 0); EmitRegRm (MOV, trapno, 4); Fix_link (L)
	*)
END Trap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Load/Store *)

(* For consistency purpose, all value with size
   smaller than 64 bits are zero-extended when load to register *)
   
PROCEDURE Ref_to_regI (VAR x : Base.Item);
BEGIN
	x.r := Alloc_reg(); SetRmOperand (x); EmitRegRm (MOVd, x.r, 8);
	x.a := x.c; x.mode := mode_regI
END Ref_to_regI;

(* Untyped (not safe) load and doesn't modify Item *)
PROCEDURE Load_to_reg (reg, rsize : INTEGER; x : Base.Item);
	VAR op : INTEGER;
BEGIN
	IF x.mode IN Base.classes_Variable THEN
		SetRmOperand (x);
		IF x.mode # Base.class_ref THEN (* nothing *)
		ELSE EmitRegRm (MOVd, reg, 8); SetRmOperand_regI (reg, x.c)
		END;
		IF rsize >= 4 THEN op := MOVd
		ELSIF rsize = 2 THEN op := MOVZX; rsize := 4
		ELSIF rsize = 1 THEN op := MOVZX
		ELSE op := 0; ASSERT(FALSE)
		END;
		EmitRegRm (op, reg, rsize)
	ELSIF x.mode = mode_imm THEN
		IF x.a = 0 THEN EmitRR (XOR, reg, 4, reg)
		ELSIF (rsize <= 4) OR (x.a > 0) & (x.a <= MAX_UINT32) THEN
			MoveRI (reg, 4, x.a)
		ELSE MoveRI (reg, rsize, x.a)
		END
	ELSIF x.mode = Base.class_proc THEN
		SetRmOperand (x);
		IF x.lev >= 0 THEN EmitRegRm (LEA, reg, 8)
		ELSE EmitRegRm (MOVd, reg, 8)
		END
	ELSIF x.mode = mode_reg THEN
		EmitRR (MOVd, reg, rsize, x.r)
	ELSE ASSERT(FALSE)
	END
END Load_to_reg;

(* Typed (safe) load and modify Item *)
PROCEDURE load* (VAR x : Base.Item);
	VAR rsize, L : INTEGER;
BEGIN
	ASSERT (x.mode IN Base.classes_Value);
	ASSERT (x.type.size IN {1, 2, 4, 8});
	IF x.mode # mode_reg THEN
		IF x.mode # mode_cond THEN
			rsize := x.type.size;
			IF x.mode # mode_regI THEN x.r := Alloc_reg() END;
			Load_to_reg (x.r, rsize, x)
		ELSE
			x.r := Alloc_reg();
			IF (x.a = 0) & (x.b = 0) THEN
				IF (x.c < 16) & (x.c >= 0) THEN
					SetRmOperand_reg (x.r); SetccRm (x.c);
					EmitRR (MOVZX, x.r, 1, x.r)
				ELSIF x.c = ccAlways THEN MoveRI (x.r, 4, 1)
				ELSIF x.c = ccNever THEN EmitRR (XOR, x.r, 4, x.r)
				ELSE ASSERT(FALSE)
				END
			ELSE
				L := pc; CondBranch (negated(x.c), 0); Fix_link (x.b);
				MoveRI (x.r, 4, 1);
				Branch (0); Fix_link (L); Fix_link (SHORT (x.a)); L := pc - 1;
				EmitRR (XOR, x.r, 4, x.r); Fix_link (L)
			END
		END;
		x.mode := mode_reg
	END
END load;

PROCEDURE Load_adr_to_reg (reg : INTEGER; x : Base.Item);
BEGIN
	SetRmOperand (x);
	IF x.mode = Base.class_var THEN EmitRegRm (LEA, reg, 8)
	ELSIF x.mode = Base.class_ref THEN
		EmitRegRm (MOVd, reg, 8);
		IF x.c # 0 THEN EmitRI (ADDi, reg, 8, x.c) END
	ELSIF x.mode = mode_regI THEN
		EmitRR (MOVd, reg, 8, x.r);
		IF x.a # 0 THEN EmitRI (ADDi, reg, 8, x.a) END
	ELSE ASSERT(FALSE)
	END
END Load_adr_to_reg;

PROCEDURE Load_adr (VAR x : Base.Item);
BEGIN
	IF x.mode # mode_regI THEN x.r := Alloc_reg() END;
	Load_adr_to_reg (x.r, x);
	x.mode := mode_reg
END Load_adr;

(* Load boolean value to condition flag. This is NOT load cond to reg! *)
PROCEDURE Load_cond (VAR x : Base.Item);
BEGIN
	IF x.mode = mode_imm THEN Set_cond (x, ccNever - SHORT (x.a))
	ELSE load (x); EmitRR (TEST, x.r, 4, x.r); Free_reg; Set_cond (x, ccNZ)
	END
END Load_cond;

PROCEDURE Store* (VAR x, y : Base.Item);
	VAR rsize, count : INTEGER;
BEGIN
	IF x.type.form IN Base.types_Scalar THEN
		load (y); IF x.mode = Base.class_ref THEN Ref_to_regI (x) END;
		SetRmOperand (x); EmitRegRm (MOV, y.r, x.type.size);
		IF x.mode = mode_regI THEN Free_reg END;
		Free_reg
	ELSIF x.type.form IN {Base.type_array, Base.type_record} THEN
		(* Need rewriting *)
		count := x.type.size;
		IF (y.type.form = Base.type_string) & (y.type.size < count) THEN
			count := y.type.size
		END;
		Load_adr (x); Load_adr (y); EmitRR (MOVd, reg_DI, 8, x.r);
		EmitRR (MOVd, reg_SI, 8, y.r);
		IF count MOD 8 = 0 THEN count := count DIV 8; rsize := 8
		ELSIF count MOD 4 = 0 THEN count := count DIV 4; rsize := 4
		ELSE rsize := 1
		END;
		MoveRI (reg_C, 4, count); EmitREPop (MOVSrep, rsize, 1);
		Free_reg; Free_reg;
		ProcState.usedRegs := ProcState.usedRegs + {reg_DI, reg_SI}
	END
END Store;

PROCEDURE Small_const (VAR x : Base.Item) : BOOLEAN;
BEGIN
	RETURN (x.mode = mode_imm) & (x.a >= MIN_INT32) & (x.a <= MAX_INT32)
END Small_const;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* SET construction *)

PROCEDURE Set1* (VAR x : Base.Item);
	VAR r : INTEGER;
BEGIN
	IF (x.mode = mode_reg) & (x.a # 0) THEN
		IF (x.a > 0) & (x.a <= MAX_INT32) THEN EmitRI (ORi, x.r, 8, x.a)
		ELSE r := Alloc_reg(); MoveRI (r, 8, x.a);
			EmitRR (ORd, x.r, 8, r); Free_reg
		END
	END
END Set1;
	
PROCEDURE Set2* (VAR x, y : Base.Item);
	VAR s : ARRAY 2 OF SET; r : INTEGER;
BEGIN
	IF y.mode = mode_imm THEN
		(* Delay code generation *)
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR(s[0]), x.a);
		IF y.a > 31 THEN INCL (s[0], y.a) ELSE INCL (s[1], y.a - 32) END;
		SYSTEM.GET (SYSTEM.ADR(s[0]), x.a)
	ELSIF x.mode = mode_imm THEN
		r := Alloc_reg(); EmitRR (XOR, r, 4, r);
		load (y); EmitRR (BTS, y.r, 8, r);
		IF r # Reg_stack(-2) THEN EmitRR (MOVd, r, 8, y.r); r := y.r END;
		Free_reg; x.mode := mode_reg; x.r := r
	ELSIF x.mode = mode_reg THEN
		load (y); EmitRR (BTS, y.r, 8, x.r); Free_reg
	END
END Set2;
	
PROCEDURE Set3* (VAR x, y, z : Base.Item);
	VAR s : ARRAY 2 OF SET; imm : LONGINT; r : INTEGER;
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
		ELSE Scanner.Mark ('First element is greater than last element')
		END
	ELSE
		IF x.mode = mode_imm THEN r := Alloc_reg() ELSE r := x.r END;
		load (y); load (z);
		
		IF y.r = reg_C THEN
			MoveRI (r, 8, -1); EmitR (SHLcl, r, 8);
			EmitRR (MOVd, reg_C, 1, z.r); MoveRI (z.r, 8, -2);
			EmitR (SHLcl, z.r, 8); EmitRR (XORd, r, 8, z.r)
		ELSIF z.r = reg_C THEN
			MoveRI (r, 8, -2); EmitR (SHLcl, r, 8);
			EmitRR (MOVd, reg_C, 1, y.r); MoveRI (y.r, 8, -1);
			EmitR (SHLcl, y.r, 8); EmitRR (XORd, r, 8, y.r)
		ELSE
			MoveRI (r, 8, -1); EmitRR (XCHG, reg_C, 1, y.r);
			EmitR (SHLcl, r, 8); EmitRR (MOVd, reg_C, 1, z.r);
			MoveRI (z.r, 8, -2); EmitR (SHLcl, z.r, 8);
			EmitRR (XORd, r, 8, z.r); EmitRR (XCHG, reg_C, 1, y.r)
		END;
		
		x.r := Reg_stack(-3); x.mode := mode_reg;
		IF r = x.r THEN (* Do nothing *) ELSE EmitRR (MOVd, x.r, 8, r) END;
		Free_reg; Free_reg
	END
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Arithmetic *)

PROCEDURE Op1* (op : INTEGER; VAR x : Base.Item);
	VAR s : ARRAY 2 OF SET; t, cond : INTEGER;
BEGIN
	IF op = Scanner.not THEN
		IF x.mode = mode_imm THEN x.a := (x.a + 1) MOD 2
		ELSE
			IF x.mode # mode_cond THEN Load_cond (x) END;
			x.c := negated (x.c);
			t := SHORT(x.a); x.a := x.b; x.b := t
		END
	ELSIF op = Scanner.minus THEN
		IF x.mode = mode_imm THEN
			IF x.type.form = Base.type_integer THEN
				IF x.a = Base.MIN_INT THEN
					Scanner.Mark ('Integer overflow detected')
				ELSE x.a := -x.a
				END
			ELSIF x.type = Base.set_type THEN
				s[0] := {}; s[1] := {}; SYSTEM.PUT (SYSTEM.ADR(s[0]), x.a);
				s[0] := -s[0]; s[1] := -s[1]; SYSTEM.GET (SYSTEM.ADR(s[0]), x.a)
			END
		ELSE
			load (x);
			IF x.type.form = Base.type_integer THEN EmitR (NEG, x.r, 8);
				IF Base.CompilerFlag.overflow_check THEN
					Trap (ccO, overflow_trap)
				END
			ELSIF x.type = Base.set_type THEN EmitR (NOT, x.r, 8)
			END
		END
	ELSIF op = Scanner.and THEN
		IF x.mode # mode_cond THEN Load_cond (x) END;
		cond := negated (x.c);
		IF cond # ccNever THEN CondBranch (cond, SHORT (x.a));
			x.a := pc - 1
		END;
		Fix_link (x.b); x.b := 0
	ELSIF op = Scanner.or THEN
		IF x.mode # mode_cond THEN Load_cond (x) END;
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
		ELSE load (y); EmitRR (SUBd, x.r, 8, y.r); Free_reg
		END
	ELSE
		load (x); load (y); x.r := Reg_stack(-2);
		EmitRR (SUBd, x.r, 8, Reg_stack(-1)); Free_reg
	END
END Subtract;

PROCEDURE Divide (op : INTEGER; VAR x, y : Base.Item);
	VAR dst, r, r2, L : INTEGER; saveA, saveD : BOOLEAN;
BEGIN
	IF (y.mode # mode_imm) OR (y.a > 0) & (Base.log2(y.a) < 0) THEN
		r := 0; r2 := 0;

		load (x); load (y);
		IF Base.CompilerFlag.overflow_check THEN
			EmitRR (TEST, y.r, 8, y.r); Trap (ccLE, overflow_trap)
		END;
		
		dst := Reg_stack(-2);
		
		IF (reg_A IN curRegs) & (x.r # reg_A) THEN
			r := Alloc_reg(); EmitRR (MOVd, r, 8, reg_A); saveA := TRUE
		ELSE saveA := FALSE
		END;
		IF (reg_D IN curRegs) & (x.r # reg_D) THEN
			r2 := Alloc_reg(); EmitRR (MOVd, r2, 8, reg_D); saveD := TRUE
		ELSE saveD := FALSE
		END;
		
		IF x.r # reg_A THEN EmitRR (MOVd, reg_A, 8, x.r) END;
		EmitBare (CQO);
		
		EmitRR (TEST, reg_A, 8, reg_A);
		L := pc; CondBranch (ccS, 0);
		
		IF ~ (y.r IN {reg_A, reg_D}) THEN EmitR (IDIVa, y.r, 8)
		ELSIF y.r = reg_D THEN EmitR (IDIVa, r2, 8)
		ELSE EmitR (IDIVa, r, 8)
		END;
		
		Branch (0); Fix_link (L); L := pc - 1; 
		
		IF ~ (y.r IN {reg_A, reg_D}) THEN EmitR (IDIVa, y.r, 8)
		ELSIF y.r = reg_D THEN EmitR (IDIVa, r2, 8)
		ELSE EmitR (IDIVa, r, 8)
		END;
		
		EmitRR (TEST, reg_D, 8, reg_D);
		CondBranch (ccZ, L); L := pc - 1;
		
		IF op = Scanner.div THEN EmitRI (SUBi, reg_A, 8, 1)
		ELSIF ~ (y.r IN {reg_A, reg_D}) THEN EmitRR (ADDd, reg_D, 8, y.r)
		ELSIF y.r = reg_D THEN EmitRR (ADDd, reg_D, 8, r2)
		ELSE EmitRR (ADDd, reg_D, 8, r)
		END;
		
		Fix_link (L);
		
		IF op = Scanner.div THEN
			IF dst # reg_A THEN EmitRR (MOVd, dst, 8, reg_A) END
		ELSIF dst # reg_D THEN EmitRR (MOVd, dst, 8, reg_D)
		END;
		
		IF saveD THEN Free_reg;
			IF y.r # reg_D THEN EmitRR (MOVd, reg_D, 8, r2) END
		END;
		IF saveA THEN Free_reg;
			IF y.r # reg_A THEN EmitRR (MOVd, reg_A, 8, r) END
		END;
		
		x.r := dst; Free_reg
	ELSE
		IF y.a > 1 THEN
			IF op = Scanner.div THEN EmitRI (SARi, x.r, 8, Base.log2(y.a))
			ELSE DEC (y.a);
				IF y.a <= MAX_INT32 THEN EmitRI (ANDi, x.r, 8, y.a)
				ELSE load (y); EmitRR (ANDd, x.r, 8, y.r); Free_reg
				END
			END
		ELSIF y.a = 1 THEN
			IF op = Scanner.mod THEN EmitRR (XORd, x.r, 4, x.r) END
		ELSE Scanner.Mark ('Division by non-positive number')
		END
	END
END Divide;

PROCEDURE BitwiseOp (op : INTEGER; VAR x, y : Base.Item);
	VAR dst, src : INTEGER;
BEGIN
	load (x); load (y); dst := Reg_stack(-2); src := Reg_stack(-1);
	EmitRR (op, dst, 8, src); Free_reg; x.r := dst
END BitwiseOp;

PROCEDURE Difference (VAR x, y : Base.Item);
	VAR s : ARRAY 2 OF SET;
BEGIN
	IF y.mode # mode_imm THEN
		load (y); EmitR (NOT, y.r, 8); BitwiseOp (ANDd, x, y)
	ELSE
		s[0] := {}; s[1] := {};
		SYSTEM.PUT (SYSTEM.ADR (s[0]), y.a); s[0] := -s[0]; s[1] := -s[1];
		SYSTEM.GET (SYSTEM.ADR (s[0]), y.a);
		load (y); EmitRR (ANDd, x.r, 8, y.r); Free_reg
	END
END Difference;

PROCEDURE IntConstOp (op : INTEGER; x, y : LONGINT) : LONGINT;
BEGIN
	CASE op OF
		Scanner.plus:
		IF Base.Safe_to_add (x, y) THEN INC (x, y)
		ELSE Scanner.Mark ('Integer overflow detected')
		END |
		
		Scanner.minus:
		IF Base.Safe_to_subtract (x, y) THEN DEC (x, y)
		ELSE Scanner.Mark ('Integer overflow detected')
		END |
		
		Scanner.times:
		IF Base.Safe_to_multiply (x, y) THEN x := x * y
		ELSE Scanner.Mark ('Integer overflow detected')
		END |
		
		Scanner.div:
		IF y > 0 THEN x := x DIV y
		ELSE Scanner.Mark ('Division by non-positive number')
		END |
		
		Scanner.mod:
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
		Scanner.plus: dst[0] := dst[0] + src[0]; dst[1] := dst[1] + src[1] |
		Scanner.minus: dst[0] := dst[0] - src[0]; dst[1] := dst[1] - src[1] |
		Scanner.times: dst[0] := dst[0] * src[0]; dst[1] := dst[1] * src[1] |
		Scanner.slash: dst[0] := dst[0] / src[0]; dst[1] := dst[1] / src[1]
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
			IF op = Scanner.plus THEN IntegerOp (ADDd, ADDi, x, y)
			ELSIF op = Scanner.minus THEN Subtract (x, y)
			ELSIF op = Scanner.times THEN IntegerOp (IMUL, IMULi, x, y)
			ELSIF op = Scanner.div THEN Divide (Scanner.div, x, y)
			ELSIF op = Scanner.mod THEN Divide (Scanner.mod, x, y)
			END;
			IF (op = Scanner.plus) OR (op = Scanner.minus)
			OR (op = Scanner.times) & Base.CompilerFlag.overflow_check THEN
				Trap (ccO, overflow_trap)
			END
		ELSIF x.type = Base.set_type THEN
			IF op = Scanner.plus THEN BitwiseOp (ORd, x, y)
			ELSIF op = Scanner.minus THEN Difference (x, y)
			ELSIF op = Scanner.times THEN BitwiseOp (ANDd, x, y)
			ELSIF op = Scanner.slash THEN BitwiseOp (XORd, x, y)
			END
		ELSIF x.type = Base.bool_type THEN
			IF y.mode # mode_cond THEN Load_cond (y) END;
			IF op = Scanner.and THEN
				x.a := merged (SHORT(y.a), SHORT(x.a)); x.b := y.b
			ELSIF op = Scanner.or THEN
				x.b := merged (y.b, x.b); x.a := y.a
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
		x.r := Alloc_reg(); SetRmOperand (x); EmitRegRm (MOVd, x.r, 8);
		x.a := x.c; x.mode := mode_regI
	ELSIF x.mode = Base.class_var THEN
		load (x); x.a := 0; x.mode := mode_regI
	ELSIF x.mode = mode_regI THEN
		SetRmOperand (x); EmitRegRm (MOVd, x.r, 8); x.a := 0
	ELSIF x.mode = mode_reg THEN
		x.mode := mode_regI; x.a := 0
	END;
	x.param := FALSE; x.readonly := FALSE; x.type := x.type.base
END Deref;
	
PROCEDURE Open_array_index (VAR x, y : Base.Item);
	VAR	flag : BOOLEAN; size, e : INTEGER;

	PROCEDURE Calculate_offset (r : INTEGER; typ : Base.Type; loff : INTEGER);
		VAR size, e : INTEGER; r2 : INTEGER;
	BEGIN
		r2 := Alloc_reg(); SetRmOperand_regI (reg_BP, loff);
		EmitRegRm (MOVd, r2, 8); EmitRR (IMUL, r, 8, r2); Free_reg;
		
		size := typ.base.size;
		IF size > 0 THEN
			e := Base.log2 (size);
			IF e > 0 THEN EmitRI (SHLi, r, 8, e)
			ELSIF e < 0 THEN EmitRI (IMULi, r, 8, size)
			END
		ELSIF typ.base.form = Base.type_array THEN
			Calculate_offset (r, typ.base, loff + 8)
		END
	END Calculate_offset;
		
	PROCEDURE Check_array_index (VAR x, y : Base.Item);
		VAR r : INTEGER;
	BEGIN
		r := Alloc_reg(); SetRmOperand_regI (reg_BP, x.b); EmitRegRm (MOVd, r, 8);
		IF y.mode = mode_imm THEN EmitRI (CMPi, r, 8, y.a)
		ELSIF y.mode = mode_reg THEN EmitRR (CMPd, r, 8, y.r)
		END;
		Trap (ccBE, array_trap); Free_reg
	END Check_array_index;

BEGIN (* Open_array_index *)
	flag := FALSE;
	IF y.mode # mode_imm THEN load (y); flag := TRUE
	ELSIF y.a < 0 THEN Scanner.Mark ('Array index out of bound')
	ELSIF y.a > 0 THEN flag := TRUE
	END;
	
	IF flag THEN
		IF Base.CompilerFlag.array_check THEN Check_array_index (x, y) END;
		size := x.type.base.size;
		IF size > 0 THEN
			IF y.mode = mode_imm THEN
				IF x.mode = Base.class_ref THEN INC (x.c, SHORT(y.a) * size)
				ELSIF x.mode = mode_regI THEN INC (x.a, y.a * size)
				END
			ELSIF y.mode = mode_reg THEN
				e := Base.log2 (size);
				IF e > 0 THEN EmitRI (SHLi, y.r, 8, e)
				ELSIF e < 0 THEN EmitRI (IMULi, y.r, 8, size)
				END;
				
				IF x.mode = Base.class_ref THEN
					Ref_to_regI (x); EmitRR (ADDd, y.r, 8, x.r); x.r := y.r
				ELSIF x.mode = Base.mode_regI THEN EmitRR (ADDd, x.r, 8, y.r)
				ELSE ASSERT(FALSE)
				END;
				Free_reg
			END
		ELSIF x.type.base.form = Base.type_array THEN
			load (y); Calculate_offset (y.r, x.type.base, x.b + 8);
			IF x.mode = Base.class_ref THEN
				Ref_to_regI (x); EmitRR (ADDd, y.r, 8, x.r); x.r := y.r
			ELSIF x.mode = mode_regI THEN EmitRR (ADDd, x.r, 8, y.r)
			END;
			Free_reg
		END
	END;
	INC (x.b, 8)
END Open_array_index;
	
PROCEDURE Index* (VAR x, y : Base.Item);
	VAR e, size : INTEGER;
BEGIN
	IF x.type.len > 0 THEN size := x.type.base.size;
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
			IF Base.CompilerFlag.array_check THEN
				EmitRI (CMPi, y.r, 8, x.type.len); Trap (ccAE, array_trap)
			END;
				
			e := Base.log2 (size);
			IF e > 0 THEN EmitRI (SHLi, y.r, 8, e)
			ELSIF e < 0 THEN EmitRI (IMULi, y.r, 8, size)
			END;
			
			IF x.mode = Base.class_var THEN
				Load_adr (x); EmitRR (ADDd, y.r, 8, x.r);
				x.r := y.r; x.a := 0; x.mode := mode_regI
			ELSIF x.mode = Base.class_ref THEN
				Ref_to_regI (x); EmitRR (ADDd, y.r, 8, x.r); x.r := y.r
			ELSIF x.mode = Base.mode_regI THEN
				EmitRR (ADDd, x.r, 8, y.r)
			END;
			Free_reg
		END
	ELSE Open_array_index (x, y)
	END;
	x.type := x.type.base
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Standard procedure *)

PROCEDURE SProc_INC* (VAR x : Base.Item; amount : INTEGER);
BEGIN
	IF x.mode = Base.class_ref THEN Ref_to_regI (x) END; SetRmOperand (x);
	EmitRmImm (ADDi, x.type.size, amount);
	IF x.mode = mode_regI THEN Free_reg END 
END SProc_INC;

PROCEDURE Assert* (VAR x : Base.Item);
BEGIN
	(* Implement later *)
END Assert;

PROCEDURE SProc_GET* (VAR x, y : Base.Item);
BEGIN
	load (x); x.mode := mode_regI; x.a := 0;
	x.type := y.type; Store (y, x)
END SProc_GET;
	
PROCEDURE SProc_PUT* (VAR x, y : Base.Item);
BEGIN
	load (x); x.mode := mode_regI; x.a := 0; x.type := y.type;
	Store (x, y)
END SProc_PUT;
	
PROCEDURE SProc_COPY* (VAR x, y, z : Base.Item);
	VAR	i, rsize : INTEGER;
BEGIN
	Load_to_reg (reg_SI, USHORT (x.type.size), x);
	Load_to_reg (reg_DI, USHORT (y.type.size), y);
	Load_to_reg (reg_C, USHORT (z.type.size), z);
	
	EmitREPop (MOVSrep, 1, 1);

	i := reg_stacks [crs].i; WHILE i > 0 DO Free_reg; DEC (i) END;
	ProcState.usedRegs := ProcState.usedRegs + {reg_DI, reg_SI}
END SProc_COPY;

PROCEDURE SFunc_ADR* (VAR x : Base.Item);
BEGIN
	Load_adr (x)
END SFunc_ADR;

PROCEDURE SFunc_ODD* (VAR x : Base.Item);
BEGIN
	IF x.mode = mode_imm THEN x.a := x.a MOD 2
	ELSE EmitRI (ANDi, x.r, 8, 1)
	END
END SFunc_ODD;

PROCEDURE SFunc_LEN* (VAR x : Base.Item);
BEGIN
	IF x.type.len > 0 THEN x.a := x.type.len; x.mode := mode_imm
	ELSE (* Open array *)
		x.r := Alloc_reg(); SetRmOperand_regI (reg_BP, x.b);
		EmitRegRm (MOVd, x.r, 8); x.mode := mode_reg
	END
END SFunc_LEN;

PROCEDURE SFunc_SHIFT* (shf : INTEGER; VAR x, y : Base.Item);
	VAR opR, opI, dst : INTEGER;
BEGIN
	ASSERT (x.mode IN {mode_reg, mode_imm});
	ASSERT (y.mode IN Base.classes_Value);
	opR := 0; opI := 0;	
	IF shf = 0 THEN opR := SHLcl; opI := SHLi
	ELSIF shf = 1 THEN opR := SARcl; opI := SARi
	ELSIF shf = 2 THEN opR := RORcl; opI := RORi
	ELSE ASSERT (FALSE)
	END;
	
	IF (x.mode = mode_imm) & (y.mode = mode_imm) THEN
		IF shf = 0 THEN x.a := ASH (x.a, y.a)
		ELSIF shf = 1 THEN x.a := ASH (x.a, -y.a)
		ELSE x.a := ROT (x.a, -y.a)
		END
	ELSE
		load (x);
		IF (y.mode = mode_imm) & (y.a >= 0) & (y.a < 100H) THEN
			IF y.a # 0 THEN EmitRI (opI, x.r, 8, y.a) END
		ELSE
			load (y); dst := Reg_stack(-2);
			IF x.r = reg_C THEN
				EmitRR (XCHG, reg_C, 8, y.r); EmitR (opR, y.r, 8);
				IF dst # y.r THEN EmitRR (MOVd, dst, 8, y.r) END
			ELSIF y.r = reg_C THEN
				EmitR (opR, x.r, 8);
				IF dst # x.r THEN EmitRR (MOVd, dst, 8, x.r) END
			ELSE
				IF reg_C IN curRegs THEN PushR (reg_C) END;
				EmitRR (MOVd, reg_C, 8, y.r); EmitR (opR, x.r, 8);
				IF reg_C IN curRegs THEN PopR (reg_C) END;
				IF dst # x.r THEN EmitRR (MOVd, dst, 8, x.r) END
			END;
			Free_reg; x.r := dst
		END
	END
END SFunc_SHIFT;

PROCEDURE SFunc_BIT* (VAR x, y : Base.Item);
BEGIN
	ASSERT (x.mode IN {mode_reg, mode_imm});
	ASSERT (y.mode IN Base.classes_Value);
	load (x); x.mode := mode_regI; x.a := 0; load (x);
	load (y); EmitRR (BT, y.r, 8, x.r); Free_reg; Free_reg;
	Set_cond (x, ccB)
END SFunc_BIT;

PROCEDURE SFunc_ABS* (VAR x : Base.Item);
	VAR r, L : INTEGER;
BEGIN
	ASSERT (x.mode IN Base.classes_Value);
	IF x.mode = mode_imm THEN
		IF x.type.form = Base.type_integer THEN x.a := ABS (x.a)
		ELSE ASSERT(FALSE)
		END
	ELSIF x.type.form = Base.type_integer THEN
		load (x); r := Alloc_reg();
		EmitRR (MOVd, r, 8, x.r); EmitRI (SHLi, r, 8, 1);
		IF Base.CompilerFlag.overflow_check THEN
			Trap (ccBE, overflow_trap) (* Check for Z and C flag *)
		END;
		L := pc; CondBranch (ccAE, 0);
		EmitR (NEG, x.r, 8); Fix_link (L); Free_reg
	ELSE ASSERT(FALSE)
	END
END SFunc_ABS;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Procedure calling *)

PROCEDURE Save_reg_stacks (VAR pinfo : ProcInfo);
	VAR i : INTEGER;
BEGIN
	pinfo.oldrs := crs;
	pinfo.oldi := reg_stacks [crs].i;
	pinfo.oldcurRegs := curRegs;
	
	FOR i := 0 TO reg_stacks [crs].i - 1 DO
		PushR (reg_stacks [crs].ord [i])
	END;
	crs := 1; reg_stacks [1].i := 0
END Save_reg_stacks;

PROCEDURE Restore_reg_stacks (VAR pinfo : ProcInfo);
	VAR r, newreg, i : INTEGER;
BEGIN
	crs := pinfo.oldrs;
	reg_stacks [crs].i := pinfo.oldi;
	curRegs := pinfo.oldcurRegs;
	
	i := reg_stacks [crs].i - 1;
	WHILE i >= 0 DO
		r := reg_stacks [crs].ord [i];
		IF (r = reg_A) & (pinfo.rtype # NIL) THEN
			newreg := Alloc_reg();
			EmitRR (MOVd, newreg, 8, reg_A)
		END;
		PopR (r); DEC (i)
	END
END Restore_reg_stacks;
	
PROCEDURE Prepare_to_call* (VAR x : Base.Item; VAR pinfo : ProcInfo);
BEGIN
	IF x.mode = Base.class_proc THEN (* Do nothing *)
	ELSE load (x); PushR (x.r); Free_reg
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
	IF x.mode = Base.class_proc THEN n := 0;
		IF x.lev >= 0 THEN CallNear (SHORT(x.a));
		ELSE SetRmOperand (x); EmitRm (CALL, 8)
		END
	ELSE SetRmOperand_regI (reg_SP, pinfo.parblksize); EmitRm (CALL, 8); n := 8
	END;
	
	(* Release the stack area used for parameters and proc address *)
	EmitRI (ADDi, reg_SP, 8, pinfo.parblksize + n);
	DEC (pinfo.memstack, pinfo.parblksize + n);
	
	Restore_reg_stacks (pinfo);
	
	(* Return value *)
	IF pinfo.rtype # NIL THEN
		IF x.mode # Base.class_proc THEN x.type := pinfo.rtype END;
		IF reg_A IN curRegs THEN x.r := Reg_stack(-1)
		ELSE x.r := Alloc_reg();
			IF x.r # reg_A THEN EmitRR (MOVd, x.r, 8, reg_A) END
		END;
		x.mode := mode_reg
	END
END Call;
	
PROCEDURE Value_param* (VAR x : Base.Item; VAR pinfo : ProcInfo);
BEGIN
	load (x);
	IF pinfo.paradr >= 32 THEN
		SetRmOperand_regI (reg_SP, pinfo.paradr); EmitRegRm (MOV, x.r, 8)
	END;
	INC (pinfo.paradr, 8)
END Value_param;
	
PROCEDURE Ref_param* (VAR x : Base.Item; VAR pinfo : ProcInfo);
BEGIN
	Load_adr (x);
	IF pinfo.paradr >= 32 THEN
		SetRmOperand_regI (reg_SP, pinfo.paradr); EmitRegRm (MOV, x.r, 8)
	END;
	INC (pinfo.paradr, 8)
END Ref_param;
	
PROCEDURE Record_var_param* (VAR x : Base.Item; VAR pinfo : ProcInfo);
	VAR tag : Base.Item;
BEGIN
	IF x.param & ~ x.readonly THEN
		tag.mode := Base.class_ref; tag.lev := x.lev; tag.a := x.a + 8
	ELSE Get_typedesc (tag, x.type)
	END;
	Ref_param (x, pinfo); Ref_param (tag, pinfo)
END Record_var_param;
	
PROCEDURE Open_array_param* (VAR x : Base.Item; VAR pinfo : ProcInfo; ftype : Base.Type);
	VAR array, len : Base.Item; tp : Base.Type;
BEGIN
	array := x; Ref_param (array, pinfo); tp := x.type;
	WHILE (ftype.form = Base.type_array) & (ftype.len < 0) DO
		IF tp.len < 0 THEN
			len.mode := Base.class_var; len.lev := x.lev; len.a := x.b;
			len.type := Base.int_type; x.b := x.b + 8
		ELSE Make_const (len, Base.int_type, tp.len)
		END;
		Value_param (len, pinfo); ftype := ftype.base; tp := tp.base
	END
END Open_array_param;

PROCEDURE String_param* (VAR x : Base.Item; VAR pinfo : ProcInfo; ftype : Base.Type);
BEGIN
	ASSERT ((x.mode = Base.class_var) & (x.type.form = Base.type_string));
	Scanner.Mark ('Compiler limit: String parameter not supported yet');
	ASSERT(FALSE)
	(* Implement later *)
END String_param;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Standard Procedures - the remaining *)

PROCEDURE SProc_NEW* (VAR x : Base.Item);
	VAR proc, par, td : Base.Item; pinfo : ProcInfo;
BEGIN
	proc.mode := Base.class_proc; proc.type := Base.int_type;
	proc.lev := -2; proc.a := -24; (* HeapAlloc *)
	pinfo.parblksize := 24; pinfo.rtype := Base.int_type;
	Prepare_to_call (proc, pinfo);
	
	par.mode := Base.class_var; par.lev := -1; par.a := -64; (* HeapHandle *)
	par.type := Base.int_type; Value_param (par, pinfo);
	Make_const (par, Base.int_type, 8); Value_param (par, pinfo);
	Make_const (par, Base.int_type, x.type.base.size + 16);
	Value_param (par, pinfo);
	
	Call (proc, pinfo); EmitRI (ADDi, proc.r, 8, 16);
	Get_typedesc (td, x.type.base); Load_adr (td);
	SetRmOperand_regI (proc.r, -8); EmitRegRm (MOV, td.r, 8); Store (x, proc)
END SProc_NEW;

PROCEDURE SProc_DISPOSE* (VAR proc : Base.Item; VAR pinfo : ProcInfo);
	VAR par, td : Base.Item;
BEGIN
	proc.mode := Base.class_proc; proc.type := NIL; proc.lev := -2;
	proc.a := -16; (* HeapFree *) pinfo.parblksize := 24; pinfo.rtype := NIL;
	Prepare_to_call (proc, pinfo);
	
	par.mode := Base.class_var; par.lev := -1; par.a := -64; (* HeapHandle *)
	par.type := Base.int_type; Value_param (par, pinfo);
	Make_const (par, Base.int_type, 0); Value_param (par, pinfo)
END SProc_DISPOSE;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Conditional Branch *)

PROCEDURE FJump* (VAR L : INTEGER);
BEGIN
	Branch (L);
	L := pc - 1
END FJump;

PROCEDURE CFJump* (VAR x : Base.Item);
	VAR cond : INTEGER;
BEGIN
	IF x.mode # mode_cond THEN Load_cond (x) END;
    cond := negated (x.c);
	IF cond < 16 THEN CondBranch (cond, SHORT (x.a)); x.a := pc - 1
	ELSIF cond = ccAlways THEN Branch (SHORT (x.a)); x.a := pc - 1
	END;
	Fix_link (x.b)
END CFJump;
	
PROCEDURE CBJump* (VAR x : Base.Item; L : INTEGER);
	VAR cond : INTEGER;
BEGIN
	IF x.mode # mode_cond THEN Load_cond (x) END;
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
(* Relation expression *)

PROCEDURE Scalar_comparison (op : INTEGER; VAR x, y : Base.Item);
	VAR rsize : INTEGER;
		
	PROCEDURE Invert_op (VAR op : INTEGER);
	BEGIN
		IF op > Scanner.not_equal THEN
			IF op = Scanner.less THEN op := Scanner.greater
			ELSIF op = Scanner.less_equal THEN op := Scanner.greater_equal
			ELSIF op = Scanner.greater THEN op := Scanner.less
			ELSIF op = Scanner.greater_equal THEN op := Scanner.less_equal
			END
		END
	END Invert_op;
		
BEGIN
	rsize := x.type.size;
	IF ~ (y.mode IN {mode_imm, mode_reg}) THEN load (y) END;
	
	IF x.mode = mode_reg THEN
		IF Small_const (y) THEN EmitRI (CMPi, x.r, rsize, y.a)
		ELSE load (y); EmitRR (CMPd, x.r, rsize, y.r); Free_reg
		END
	ELSIF x.mode = mode_imm THEN
		IF Small_const (x) THEN EmitRI (CMPi, y.r, rsize, x.a); Invert_op (op)
		ELSE load (x); EmitRR (CMPd, x.r, rsize, y.r); Free_reg
		END
	END;
	Free_reg
END Scalar_comparison;
	
PROCEDURE String_comparison (op : INTEGER; VAR x, y : Base.Item);
	VAR xform, yform, xsize, ysize, i : INTEGER;
		charsize, L, r, rc : INTEGER; xstr, ystr : Base.LongString;
BEGIN
	ASSERT (x.type.len > 0); ASSERT (y.type.len > 0);
	(* Open array case not implemented yet *)
	
	xform := x.type.form; yform := y.type.form;
	xsize := x.type.size; ysize := y.type.size;
	charsize := Base.char_type.size;

	IF {xform, yform} = {Base.type_string} THEN
		IF xsize # ysize THEN x.a := 0
		ELSE
			Base.Get_string (xstr, x.type.ref);
			Base.Get_string (ystr, y.type.ref);
			IF xstr = ystr THEN x.a := 1 ELSE x.a := 0 END
		END;
		x.mode := mode_imm;
	ELSIF (xform = Base.type_string) & (xsize - charsize > ysize)
			OR (yform = Base.type_string) & (ysize - charsize > xsize) THEN
		x.mode := mode_imm; x.a := 0
	ELSE
		Load_adr (x); Load_adr (y);
		EmitRR (MOVd, reg_DI, 8, x.r); EmitRR (MOVd, reg_SI, 8, y.r);
		r := x.r; rc := y.r; IF xsize > ysize THEN xsize := ysize END;
		MoveRI (rc, 4, xsize);
		
		L := pc; SetRmOperand_regI (reg_DI, 0); EmitRegRm (MOVd, r, 2);
		SetRmOperand_regI (reg_SI, 0);
		EmitRegRm (CMPd, r, 2); Set_cond (x, ccZ); CFJump (x);
		EmitRR (TEST, x.r, 4, x.r); Set_cond (x, ccNZ); CFJump (x);
		EmitRI (SUBi, rc, 4, 2); Set_cond (x, ccNZ); CFJump (x);
		EmitRI (ADDi, reg_DI, 8, 2); EmitRI (ADDi, reg_SI, 8, 2);
		BJump (L); Fixup (x);
		SetRmOperand_regI (reg_SI, 0); EmitRegRm (CMPd, r, 2);
		Set_cond (x, ccZ);
		
		Free_reg; Free_reg
	END
END String_comparison;
	
PROCEDURE Comparison* (op : INTEGER; VAR x, y : Base.Item);
BEGIN
	IF (x.mode = mode_imm) & (y.mode = mode_imm) THEN
		IF op = Scanner.equal THEN
			IF x.a = y.a THEN x.a := 1 ELSE x.a := 0 END
		ELSIF op = Scanner.not_equal THEN
			IF x.a # y.a THEN x.a := 1 ELSE x.a := 0 END
		ELSIF op = Scanner.greater THEN
			IF x.a > y.a THEN x.a := 1 ELSE x.a := 0 END
		ELSIF op = Scanner.greater_equal THEN
			IF x.a >= y.a THEN x.a := 1 ELSE x.a := 0 END
		ELSIF op = Scanner.less THEN
			IF x.a < y.a THEN x.a := 1 ELSE x.a := 0 END
		ELSIF op = Scanner.less_equal THEN
			IF x.a <= y.a THEN x.a := 1 ELSE x.a := 0 END
		END
	ELSIF (x.mode = Base.class_proc) OR (x.type.form IN Base.types_Scalar) THEN
		Scalar_comparison (op, x, y); Set_cond (x, ccCodeOf(op))
	ELSE String_comparison (op, x, y); Set_cond (x, ccCodeOf(op))
	END;
	x.type := Base.bool_type
END Comparison;
	
PROCEDURE Membership* (VAR x, y : Base.Item);
BEGIN
	IF (x.mode = mode_imm) & (y.mode = mode_imm) THEN
		x.a := ASH (y.a, -x.a) MOD 2
	ELSE
		load (y);
		IF x.mode = mode_reg THEN EmitRR (BT, x.r, 8, y.r); Free_reg
		ELSE EmitRI (BTi, y.r, 8, x.a)
		END;
		Free_reg; Set_cond (x, ccB)
	END;
	x.type := Base.bool_type
END Membership;
	
PROCEDURE Inclusion* (op : INTEGER; VAR x, y : Base.Item);
	VAR xset, yset : ARRAY 2 OF SET;
BEGIN
	IF (x.mode = mode_imm) & (y.mode = mode_imm) THEN
		xset[0] := {}; xset[1] := {}; SYSTEM.PUT (SYSTEM.ADR(xset[0]), x.a);
		yset[0] := {}; yset[1] := {}; SYSTEM.PUT (SYSTEM.ADR(yset[0]), y.a);
		IF op = Scanner.less_equal THEN
			IF (xset[0] - yset[0] = {}) & (xset[1] - yset[1] = {})
			THEN x.a := 1 ELSE x.a := 0
			END
		ELSE
			IF (yset[0] - xset[0] = {}) & (yset[1] - xset[1] = {})
			THEN x.a := 1 ELSE x.a := 0
			END
		END
	ELSE
		IF op = Scanner.less_equal
		THEN Difference (x, y) ELSE Difference (y, x)
		END;
		Free_reg; Set_cond (x, ccZ)
	END;
	x.type := Base.bool_type
END Inclusion;

PROCEDURE Type_test* (VAR x : Base.Item; typ : Base.Type; guard : BOOLEAN);
	VAR r : INTEGER; td : Base.Item;
BEGIN
	ASSERT ((typ.form = Base.type_record) & (x.mode IN Base.classes_Value));
	IF x.type # typ THEN
		IF x.type.form = Base.type_pointer THEN
			load (x); IF guard THEN r := Alloc_reg() ELSE r := x.r END;
			SetRmOperand_regI (x.r, -8); EmitRegRm (MOVd, r, 8)
		ELSIF (x.mode = Base.class_ref) & x.param & ~ x.readonly
		& (x.type.form = Base.type_record) THEN
			r := Alloc_reg(); SetRmOperand_regI (reg_BP, SHORT(x.a + 8));
			EmitRegRm (LEA, r, 8)
		ELSE ASSERT(FALSE); r := 0
		END;

		Get_typedesc (td, typ); Load_adr (td);
		SetRmOperand_regI (r, typ.len * 8); EmitRegRm (CMPd, td.r, 8);
		IF guard THEN Trap (ccNZ, type_trap)
		ELSE Set_cond (x, ccZ); x.type := Base.bool_type
		END;
		Free_reg; Free_reg
	ELSIF ~ guard THEN
		Free_item (x); Make_const (x, Base.bool_type, 1)
	END
END Type_test;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Procedure Prolog, Epilog - Code Generation to File *)

PROCEDURE Write_to_file* (from, to : INTEGER);
	VAR	p, i, k : INTEGER;
		
	PROCEDURE Fixup_disp (p, i : INTEGER);
		VAR disp : INTEGER;
	BEGIN
		disp := 0; i := i + codeinfo [p].dispPos;
		SYSTEM.GET (SYSTEM.ADR (code [i]), disp);
		DEC (disp, codeinfo [p].ip + codeinfo [p].size + ProcState.prolog_size);
		SYSTEM.PUT (SYSTEM.ADR (code [i]), disp)
	END Fixup_disp;
	
BEGIN (* Write_to_file *)
	ASSERT ((from > 0) & (to > 0) & (from <= pc) & (to <= pc));
	i := codeinfo [from].ip - ProcState.adr;
	FOR p := from TO to DO
		IF codeinfo [p].relfixup THEN Fixup_disp (p, i) END;
		FOR k := 1 TO codeinfo [p].size DO
			Sys.Write_byte (out, code [i]); INC (i)
		END
	END
END Write_to_file;

PROCEDURE Module_init;
	VAR i, framesize, L, r : INTEGER; n1, n2, n3, n4 : LONGINT;
		str : Base.LongString; tp : Base.Type; td : Base.Item;
		modul, obj : Base.Object; exit : BOOLEAN;
BEGIN
	IF ~ Base.CompilerFlag.main THEN
		EmitRI (CMPi, reg_D, 4, 1); L := pc; CondBranch (ccZ, 0);
		EmitBare (LEAVE); EmitBare (RET); Fix_link (L)
	END;
	
	EmitRI (SUBi, reg_SP, 8, 40);
	SetRmOperand_staticvar (-32); EmitRm (CALL, 8); (* GetProcessHeap *)
	EmitRI (ADDi, reg_SP, 8, 32);
	SetRmOperand_staticvar (-64); EmitRegRm (MOV, reg_A, 8); (* Heap Handle of Process *)
	
	(* Import other modules, if there are any *)
	modul := Base.universe.next;
	WHILE (modul # Base.guard)
	& ((modul.class # Base.class_module) OR (modul.realname = 'SYSTEM')) DO
		modul := modul.next
	END;
	IF modul # Base.guard THEN
		framesize := (Base.max_ident_len + 5) * 2;
		framesize := (-framesize) MOD 16 + framesize + 32;
		EmitRI (SUBi, reg_SP, 8, framesize);
		REPEAT
			str := ''; Base.Append_str (str, modul.realname);
			Base.Append_str (str, '.dll');
			i := 0; n1 := 1; n2 := 1; n3 := 1; n4 := 1;
			WHILE (n1 # 0) & (n2 # 0) & (n3 # 0) & (n4 # 0) DO
				n1 := ORD(str[i]);
				n2 := ORD(str[i + 1]); n2 := ASH(n2, 16);
				n3 := ORD(str[i + 2]); n3 := ASH(n3, 32);
				n4 := ORD(str[i + 3]); n4 := ASH(n4, 48);
				MoveRI (reg_A, 8, n1 + n2 + n3 + n4);
				SetRmOperand_regI (reg_SP, 32 + i * 2);
				EmitRegRm (MOV, reg_A, 8);
				i := i + 4
			END;
			SetRmOperand_regI (reg_SP, 32); EmitRegRm (LEA, reg_C, 8);
			SetRmOperand_staticvar (-48); EmitRm (CALL, 8); (* LoadLibraryW *)
			EmitRR (MOVd, reg_SI, 8, reg_A);
			obj := modul.dsc;
			WHILE obj # Base.guard DO
				IF (obj.val # 0) & (obj.val2 # 0) THEN
					EmitRR (MOVd, reg_C, 8, reg_SI);
					MoveRI (reg_D, 4, obj.val2);
					SetRmOperand_staticvar (-40); (* GetProcAddress *)
					EmitRm (CALL, 8);
					SetRmOperand_staticvar (SHORT (obj.val));
					EmitRegRm (MOV, reg_A, 8)
				END;
				obj := obj.next
			END;
			modul := modul.next;
			IF modul.realname = 'SYSTEM' THEN modul := modul.next END
		UNTIL modul.class # Base.class_module;
		EmitRI (ADDi, reg_SP, 8, framesize)
	END;
	
	(* Fill value into type descriptors *)
	tp := staticlist;
	WHILE tp # NIL DO
		IF (tp.form = Base.type_record) & (tp.base # NIL) THEN
			Get_typedesc (td, tp); Load_adr (td); r := td.r;
			REPEAT
				SetRmOperand_regI (r, tp.len * 8); EmitRegRm (MOV, td.r, 8);
				IF td.r # r THEN Free_reg END;
				tp := tp.base; Get_typedesc (td, tp); Load_adr (td)
			UNTIL tp.len = 0;
			Free_reg
		END;
		tp := tp.next
	END
END Module_init;

PROCEDURE Enter* (proc : Base.Object; locblksize : INTEGER);
	VAR k : INTEGER;
BEGIN
	Reset_code_buffer; pc := 1; ProcState.adr := ip; Reset_reg_stack;
	ProcState.usedRegs := {}; curRegs := {}; ProcState.memstack := 0;
	IF proc # NIL THEN
		ProcState.locblksize := locblksize;
		ProcState.parlist := proc.dsc;
		IF proc.val # ip THEN (* Need fixup *)
			Sys.Write_4bytes (fixupFile, SHORT (proc.val));
			Sys.Write_4bytes (fixupFile, ip);
			proc.val := ip
		END
	ELSE
		Linker.entry := ip;
		ProcState.locblksize := 0;
		ProcState.parlist := NIL;
		Module_init
	END;
END Enter;

PROCEDURE Module_exit;
BEGIN
	IF Base.CompilerFlag.main THEN
		EmitRI (SUBi, reg_SP, 8, 32);
		EmitRR (XOR, reg_C, 4, reg_C);
		SetRmOperand_staticvar (-56); EmitRm (CALL, 8) (* ExitProcess *)
	ELSE MoveRI (reg_A, 4, 1)
	END
END Module_exit;
	
PROCEDURE Return*;
	CONST savedRegs = {reg_DI, reg_SI, reg_R12, reg_R13, reg_R14, reg_R15};
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
	IF ProcState.parlist # NIL THEN
		nRegs := 0;
		FOR i := 15 TO 0 BY -1 DO
			IF i IN (savedRegs * ProcState.usedRegs) THEN PopR (i); INC (nRegs) END
		END;
		
		i := (ProcState.locblksize + nRegs * 8) MOD 16;
		IF i > 0 THEN INC (ProcState.locblksize, 16 - i) END;
		EmitBare (LEAVE); EmitBare (RET);
		endPC := pc; endIP := ip;
		
		PushR (reg_BP); EmitRR (MOVd, reg_BP, 8, reg_SP);
		IF ProcState.locblksize > 0 THEN
			EmitRI (SUBi, reg_SP, 8, ProcState.locblksize)
		END;
		
		FOR i := 0 TO 15 DO
			IF i IN (savedRegs * ProcState.usedRegs) THEN PushR (i); INC (nRegs) END
		END;
		
		paramRegs [0] := reg_C; paramRegs [1] := reg_D;
		paramRegs [2] := reg_R8; paramRegs [3] := reg_R9;
		obj := ProcState.parlist; i := 0; k := 0;
		WHILE (i < 4) & (obj # Base.guard) DO
			IF k = 0 THEN k := Param_size (obj) END;
			SetRmOperand_regI (reg_BP, 16 + i * 8);
			EmitRegRm (MOV, paramRegs [i], 8);
			i := i + 1; k := k - 8;
			IF k = 0 THEN obj := obj.next END
		END;
	
		ProcState.prolog_size := ip - endIP;
		Write_to_file (endPC, pc - 1);
		Write_to_file (1, endPC - 1)
	ELSE
		ProcState.prolog_size := 0;
		Module_exit; Write_to_file (1, pc - 1)
	END
END Return;

PROCEDURE Check_varsize* (n : LONGINT; g : BOOLEAN);
BEGIN
	IF n > MAX_INT32 THEN
		Scanner.Mark ('Type size or variables area size > 2 GB Limit!'); n := 0
	END;
	IF g THEN varsize := SHORT(n); Base.Adjust_alignment (varsize, 16);
		staticbase := -varsize
	END
END Check_varsize;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Linker *)

PROCEDURE Init* (modname : Base.String);
BEGIN
	ip := 0; varbase := 0; staticsize := 128; modid := modname;
	Sys.Rewrite (out, tempOutputName);
	Sys.Seek (out, 400H)
END Init;

PROCEDURE Align (VAR off : INTEGER; alignment : INTEGER);
BEGIN
	ASSERT (off >= 0);
	off := (alignment - off) MOD alignment + off
END Align;

PROCEDURE Write_idata_section;
	CONST
		table_len = LEN(Kernel32Table);
		table_size = table_len * 8;
		table_rva = 40;
		name_rva = table_rva + table_size;
		hint_rva = name_rva + 16;
	VAR i : INTEGER;
BEGIN
	Sys.Seek (out, Linker.idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Sys.Write_4bytes (out, Linker.idata_rva + table_rva);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, Linker.idata_rva + name_rva);
	i := Linker.data_rva + Linker.data_size + staticbase - table_size;
	Sys.Write_4bytes (out, i);

	Sys.Seek (out, Linker.idata_fadr + table_rva);
	FOR i := 0 TO table_len - 2 DO
		Kernel32Table [i] := Linker.idata_rva + hint_rva + 32 * i;
		Sys.Write_8bytes (out, Kernel32Table [i]);
	END;
	Kernel32Table [table_len - 1] := 0;
	
	Sys.Seek (out, Linker.idata_fadr + name_rva);
	Sys.Write_ansi_str (out, 'KERNEL32.DLL');
	
	Sys.Seek (out, Linker.idata_fadr + hint_rva + 2);
	Sys.Write_ansi_str (out, 'ExitProcess');
	Sys.Seek (out, Linker.idata_fadr + hint_rva + (32 + 2));
	Sys.Write_ansi_str (out, 'LoadLibraryW');
	Sys.Seek (out, Linker.idata_fadr + hint_rva + (64 + 2));
	Sys.Write_ansi_str (out, 'GetProcAddress');
	Sys.Seek (out, Linker.idata_fadr + hint_rva + (96 + 2));
	Sys.Write_ansi_str (out, 'GetProcessHeap');
	Sys.Seek (out, Linker.idata_fadr + hint_rva + (128 + 2));
	Sys.Write_ansi_str (out, 'HeapAlloc');
	Sys.Seek (out, Linker.idata_fadr + hint_rva + (160 + 2));
	Sys.Write_ansi_str (out, 'HeapFree')
END Write_idata_section;

PROCEDURE Fill_pointer_offset (offset : INTEGER; type : Base.Type);
	VAR field : Base.Object; n, k, ptrcnt : INTEGER;
BEGIN
	ptrcnt := type.num_ptr;
	IF type.form = Base.type_record THEN
		IF (type.base # NIL) & (type.base.num_ptr > 0) THEN
			Fill_pointer_offset (offset, type.base)
		END;
		field := type.fields;
		REPEAT
			k := field.type.num_ptr;
			IF k > 0 THEN
				ptrcnt := ptrcnt - k; n := offset + SHORT(field.val);
				IF field.type.form = Base.type_pointer THEN
					Sys.Write_4bytes (out, n)
				ELSE Fill_pointer_offset (n, field.type)
				END
			END;
			field := field.next
		UNTIL ptrcnt <= 0; ASSERT (ptrcnt = 0)
	ELSIF type.form = Base.type_array THEN
		k := type.base.num_ptr; type := type.base;
		IF type.form = Base.type_pointer THEN
			REPEAT Sys.Write_4bytes (out, offset); ptrcnt := ptrcnt - 1;
				offset := offset + 8
			UNTIL ptrcnt <= 0; ASSERT (ptrcnt = 0)
		ELSE
			REPEAT
				Fill_pointer_offset (offset, type); ptrcnt := ptrcnt - k;
				offset := offset + type.size
			UNTIL ptrcnt <= 0; ASSERT (ptrcnt = 0)
		END
	ELSE ASSERT(FALSE)
	END
END Fill_pointer_offset;

PROCEDURE Write_data_section;
	VAR elm : Base.Type; basefadr, i, n : INTEGER;
		str : Base.LongString;
BEGIN
	ASSERT (Base.Char_size = 2);
	basefadr := Linker.data_fadr + Linker.data_size - varsize;
	elm := staticlist;
	WHILE elm # NIL DO
		IF elm.form = Base.type_string THEN
			Sys.Seek (out, basefadr + elm.tdAdr);
			Base.Get_string (str, elm.ref); i := 0;
			WHILE i < elm.len DO
				n := ORD (str[i]); Sys.Write_2bytes (out, n); i := i + 1
			END
		ELSIF elm.form = Base.type_record THEN
			Sys.Seek (out, basefadr + elm.tdAdr);
			Sys.Write_8bytes (out, elm.size);
			Sys.SeekRel (out, (Base.max_extension - 1) * 8);
			IF elm.num_ptr > 0 THEN Fill_pointer_offset (0, elm) END;
			Sys.Write_4bytes (out, -1)
		END;
		elm := elm.next
	END;
	
	Sys.Seek (out, basefadr - LEN (Kernel32Table) * 8); i := 0;
	WHILE i < LEN (Kernel32Table) DO
		Sys.Write_8bytes (out, Kernel32Table[i]); i := i + 1
	END
END Write_data_section;

PROCEDURE Write_edata_section;
	CONST dirsize = 40;
	VAR obj : Base.Object;
		name : Base.LongString;
		namesize, tablesize, expno, rva : INTEGER;
BEGIN
	name := ''; Base.Append_str (name, modid);
	IF Base.CompilerFlag.main THEN Base.Append_str (name, '.exe')
	ELSE Base.Append_str (name, '.dll')
	END;
	namesize := Base.Str_len (name) + 1;
	
	tablesize := Base.expno * 4; expno := 0;

	(* Export directory *)
	Sys.Seek (out, Linker.edata_fadr + 12);
	Sys.Write_4bytes (out, Linker.edata_rva + dirsize + tablesize);
	Sys.Write_4bytes (out, 1);
	Sys.Write_4bytes (out, Base.expno);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, Linker.edata_rva + dirsize);
	
	(* Export address table *)
	Sys.Seek (out, Linker.edata_fadr + dirsize);
	obj := Base.universe.next;
	WHILE obj # Base.guard DO
		IF obj.export THEN
			IF (obj.class = Base.class_type)
			& (obj.type.form = Base.type_record) THEN
				rva := Linker.data_rva + Linker.data_size;
				rva := rva + staticbase + SHORT(obj.val);
				Sys.Write_4bytes (out, rva); expno := expno + 1
			ELSIF obj.class = Base.class_var THEN
				rva := Linker.data_rva + Linker.data_size;
				rva := rva + varbase + SHORT(obj.val);
				Sys.Write_4bytes (out, rva); expno := expno + 1
			ELSIF obj.class = Base.class_proc THEN
				Sys.Write_4bytes (out, Linker.code_rva + SHORT(obj.val));
				expno := expno + 1
			END
		END;
		obj := obj.next
	END;
	ASSERT (expno = Base.expno);
	
	(* Name string *)
	Sys.Write_ansi_str (out, name);
	
	Linker.edata_size := dirsize + tablesize + namesize;
	Linker.edata_rawsize := Linker.edata_size;
	IF Linker.edata_rawsize MOD 512 # 0 THEN
		Align (Linker.edata_rawsize, 512);
		Sys.Seek (out, Linker.edata_fadr + Linker.edata_rawsize - 1);
		Sys.Write_byte (out, 1)
	END
END Write_edata_section;

PROCEDURE Write_reloc_section;
BEGIN
	Sys.Seek (out, Linker.reloc_fadr);
	Sys.Write_4bytes (out, 4);
	Sys.Write_4bytes (out, 12);
	Sys.Write_2bytes (out, 0);
	Sys.Write_2bytes (out, 0)
END Write_reloc_section;

PROCEDURE Write_SectionHeader (
	name : ARRAY OF CHAR; chr, rva, rawsize, size, fileadr : INTEGER
);	
	VAR b : UBYTE; i : INTEGER;	
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

PROCEDURE Write_PEHeader;
	VAR k : INTEGER;
BEGIN
	Sys.Seek (out, 0);
	Sys.Write_2bytes (out, 5A4DH);
	Sys.Seek (out, 60);
	Sys.Write_4bytes (out, 128);
	Sys.Seek (out, 128);
	Sys.Write_4bytes (out, 4550H);
	
	Sys.Write_2bytes (out, 8664H); (* Machine = AMD64/Intel 64 *)
	Sys.Write_2bytes (out, 5); (* NumberOfSections *)
	Sys.SeekRel (out, 4 * 3);
	Sys.Write_2bytes (out, 240);
	
	(* Characteristics *)
	IF Base.CompilerFlag.main THEN Sys.Write_2bytes (out, 20H + 2 + 1)
	ELSE Sys.Write_2bytes (out, 2000H + 20H + 2)
	END;
	
	Sys.Write_2bytes (out, 20BH);
	Sys.SeekRel (out, 2);
	Sys.Write_4bytes (out, Linker.code_rawsize);
	k := Linker.data_rawsize + 200H * 2 + Linker.edata_rawsize;
	Sys.Write_4bytes (out, k);
	Sys.SeekRel (out, 4);
	Sys.Write_4bytes (out, Linker.code_rva + Linker.entry);
	Sys.Write_4bytes (out, Linker.code_rva);
	
	Sys.Write_8bytes (out, Linker.imagebase);
	Sys.Write_4bytes (out, 4096);
	Sys.Write_4bytes (out, 512);
	Sys.Write_2bytes (out, 5); (* MajorOSVer *)
	Sys.SeekRel (out, 2 * 3);
	Sys.Write_2bytes (out, 5);
	Sys.SeekRel (out, 2 + 4);
	k := 4096 + (4096 - Linker.code_size) MOD 4096 + Linker.code_size;
	k := k + Linker.data_size + 4096 + 4096;
	k := k + (4096 - Linker.edata_size) MOD 4096 + Linker.edata_size;
	Sys.Write_4bytes (out, k);
	Sys.Write_4bytes (out, 400H);
	Sys.SeekRel (out, 4);
	IF Base.CompilerFlag.console THEN
		Sys.Write_2bytes (out, 3) (* Subsys = Console *)
	ELSE Sys.Write_2bytes (out, 2) (* Subsys = GUI *)
	END;
	
	(* DLL Characteristics *)
	IF Base.CompilerFlag.main THEN Sys.Write_2bytes (out, 0)
	ELSE Sys.Write_2bytes (out, 100H + 40H)
	END;
	
	Sys.Write_8bytes (out, 1000H);
	Sys.Write_8bytes (out, 1000H);
	Sys.Write_8bytes (out, 10000H);
	Sys.SeekRel (out, 8 + 4);
	Sys.Write_4bytes (out, 16);
	
	Sys.Write_4bytes (out, Linker.edata_rva);
	Sys.Write_4bytes (out, Linker.edata_size);
	Sys.Write_4bytes (out, Linker.idata_rva);
	Sys.Write_4bytes (out, 130H);
	Sys.SeekRel (out, 8 * 3);
	Sys.Write_4bytes (out, Linker.reloc_rva);
	Sys.Write_4bytes (out, 12);
	Sys.SeekRel (out, 8 * 10);
	
	Write_SectionHeader (
		'.data', -1073741760, Linker.data_rva, Linker.data_rawsize,
		Linker.data_size, Linker.data_fadr
	);
	Write_SectionHeader (
		'.text', 60000020H, Linker.code_rva, Linker.code_rawsize,
		Linker.code_size, Linker.code_fadr
	);
	Write_SectionHeader (
		'.idata', -1073741760, Linker.idata_rva, 200H,
		130H, Linker.idata_fadr
	);
	Write_SectionHeader (
		'.reloc', 42000040H, Linker.reloc_rva, 200H,
		12, Linker.reloc_fadr
	);
	Write_SectionHeader (
		'.edata', 40000040H, Linker.edata_rva, Linker.edata_rawsize,
		Linker.edata_size, Linker.edata_fadr
	);
END Write_PEHeader;

PROCEDURE Finish*;
	VAR i, n, padding, filesize : INTEGER; k : LONGINT;
		str : Base.LongString;
BEGIN
	IF ~ Scanner.haveError THEN
		Base.Write_symbols_file;

		IF Base.CompilerFlag.main THEN Linker.imagebase := 400000H
		ELSE Linker.imagebase := 10000000H
		END;

		Linker.code_rawsize := (512 - ip) MOD 512 + ip;
		Linker.code_size := ip;
		
		Linker.data_size := staticsize + varsize;
		Align (Linker.data_size, 4096);
		padding := Linker.data_size - staticsize - varsize;
		Linker.data_rawsize := padding + staticsize;
		Align (Linker.data_rawsize, 512);
		
		Linker.data_rva := 1000H;
		Linker.code_rva := Linker.data_rva + Linker.data_size;
		n := (4096 - Linker.code_size) MOD 4096 + Linker.code_size;
		Linker.idata_rva := Linker.code_rva + n;
		Linker.reloc_rva := Linker.idata_rva + 4096;
		Linker.edata_rva := Linker.reloc_rva + 4096;
		
		Linker.code_fadr := 400H;
		Linker.idata_fadr := Linker.code_fadr + Linker.code_rawsize;
		Linker.data_fadr := Linker.idata_fadr + 200H;
		Linker.reloc_fadr := Linker.data_fadr + Linker.data_rawsize;
		Linker.edata_fadr := Linker.reloc_fadr + 200H;
		
		Write_idata_section; Write_data_section; Write_reloc_section;
		Write_edata_section; Write_PEHeader;
		Sys.Close (out);

		(* Show statistics *)
		Sys.Console_WriteString ('No errors found.'); Sys.Console_WriteLn;
		Sys.Console_WriteString ('Code size: ');
		Sys.Console_WriteInt (Linker.code_size); Sys.Console_WriteLn;
		Sys.Console_WriteString ('Global variables size: ');
		Sys.Console_WriteInt (varsize); Sys.Console_WriteLn;
		Sys.Console_WriteString ('Static data size: ');
		Sys.Console_WriteInt (staticsize); Sys.Console_WriteLn;
		
		(* Rename files *)
		str := ''; Base.Append_str (str, modid);
		IF Base.CompilerFlag.main THEN Base.Append_str (str, '.exe')
		ELSE Base.Append_str (str, '.dll')
		END;
		Sys.Delete_file (str); Sys.Rename_file (tempOutputName, str);
		str := ''; Base.Append_str (str, modid); Base.Append_str (str, '.sym');
		Sys.Delete_file (str); Sys.Rename_file (tempSymName, str)
	ELSE
		Sys.Close (out);
		Sys.Console_WriteLn; Sys.Console_WriteString ('No output generated.');
		Sys.Delete_file (tempOutputName)
	END
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
END Generator3.