MODULE Generator;

IMPORT
	SYSTEM, Console, Sys, Base, Scanner;
	
CONST
	tempOutputName = 'output.temp_';

	mReg = Base.mReg;
	mXreg = Base.mXreg;
	mImm = Base.cConst;
	mRegI = Base.mRegI;
	mCond = Base.mCond;
	modeMem = {Base.cVar, Base.cRef, mRegI};

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
	LDMXCSR = 2AE0FH; STMXCSR = 3AE0FH;
	
	(* Opcodes used with EmitRmImm *)
	ADDi = 80H; ORi = 180H; ANDi = 480H; SUBi = 580H; XORi = 680H; CMPi = 780H;
	RORi = 1C0H; SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H; MOVi = 0C6H;
	TESTi = 76H; BTi = 4BA0FH; BTSi = 5BA0FH; BTRi = 6BA0FH; BTCi = 7BA0FH;
	IMULi = 69H (* Special case *);
	
	(* Opcodes used with EmitBare *)
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H; INT3 = 0CCH;
	
	(* REP instructions *)
	MOVSrep = 0A4H;
	
	(* Opcodes used with EmitXmmRm *)
	VMOVDQ = 6E0F66H; VMOVDQd = 7E0F66H; VMOVSS = 100FF3H; VMOVSSd = 110FF3H;
	VADDSS = 580FF3H; VMULSS = 590FF3H; VSUBSS = 5C0FF3H; VDIVSS = 5E0FF3H;
	VADDPS = 580F00H; VMULPS = 590F00H; VSUBPS = 5C0F00H; VDIVPS = 5E0F00H;
	VANDPS = 540F00H; VANDNPS = 550F00H; VORPS = 560F00H; VXORPS = 570F00H;
	VMOVAPS = 280F00H; VMOVAPSd = 290F00H;
	VCOMISS = 2F0F00H;
	
	(* Opcodes used with EmitSeeRegRm *)
	VCVTSS2SI = 2D0FF3H; VCVTSI2SS = 2A0FF3H;
	
TYPE
	ProcCall* = RECORD
		fpar*: Base.Object;
		nofact*, nofpar*: INTEGER
	END;
	
	Inst = RECORD
		relfixup: BOOLEAN;
		size, dispPos: BYTE;
		ip: INTEGER
	END;
	
	RegStack = RECORD
		ord : ARRAY 16 OF INTEGER;
		i, n : INTEGER
	END;

VAR
	out: Sys.FileHandle;

	static_buf: ARRAY 1048576 OF BYTE;
	code: ARRAY 1048576 OF BYTE;
	metacode: ARRAY 131072 OF Inst;
	staticsize, staticbase, varbase: INTEGER;
	pc*, ip: INTEGER;
	
	(* Global variables for code emitter *)
	Emit: RECORD
		i, oldi : INTEGER;
		mem : RECORD
			mod : INTEGER;
			rm, bas, idx, scl : INTEGER;
			disp : INTEGER
		END
	END;
	
	(* Procedure's current state *)
	ProcState: RECORD
		adr, prologsize, locblksize, memstack: INTEGER;
		parlist: Base.Object;
		usedRegs, usedXregs: SET
	END;
	
	(* Reg stacks *)
	reg_stacks : ARRAY 2 OF RegStack;
	xmm_stack, crs : INTEGER;
	curRegs, curXregs : SET;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)

PROCEDURE Reset_code_buffer;
BEGIN Emit.i := 0; Emit.oldi := 0; pc := 1; metacode[pc].relfixup := FALSE
END Reset_code_buffer;

PROCEDURE Put_byte (n: INTEGER);
BEGIN code[Emit.i] := n MOD 256; INC (Emit.i)
END Put_byte;

PROCEDURE Put_2bytes (n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; INC (Emit.i, 2)
END Put_2bytes;

PROCEDURE Put_4bytes (n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; n := n DIV 256;
	code[Emit.i + 2] := n MOD 256; n := n DIV 256;
	code[Emit.i + 3] := n MOD 256; INC (Emit.i, 4)
END Put_4bytes;

PROCEDURE Put_8bytes (n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; n := n DIV 256;
	code[Emit.i + 2] := n MOD 256; n := n DIV 256;
	code[Emit.i + 3] := n MOD 256; n := n DIV 256;
	code[Emit.i + 4] := n MOD 256; n := n DIV 256;
	code[Emit.i + 5] := n MOD 256; n := n DIV 256;
	code[Emit.i + 6] := n MOD 256; n := n DIV 256;
	code[Emit.i + 7] := n MOD 256; INC (Emit.i, 8)
END Put_8bytes;
	
PROCEDURE Emit_REX_prefix (reg, rsize: INTEGER);
	CONST W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1;
	VAR rex: INTEGER;
BEGIN
	rex := 40H;
	IF rsize = 8 THEN rex := rex + W_bit END;
	IF reg >= reg_R8 THEN rex := rex + R_bit END;
	IF (Emit.mem.rm >= reg_R8)
	OR (Emit.mem.mod # 3) & (Emit.mem.rm = reg_SP) & (Emit.mem.bas >= reg_R8)
	THEN rex := rex + B_bit
	END;
	IF (Emit.mem.mod # 3) & (Emit.mem.rm = reg_SP) & (Emit.mem.idx >= reg_R8)
	THEN rex := rex + X_bit
	END;
	IF (rex # 40H)
	OR (rsize = 1) & ((reg IN {reg_SP .. reg_DI})
		OR (Emit.mem.mod = 3) & (Emit.mem.rm IN {reg_SP .. reg_DI}))
	THEN Put_byte (rex)
	END
END Emit_REX_prefix;

PROCEDURE Emit_16bit_prefix (rsize : INTEGER);
BEGIN IF rsize = 2 THEN Put_byte (66H) END
END Emit_16bit_prefix;

PROCEDURE Handle_multibytes_opcode (VAR op: INTEGER);
BEGIN
	IF op MOD 256 = 0FH THEN
		Put_byte (0FH); op := op DIV 256;
		IF (op MOD 256 = 38H) OR (op MOD 256 = 3AH) THEN
			Put_byte (op); op := op DIV 256
		END
	END
END Handle_multibytes_opcode;

PROCEDURE Emit_ModRM (reg: INTEGER);
BEGIN
	Put_byte (Emit.mem.mod * 64 + reg MOD 8 * 8 + Emit.mem.rm MOD 8);
	IF Emit.mem.mod # 3 THEN
		IF Emit.mem.rm IN {reg_SP, reg_R12} THEN
			Put_byte (
				Emit.mem.scl * 64 + Emit.mem.idx MOD 8 * 8 + Emit.mem.bas MOD 8
			)
		END;
		metacode[pc].dispPos := Emit.i - Emit.oldi;
		IF (Emit.mem.mod = 0) & (Emit.mem.rm IN {reg_BP, reg_R13})
		OR (Emit.mem.mod = 2)
		THEN Put_4bytes (Emit.mem.disp)
		ELSIF Emit.mem.mod = 1 THEN Put_byte (Emit.mem.disp)
		END
	END
END Emit_ModRM;

PROCEDURE Next_inst;
BEGIN
	metacode[pc].ip := ip; metacode[pc].size := Emit.i - Emit.oldi;
	ip := ip + Emit.i - Emit.oldi; INC (pc); Emit.oldi := Emit.i;
	metacode[pc].relfixup := FALSE
END Next_inst;

(* -------------------------------------------------------------------------- *)

PROCEDURE IntToSet (n: INTEGER) : SET;
	RETURN SYSTEM.VAL(SET, n)
END IntToSet;
	
PROCEDURE EmitRegRm (op, reg, rsize: INTEGER);
	CONST w_bit = 1;
	VAR org: INTEGER;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (reg, rsize);
	org := op; Handle_multibytes_opcode (op);
	
	IF (rsize > 1) & ((org < LEA) OR (org = MOVZX) OR (org = IMUL)) THEN
		op := op + w_bit
	END;
	Put_byte (op); Emit_ModRM (reg);
	
	Next_inst
END EmitRegRm;

PROCEDURE EmitRm (op, rsize: INTEGER);
	CONST w_bit = 1;
	VAR op3bits, org: INTEGER;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	org := op; Handle_multibytes_opcode (op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF (rsize > 1) & (IntToSet(op) * IntToSet(w_bit) = {}) & (org # LDMXCSR)
		& (org # STMXCSR)
	THEN op := op + w_bit
	END;
	Put_byte (op); Emit_ModRM (op3bits);
	
	Next_inst
END EmitRm;

PROCEDURE EmitRmImm (op, rsize, imm: INTEGER);
	CONST w_bit = 1; s_bit = 2;
	VAR op3bits: INTEGER;
BEGIN
	Emit_16bit_prefix (rsize);
	IF op MOD 256 # IMULi THEN Emit_REX_prefix (0, rsize)
	ELSE Emit_REX_prefix (op DIV 256, rsize)
	END;
	Handle_multibytes_opcode (op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF rsize > 1 THEN
		IF (op = 0C0H) OR (op = 0BAH) THEN rsize := 1
		ELSIF (imm >= -128) & (imm <= 127) & (op = 80H) THEN
			op := op + s_bit; rsize := 1
		END;
		IF (IntToSet(op) * IntToSet(w_bit) = {}) & (op # 0BAH) THEN
			op := op + w_bit
		END
	END;
	Put_byte (op); Emit_ModRM (op3bits);
	
	IF rsize = 1 THEN Put_byte (imm) ELSIF rsize = 2 THEN Put_2bytes (imm)
	ELSE Put_4bytes (imm)
	END;
	
	Next_inst
END EmitRmImm;

PROCEDURE EmitBare (op: INTEGER);
BEGIN WHILE op > 0 DO Put_byte (op); op := op DIV 256 END; Next_inst
END EmitBare;

PROCEDURE EmitXmmRm (op, xreg, rsize: INTEGER);
	VAR prefix : INTEGER;
BEGIN
	prefix := op MOD 256; op := op DIV 256;
	IF prefix # 0 THEN Put_byte (prefix) END;
	Emit_REX_prefix (xreg, rsize); Handle_multibytes_opcode (op);
	Put_byte (op MOD 256); Emit_ModRM (xreg); Next_inst
END EmitXmmRm;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetRmOperand_reg (reg: INTEGER);
BEGIN Emit.mem.rm := reg; Emit.mem.mod := 3
END SetRmOperand_reg;

PROCEDURE SetRmOperand_regI (reg, disp: INTEGER);
BEGIN
	Emit.mem.rm := reg; Emit.mem.disp := disp;
	IF (disp >= -128) & (disp <= 127) THEN
		IF (disp = 0) & ~ (reg IN {reg_BP, reg_R13}) THEN Emit.mem.mod := 0
		ELSE Emit.mem.mod := 1
		END
	ELSE Emit.mem.mod := 2
	END;
	IF reg IN {reg_SP, reg_R12} THEN
		Emit.mem.bas := reg_SP; Emit.mem.idx := reg_SP; Emit.mem.scl := 0
	END
END SetRmOperand_regI;

PROCEDURE SetRmOperand_staticvar (adr: INTEGER);
BEGIN
	Emit.mem.rm := reg_BP; Emit.mem.disp := adr + staticbase;
	Emit.mem.mod := 0; metacode[pc].relfixup := TRUE
END SetRmOperand_staticvar;

PROCEDURE SetRmOperand (x : Base.Item);
BEGIN
	IF x.mode IN {Base.cVar, Base.cRef} THEN
		Emit.mem.rm := reg_BP; Emit.mem.disp := x.a;
		IF x.lev > 0 THEN
			IF (x.a >= -128) & (x.a <= 127) THEN Emit.mem.mod := 1
			ELSE Emit.mem.mod := 2
			END
		ELSE
			Emit.mem.mod := 0; metacode[pc].relfixup := TRUE;
			IF x.lev = 0 THEN Emit.mem.disp := Emit.mem.disp + varbase
			ELSE Emit.mem.disp := Emit.mem.disp + staticbase
			END
		END
	ELSIF x.mode = mRegI THEN SetRmOperand_regI (x.r, x.a)
	ELSIF x.mode = mReg THEN SetRmOperand_reg (x.r)
	ELSIF x.mode = Base.cProc THEN Emit.mem.rm := reg_BP; Emit.mem.disp := x.a;
		Emit.mem.mod := 0; metacode[pc].relfixup := TRUE;
		IF x.lev < 0 THEN Emit.mem.disp := Emit.mem.disp + staticbase END
	END
END SetRmOperand;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRR (op, reg, rsize, rm: INTEGER);
BEGIN SetRmOperand_reg (rm); EmitRegRm (op, reg, rsize)
END EmitRR;

PROCEDURE EmitRI (op, rm, rsize, imm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); IF op = IMULi THEN op := op + rm * 256 END;
	EmitRmImm (op, rsize, imm)
END EmitRI;

PROCEDURE EmitR (op, rm, rsize: INTEGER);
BEGIN SetRmOperand_reg (rm); EmitRm (op, rsize)
END EmitR;

(* -------------------------------------------------------------------------- *)

PROCEDURE MoveRI (rm, rsize, imm : INTEGER);
	CONST w_bit = 8;
	VAR op: INTEGER;
BEGIN
	SetRmOperand_reg (rm); Emit_16bit_prefix (rsize);
	Emit_REX_prefix (0, rsize); op := 0B0H + rm MOD 8;
	IF rsize > 1 THEN op := op + w_bit END; Put_byte (op); 
	IF rsize = 1 THEN Put_byte (imm) ELSIF rsize = 2 THEN Put_2bytes (imm)
	ELSIF rsize = 4 THEN Put_4bytes (imm) ELSE Put_8bytes (imm)
	END;
	Next_inst
END MoveRI;

PROCEDURE PushR (rm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (50H + rm MOD 8);
	INC (ProcState.memstack, 8); Next_inst
END PushR;

PROCEDURE PopR (rm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (58H + rm MOD 8);
	INC (ProcState.memstack, 8); Next_inst
END PopR;

PROCEDURE Branch (disp: INTEGER);
BEGIN
	Put_byte (0E9H); metacode[pc].dispPos := Emit.i - Emit.oldi;
	Put_4bytes (disp); Next_inst
END Branch;

PROCEDURE CallNear (disp: INTEGER);
BEGIN
	Put_byte (0E8H); metacode[pc].dispPos := Emit.i - Emit.oldi;
	Put_4bytes (disp); metacode[pc].relfixup := TRUE; Next_inst
END CallNear;

PROCEDURE CondBranch (cond, disp: INTEGER);
BEGIN
	Put_byte (0FH); Put_byte (80H + cond);
	metacode[pc].dispPos := Emit.i - Emit.oldi;
	Put_4bytes (disp); Next_inst
END CondBranch;

PROCEDURE SetccRm (cond: INTEGER);
BEGIN
	Emit_REX_prefix (0, 1); Put_byte (0FH); Put_byte (90H + cond);
	Emit_ModRM (0); Next_inst
END SetccRm;

PROCEDURE EmitRep (op, rsize, z: INTEGER);
	CONST w_bit = 1;
BEGIN
	Put_byte (0F2H + z); (* REP prefix *)
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	IF (rsize > 1) & (IntToSet(op) * IntToSet(w_bit) = {}) THEN INC (op, w_bit)
	END;
	Put_byte (op); Next_inst
END EmitRep;

PROCEDURE Write_to_file* (from, to: INTEGER);
	VAR	p, i, k: INTEGER;
		
	PROCEDURE Fixup_disp (p, i: INTEGER);
		VAR disp: INTEGER;
	BEGIN
		disp := 0; i := i + metacode[p].dispPos;
		SYSTEM.GET (SYSTEM.ADR(code[i]), disp);
		DEC (disp, metacode[p].ip + metacode[p].size + ProcState.prologsize);
		SYSTEM.PUT (SYSTEM.ADR(code[i]), disp)
	END Fixup_disp;
	
BEGIN (* Write_to_file *)
	i := metacode[from].ip - ProcState.adr; p := from;
	WHILE p <= to DO
		IF metacode[p].relfixup THEN Fixup_disp (p, i) END; k := 1;
		WHILE k <= metacode[p].size DO
			Sys.Write_byte (out, code[i]); INC (i); INC (k)
		END;
		INC (p)
	END
END Write_to_file;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Align* (VAR off: INTEGER; alignment: INTEGER);
BEGIN
	IF off > 0 THEN INC (off, (-off) MOD alignment)
	ELSIF off < 0 THEN DEC (off, off MOD alignment)
	END
END Align;

PROCEDURE FitInReg* (size: INTEGER) : BOOLEAN;
	RETURN (size = 1) & (size = 2) & (size = 4) & (size = 8)
END FitInReg;

PROCEDURE Fix_param_adr* (VAR adr: INTEGER);
BEGIN INC (adr, 16)
END Fix_param_adr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Register stacks *)

PROCEDURE Reset_reg_stack;
BEGIN reg_stacks[0].i := 0; crs := 0; xmm_stack := 0
END Reset_reg_stack;

PROCEDURE RegHead (offset: INTEGER) : INTEGER;
	VAR i: INTEGER;
BEGIN i := reg_stacks[crs].i;
	RETURN reg_stacks[crs].ord[i + offset]
END RegHead;

PROCEDURE Alloc_reg (VAR r: INTEGER);
	VAR i: INTEGER;
BEGIN i := reg_stacks[crs].i;
	IF i < reg_stacks[crs].n THEN INC (reg_stacks[crs].i)
	ELSE Scanner.Mark ('Compiler limit: Register stack overflow')
	END;
	r := reg_stacks[crs].ord[i]; INCL (curRegs, r);
	INCL (ProcState.usedRegs, r)
END Alloc_reg;

PROCEDURE Free_reg;
	VAR i, r: INTEGER;
BEGIN i := reg_stacks[crs].i;
	IF i > 0 THEN DEC (reg_stacks[crs].i);
		r := reg_stacks[crs].ord[i - 1]; EXCL (curRegs, r)
	ELSE Scanner.Mark ('Fault: Register stack underflow')
	END
END Free_reg;

PROCEDURE Alloc_xreg;
BEGIN
	INCL (curXregs, xmm_stack); INCL (ProcState.usedXregs, xmm_stack);
	IF xmm_stack < 16 THEN INC (xmm_stack)
	ELSE Scanner.Mark ('Compiler limit: XMM register stack overflow')
	END
END Alloc_xreg;

PROCEDURE Free_xreg;
BEGIN
	IF xmm_stack > 0 THEN DEC (xmm_stack); EXCL (curXregs, xmm_stack)
	ELSE Scanner.Mark ('Fault: XMM register stack underflow')
	END
END Free_xreg;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Clean_item* (VAR x: Base.Item);
BEGIN
	IF x.mode IN {mRegI, mReg} THEN Free_reg
	ELSIF x.mode = mXreg THEN Free_xreg
	END
END Clean_item;

PROCEDURE Make_const* (VAR x: Base.Item; tp: Base.Type; val: INTEGER);
BEGIN x.mode := Base.cConst; x.type := tp; x.a := val
END Make_const;

PROCEDURE Make_item* (VAR x: Base.Item; obj: Base.Object);
BEGIN
	x.readonly := obj.readonly; x.param := obj.param;
	x.mode := obj.class; x.lev := obj.lev;
	x.type := obj.type; x.obj := obj;
	x.a := obj.val; x.b := obj.val2; x.c := 0
END Make_item;

PROCEDURE Make_string* (VAR x: Base.Item);
	VAR i, slen: INTEGER; str: Base.String;
BEGIN
	x.b := Scanner.slen; DEC (staticsize, slen * Base.CharSize);
	x.a := staticsize; i := 0; str := Scanner.str;
	WHILE i < x.b DO
		SYSTEM.PUT (SYSTEM.ADR(staticsize) + i * Base.CharSize, str[i]);
		INC (i)
	END;
	x.mode := Base.cVar; x.type := Base.stringType; x.lev := -1
END Make_string;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Branch *)

PROCEDURE Fix_link_with (L: INTEGER; dst: INTEGER);
END Fix_link_with;

PROCEDURE Fix_link* (L: INTEGER);
END Fix_link;

PROCEDURE merged (L0, L1: INTEGER) : INTEGER;
	RETURN 0
END merged;

PROCEDURE ccCodeOf (op: INTEGER) : INTEGER;
	RETURN 0
END ccCodeOf;

PROCEDURE ccCodeOfInt (op: INTEGER) : INTEGER;
	RETURN 0
END ccCodeOfInt;

PROCEDURE Set_cond (VAR x: Base.Item; n: INTEGER);
END Set_cond;

PROCEDURE negated (cond: INTEGER) : INTEGER;
	RETURN 0
END negated;

PROCEDURE FJump* (VAR L: INTEGER);
END FJump;

PROCEDURE CFJump* (VAR x: Base.Item);
END CFJump;
	
PROCEDURE CBJump* (VAR x: Base.Item; L: INTEGER);
END CBJump;
  
PROCEDURE BJump* (L: INTEGER);
END BJump;

PROCEDURE Fixup* (x: Base.Item);
BEGIN Fix_link (x.a)
END Fixup;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Load/Store *)
(* For consistency purpose, all value with size *)
(* smaller than 64 bits are zero-extended when load to register *)

PROCEDURE Ref_to_regI (VAR x: Base.Item);
BEGIN
	Alloc_reg (x.r); SetRmOperand (x); EmitRegRm (MOVd, x.r, 8);
	x.a := x.c; x.mode := mRegI
END Ref_to_regI;

(* Untyped (not safe) load and doesn't modify Item *)
PROCEDURE Load_to_reg (reg, rsize: INTEGER; x: Base.Item);
	VAR op: INTEGER;
BEGIN
	IF x.mode IN Base.clsVariable THEN SetRmOperand (x);
		IF x.mode = Base.cRef THEN
			EmitRegRm (MOVd, reg, 8); SetRmOperand_regI (reg, x.c)
		END;
		IF rsize >= 4 THEN op := MOVd
		ELSIF rsize = 2 THEN op := MOVZX; rsize := 4
		ELSIF rsize = 1 THEN op := MOVZX
		END;
		EmitRegRm (op, reg, rsize)
	ELSIF x.mode = mImm THEN
		IF x.a = 0 THEN EmitRR (XOR, reg, 4, reg)
		ELSIF (rsize <= 4) OR (x.a > 0) & (x.a <= 0FFFFFFFFH) THEN
			MoveRI (reg, 4, x.a)
		ELSE MoveRI (reg, rsize, x.a)
		END
	ELSIF x.mode = Base.cProc THEN SetRmOperand (x);
		IF x.lev >= 0 THEN EmitRegRm (LEA, reg, 8)
		ELSE EmitRegRm (MOVd, reg, 8)
		END
	ELSIF x.mode = mReg THEN EmitRR (MOVd, reg, rsize, x.r)
	END
END Load_to_reg;

PROCEDURE fpload (VAR x: Base.Item);
	VAR r: INTEGER;
BEGIN
	IF x.mode # mXreg THEN
		IF x.mode = Base.cRef THEN Ref_to_regI (x) END; Alloc_xreg;
		IF x.mode # mImm THEN
			SetRmOperand (x); EmitXmmRm (VMOVSS, xmm_stack - 1, 4);
			IF x.mode = mRegI THEN Free_reg END
		ELSIF x.a = 0 THEN SetRmOperand_reg (xmm_stack - 1);
			EmitXmmRm (VXORPS, xmm_stack - 1, 4)
		ELSE Alloc_reg (r); Load_to_reg (r, 4, x); SetRmOperand_reg (r);
			EmitXmmRm (VMOVDQ, xmm_stack - 1, 4); Free_reg
		END;
		x.r := xmm_stack - 1; x.mode := mXreg
	END
END fpload;

PROCEDURE load* (VAR x: Base.Item);
	VAR rsize, L : INTEGER;
BEGIN rsize := x.type.size;
	IF x.type # Base.realType THEN
		IF x.mode # mReg THEN
			IF x.mode # mCond THEN
				IF x.mode # mRegI THEN Alloc_reg (x.r) END;
				Load_to_reg (x.r, rsize, x)
			ELSE Alloc_reg (x.r);
				IF (x.a = 0) & (x.b = 0) THEN
					IF (x.c < 16) & (x.c >= 0) THEN SetRmOperand_reg (x.r);
						SetccRm (x.c); EmitRR (MOVZX, x.r, 1, x.r)
					ELSIF x.c = ccAlways THEN MoveRI (x.r, 4, 1)
					ELSIF x.c = ccNever THEN EmitRR (XOR, x.r, 4, x.r)
					END
				ELSE
					L := pc; CondBranch (negated(x.c), 0); Fix_link (x.b);
					MoveRI (x.r, 4, 1);
					Branch (0); Fix_link (L); Fix_link (x.a);
					L := pc - 1; EmitRR (XOR, x.r, 4, x.r); Fix_link (L)
				END
			END;
			x.mode := mReg
		END
	ELSE fpload (x)
	END
END load;

PROCEDURE Str_to_char* (VAR x: Base.Item);
	VAR ch: CHAR;
BEGIN
	IF x.lev = -1 THEN
		SYSTEM.GET (SYSTEM.ADR(static_buf[LEN(static_buf) + x.a]), ch);
		x.a := ORD(ch); x.mode := mImm; x.type := Base.charType
	ELSIF x.lev = -2 THEN (* stub *)
	END
END Str_to_char;

PROCEDURE Load_adr_to_reg (reg: INTEGER; x: Base.Item);
BEGIN SetRmOperand (x);
	IF x.mode = Base.cVar THEN EmitRegRm (LEA, reg, 8)
	ELSIF x.mode = Base.cRef THEN EmitRegRm (MOVd, reg, 8);
		IF x.c # 0 THEN EmitRI (ADDi, reg, 8, x.c) END
	ELSIF x.mode = mRegI THEN
		IF reg # x.r THEN EmitRR (MOVd, reg, 8, x.r) END;
		IF x.a # 0 THEN EmitRI (ADDi, reg, 8, x.a) END
	END
END Load_adr_to_reg;

PROCEDURE Load_adr (VAR x: Base.Item);
BEGIN
	IF x.mode # mRegI THEN Alloc_reg (x.r) END;
	Load_adr_to_reg (x.r, x); x.mode := mReg
END Load_adr;

PROCEDURE Store* (VAR x, y: Base.Item);
BEGIN load (y);
	IF x.mode = Base.cRef THEN Ref_to_regI (x) END; SetRmOperand (x);
	IF x.type # Base.realType THEN EmitRegRm (MOV, y.r, x.type.size)
	ELSE EmitXmmRm (VMOVSSd, y.r, x.type.size)
	END;
	IF x.mode = mRegI THEN Free_reg END;
	IF y.mode = mReg THEN Free_reg ELSE Free_xreg END
END Store;

PROCEDURE Store_struct* (VAR x, y: Base.Item);
	VAR count, rsize: INTEGER;
BEGIN count := x.type.size;
	Load_adr_to_reg (reg_DI, x); Load_adr_to_reg (reg_SI, y);
	IF count MOD 8 = 0 THEN count := count DIV 8; rsize := 8
	ELSIF count MOD 4 = 0 THEN count := count DIV 4; rsize := 4 ELSE rsize := 1
	END;
	MoveRI (reg_C, 4, count); EmitRep (MOVSrep, rsize, 1);
	IF y.mode = mRegI THEN Free_reg END; IF x.mode = mRegI THEN Free_reg END;
	ProcState.usedRegs := ProcState.usedRegs + {reg_DI, reg_SI}
END Store_struct;

PROCEDURE Store_string* (VAR x, y: Base.Item);
	(* stub *)
END Store_string;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Not* (VAR x: Base.Item);
END Not;

PROCEDURE Negate* (VAR x: Base.Item);
END Negate;

PROCEDURE Int_op1* (op: INTEGER; VAR x, y: Base.Item);
END Int_op1;

PROCEDURE Int_op2* (op: INTEGER; VAR x, y: Base.Item);
END Int_op2;

PROCEDURE Real_op1* (op: INTEGER; VAR x, y: Base.Item);
END Real_op1;

PROCEDURE Real_op2* (op: INTEGER; VAR x, y: Base.Item);
END Real_op2;

PROCEDURE Set_op1* (op: INTEGER; VAR x, y: Base.Item);
END Set_op1;

PROCEDURE Set_op2* (op: INTEGER; VAR x, y: Base.Item);
END Set_op2;

PROCEDURE Or1* (VAR x: Base.Item);
END Or1;

PROCEDURE Or2* (VAR x, y: Base.Item);
END Or2;

PROCEDURE And1* (VAR x: Base.Item);
END And1;

PROCEDURE And2* (VAR x, y: Base.Item);
END And2;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Int_relation* (rel: INTEGER; VAR x, y: Base.Item);
END Int_relation;

PROCEDURE Real_relation* (rel: INTEGER; VAR x, y: Base.Item);
END Real_relation;

PROCEDURE Set_relation* (rel: INTEGER; VAR x, y: Base.Item);
END Set_relation;

PROCEDURE String_relation* (rel: INTEGER; VAR x, y: Base.Item);
END String_relation;

PROCEDURE Member_test* (VAR x, y: Base.Item);
END Member_test;

PROCEDURE Type_test* (VAR x: Base.Item; tp: Base.Type; guard: BOOLEAN);
END Type_test;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x: Base.Item);
END Set1;

PROCEDURE Set2* (VAR x, y: Base.Item);
END Set2;

PROCEDURE Set3* (VAR x, y, z: Base.Item);
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* selector *)

PROCEDURE Deref* (VAR x: Base.Item);
END Deref;

PROCEDURE Field* (VAR x: Base.Item; field: Base.Object);
END Field;

PROCEDURE Index* (VAR x, y: Base.Item);
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE For1* (VAR z: Base.Item);
END For1;

PROCEDURE For2* (VAR x, z: Base.Item);
END For2;

PROCEDURE For3* (VAR x, z: Base.Item; n, L: INTEGER);
END For3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Procedure call *)

PROCEDURE Prepare_to_call* (VAR x: Base.Item; VAR call: ProcCall);
END Prepare_to_call;

PROCEDURE Call* (VAR call: ProcCall);
END Call;

PROCEDURE Return_value* (VAR x: Base.Item);
BEGIN
END Return_value;

PROCEDURE Value_param* (VAR x: Base.Item; VAR call: ProcCall);
END Value_param;

PROCEDURE Ref_param* (VAR x: Base.Item; VAR call: ProcCall);
END Ref_param;

PROCEDURE Record_param* (VAR x: Base.Item; VAR call: ProcCall);
END Record_param;

PROCEDURE Array_param* (VAR x: Base.Item; VAR call: ProcCall);
END Array_param;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Enter* (proc: Base.Object; locblksize: INTEGER);
BEGIN
	Reset_code_buffer
END Enter;

PROCEDURE Return*;
BEGIN Write_to_file (1, pc - 1)
END Return;

PROCEDURE Module_init*;
END Module_init;

PROCEDURE Module_exit*;
END Module_exit;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init* (modid: Base.IdentStr);
BEGIN
	staticsize := 0; staticbase := -24; Sys.Rewrite (out, tempOutputName);
END Init;

PROCEDURE Finish*;
BEGIN Sys.Close (out)
END Finish;

BEGIN
	reg_stacks[0].i := 0;
	reg_stacks[0].n := 11;
	reg_stacks[0].ord[0] := reg_A;
	reg_stacks[0].ord[1] := reg_C;
	reg_stacks[0].ord[2] := reg_D;
	reg_stacks[0].ord[3] := reg_R8;
	reg_stacks[0].ord[4] := reg_R9;
	reg_stacks[0].ord[5] := reg_R10;
	reg_stacks[0].ord[6] := reg_R11;
	reg_stacks[0].ord[7] := reg_R12;
	reg_stacks[0].ord[8] := reg_R13;
	reg_stacks[0].ord[9] := reg_R14;
	reg_stacks[0].ord[10] := reg_R15;
	
	reg_stacks[1].i := 0;
	reg_stacks[1].n := 11;
	reg_stacks[1].ord[0] := reg_C;
	reg_stacks[1].ord[1] := reg_D;
	reg_stacks[1].ord[2] := reg_R8;
	reg_stacks[1].ord[3] := reg_R9;
	reg_stacks[1].ord[4] := reg_A;
	reg_stacks[1].ord[5] := reg_R10;
	reg_stacks[1].ord[6] := reg_R11;
	reg_stacks[1].ord[7] := reg_R12;
	reg_stacks[1].ord[8] := reg_R13;
	reg_stacks[1].ord[9] := reg_R14;
	reg_stacks[1].ord[10] := reg_R15
END Generator.