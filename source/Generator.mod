MODULE Generator;

IMPORT
	SYSTEM, Strings, Console, Sys, Base, Scanner, SymTable;
	
CONST
	MaxInt = Base.MaxInt;
	MinInt = Base.MinInt;
	MaxInt32 = 2147483647;
	MinInt32 = -2147483648;

	tempOutputName = 'output.temp_';
	tempSymName = 'sym.temp_';
	dividedByNonPositiveError = 'Divided by non-positive number';
	arrayIndexError = 'Array index out of bound';
	
	divide_trap = 0;
	array_trap = 1;
	type_trap = 2;
	assert_trap = 3;
	nil_trap = 4;
	modkey_trap = 5;
	overflow_trap = 6;

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
	ccC = ccB; ccNC = ccAE;

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
	SeeMOVD = 6E0F66H; SeeMOVDd = 7E0F66H; MOVSS = 100FF3H; MOVSSd = 110FF3H;
	ADDSS = 580FF3H; MULSS = 590FF3H; SUBSS = 5C0FF3H; DIVSS = 5E0FF3H;
	ADDPS = 580F00H; MULPS = 590F00H; SUBPS = 5C0F00H; DIVPS = 5E0F00H;
	ANDPS = 540F00H; ANDNPS = 550F00H; ORPS = 560F00H; XORPS = 570F00H;
	MOVAPS = 280F00H; MOVAPSd = 290F00H;
	COMISS = 2F0F00H;
	CVTSS2SI = 2D0FF3H; CVTSI2SS = 2A0FF3H;
	
TYPE
	Inst = RECORD
		relfixup: BOOLEAN;
		size, dispPos: BYTE;
		ip: INTEGER
	END;
	
	RegAllocOrder = RECORD
		ord: ARRAY 16 OF INTEGER;
		numOfRegs: INTEGER
	END;
	
	RegStack = RECORD
		allocOrd, rh: INTEGER
	END;
	
	ProcCall* = RECORD
		oldRegStack: RegStack;
		oldXregStack: INTEGER;
		oldCurRegs, oldCurXregs: SET;
		
		proc: Base.Item;
		parOff, parblksize: INTEGER;
		
		rtype*: Base.Type; fpar*: Base.Object;
		nofact*, nofpar*: INTEGER
	END;
	
	FixupList = POINTER TO FixupListDesc;
	FixupListDesc = RECORD
		loc, val: INTEGER;
		next: FixupList
	END;

VAR
	out: Sys.FileHandle;
	modid: Base.IdentStr;

	staticBuf: ARRAY 1048576 OF BYTE;
	code: ARRAY 1048576 OF BYTE;
	metacode: ARRAY 131072 OF Inst;
	staticsize, staticbase, varsize, varbase: INTEGER;
	pc*, ip*: INTEGER;
	fixupList: FixupList;
	
	(* Global variables for code emitter *)
	Emit: RECORD
		i, oldi: INTEGER;
		mem: RECORD
			mod: INTEGER;
			rm, bas, idx, scl: INTEGER;
			disp: INTEGER
		END
	END;
	
	(* Procedure's current state *)
	ProcState: RECORD
		adr, prologsize, locblksize, memstack: INTEGER;
		parlist: Base.Object;
		usedRegs, usedXregs: SET
	END;
	
	(* Reg stacks *)
	regAllocOrds: ARRAY 2 OF RegAllocOrder;
	reg_stack: RegStack;
	xmm_stack: INTEGER;
	curRegs, curXregs: SET;
	
	(* Linker state *)
	Linker: RECORD
		imagebase, entry : INTEGER;
		code_rawsize, data_rawsize, edata_rawsize : INTEGER;
		code_size, data_size, edata_size : INTEGER;
		code_rva, data_rva, idata_rva, reloc_rva, edata_rva : INTEGER;
		code_fadr, data_fadr, idata_fadr, reloc_fadr, edata_fadr : INTEGER;
		Kernel32Table: ARRAY 7 OF INTEGER
	END;
	
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
	code[Emit.i + 1] := n MOD 256; Emit.i := Emit.i + 2
END Put_2bytes;

PROCEDURE Put_4bytes (n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; n := n DIV 256;
	code[Emit.i + 2] := n MOD 256; n := n DIV 256;
	code[Emit.i + 3] := n MOD 256; Emit.i := Emit.i + 4
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
	code[Emit.i + 7] := n MOD 256; Emit.i := Emit.i + 8
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
	
	IF (rsize > 1) & ((org < LEA) OR (org = MOVZX)) THEN
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
	ProcState.memstack := ProcState.memstack + 8; Next_inst
END PushR;

PROCEDURE PopR (rm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (58H + rm MOD 8);
	ProcState.memstack := ProcState.memstack - 8; Next_inst
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
	IF (rsize > 1) & (IntToSet(op) * IntToSet(w_bit) = {}) THEN
		op := op + w_bit
	END;
	Put_byte (op); Next_inst
END EmitRep;

PROCEDURE Write_to_file* (from, to: INTEGER);
	VAR	p, i, k: INTEGER;
		
	PROCEDURE Fixup_disp (p, i: INTEGER);
		VAR disp: SYSTEM.DWORD;
	BEGIN
		disp := 0; i := i + metacode[p].dispPos;
		SYSTEM.GET (SYSTEM.ADR(code[i]), disp);
		disp := disp - metacode[p].ip - metacode[p].size - ProcState.prologsize;
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
(* Misc *)
	
PROCEDURE Align* (VAR off: INTEGER; alignment: INTEGER);
BEGIN
	IF off > 0 THEN off := off + (-off) MOD alignment
	ELSIF off < 0 THEN off := off - off MOD alignment
	END
END Align;

PROCEDURE FitInReg* (size: INTEGER) : BOOLEAN;
	RETURN (size = 1) OR (size = 2) OR (size = 4) OR (size = 8)
END FitInReg;

PROCEDURE Fix_param_adr* (VAR adr: INTEGER);
BEGIN adr := adr + 16
END Fix_param_adr;

PROCEDURE SmallConst (x: Base.Item) : BOOLEAN;
	RETURN (x.mode = mImm) & (x.a >= MinInt32) & (x.a <= MaxInt32)
END SmallConst;

PROCEDURE log2 (n: INTEGER) : INTEGER;
	VAR e: INTEGER;
BEGIN e := 0;
	IF n > 1 THEN
		WHILE n > 1 DO
			IF ODD(n) THEN e := -1; n := 0 ELSE INC (e); n := n DIV 2 END 
		END
	ELSIF n # 1 THEN e := -1
	END;
	RETURN e
END log2;

PROCEDURE ParamSize (obj: Base.Object) : INTEGER;
	VAR result: INTEGER;
BEGIN
	IF (obj.type.form = Base.tArray) & (obj.type.len = 0) THEN
		result := obj.type.size
	ELSIF ~obj.readonly & (obj.class = Base.cRef)
		& (obj.type.form = Base.tRecord)
	THEN result := 16
	ELSE result := 8
	END;
	RETURN result
END ParamSize;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Register stacks *)

PROCEDURE Reset_reg_stack;
BEGIN
	reg_stack.rh := 0; reg_stack.allocOrd := 0; xmm_stack := 0;
	curRegs := {}; curXregs := {}
END Reset_reg_stack;

PROCEDURE RegHead (offset: INTEGER) : INTEGER;
	RETURN regAllocOrds[reg_stack.allocOrd].ord[reg_stack.rh + offset]
END RegHead;

PROCEDURE Alloc_reg (VAR r: INTEGER);
	VAR i: INTEGER;
BEGIN i := reg_stack.allocOrd; r := regAllocOrds[i].ord[reg_stack.rh];
	IF reg_stack.rh < regAllocOrds[i].numOfRegs THEN
		INC (reg_stack.rh); curRegs := curRegs + {r};
		ProcState.usedRegs := ProcState.usedRegs + {r}
	ELSE Scanner.Mark ('Compiler limit: Register stack overflow')
	END
END Alloc_reg;

PROCEDURE Free_reg;
	VAR i, r: INTEGER;
BEGIN
	IF reg_stack.rh > 0 THEN DEC (reg_stack.rh); i := reg_stack.allocOrd;
		r := regAllocOrds[i].ord[reg_stack.rh]; curRegs := curRegs - {r}
	ELSE Scanner.Mark ('Fault: Register stack underflow')
	END
END Free_reg;

PROCEDURE Alloc_xreg;
BEGIN
	curXregs := curXregs + {xmm_stack};
	ProcState.usedXregs := ProcState.usedXregs + {xmm_stack};
	IF xmm_stack < 16 THEN INC (xmm_stack)
	ELSE Scanner.Mark ('Compiler limit: XMM register stack overflow')
	END
END Alloc_xreg;

PROCEDURE Free_xreg;
BEGIN
	IF xmm_stack > 0 THEN DEC (xmm_stack); curXregs := curXregs - {xmm_stack}
	ELSE Scanner.Mark ('Fault: XMM register stack underflow')
	END
END Free_xreg;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Item *)
	
PROCEDURE Clean_item* (VAR x: Base.Item);
BEGIN
	IF x.mode IN {mRegI, mReg} THEN Free_reg
	ELSIF x.mode = mXreg THEN Free_xreg
	END
END Clean_item;

PROCEDURE Alloc_static_data (size, alignment: INTEGER);
BEGIN
	IF staticsize + size <= LEN(staticBuf) THEN
		staticsize := staticsize + size; Align (staticsize, alignment)
	ELSE Scanner.Mark ('The limit for static data is reached.')
	END
END Alloc_static_data;

PROCEDURE Make_const* (VAR x: Base.Item; tp: Base.Type; val: INTEGER);
BEGIN x.mode := Base.cConst; x.type := tp; x.a := val
END Make_const;

PROCEDURE Make_item* (VAR x: Base.Item; obj: Base.Object);
BEGIN
	IF (obj.lev = -2) & (obj.val = 0) & (obj.expno # 0) THEN
		Alloc_static_data (8, 8); obj.val := -staticsize
	END;
	x.readonly := obj.readonly; x.param := obj.param;
	x.mode := obj.class; x.lev := obj.lev;
	x.type := obj.type; x.obj := obj;
	x.a := obj.val; x.b := obj.val2; x.c := 0
END Make_item;

PROCEDURE Make_string* (VAR x: Base.Item; str: ARRAY OF CHAR; slen: INTEGER);
	CONST chSz = Base.CharSize;
	VAR i, adr: INTEGER;
BEGIN x.b := slen; Alloc_static_data (x.b * chSz, chSz);
	adr := SYSTEM.ADR(staticBuf) + LEN(staticBuf) - staticsize;
	x.a := -staticsize; i := 0;
	WHILE i < slen DO SYSTEM.PUT (adr + i * chSz, str[i]); INC (i) END;
	x.mode := Base.cVar; x.type := Base.stringType; x.lev := -1
END Make_string;

PROCEDURE Get_array_length (VAR len: Base.Item; array: Base.Item);
	VAR n: INTEGER;
BEGIN
	IF array.type.form = Base.tArray THEN n := array.type.len;
		IF n > 0 THEN Make_const (len, Base.intType, n)
		ELSIF n = 0 THEN len.mode := Base.cVar; len.lev := array.lev;
			len.type := Base.intType; len.a := array.b
		ELSE ASSERT(FALSE)
		END
	ELSIF array.type.form = Base.tString THEN
		Make_const (len, Base.intType, array.b)
	ELSE ASSERT(FALSE)
	END
END Get_array_length;

PROCEDURE Alloc_temp_var (VAR x: Base.Item; tp: Base.Type);
BEGIN
	ProcState.locblksize := ProcState.locblksize + tp.size;
	Align (ProcState.locblksize, tp.alignment);
	x.mode := Base.cVar; x.lev := 1; x.type := tp; x.a := -ProcState.locblksize
END Alloc_temp_var;

PROCEDURE Alloc_typedesc* (type: Base.Type; obj: Base.Object);
	VAR tdsize, i, n: INTEGER;
BEGIN tdsize := 8 + 8 * (Base.MaxExtension - 1) + 4 * (type.nptr + 1);
	Alloc_static_data (tdsize, 8); obj.val := -staticsize
END Alloc_typedesc;

PROCEDURE Get_typedesc (VAR x: Base.Item);
BEGIN x.type := Base.intType;
	IF x.lev = 0 THEN x.mode := Base.cVar; x.lev := -1
	ELSIF x.lev = -2 THEN x.mode := Base.cRef; x.c := 0
	ELSE ASSERT(FALSE)
	END
END Get_typedesc;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Fixup & Branch cond *)

PROCEDURE Fix_link_with (L, dst: SYSTEM.DWORD);
	VAR i, dispPos, size: INTEGER; n: SYSTEM.DWORD;
BEGIN
	WHILE L # 0 DO
		size := metacode[L].size; dispPos := metacode[L].dispPos;
		i := metacode[L].ip; n := dst - i - size;
		i := i - ProcState.adr + dispPos;
		SYSTEM.GET (SYSTEM.ADR(code[i]), L);
		SYSTEM.PUT (SYSTEM.ADR(code[i]), n)
	END
END Fix_link_with;

PROCEDURE Fix_link* (L: INTEGER);
BEGIN Fix_link_with (L, ip)
END Fix_link;

PROCEDURE merged (L0, L1: SYSTEM.DWORD) : INTEGER;
	VAR L2, L3: SYSTEM.DWORD; size, i, dispPos: INTEGER;
BEGIN 
	IF L0 # 0 THEN L3 := L0;
		REPEAT L2 := L3; size := metacode[L2].size;
			dispPos := metacode[L2].dispPos;
			i := metacode[L2].ip - ProcState.adr + dispPos;
			SYSTEM.GET (SYSTEM.ADR(code[i]), L3)
		UNTIL L3 = 0;
		SYSTEM.PUT (SYSTEM.ADR(code[i]), L1); L1 := L0
	END;
    RETURN L1
END merged;

PROCEDURE ccCodeOf (op: INTEGER) : INTEGER;
BEGIN
	IF op = Scanner.eql THEN op := ccZ
	ELSIF op = Scanner.neq THEN op := ccNZ
	ELSIF op = Scanner.lss THEN op := ccB
	ELSIF op = Scanner.gtr THEN op := ccA
	ELSIF op = Scanner.leq THEN op := ccBE
	ELSE op := ccAE
	END;
	RETURN op
END ccCodeOf;

PROCEDURE ccCodeOfInt (op: INTEGER) : INTEGER;
BEGIN
	IF op = Scanner.eql THEN op := ccZ
	ELSIF op = Scanner.neq THEN op := ccNZ
	ELSIF op = Scanner.lss THEN op := ccL
	ELSIF op = Scanner.gtr THEN op := ccG
	ELSIF op = Scanner.leq THEN op := ccLE
	ELSE op := ccGE
	END;
	RETURN op
END ccCodeOfInt;

PROCEDURE Fixup* (x: Base.Item);
BEGIN Fix_link (x.a)
END Fixup;

PROCEDURE Set_cond (VAR x: Base.Item; n: INTEGER);
BEGIN x.mode := mCond; x.a := 0; x.b := 0; x.c := n
END Set_cond;

PROCEDURE negated (cond: INTEGER) : INTEGER;
BEGIN
	IF ODD(cond) THEN DEC (cond) ELSE INC (cond) END;
	RETURN cond
END negated;

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
	ELSIF x.mode = mXreg THEN
		SetRmOperand_reg (reg); EmitXmmRm (SeeMOVDd, x.r, rsize)
	END
END Load_to_reg;

(* Untyped (not safe) fpload and doesn't modify Item *)
PROCEDURE Load_to_xreg (xreg: INTEGER; x: Base.Item);
	VAR r: INTEGER; y: Base.Item;
BEGIN
	IF x.mode = mImm THEN
		IF x.a = 0 THEN SetRmOperand_reg (xreg); EmitXmmRm (XORPS, xreg, 4)
		ELSE Alloc_reg (r); MoveRI (r, 4, x.a);
			SetRmOperand_reg (r); EmitXmmRm (SeeMOVD, xreg, 4); Free_reg
		END
	ELSIF x.mode = mReg THEN
		SetRmOperand_reg (x.r); EmitXmmRm (SeeMOVD, xreg, 4)
	ELSIF x.mode = mXreg THEN
		SetRmOperand_reg (x.r); EmitXmmRm (MOVSS, xreg, 4)
	ELSIF x.mode IN modeMem THEN y := x;
		IF y.mode = Base.cRef THEN Ref_to_regI (y) END;
		SetRmOperand (y); EmitXmmRm (MOVSS, xreg, 4);
		IF y.mode # x.mode (* ref *) THEN Free_reg END
	ELSE ASSERT(FALSE)
	END;
END Load_to_xreg;

PROCEDURE fpload (VAR x: Base.Item);
	VAR r: INTEGER;
BEGIN
	IF x.mode # mXreg THEN
		Alloc_xreg; Load_to_xreg (xmm_stack - 1, x);
		IF x.mode IN {mReg, mRegI} THEN Free_reg END;
		x.r := xmm_stack - 1; x.mode := mXreg
	END
END fpload;

PROCEDURE load* (VAR x: Base.Item);
	VAR rsize, L: INTEGER;
BEGIN rsize := x.type.size;
	IF x.type # Base.realType THEN
		IF x.mode # mReg THEN
			IF x.mode # mCond THEN
				IF x.mode # mRegI THEN Alloc_reg (x.r) END;
				Load_to_reg (x.r, rsize, x)
			ELSE Alloc_reg (x.r);
				IF (x.a = 0) & (x.b = 0) THEN
					IF (x.c < 16) & (x.c >= 0) THEN
						SetRmOperand_reg (x.r); SetccRm (x.c);
						EmitRR (MOVZX, x.r, 1, x.r)
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

PROCEDURE Load_to_new_reg* (VAR x: Base.Item);
	VAR r: INTEGER;
BEGIN
	IF x.type # Base.realType THEN
		IF x.mode IN {mReg, mRegI, mXreg} THEN Alloc_reg (r);
			Load_to_reg (r, x.type.size, x); x.mode := mReg; x.r := r
		ELSE load (x)
		END
	ELSE
		IF x.mode IN {mReg, mRegI, mXreg} THEN
			Alloc_xreg; Load_to_xreg (xmm_stack - 1, x);
			x.mode := mXreg; x.r := xmm_stack - 1
		ELSE fpload (x)
		END
	END
END Load_to_new_reg;

PROCEDURE Str_to_char* (VAR x: Base.Item);
	VAR ch: CHAR;
BEGIN
	IF x.lev = -1 THEN
		SYSTEM.GET (SYSTEM.ADR(staticBuf) + LEN(staticBuf) + x.a, ch);
		x.a := ORD(ch); x.mode := mImm; x.type := Base.charType
	ELSIF x.lev = -2 THEN
		x.type := Base.charType; load (x)
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

(* Load boolean value to condition flag *)
PROCEDURE Load_cond (VAR x: Base.Item);
BEGIN
	IF x.mode # mCond THEN
		IF x.mode = mImm THEN Set_cond (x, ccNever - x.a)
		ELSE load (x); EmitRR (TEST, x.r, 4, x.r); Free_reg; Set_cond (x, ccNZ)
		END
	END
END Load_cond;

PROCEDURE Store* (VAR x, y: Base.Item);
BEGIN load (y);
	IF x.mode = Base.cRef THEN Ref_to_regI (x) END; SetRmOperand (x);
	IF x.type # Base.realType THEN EmitRegRm (MOV, y.r, x.type.size)
	ELSE EmitXmmRm (MOVSSd, y.r, x.type.size)
	END;
	IF x.mode = mRegI THEN Free_reg END;
	IF y.mode = mReg THEN Free_reg ELSE Free_xreg END
END Store;

PROCEDURE Multibytes_store (x, y: Base.Item; count: INTEGER);
	VAR rsize: INTEGER;
BEGIN
	IF count MOD 8 = 0 THEN count := count DIV 8; rsize := 8
	ELSIF count MOD 4 = 0 THEN count := count DIV 4; rsize := 4 ELSE rsize := 1
	END;
	Load_adr_to_reg (reg_DI, x); Load_adr_to_reg (reg_SI, y);
	MoveRI (reg_C, 4, count); EmitRep (MOVSrep, rsize, 1);
	ProcState.usedRegs := ProcState.usedRegs + {reg_DI, reg_SI}
END Multibytes_store;

PROCEDURE Store_struct* (VAR x, y: Base.Item);
BEGIN
	Multibytes_store (x, y, x.type.size);
	IF y.mode = mRegI THEN Free_reg END;
	IF x.mode = mRegI THEN Free_reg END
END Store_struct;

PROCEDURE Store_string* (VAR x, y: Base.Item);
BEGIN
	IF y.b <= x.type.len THEN Multibytes_store (x, y, y.b * Base.CharSize)
	ELSIF y.b = x.type.len + 1 THEN Multibytes_store (x, y, x.type.size)
	ELSE Scanner.Mark ('String too long')
	END
END Store_string;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Branch *)

PROCEDURE FJump* (VAR L: INTEGER);
BEGIN Branch (L); L := pc - 1
END FJump;

PROCEDURE CFJump* (VAR x: Base.Item);
	VAR cond: INTEGER;
BEGIN Load_cond (x); cond := negated(x.c);
	IF cond < 16 THEN CondBranch (cond, x.a); x.a := pc - 1
	ELSIF cond = ccAlways THEN Branch (x.a); x.a := pc - 1
	END;
	Fix_link (x.b)
END CFJump;
	
PROCEDURE CBJump* (VAR x: Base.Item; L: INTEGER);
	VAR cond: INTEGER;
BEGIN Load_cond (x); cond := negated(x.c);
	IF cond < 16 THEN CondBranch (cond, metacode[L].ip - ip - 6)
	ELSIF cond = ccAlways THEN Branch (metacode[L].ip - ip - 5)
	END;
	Fix_link (x.b); Fix_link_with (x.a, metacode[L].ip)
END CBJump;
  
PROCEDURE BJump* (L: INTEGER);
BEGIN Branch (metacode[L].ip - ip - 5)
END BJump;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Trap *)

PROCEDURE Trap (cond, trapno: INTEGER);
	VAR L: INTEGER;
BEGIN
	IF ~(cond IN {ccAlways, ccNever}) THEN
		L := pc; CondBranch (cond, 0); Branch (0); Fix_link (L); L := pc - 1;
		MoveRI (reg_A, 1, trapno); MoveRI (reg_C, 4, Scanner.Pos());
		EmitBare (INT3); Fix_link (L)
	ELSIF cond = ccAlways THEN
		MoveRI (reg_A, 1, trapno);
		MoveRI (reg_C, 4, Scanner.Pos());
		EmitBare (INT3)
	END
END Trap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Arithmetic *)

PROCEDURE Not* (VAR x: Base.Item);
	VAR t: INTEGER;
BEGIN
	IF x.mode = mImm THEN x.a := (x.a + 1) MOD 2
	ELSE Load_cond (x); x.c := negated(x.c); t := x.a; x.a := x.b; x.b := t
	END
END Not;

PROCEDURE Negate* (VAR x: Base.Item);
BEGIN
	IF x.mode = mImm THEN
		IF x.type.form = Base.tInteger THEN x.a := -x.a
		ELSIF x.type = Base.setType THEN x.a := ORD(-SYSTEM.VAL(SET, x.a))
		ELSIF x.type = Base.realType THEN
			x.a := SYSTEM.VAL(INTEGER, -SYSTEM.VAL(REAL, x.a))
		END
	ELSE load (x);
		IF x.type.form = Base.tInteger THEN
			EmitR (NEG, x.r, 8); x.type := Base.intType;
			IF Base.CplFlag.overflowCheck THEN Trap (ccO, overflow_trap) END
		ELSIF x.type = Base.setType THEN EmitR (NOT, x.r, 8)
		ELSIF x.type = Base.realType THEN
			Alloc_xreg; SetRmOperand_reg (xmm_stack - 1);
			EmitXmmRm (XORPS, xmm_stack - 1, 4);
			SetRmOperand_reg (x.r); EmitXmmRm (SUBSS, xmm_stack - 1, 4);
			SetRmOperand_reg (xmm_stack - 1); EmitXmmRm (MOVSS, x.r, 4);
			Free_xreg
		END
	END
END Negate;

PROCEDURE Add* (op: INTEGER; VAR x, y: Base.Item);
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		IF op = Scanner.plus THEN x.a := x.a + y.a
		ELSIF op = Scanner.minus THEN x.a := x.a - y.a
		ELSE ASSERT(FALSE)
		END
	ELSIF op = Scanner.plus THEN
		IF SmallConst(y) THEN load (x); EmitRI (ADDi, x.r, 8, y.a)
		ELSIF ~SmallConst(x) THEN load (x); load (y); x.r := RegHead(-2);
			EmitRR (ADDd, x.r, 8, RegHead(-1)); Free_reg
		ELSE load (y); EmitRI (ADDi, y.r, 8, x.a); x.mode := mReg; x.r := y.r
		END;
		IF Base.CplFlag.overflowCheck THEN Trap (ccO, overflow_trap) END
	ELSIF op = Scanner.minus THEN
		IF x.mode = mReg THEN
			IF SmallConst(y) THEN EmitRI (SUBi, x.r, 8, y.a)
			ELSE load (y); EmitRR (SUBd, x.r, 8, y.r); Free_reg
			END
		ELSE load (x); load (y);
			IF x.r # RegHead(-2) THEN EmitRR (XCHG, x.r, 8, y.r); x.r := y.r
			END;
			EmitRR (SUBd, x.r, 8, RegHead(-1)); Free_reg
		END;
		IF Base.CplFlag.overflowCheck THEN Trap (ccO, overflow_trap) END
	ELSE ASSERT(FALSE)
	END;
	x.type := Base.intType
END Add;

PROCEDURE Multiply* (VAR x, y: Base.Item; noCheck: BOOLEAN);
	VAR e: INTEGER;
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN x.a := x.a * y.a
	ELSIF x.mode = mImm THEN load (y);
		IF Base.CplFlag.overflowCheck & ~ noCheck THEN
			EmitRI (IMULi, y.r, 8, x.a); Trap (ccO, overflow_trap)
		ELSE e := log2(x.a);
			IF e > 0 THEN EmitRI (SHLi, y.r, 8, e)
			ELSIF e < 0 THEN EmitRI (IMULi, y.r, 8, x.a)
			END
		END;
		x.mode := mReg; x.r := y.r
	ELSIF y.mode = mImm THEN load (x);
		IF Base.CplFlag.overflowCheck & ~ noCheck THEN
			EmitRI (IMULi, x.r, 8, y.a); Trap (ccO, overflow_trap)
		ELSE e := log2(y.a);
			IF e > 0 THEN EmitRI (SHLi, x.r, 8, e)
			ELSIF e < 0 THEN EmitRI (IMULi, x.r, 8, y.a)
			END
		END
	ELSE load (x); load (y); x.r := RegHead(-2);
		EmitRR (IMUL, x.r, 8, RegHead(-1)); Free_reg;
		IF Base.CplFlag.overflowCheck & ~ noCheck THEN 
			Trap (ccO, overflow_trap)
		END
	END
END Multiply;

(* One of the most complex procedure in this module... *)
PROCEDURE Divide* (op: INTEGER; VAR x, y: Base.Item);
	VAR dst, r, r2, L: INTEGER; saveA, saveD: BOOLEAN;
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		IF y.a > 0 THEN x.a := x.a DIV y.a
		ELSE Scanner.Mark (dividedByNonPositiveError)
		END
	ELSIF (y.mode # mImm) OR (y.a > 0) & (log2(y.a) < 0) THEN
		r := 0; r2 := 0; load (x);
		IF y.mode = mImm THEN
			IF y.a <= 0 THEN Scanner.Mark (dividedByNonPositiveError) END;
			load (y)
		ELSE load (y);
			IF Base.CplFlag.divideCheck THEN
				EmitRR (TEST, y.r, 8, y.r); Trap (ccLE, divide_trap)
			END
		END;
		
		dst := RegHead(-2);
		
		IF (reg_A IN curRegs) & (x.r # reg_A) THEN
			Alloc_reg (r); EmitRR (MOVd, r, 8, reg_A); saveA := TRUE
		ELSE saveA := FALSE
		END;
		IF (reg_D IN curRegs) & (x.r # reg_D) THEN
			Alloc_reg(r2); EmitRR (MOVd, r2, 8, reg_D); saveD := TRUE
		ELSE saveD := FALSE
		END;
		
		IF x.r # reg_A THEN EmitRR (MOVd, reg_A, 8, x.r) END;
		EmitBare (CQO);
		
		EmitRR (TEST, reg_A, 8, reg_A); L := pc; CondBranch (ccS, 0);
		
		IF ~ (y.r IN {reg_A, reg_D}) THEN EmitR (IDIVa, y.r, 8)
		ELSIF y.r = reg_D THEN EmitR (IDIVa, r2, 8)
		ELSE EmitR (IDIVa, r, 8)
		END;
		
		Branch (0); Fix_link (L); L := pc - 1; 
		
		IF ~ (y.r IN {reg_A, reg_D}) THEN EmitR (IDIVa, y.r, 8)
		ELSIF y.r = reg_D THEN EmitR (IDIVa, r2, 8)
		ELSE EmitR (IDIVa, r, 8)
		END;
		
		EmitRR (TEST, reg_D, 8, reg_D); CondBranch (ccZ, L); L := pc - 1;
		
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
			IF op = Scanner.div THEN EmitRI (SARi, x.r, 8, log2(y.a))
			ELSE DEC (y.a);
				IF y.a <= MaxInt32 THEN EmitRI (ANDi, x.r, 8, y.a)
				ELSE load (y); EmitRR (ANDd, x.r, 8, y.r); Free_reg
				END
			END
		ELSIF y.a = 1 THEN
			IF op = Scanner.mod THEN EmitRR (XORd, x.r, 4, x.r) END
		ELSE Scanner.Mark (dividedByNonPositiveError)
		END
	END
END Divide;

PROCEDURE Add_real* (op: INTEGER; VAR x, y: Base.Item);
	VAR xa, ya: REAL;
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		xa := SYSTEM.VAL(REAL, x.a); ya := SYSTEM.VAL(REAL, y.a);
		IF op = Scanner.plus THEN x.a := SYSTEM.VAL(INTEGER, xa + ya)
		ELSE x.a := SYSTEM.VAL(INTEGER, xa - ya)
		END
	ELSE load (x); load (y);
		IF op = Scanner.plus THEN SetRmOperand_reg (xmm_stack - 1);
			EmitXmmRm (ADDSS, xmm_stack - 2, 4); x.r := xmm_stack - 2	
		ELSE SetRmOperand_reg (y.r); EmitXmmRm (SUBSS, x.r, 4);
			IF x.r # xmm_stack - 2 THEN
				SetRmOperand_reg (x.r); EmitXmmRm (MOVSS, xmm_stack - 2, 4);
				x.r := xmm_stack - 2
			END
		END;
		Free_xreg
	END
END Add_real;

PROCEDURE Multiply_real* (op: INTEGER; VAR x, y: Base.Item);
	VAR xa, ya: REAL;
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		xa := SYSTEM.VAL(REAL, x.a); ya := SYSTEM.VAL(REAL, y.a);
		IF op = Scanner.times THEN x.a := SYSTEM.VAL(INTEGER, xa * ya)
		ELSE x.a := SYSTEM.VAL(INTEGER, xa / ya)
		END
	ELSE load (x); load (y);
		IF op = Scanner.times THEN SetRmOperand_reg (xmm_stack - 1);
			EmitXmmRm (MULSS, xmm_stack - 2, 4); x.r := xmm_stack - 2	
		ELSE SetRmOperand_reg (y.r); EmitXmmRm (DIVSS, x.r, 4);
			IF x.r # xmm_stack - 2 THEN
				SetRmOperand_reg (x.r); EmitXmmRm (MOVSS, xmm_stack - 2, 4);
				x.r := xmm_stack - 2
			END
		END;
		Free_xreg
	END
END Multiply_real;

PROCEDURE Set_op (opR, opI: INTEGER; VAR x, y: Base.Item);
BEGIN
	IF SmallConst(y) & (y.a >= 0) THEN load (x); EmitRI (opI, x.r, 8, y.a)
	ELSIF SmallConst(x) & (x.a >= 0) THEN
		load (y); EmitRI (opI, y.r, 8, x.a); x.mode := mReg; x.r := y.r
	ELSE load (x); load (y); x.r := RegHead(-2);
		EmitRR (opR, x.r, 8, RegHead(-1)); Free_reg
	END
END Set_op;

PROCEDURE Union* (VAR x, y: Base.Item);
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		x.a := ORD(SYSTEM.VAL(SET, x.a) + SYSTEM.VAL(SET, y.a))
	ELSE Set_op (ORd, ORi, x, y)
	END
END Union;

PROCEDURE Intersection* (VAR x, y: Base.Item);
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		x.a := ORD(SYSTEM.VAL(SET, x.a) * SYSTEM.VAL(SET, y.a))
	ELSE Set_op (ANDd, ANDi, x, y)
	END
END Intersection;

PROCEDURE Symmetric_difference* (VAR x, y: Base.Item);
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		x.a := ORD(SYSTEM.VAL(SET, x.a) / SYSTEM.VAL(SET, y.a))
	ELSE Set_op (XORd, XORi, x, y)
	END
END Symmetric_difference;

PROCEDURE Or1* (VAR x: Base.Item);
BEGIN Load_cond (x);
	IF x.c # ccNever THEN CondBranch (x.c, x.b); x.b := pc - 1 END;
	Fix_link (x.a); x.a := 0
END Or1;

PROCEDURE Or2* (VAR x, y: Base.Item);
BEGIN Load_cond (y); x.b := merged(y.b, x.b); x.a := y.a; x.c := y.c
END Or2;

PROCEDURE And1* (VAR x: Base.Item);
	VAR cond: INTEGER;
BEGIN Load_cond (x); cond := negated(x.c);
	IF cond # ccNever THEN CondBranch (cond, x.a); x.a := pc - 1 END;
	Fix_link (x.b); x.b := 0
END And1;

PROCEDURE And2* (VAR x, y: Base.Item);
BEGIN Load_cond (y); x.a := merged(y.a, x.a); x.b := y.b; x.c := y.c
END And2;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Relation *)

PROCEDURE Int_relation* (rel: INTEGER; VAR x, y: Base.Item);
	VAR cond, rsize: INTEGER;
BEGIN
	IF x.type = y.type THEN rsize := x.type.size ELSE rsize := 8 END;
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		IF rel = Scanner.eql THEN x.a := ORD(x.a = y.a)
		ELSIF rel = Scanner.neq THEN x.a := ORD(x.a # y.a)
		ELSIF rel = Scanner.lss THEN x.a := ORD(x.a < y.a)
		ELSIF rel = Scanner.gtr THEN x.a := ORD(x.a > y.a)
		ELSIF rel = Scanner.leq THEN x.a := ORD(x.a <= y.a)
		ELSIF rel = Scanner.geq THEN x.a := ORD(x.a >= y.a)
		END
	ELSE
		IF SmallConst(y) THEN load (x);
			IF y.a # 0 THEN EmitRI (CMPi, x.r, rsize, y.a)
			ELSE EmitRR (TEST, x.r, rsize, x.r)
			END;
			Free_reg
		ELSIF SmallConst(x) THEN load (y);
			IF x.a # 0 THEN EmitRI (CMPi, y.r, rsize, x.a)
			ELSE EmitRR (TEST, y.r, rsize, y.r)
			END;
			Free_reg;
			IF (rel <= Scanner.neq) THEN (* nothing *)
			ELSIF rel = Scanner.lss THEN rel := Scanner.gtr
			ELSIF rel = Scanner.leq THEN rel := Scanner.geq
			ELSIF rel = Scanner.gtr THEN rel := Scanner.lss
			ELSIF rel = Scanner.geq THEN rel := Scanner.leq
			END
		ELSE load (x); load (y); EmitRR (CMPd, x.r, rsize, y.r);
			Free_reg; Free_reg
		END;
		IF x.type.form = Base.tInteger THEN cond := ccCodeOfInt(rel)
		ELSE cond := ccCodeOf(rel)
		END;
		Set_cond (x, cond)
	END;
	x.type := Base.boolType
END Int_relation;

PROCEDURE Real_relation* (rel: INTEGER; VAR x, y: Base.Item);
BEGIN
	load (x); load (y); SetRmOperand_reg (y.r); EmitXmmRm (COMISS, x.r, 4);
	Free_xreg; Free_xreg; Set_cond (x, ccCodeOf(rel)); x.type := Base.boolType
END Real_relation;

PROCEDURE Set_relation* (rel: INTEGER; VAR x, y: Base.Item);
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		IF rel = Scanner.leq THEN
			x.a := ORD(SYSTEM.VAL(SET, x.a) <= SYSTEM.VAL(SET, y.a))
		ELSIF rel = Scanner.geq THEN
			x.a := ORD(SYSTEM.VAL(SET, x.a) >= SYSTEM.VAL(SET, y.a))
		END
	ELSE
		IF rel = Scanner.leq THEN Negate (y) ELSE Negate (x) END;
		Intersection (x, y); Free_reg; Set_cond (x, ccZ)
	END;
	x.type := Base.boolType
END Set_relation;

PROCEDURE String_relation* (rel: INTEGER; VAR x, y: Base.Item);
	CONST chSz = Base.CharSize;
	VAR xlen, ylen: Base.Item; rc, reg, L, L1, L2, L3, cond: INTEGER;
	
	PROCEDURE Compare_length (rc: INTEGER; len: Base.Item);
	BEGIN
		IF len.mode = mImm THEN EmitRI (CMPi, rc, 8, len.a)
		ELSIF len.mode IN modeMem THEN
			SetRmOperand (len); EmitRegRm (CMPd, rc, 8)
		ELSE ASSERT(FALSE)
		END
	END Compare_length;
	
BEGIN (* String_relation *)
	ProcState.usedRegs := ProcState.usedRegs + {reg_DI, reg_SI};
	Get_array_length (xlen, x); Get_array_length (ylen, y);
	Load_adr_to_reg (reg_DI, x); Load_adr_to_reg (reg_SI, y);
	Alloc_reg (rc); EmitRR (XORd, rc, 4, rc); Alloc_reg (reg); L := pc;
	
	Compare_length (rc, ylen); L1 := pc; CondBranch (ccZ, 0);
	SetRmOperand_regI (reg_SI, 0); EmitRmImm (CMPi, chSz, 0);
	CondBranch (ccZ, L1); L1 := pc - 1;
	
	Compare_length (rc, xlen); L2 := pc; CondBranch (ccZ, 0);
	SetRmOperand_regI (reg_DI, 0); EmitRegRm (MOVd, reg, chSz);
	EmitRR (TEST, reg, chSz, reg); CondBranch (ccZ, L2); L2 := pc - 1;
	
	SetRmOperand_regI (reg_SI, 0); EmitRegRm (CMPd, reg, chSz);
	L3 := pc; CondBranch (ccNZ, 0);
	
	EmitRI (ADDi, reg_DI, 8, chSz); EmitRI (ADDi, reg_SI, 8, chSz);
	EmitRI (ADDi, rc, 8, 1); BJump (L);
	
	Fix_link (L1);
	Compare_length (rc, xlen); L1 := pc; CondBranch (ccZ, 0);
	SetRmOperand_regI (reg_DI, 0); EmitRmImm (CMPi, chSz, 0);
	
	Free_reg; Free_reg;
	IF x.mode = mRegI THEN Free_reg END; IF y.mode = mRegI THEN Free_reg END;
	
	cond := ccCodeOf(rel); Set_cond (x, cond);
	IF cond = ccZ THEN x.b := L1; x.a := merged (L2, L3)
	ELSIF cond = ccNZ THEN x.b := merged (L2, L3); x.a := L1
	ELSE Fix_link (L3);
		IF cond = ccA THEN x.a := merged (L1, L2)
		ELSIF cond = ccAE THEN x.a := L2; x.b := L1
		ELSIF cond = ccB THEN x.a := L1; x.b := L2
		ELSE (* cond = ccBE *) x.b := merged (L1, L2)
		END
	END;
	x.type := Base.boolType
END String_relation;

PROCEDURE Member_test* (VAR x, y: Base.Item);
BEGIN
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		x.a := ORD(x.a IN SYSTEM.VAL(SET, y.a))
	ELSE
		IF SmallConst(x) THEN load (y); EmitRI (BTi, y.r, 8, x.a)
		ELSE load (x); load (y); EmitRR (BT, x.r, 8, y.r); Free_reg
		END;
		Free_reg; Set_cond (x, ccC)
	END;
	x.type := Base.boolType
END Member_test;

PROCEDURE Type_test* (VAR x, y: Base.Item; guard: BOOLEAN);
	VAR xExtLev, yExtLev, td: INTEGER; td2: Base.Item;
BEGIN
	IF x.type.form = Base.tRecord THEN
		xExtLev := x.type.len; yExtLev := y.type.len
	ELSE xExtLev := x.type.base.len; yExtLev := y.type.base.len 
	END;
	IF xExtLev >= yExtLev THEN
		IF guard THEN x.type := y.type
		ELSE
			IF x.mode IN {mReg, mRegI} THEN Free_reg END;
			Make_const (x, Base.boolType, 1)
		END
	ELSE
		IF x.type.form = Base.tRecord THEN SetRmOperand_regI (reg_BP, x.a + 8)
		ELSE load (x); SetRmOperand_regI (x.r, -8)
		END;
		Alloc_reg (td); EmitRegRm (MOVd, td, 8); td2 := y; Get_typedesc (td2);
		Load_adr (td2); SetRmOperand_regI (td, yExtLev * 8);
		EmitRegRm (CMP, td2.r, 8); Free_reg; Free_reg;
		IF guard THEN Trap (ccNZ, type_trap); x.type := y.type
		ELSE IF x.mode IN {mReg, mRegI} THEN Free_reg END; Set_cond (x, ccZ)
		END;
	END
END Type_test;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1* (VAR x: Base.Item);
	VAR r: INTEGER;
BEGIN
	IF (x.mode = mReg) & (x.a # 0) THEN
		IF (x.a > 0) & (x.a <= MaxInt32) THEN EmitRI (ORi, x.r, 8, x.a)
		ELSE Alloc_reg (r); MoveRI (r, 8, x.a);
			EmitRR (ORd, x.r, 8, r); Free_reg
		END
	END
END Set1;
	
PROCEDURE Set2* (VAR x, y: Base.Item);
	VAR r: INTEGER;
BEGIN
	IF y.mode = mImm THEN (* Delay code generation *)
		x.a := ORD(SYSTEM.VAL(SET, x.a) + {y.a})
	ELSIF x.mode = mImm THEN
		Alloc_reg (r); EmitRR (XOR, r, 4, r); load (y); EmitRR (BTS, y.r, 8, r);
		IF r # RegHead(-2) THEN EmitRR (MOVd, r, 8, y.r); r := y.r END;
		Free_reg; x.mode := mReg; x.r := r
	ELSIF x.mode = mReg THEN load (y); EmitRR (BTS, y.r, 8, x.r); Free_reg
	END
END Set2;
	
PROCEDURE Set3* (VAR x, y, z: Base.Item);
	VAR imm, r: INTEGER;
BEGIN
	IF (y.mode = mImm) & (z.mode = mImm) THEN (* Delay code generation *)
		x.a := ORD(SYSTEM.VAL(SET, x.a) + {y.a .. z.a})
	ELSE
		IF x.mode = mImm THEN Alloc_reg (r); x.mode := mReg ELSE r := x.r END;
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
		
		x.r := RegHead(-3); IF r # x.r THEN EmitRR (MOVd, x.r, 8, r) END;
		Free_reg; Free_reg
	END
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* selector *)

PROCEDURE Deref* (VAR x: Base.Item);
BEGIN load (x); x.mode := mRegI; x.a := 0;
	IF Base.CplFlag.nilCheck THEN
		EmitRR (TEST, x.r, 8, x.r); Trap (ccZ, nil_trap)
	END;
	x.param := FALSE; x.readonly := FALSE; x.type := x.type.base
END Deref;

PROCEDURE Field* (VAR x: Base.Item; field: Base.Object);
BEGIN
	IF x.mode = Base.cRef THEN x.c := x.c + field.val
	ELSE x.a := x.a + field.val
	END;
	x.param := FALSE; x.type := field.type
END Field;

PROCEDURE Index* (VAR x, y: Base.Item);
	VAR z: Base.Item; len, size: INTEGER;
	
	PROCEDURE Get_element_size (VAR x: Base.Item; tp: Base.Type; lenoff, n: INTEGER);
		VAR y: Base.Item;
	BEGIN
		IF (tp.form # Base.tArray) OR (tp.len > 0) THEN
			IF n = 1 THEN Make_const (x, Base.intType, tp.size)
			ELSE Make_const (y, Base.intType, tp.size); Multiply (x, y, TRUE)
			END
		ELSE
			x.mode := Base.cVar; x.lev := 1;
			x.a := lenoff; x.type := Base.intType;
			Get_element_size (x, tp.base, lenoff + 8, n + 1)
		END
	END Get_element_size;
	
BEGIN (* Index *)
	len := x.type.len;
	IF len > 0 THEN size := x.type.base.size;
		IF y.mode = mImm THEN
			IF (y.a >= 0) OR (y.a < len) THEN
				IF x.mode IN {Base.cVar, mRegI} THEN x.a := x.a + y.a * size
				ELSIF x.mode = Base.cRef THEN x.c := x.c + y.a * size
				END
			ELSE Scanner.Mark (arrayIndexError)
			END
		ELSE load (y);
			IF Base.CplFlag.arrayCheck THEN
				EmitRI (CMPi, y.r, 8, len); Trap (ccAE, array_trap)
			END;
			Make_const (z, Base.intType, size); Multiply (y, z, TRUE);
			IF (x.mode = Base.cVar) & (x.lev > 0) THEN
				EmitRR (ADDd, y.r, 8, reg_BP); x.r := y.r; x.mode := mRegI
			ELSE
				IF x.mode = Base.cVar THEN Load_adr (x); x.a := 0
				ELSIF x.mode = Base.cRef THEN Ref_to_regI (x)
				END;
				EmitRR (ADDd, RegHead(-2), 8, RegHead(-1));
				x.mode := mRegI; x.r := RegHead(-2); Free_reg
			END
		END
	ELSIF len = 0 THEN
		IF (y.mode = mImm) & (y.a < 0) THEN Scanner.Mark (arrayIndexError) END;
		load (y);
		IF Base.CplFlag.arrayCheck THEN
			Get_array_length (z, x); SetRmOperand (z);
			EmitRegRm (CMPd, y.r, 8); Trap (ccAE, array_trap)
		END;
		Get_element_size (z, x.type.base, x.b + 8, 1); Multiply (y, z, TRUE);
		IF x.mode = Base.cRef THEN Ref_to_regI (x) END;
		EmitRR (ADDd, RegHead(-2), 8, RegHead(-1));
		x.r := RegHead(-2); x.b := x.b + 8; Free_reg
	ELSE ASSERT(FALSE)
	END;
	x.type := x.type.base
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE For1* (VAR z: Base.Item);
	VAR t: Base.Item;
BEGIN t := z;
	Alloc_temp_var (z, Base.intType); Store (z, t)
END For1;

PROCEDURE For2* (x, z: Base.Item; inc: INTEGER; VAR L2: INTEGER);
	VAR c: Base.Item; cond: INTEGER;
BEGIN c := x; load (c); SetRmOperand (z); EmitRegRm (CMPd, c.r, 8);
	IF inc >= 0 THEN cond := ccLE ELSE cond := ccGE END;
	L2 := pc; CondBranch (cond, 0)
END For2;

PROCEDURE For3* (VAR x: Base.Item; inc, L, L2: INTEGER);
	VAR i: Base.Item;
BEGIN ASSERT (x.mode IN {Base.cVar, Base.cRef});
	IF x.mode = Base.cRef THEN Ref_to_regI (x) END; SetRmOperand (x);
	IF (inc >= MinInt32) & (inc <= MaxInt32) THEN
		EmitRmImm (ADDi, x.type.size, inc)
	ELSE Make_const (i, Base.intType, inc); EmitRegRm (ADD, i.r, x.type.size)
	END;
	IF x.mode = mRegI THEN Free_reg END;
	BJump (L); Fix_link (L2)
END For3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Procedure call *)

PROCEDURE Save_reg_stacks (VAR c: ProcCall);
	VAR i, n: INTEGER;
BEGIN
	c.oldRegStack := reg_stack; c.oldCurRegs := curRegs; i := 0;
	WHILE i <= 15 DO
		IF i IN curRegs THEN PushR (i) END; INC (i)
	END;
	reg_stack.allocOrd := 1; reg_stack.rh := 0; curRegs := {};
	
	c.oldXregStack := xmm_stack; c.oldCurXregs := curXregs;
	IF curXregs # {} THEN n := 0; i := 0;
		WHILE i <= 15 DO
			IF i IN curXregs THEN INC (n) END; INC (i)
		END;
		EmitRI (SUBi, reg_SP, 8, n * 8);
		ProcState.memstack := ProcState.memstack + n * 8; i := 0;
		WHILE i <= 15 DO
			IF i IN curXregs THEN DEC (n);
				SetRmOperand_regI (reg_SP, n * 8); EmitXmmRm (MOVSSd, i, 4)
			END;
			INC (i)
		END
	END;
	xmm_stack := 0; curXregs := {}
END Save_reg_stacks;

PROCEDURE Restore_reg_stacks (VAR c: ProcCall);
	VAR r, i, n: INTEGER;
BEGIN
	xmm_stack := c.oldXregStack; curXregs := c.oldCurXregs;
	IF curXregs # {} THEN n := 0; i := 15;
		WHILE i >= 1 DO
			IF i IN curXregs THEN SetRmOperand_regI (reg_SP, n * 8);
				EmitXmmRm (MOVSS, i, 4); INC (n)
			END;
			DEC (i)
		END;
		IF 0 IN curXregs THEN
			IF c.rtype = Base.realType THEN Alloc_xreg;
				SetRmOperand_reg (0); EmitXmmRm (MOVSS, xmm_stack - 1, 4)
			END;
			SetRmOperand_regI (reg_SP, n * 8); EmitXmmRm (MOVSS, 0, 4); INC (n)
		END;
		ProcState.memstack := ProcState.memstack - n * 8;
		EmitRI (ADDi, reg_SP, 8, n * 8)
	END;

	reg_stack := c.oldRegStack; curRegs := c.oldCurRegs; i := 15;
	WHILE i >= 1 DO
		IF i IN curRegs THEN PopR (i) END; DEC (i)
	END;
	IF reg_A IN curRegs THEN
		IF (c.rtype # NIL) & (c.rtype # Base.realType) THEN
			Alloc_reg (r); EmitRR (MOVd, r, 8, reg_A)
		END;
		PopR (reg_A)
	END
END Restore_reg_stacks;

PROCEDURE Prepare_to_call* (VAR x: Base.Item; VAR c: ProcCall);
BEGIN Save_reg_stacks (c);
	c.proc := x; c.parOff := 0; c.parblksize := x.type.parblksize;
	IF x.mode # Base.cProc THEN load (x); PushR (x.r); Free_reg END;
	
	IF c.parblksize < 32 THEN c.parblksize := 32 END;
	ProcState.memstack := ProcState.memstack + c.parblksize;
	IF ProcState.memstack MOD 16 # 0 THEN
		c.parblksize := c.parblksize + 8;
		ProcState.memstack := ProcState.memstack + 8
	END;
	EmitRI (SUBi, reg_SP, 8, c.parblksize)
END Prepare_to_call;

PROCEDURE Call* (VAR c: ProcCall);
BEGIN
	IF c.proc.mode = Base.cProc THEN
		IF c.proc.lev >= 0 THEN CallNear (c.proc.a)
		ELSE SetRmOperand (c.proc); EmitRm (CALL, 4)
		END
	ELSE
		SetRmOperand_regI (reg_SP, c.parblksize); EmitRegRm (MOVd, reg_A, 8);
		SetRmOperand_reg (reg_A); EmitRm (CALL, 4);
		c.parblksize := c.parblksize + 8
	END;
	
	(* Release the stack area used for parameters and proc address *)
	EmitRI (ADDi, reg_SP, 8, c.parblksize);
	ProcState.memstack := ProcState.memstack - c.parblksize;
	Restore_reg_stacks (c)
END Call;

PROCEDURE Return_value* (VAR x: Base.Item; rtype: Base.Type);
BEGIN x.type := rtype;
	IF rtype.form # Base.tReal THEN x.mode := mReg;
		IF reg_A IN curRegs THEN x.r := RegHead(-1)
		ELSE Alloc_reg (x.r);
			IF x.r # reg_A THEN EmitRR (MOVd, x.r, 8, reg_A) END
		END
	ELSE x.mode := mXreg;
		IF 0 IN curXregs THEN x.r := xmm_stack - 1
		ELSE Alloc_xreg; x.r := 0; ASSERT (xmm_stack = 1)
		END
	END
END Return_value;

PROCEDURE Value_param* (VAR x: Base.Item; VAR c: ProcCall);
	VAR i: INTEGER;
BEGIN i := c.parOff DIV 8 + 1; load (x);
	IF c.parOff >= 32 THEN SetRmOperand_regI (reg_SP, c.parOff);
		IF x.mode = Base.mReg THEN EmitRegRm (MOV, x.r, 8); Free_reg
		ELSE EmitXmmRm (SeeMOVDd, x.r, 8); Free_xreg
		END
	ELSIF reg_stack.rh # i THEN reg_stack.rh := i
	ELSIF xmm_stack # i THEN xmm_stack := i
	END;
	c.parOff := c.parOff + 8
END Value_param;

PROCEDURE Ref_param* (VAR x: Base.Item; VAR c: ProcCall);
	VAR i: INTEGER;
BEGIN i := c.parOff DIV 8 + 1; Load_adr (x);
	IF c.parOff >= 32 THEN
		SetRmOperand_regI (reg_SP, c.parOff); EmitRegRm (MOV, x.r, 8); Free_reg
	ELSIF xmm_stack # i THEN xmm_stack := i
	END;
	c.parOff := c.parOff + 8
END Ref_param;

PROCEDURE Record_param* (VAR x: Base.Item; VAR c: ProcCall);
	VAR td: Base.Item;
BEGIN
	IF x.param & ~x.readonly THEN
		td := x; td.a := td.a + 8; td.type := Base.intType
	ELSE Make_item (td, x.type.obj); Get_typedesc (td)
	END;
	Ref_param (x, c);
	Ref_param (td, c)
END Record_param;

PROCEDURE Array_param* (VAR x: Base.Item; VAR c: ProcCall);
	VAR array, len : Base.Item; tp, ftype : Base.Type;
BEGIN array := x; Ref_param (array, c); tp := x.type; ftype := c.fpar.type;
	WHILE (ftype.form = Base.tArray) & (ftype.len = 0) DO
		IF tp = Base.stringType THEN Make_const (len, Base.intType, x.b)
		ELSIF tp.len = 0 THEN len.mode := Base.cVar; len.lev := x.lev;
			len.a := x.b; len.type := Base.intType; x.b := x.b + 8
		ELSE Make_const (len, Base.intType, tp.len)
		END;
		Value_param (len, c); ftype := ftype.base; tp := tp.base
	END
END Array_param;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Standard procedure *)

PROCEDURE SProc_INC* (VAR x: Base.Item; amount: INTEGER);
BEGIN IF x.mode = Base.cRef THEN Ref_to_regI (x) END; SetRmOperand (x);
	EmitRmImm (ADDi, x.type.size, amount); IF x.mode = mRegI THEN Free_reg END 
END SProc_INC;

PROCEDURE SProc_INCL* (op: INTEGER; VAR x, y: Base.Item);
BEGIN
	IF SmallConst(y) THEN
		IF op = Scanner.plus THEN op := BTSi ELSE op := BTRi END;
		EmitRI (op, x.r, 8, y.a)
	ELSE load (y);
		IF op = Scanner.plus THEN op := BTS ELSE op := BTR END;
		EmitRR (op, y.r, 8, x.r); Free_reg
	END
END SProc_INCL;

PROCEDURE Assert* (VAR x: Base.Item);
BEGIN Load_cond (x); Trap (negated(x.c), assert_trap)
END Assert;

PROCEDURE SProc_GET* (VAR x, y: Base.Item);
BEGIN load (x); x.mode := mRegI; x.a := 0; x.type := y.type; Store (y, x)
END SProc_GET;
	
PROCEDURE SProc_PUT* (VAR x, y: Base.Item);
BEGIN load (x); x.mode := mRegI; x.a := 0; x.type := y.type; Store (x, y)
END SProc_PUT;
	
PROCEDURE SProc_COPY* (VAR x, y, z : Base.Item);
	VAR	rsize: INTEGER;
BEGIN
	Load_to_reg (reg_SI, x.type.size, x);
	Load_to_reg (reg_DI, y.type.size, y);
	Load_to_reg (reg_C, z.type.size, z);
	EmitRep (MOVSrep, 1, 1);

	WHILE reg_stack.rh > 0 DO Free_reg END;
	ProcState.usedRegs := ProcState.usedRegs + {reg_DI, reg_SI}
END SProc_COPY;

PROCEDURE SProc_PACK* (VAR x, y, z: Base.Item);
BEGIN
	load (z); EmitRI (SHLi, z.r, 4, 23); EmitRR (ADDd, y.r, 4, z.r);
	Free_reg; Store (x, y)
END SProc_PACK;

PROCEDURE SProc_UNPK* (VAR x, y, z: Base.Item);
	VAR e: Base.Item; r: INTEGER;
BEGIN
	e.mode := mReg; e.type := Base.dwordType; Alloc_reg (r); e.r := r;
	EmitRR (MOVd, r, 4, y.r); EmitRI (SHRi, r, 4, 23);
	EmitRI (SUBi, r, 4, 127); Store (z, e);
	EmitRI (SHLi, r, 4, 23); EmitRR (SUBd, y.r, 4, r); Store (x, y)
END SProc_UNPK;

PROCEDURE SProc_NEW* (VAR x: Base.Item);
	VAR proc, par, td: Base.Item; c: ProcCall;
BEGIN
	proc.mode := Base.cProc; proc.type := Base.HeapAllocFuncType;
	proc.lev := -2; proc.a := Base.HeapAlloc; c.rtype := Base.intType;
	Prepare_to_call (proc, c);
	
	par.mode := Base.cVar; par.lev := -1; par.a := Base.HeapHandle;
	par.type := Base.intType; Value_param (par, c);
	Make_const (par, Base.intType, 8); Value_param (par, c);
	Make_const (par, Base.intType, x.type.base.size + 16); Value_param (par, c);
	
	Call (c); Return_value (proc, c.rtype); EmitRI (ADDi, proc.r, 8, 16);
	Make_item (td, x.type.base.obj); Get_typedesc (td); Load_adr (td);
	SetRmOperand_regI (proc.r, -8); EmitRegRm (MOV, td.r, 8); Store (x, proc)
END SProc_NEW;

PROCEDURE SProc_DISPOSE* (VAR c: ProcCall);
	VAR proc, par, td : Base.Item;
BEGIN
	proc.mode := Base.cProc; proc.type := Base.HeapFreeFuncType; proc.lev := -2;
	proc.a := Base.HeapFree; c.rtype := NIL; Prepare_to_call (proc, c);
	
	par.mode := Base.cVar; par.lev := -1; par.a := Base.HeapHandle;
	par.type := Base.intType; Value_param (par, c);
	Make_const (par, Base.intType, 0); Value_param (par, c)
END SProc_DISPOSE;

PROCEDURE SFunc_ADR* (VAR x: Base.Item);
BEGIN Load_adr (x)
END SFunc_ADR;

PROCEDURE SFunc_ODD* (VAR x: Base.Item);
BEGIN
	IF x.mode = mImm THEN x.a := x.a MOD 2
	ELSE load (x); EmitRI (ANDi, x.r, 8, 1); Free_reg
	END;
	Set_cond (x, ccNZ)
END SFunc_ODD;

PROCEDURE SFunc_LEN* (VAR x: Base.Item);
	VAR len: Base.Item;
BEGIN Get_array_length (len, x); IF x.mode = mRegI THEN Free_reg END;
	IF len.mode # mImm THEN load (len) END; x := len
END SFunc_LEN;

PROCEDURE SFunc_SHIFT* (shf: INTEGER; VAR x, y: Base.Item);
	VAR opR, opI, dst: INTEGER;
BEGIN
	IF shf = 0 THEN opR := SHLcl; opI := SHLi
	ELSIF shf = 1 THEN opR := SARcl; opI := SARi
	ELSIF shf = 2 THEN opR := RORcl; opI := RORi
	ELSE ASSERT (FALSE)
	END;
	IF (x.mode = mImm) & (y.mode = mImm) THEN
		IF shf = 0 THEN x.a := LSL(x.a, y.a)
		ELSIF shf = 1 THEN x.a := ASR(x.a, y.a)
		ELSE x.a := ROR(x.a, y.a)
		END
	ELSE load (x);
		IF (y.mode = mImm) & (y.a >= 0) & (y.a < 256) THEN
			IF y.a # 0 THEN EmitRI (opI, x.r, 8, y.a) END
		ELSE load (y); dst := RegHead(-2);
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

PROCEDURE SFunc_CHR* (VAR x: Base.Item);
BEGIN
	IF x.mode = mImm THEN
		IF Base.CharSize = 1 THEN x.a := x.a MOD 100H
		ELSIF Base.CharSize = 2 THEN x.a := x.a MOD 10000H
		ELSIF Base.CharSize = 4 THEN x.a := x.a MOD 100000000H
		END
	END
END SFunc_CHR;

PROCEDURE SFunc_BIT* (VAR x, y: Base.Item);
BEGIN load (x); x.mode := mRegI; x.a := 0; load (x); load (y);
	EmitRR (BT, y.r, 8, x.r); Free_reg; Free_reg; Set_cond (x, ccC)
END SFunc_BIT;

PROCEDURE SFunc_ABS* (VAR x: Base.Item);
	VAR r, L, xa: INTEGER;
BEGIN
	IF x.mode = mImm THEN
		IF (x.type.form = Base.tInteger) & (x.a < 0) THEN x.a := -x.a
		ELSIF x.type = Base.realType THEN
			IF 31 IN SYSTEM.VAL(SET, x.a) THEN x.a := x.a - 80000000H END
		END
	ELSIF x.type.form = Base.tInteger THEN load (x); Alloc_reg (r);
		IF Base.CplFlag.overflowCheck THEN
			EmitRR (MOVd, r, 8, x.r); EmitRI (SHLi, r, 8, 1);
			Trap (ccBE, overflow_trap) (* Check for Z and C flag *)
		END;
		L := pc; CondBranch (ccAE, 0); EmitR (NEG, x.r, 8);
		Fix_link (L); Free_reg
	ELSIF x.type = Base.realType THEN load (x); Alloc_reg (r);
		Load_to_reg (r, 4, x); EmitRI (BTCi, r, 4, 31);
		SetRmOperand_reg (r); EmitXmmRm (SeeMOVD, x.r, 4)
	END
END SFunc_ABS;

PROCEDURE SFunc_FLOOR* (VAR x: Base.Item);
	VAR r: INTEGER;
BEGIN
	EmitRI (SUBi, reg_SP, 8, 8);
	SetRmOperand_regI (reg_SP, 0); EmitRm (STMXCSR, 4);
	SetRmOperand_regI (reg_SP, 0); EmitRmImm (BTSi, 4, 13);
	SetRmOperand_regI (reg_SP, 0); EmitRm (LDMXCSR, 4);
	load (x); SetRmOperand_reg (x.r); Alloc_reg (r);
	EmitXmmRm (CVTSS2SI, r, 8); Free_xreg;
	x.type := Base.intType; x.mode := mReg; x.r := r;
	SetRmOperand_regI (reg_SP, 0); EmitRmImm (BTRi, 4, 13);
	SetRmOperand_regI (reg_SP, 0); EmitRm (LDMXCSR, 4);
	EmitRI (ADDi, reg_SP, 8, 8)
END SFunc_FLOOR;

PROCEDURE SFunc_FLT* (VAR x: Base.Item);
BEGIN load (x); Alloc_xreg;
	SetRmOperand_reg (x.r); EmitXmmRm (CVTSI2SS, xmm_stack - 1, 8);
	Free_reg; x.mode := mXreg; x.r := xmm_stack - 1
END SFunc_FLT;

PROCEDURE SFunc_VAL* (VAR x: Base.Item; castType: Base.Type);
	VAR r: INTEGER;
BEGIN
	IF castType = x.type THEN (* ok *)
	ELSIF x.mode = mImm THEN
		IF castType.size < x.type.size THEN
			IF castType.size = 1 THEN x.a := x.a MOD 100H
			ELSIF castType.size = 2 THEN x.a := x.a MOD 10000H
			ELSIF castType.size = 4 THEN x.a := x.a MOD 100000000H
			END
		END
	ELSIF x.type = Base.realType THEN
		IF x.mode = mXreg THEN
			Alloc_reg (r); Load_to_reg (r, 4, x);
			x.mode := mReg; x.r := r; Free_xreg
		ELSE x.type := castType; load (x)
		END
	ELSIF castType = Base.realType THEN
		IF ~(x.mode IN modeMem + {mReg}) THEN load (x) END;
		x.type := castType; fpload (x)
	ELSE load (x)
	END;
	x.type := castType
END SFunc_VAL;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Placeholder_proc*;
BEGIN Reset_code_buffer; Reset_reg_stack; ProcState.usedRegs := {};
	ProcState.memstack := 0; ProcState.adr := ip; ProcState.prologsize := 0;
	Branch (0); Write_to_file (1, 1)
END Placeholder_proc;

PROCEDURE Enter* (proc: Base.Object; locblksize: INTEGER);
	VAR fix: FixupList;
BEGIN (* Code generation is delayed *)
	Reset_code_buffer; Reset_reg_stack; ProcState.usedRegs := {};
	ProcState.memstack := 0; ProcState.adr := ip;
	IF proc # NIL THEN
		ProcState.locblksize := locblksize;
		IF proc # Base.guard THEN ProcState.parlist := proc.type.fields;
			IF proc.val # ip THEN (* This procedure have nested procedures *)
				IF proc.lev = 0 THEN (* Need fixup *)
					NEW (fix); fix.loc := proc.val + 1; fix.val := ip - 5;
					fix.next := fixupList; fixupList := fix
				END;
				proc.val := ip
			END
		ELSE ProcState.parlist := NIL
		END
	ELSE
		Linker.entry := ip;
		ProcState.locblksize := 0;
		ProcState.parlist := NIL
	END
END Enter;

PROCEDURE Set_pointer_to_zero (adr: INTEGER; typ: Base.Type);
	VAR x, y: Base.Item; i: INTEGER; field: Base.Object;
BEGIN
	IF typ.form IN {Base.tProcedure, Base.tPointer} THEN
		x.mode := Base.cVar; x.a := adr; x.lev := 1;
		SetRmOperand (x); EmitRmImm (MOVi, 8, 0)
	ELSIF typ.form = Base.tArray THEN i := 0;
		WHILE i < typ.len DO
			Set_pointer_to_zero (adr + i * typ.base.size, typ.base); INC (i)
		END
	ELSIF typ.form = Base.tRecord THEN field := typ.fields;
		WHILE field # Base.guard DO
			Set_pointer_to_zero (adr + field.val, field.type);
			field := field.next
		END
	END
END Set_pointer_to_zero;
	
PROCEDURE Return*;
	CONST savedRegs = {reg_DI, reg_SI, reg_R12, reg_R13, reg_R14, reg_R15};
	VAR i, k, nXregs, nRegs, endPC, endIP: INTEGER;
		paramRegs: ARRAY 4 OF BYTE; obj: Base.Object; x: Base.Item;
BEGIN nXregs := 0; i := 15;
	WHILE i >= 6 DO
		IF i IN ProcState.usedXregs THEN
			SetRmOperand_regI (reg_SP, nXregs * 16);
			EmitXmmRm (MOVAPS, i, 4); INC (nXregs)
		END;
		DEC (i)
	END;

	nRegs := 0; i := 15;
	WHILE i >= 0 DO
		IF i IN savedRegs * ProcState.usedRegs THEN PopR (i); INC (nRegs) END;
		DEC (i)
	END;
	
	i := (ProcState.locblksize + nRegs * 8) MOD 16;
	IF i > 0 THEN ProcState.locblksize := ProcState.locblksize + 16 - i END;
	EmitBare (LEAVE); EmitBare (RET); endPC := pc; endIP := ip;
	(* End of Return section *)
	
	(* Emit Enter section - delayed code generation *)
	PushR (reg_BP); EmitRR (MOVd, reg_BP, 8, reg_SP);
	IF ProcState.locblksize > 0 THEN
		EmitRI (SUBi, reg_SP, 8, ProcState.locblksize)
	END;
	
	IF nRegs > 0 THEN i := 0;
		WHILE i <= 15 DO
			IF i IN savedRegs * ProcState.usedRegs THEN PushR (i) END;
			INC (i)
		END
	END;
	
	IF nXregs > 0 THEN EmitRI (SUBi, reg_SP, 8, nXregs * 16); i := 6;
		WHILE i <= 15 DO
			IF i IN ProcState.usedXregs THEN
				DEC (nXregs); SetRmOperand_regI (reg_SP, nXregs * 16);
				EmitXmmRm (MOVAPSd, i, 4)
			END;
			INC (i)
		END
	END;
	
	IF ProcState.parlist # NIL THEN obj := ProcState.parlist; i := 0; k := 0;
		WHILE (i < 4) & (obj # Base.guard) DO
			IF k = 0 THEN k := ParamSize(obj) END;
			SetRmOperand_regI (reg_BP, 16 + i * 8);
			IF obj.type.form # Base.tReal THEN
				EmitRegRm (MOV, regAllocOrds[1].ord[i], 8)
			ELSE EmitXmmRm (SeeMOVDd, i, 8)
			END;
			i := i + 1; k := k - 8;
			IF k = 0 THEN obj := obj.next END
		END
	END;
	
	obj := SymTable.procScope;
	IF obj # SymTable.universe THEN obj := obj.next;
		WHILE obj # Base.guard DO
			IF (obj.class = Base.cVar) & ~ obj.param THEN
				Set_pointer_to_zero (obj.val, obj.type)
			END;
			obj := obj.next
		END
	END;

	ProcState.prologsize := ip - endIP;
	Write_to_file (endPC, pc - 1); Write_to_file (1, endPC - 1)
END Return;

PROCEDURE Module_init*;
	VAR i, L, r, int64: INTEGER;
		str: Base.String; imported: BOOLEAN; key: SymTable.ModuleKey;
		obj, obj2: Base.Object; x, td: Base.Item; tp: Base.Type;
		importModules: SymTable.ModuleArray; imod: SymTable.Module;
BEGIN
	Reset_code_buffer; Reset_reg_stack;
	ProcState.usedRegs := {}; ProcState.memstack := 0; ProcState.adr := ip;
	ProcState.locblksize := 0; ProcState.parlist := NIL;

	IF ~Base.CplFlag.main THEN EmitRI (CMPi, reg_D, 4, 1);
		L := pc; CondBranch (ccZ, 0); EmitBare (RET); Fix_link (L)
	END;
	
	PushR (reg_SI); PushR (reg_DI); EmitRI (SUBi, reg_SP, 8, 40);
	SetRmOperand_staticvar (Base.GetProcessHeap); EmitRm (CALL, 4);
	SetRmOperand_staticvar (Base.HeapHandle); EmitRegRm (MOV, reg_A, 8);
	
	(* Import modules, if there are any *)
	imod := SymTable.moduleList;
	WHILE imod # NIL DO
		imported := imod.modno >= 0; obj := imod.dsc;
		WHILE ~imported & (obj # Base.guard) DO
			imported := (obj.val # 0) & (obj.expno # 0); obj := obj.next
		END;
		IF imported THEN
			str[0] := 0X; Strings.Append (imod.name, str);
			Strings.Append ('.dll', str);
			Make_string (x, str, Strings.Length(str) + 1);
			SetRmOperand (x); EmitRegRm (LEA, reg_C, 8);
			SetRmOperand_staticvar (Base.LoadLibraryW); EmitRm (CALL, 4);
			EmitRR (MOVd, reg_SI, 8, reg_A);
			
			(* Check module key *)
			key := imod.key; int64 := 0;
			SYSTEM.GET (SYSTEM.ADR(key[0]), int64);
			MoveRI (reg_A, 8, int64); SetRmOperand_regI (reg_SI, 400H - 16);
			EmitRegRm (CMPd, reg_A, 8); Trap (ccNZ, modkey_trap);
			SYSTEM.GET (SYSTEM.ADR(key[8]), int64);
			MoveRI (reg_A, 8, int64); SetRmOperand_regI (reg_SI, 400H - 8);
			EmitRegRm (CMPd, reg_A, 8); Trap (ccNZ, modkey_trap);
		
			obj := imod.dsc;
			WHILE obj # Base.guard DO
				IF (obj.val # 0) & (obj.expno # 0) THEN
					EmitRR (MOVd, reg_C, 8, reg_SI);
					MoveRI (reg_D, 4, obj.expno);
					SetRmOperand_staticvar (Base.GetProcAddress);
					EmitRm (CALL, 4);
					SetRmOperand_staticvar (obj.val);
					EmitRegRm (MOV, reg_A, 8)
				END;
				obj := obj.next
			END	
		END;
		imod := imod.next
	END;
	
	(* Fill value into type descriptors *)
	obj := SymTable.universe.next;
	WHILE obj # Base.guard DO
		IF (obj.class = Base.cType) & (obj.val # 0) THEN tp := obj.type;
			IF tp.form = Base.tPointer THEN tp := tp.base END;
			IF tp.len = 1 THEN
				SetRmOperand_staticvar (obj.val); EmitRegRm (LEA, reg_C, 8);
				SetRmOperand_staticvar (obj.val + 8); EmitRegRm (MOV, reg_C, 8)
			ELSIF tp.len > 1 THEN
				SetRmOperand_staticvar (obj.val + 8);
				EmitRegRm (LEA, reg_DI, 8); obj2 := tp.base.obj;
				IF obj2.lev # -2 THEN SetRmOperand_staticvar (obj2.val + 8);
					EmitRegRm (LEA, reg_SI, 8)
				ELSE SetRmOperand_staticvar (obj2.val);
					EmitRegRm (MOVd, reg_SI, 8); EmitRI (ADDi, reg_SI, 8, 8)
				END;
				MoveRI (reg_C, 4, tp.len - 1); EmitRep (MOVSrep, 8, 1);
				SetRmOperand_regI (reg_DI, tp.len * (-8));
				EmitRegRm (LEA, reg_C, 8); SetRmOperand_regI (reg_DI, 0);
				EmitRegRm (MOV, reg_C, 8)
			END
		END;
		obj := obj.next
	END;
	
	(* Call module main procedure *)
	CallNear (Linker.entry); Linker.entry := ProcState.adr;
	
	(* Exit *)
	IF Base.CplFlag.main THEN
		EmitRI (SUBi, reg_SP, 8, 32);
		EmitRR (XOR, reg_C, 4, reg_C);
		SetRmOperand_staticvar (Base.ExitProcess); EmitRm (CALL, 4)
	ELSE
		MoveRI (reg_A, 4, 1); EmitRI (ADDi, reg_SP, 8, 40);
		PopR (reg_DI); PopR (reg_SI); EmitBare (RET)
	END;
	ProcState.prologsize := 0;
	Write_to_file (1, pc - 1)
END Module_init;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Linker *)

PROCEDURE Init* (modname: Base.IdentStr);
BEGIN
	ip := 0; varbase := 0; staticsize := 128; Base.StrCopy (modname, modid);
	Sys.Rewrite (out, tempOutputName); Sys.Seek (out, 400H)
END Init;

PROCEDURE Set_varsize* (size: INTEGER);
BEGIN varsize := size; Align (varsize, 16); staticbase := -varsize
END Set_varsize;

PROCEDURE Write_idata_section;
	CONST
		table_len = LEN(Linker.Kernel32Table);
		table_size = table_len * 8;
		table_rva = 40;
		name_rva = table_rva + table_size;
		hint_rva = name_rva + 16;
	VAR i: INTEGER;
BEGIN
	Sys.Seek (out, Linker.idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Sys.Write_4bytes (out, Linker.idata_rva + table_rva);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, Linker.idata_rva + name_rva);
	i := Linker.data_rva + Linker.data_size + staticbase - table_size;
	Sys.Write_4bytes (out, i);

	Sys.Seek (out, Linker.idata_fadr + table_rva); i := 0;
	WHILE i <= table_len - 2 DO
		Linker.Kernel32Table[i] := Linker.idata_rva + hint_rva + 32 * i;
		Sys.Write_8bytes (out, Linker.Kernel32Table[i]); INC (i)
	END;
	Linker.Kernel32Table[table_len - 1] := 0;
	
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

PROCEDURE Fill_pointer_offset (offset: INTEGER; type: Base.Type);
	VAR field: Base.Object; n, k, ptrcnt: INTEGER;
BEGIN ptrcnt := type.nptr;
	IF type.form = Base.tRecord THEN
		field := type.fields;
		REPEAT k := field.type.nptr;
			IF k > 0 THEN ptrcnt := ptrcnt - k; n := offset + field.val;
				IF field.type.form = Base.tPointer THEN
					Sys.Write_4bytes (out, n)
				ELSE Fill_pointer_offset (n, field.type)
				END
			END;
			field := field.next
		UNTIL ptrcnt <= 0; ASSERT (ptrcnt = 0)
	ELSIF type.form = Base.tArray THEN type := type.base;
		IF type.form = Base.tPointer THEN
			REPEAT Sys.Write_4bytes (out, offset);
				DEC (ptrcnt); offset := offset + 8
			UNTIL ptrcnt <= 0; ASSERT (ptrcnt = 0)
		ELSE k := type.base.nptr;
			REPEAT
				Fill_pointer_offset (offset, type); ptrcnt := ptrcnt - k;
				offset := offset + type.size
			UNTIL ptrcnt <= 0; ASSERT (ptrcnt = 0)
		END
	ELSE ASSERT(FALSE)
	END
END Fill_pointer_offset;

PROCEDURE Write_data_section;
	VAR basefadr, i, n: INTEGER; b: BYTE;
BEGIN
	i := LEN(staticBuf) - LEN(Linker.Kernel32Table) * 8;
	n := SYSTEM.ADR(Linker.Kernel32Table);
	WHILE i < LEN(staticBuf) DO SYSTEM.GET (n, b); staticBuf[i] := b;
		INC (i); INC (n)
	END;

	basefadr := Linker.data_fadr + Linker.data_size - varsize;
	Sys.Seek (out, basefadr - staticsize); i := LEN(staticBuf) - staticsize;
	WHILE i < LEN(staticBuf) DO Sys.Write_byte (out, staticBuf[i]); INC (i) END
END Write_data_section;

PROCEDURE Write_edata_section;
	CONST dirsize = 40;
	VAR obj: Base.Object; name: Base.String;
		namesize, tablesize, i, rva, expno: INTEGER;
BEGIN name[0] := 0X; Strings.Append (modid, name);
	IF Base.CplFlag.main THEN Strings.Append ('.exe', name)
	ELSE Strings.Append ('.dll', name)
	END;
	namesize := Strings.Length(name) + 1;
	
	expno := SymTable.expno; tablesize := expno * 4;

	(* Export directory *)
	Sys.Seek (out, Linker.edata_fadr + 12);
	Sys.Write_4bytes (out, Linker.edata_rva + dirsize + tablesize);
	Sys.Write_4bytes (out, 1);
	Sys.Write_4bytes (out, SymTable.expno);
	Sys.Write_4bytes (out, 0);
	Sys.Write_4bytes (out, Linker.edata_rva + dirsize);
	
	(* Export address table *)
	Sys.Seek (out, Linker.edata_fadr + dirsize); i := 1;
	WHILE i <= expno DO obj := SymTable.universe.next;
		WHILE obj.expno # i DO obj := obj.next END; rva := obj.val;
		IF (obj.class = Base.cType) OR (obj.type = Base.stringType) THEN
			rva := rva + Linker.data_rva + Linker.data_size + staticbase
		ELSIF obj.class = Base.cVar THEN
			rva := rva + Linker.data_rva + Linker.data_size + varbase
		ELSIF obj.class = Base.cProc THEN
			rva := rva + Linker.code_rva
		ELSE ASSERT(FALSE)
		END;
		Sys.Write_4bytes (out, rva); INC (i)
	END;
		
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
	name: ARRAY OF CHAR; chr, rva, rawsize, size, fileadr: INTEGER
);	
	VAR b: BYTE; i: INTEGER;	
BEGIN i := 0;
	WHILE i < 8 DO b := 0; IF i < LEN(name) THEN b := ORD(name[i]) END;
		Sys.Write_byte (out, b); INC (i)
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
	VAR k: INTEGER;
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
	IF Base.CplFlag.main THEN Sys.Write_2bytes (out, 20H + 2 + 1)
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
	IF Base.CplFlag.console THEN Sys.Write_2bytes (out, 3) (*Subsys = Console*)
	ELSE Sys.Write_2bytes (out, 2) (* Subsys = GUI *)
	END;
	
	(* DLL Characteristics *)
	IF Base.CplFlag.main THEN Sys.Write_2bytes (out, 0)
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

PROCEDURE Perform_fixup;
	VAR i: INTEGER; p: FixupList;
BEGIN p := fixupList;
	WHILE p # NIL DO p.loc := p.loc + Linker.code_fadr;
		Sys.Seek (out, p.loc); Sys.Read_4bytes (out, i);
		Sys.Seek (out, p.loc); Sys.Write_4bytes (out, i + p.val);
		p := p.next
	END
END Perform_fixup;

PROCEDURE Finish*;
	VAR i, n, padding, filesize, k: INTEGER;
		str: Base.String; modkey: SymTable.ModuleKey;
BEGIN
	IF Scanner.errcnt = 0 THEN
		SymTable.Write_symbols_file;
		Sys.Seek (out, 400H - 16); modkey := SymTable.module.key; i := 0;
		WHILE i < 16 DO Sys.Write_byte (out, modkey[i]); INC (i) END;

		IF Base.CplFlag.main THEN Linker.imagebase := 400000H
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
		Write_edata_section; Write_PEHeader; Perform_fixup;
		Sys.Close (out);

		(* Show statistics *)
		Console.WriteString ('No errors found.'); Console.WriteLn;
		Console.WriteString ('Code size: ');
		Console.WriteInt (Linker.code_size); Console.WriteLn;
		Console.WriteString ('Global variables size: ');
		Console.WriteInt (varsize); Console.WriteLn;
		Console.WriteString ('Static data size: ');
		Console.WriteInt (staticsize); Console.WriteLn;
		
		(* Rename files *)
		str[0] := 0X; Strings.Append (modid, str);
		IF Base.CplFlag.main THEN Strings.Append ('.exe', str)
		ELSE Strings.Append ('.dll', str)
		END;
		Sys.Delete_file (str); Sys.Rename_file (tempOutputName, str);
		str[0] := 0X; Strings.Append (modid, str); Strings.Append ('.sym', str);
		Sys.Delete_file (str); Sys.Rename_file (tempSymName, str)
	ELSE
		Sys.Close (out);
		Console.WriteLn; Console.WriteString ('No output generated.');
		Console.WriteLn; Sys.Delete_file (tempOutputName)
	END
END Finish;

BEGIN
	regAllocOrds[0].numOfRegs := 11;
	regAllocOrds[0].ord[0] := reg_A;
	regAllocOrds[0].ord[1] := reg_C;
	regAllocOrds[0].ord[2] := reg_D;
	regAllocOrds[0].ord[3] := reg_R8;
	regAllocOrds[0].ord[4] := reg_R9;
	regAllocOrds[0].ord[5] := reg_R10;
	regAllocOrds[0].ord[6] := reg_R11;
	regAllocOrds[0].ord[7] := reg_R12;
	regAllocOrds[0].ord[8] := reg_R13;
	regAllocOrds[0].ord[9] := reg_R14;
	regAllocOrds[0].ord[10] := reg_R15;
	
	regAllocOrds[1].numOfRegs := 11;
	regAllocOrds[1].ord[0] := reg_C;
	regAllocOrds[1].ord[1] := reg_D;
	regAllocOrds[1].ord[2] := reg_R8;
	regAllocOrds[1].ord[3] := reg_R9;
	regAllocOrds[1].ord[4] := reg_A;
	regAllocOrds[1].ord[5] := reg_R10;
	regAllocOrds[1].ord[6] := reg_R11;
	regAllocOrds[1].ord[7] := reg_R12;
	regAllocOrds[1].ord[8] := reg_R13;
	regAllocOrds[1].ord[9] := reg_R14;
	regAllocOrds[1].ord[10] := reg_R15
END Generator.