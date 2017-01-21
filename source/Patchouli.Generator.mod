MODULE Patchouli.Generator;
IMPORT
	SYSTEM, Files := [Oberon07.Files],
	S := Scanner, B := Base, Linker;

CONST
	MaxInt = 9223372036854775807;
	MinInt = -MaxInt-1;
	
	MaxSize = 80000000H; (* 2 GB limit *)
	MaxLocBlkSize = 100000H; (* 1 MB limit *)
	
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
	OR_ = 08H; ORd = 0AH; SUB = 28H; SUBd = 2AH; CMP = 38H; CMPd = 3AH;
	MOV = 88H; MOVd = 8AH; LEA = 8DH;
	BT = 0A30FH; BTR = 0B30FH;
	BTS = 0AB0FH; IMUL = 0AF0FH;
	
	(* Opcodes used with EmitRm *)
	POP = 8FH; ROR1 = 1D0H; RORcl = 1D2H; SHL1 = 4D0H; SHLcl = 4D2H;
	SHR1 = 5D0H; SHRcl = 5D2H; SAR1 = 7D0H; SARcl = 7D2H;
	NOT = 2F6H; NEG = 3F6H; IDIVa = 7F7H; INC_ = 0FEH; DEC_ = 1FEH;
	CALL = 2FFH; JMP = 4FFH; PUSH = 6FFH;
	LDMXCSR = 2AE0FH; STMXCSR = 3AE0FH;
	
	(* Opcodes used with EmitRmImm *)
	ADDi = 80H; ORi = 180H; ANDi = 480H; SUBi = 580H; XORi = 680H; CMPi = 780H;
	RORi = 1C0H; SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H; MOVi = 0C6H;
	TESTi = 76H; BTi = 4BA0FH; BTSi = 5BA0FH; BTRi = 6BA0FH; BTCi = 7BA0FH;
	IMULi = 69H (* Special case *);
	
	(* Opcodes used with EmitBare *)
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H; INT3 = 0CCH; UD2 = 0B0FH;
	CMPSB = 0A6H; CMPSW = 0A766H; CMPSD = 0A7H; CMPSQ = 0A748H;
	LODSB = 0ACH; LODSW = 0AD66H; LODSD = 0ADH; LODSQ = 0AD48H;
	STOSB = 0AAH; STOSW = 0AB66H; STOSD = 0ABH; STOSQ = 0AB48H;
	PAUSE = 90F3H;
	
	(* REP instructions *)
	MOVSrep = 0A4H;
	
	(* Opcodes used with EmitXmmRm *)
	SseMOVD = 6E0F66H; SseMOVDd = 7E0F66H;
	MOVSS = 100FF3H; MOVSSd = 110FF3H; MOVSD = 100FF2H; MOVSDd = 110FF2H;
	ADDSD = 580FF2H; MULSD = 590FF2H; SUBSD = 5C0FF2H; DIVSD = 5E0FF2H;
	ADDSS = 580FF3H; MULSS = 590FF3H; SUBSS = 5C0FF3H; DIVSS = 5E0FF3H;
	ADDPS = 580F00H; MULPS = 590F00H; SUBPS = 5C0F00H; DIVPS = 5E0F00H;
	ANDPS = 540F00H; ANDNPS = 550F00H; ORPS = 560F00H; XORPS = 570F00H;
	ANDPD = 540F66H; ANDNPD = 550F66H; ORPD = 560F66H; XORPD = 570F66H;
	MOVAPS = 280F00H; MOVAPSd = 290F00H; COMISS = 2F0F00H; COMISD = 2F0F66H;
	CVTSS2SI = 2D0FF3H; CVTSI2SS = 2A0FF3H;
	CVTSD2SI = 2D0FF2H; CVTSI2SD = 2A0FF2H;
	
	(* Item mode *)
	mReg = 0; mXReg = 1; mImm = 2; mRegI = 3; mIP = 4; mSP = 5; mBP = 6;
	mCond = 7; mProc = 8; mType = 9; mBX = 10; mNothing = 11;
	
	(* Trap code *)
	modkeyTrap = 0;
	arrayTrap = 1;
	typeTrap = 2;
	stringTrap = 3;
	nilTrap = 4;
	nilProcTrap = 5;
	divideTrap = 6;
	assertTrap = 7;
	rtlTrap = 8;
	
TYPE
	Proc = B.Proc;
	Node = B.Node;

	Item = RECORD
		mode, op, r, rm: BYTE; ref, par: BOOLEAN; type: B.Type;
		a, b, c, strlen: INTEGER; aLink, bLink: Node; obj: B.Object
	END;
	
	MakeItemState = RECORD
		avoid, xAvoid: SET; bestReg, bestXReg: BYTE
	END;
	
VAR
	(* forward decl *)
	MakeItem0: PROCEDURE(VAR x: Item; obj: B.Object);

	code: ARRAY 80000H OF BYTE; pc*, stack: INTEGER;
	sPos, pass, varSize*, staticSize*, baseOffset: INTEGER;
	
	procList, curProc: B.ProcList;
	modInitProc, trapProc, trapProc2, dllInitProc: Proc;
	
	modidStr, errFmtStr, err2FmtStr, err3FmtStr: B.Str;
	err4FmtStr, err5FmtStr, err6FmtStr, rtlName, user32name: B.Str;

	mem: RECORD mod, rm, bas, idx, scl, disp: INTEGER END;
	allocReg, allocXReg: SET;
	MkItmStat: MakeItemState; (* State for MakeItem procedures in Pass 2 *)
	
	(* Static data address*)
	(* Win32 specifics *)
	GetModuleHandleExW, ExitProcess, LoadLibraryW, GetProcAddress: INTEGER;
	AddVectoredExceptionHandler: INTEGER;
	MessageBoxW, wsprintfW: INTEGER;
	(* others *)
	adrOfNEW, modPtrTable, adrOfPtrTable: INTEGER;
	
	debug: Files.File; rider: Files.Rider;
		
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE log2(n: INTEGER): INTEGER;
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

PROCEDURE IntToSet(n: INTEGER): SET;
	RETURN SYSTEM.VAL(SET, n)
END IntToSet;

PROCEDURE SmallConst(n: INTEGER): BOOLEAN;
	RETURN (n >= -80000000H) & (n < 80000000H)
END SmallConst;

PROCEDURE Align(VAR a: INTEGER; align: INTEGER);
BEGIN
	IF a > 0 THEN a := (a + align - 1) DIV align * align
	ELSIF a < 0 THEN a := a DIV align * align
	END
END Align;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Put1(n: BYTE);
BEGIN
	IF pass > 2 THEN code[pc] := n END; INC(pc)
END Put1;

PROCEDURE Put2(n: INTEGER);
	VAR val: SYSTEM.CARD16;
BEGIN
	IF pass > 2 THEN
		ASSERT(pc+2 <= LEN(code)); val := n;
		SYSTEM.PUT(SYSTEM.ADR(code)+pc, val)
	END;
	INC(pc, 2)
END Put2;

PROCEDURE Put4(n: INTEGER);
	VAR val: SYSTEM.CARD32;
BEGIN
	IF pass > 2 THEN
		ASSERT(pc+4 <= LEN(code)); val := n;
		SYSTEM.PUT(SYSTEM.ADR(code)+pc, val)
	END;
	INC(pc, 4)
END Put4;

PROCEDURE Put8(n: INTEGER);
BEGIN
	IF pass > 2 THEN
		ASSERT(pc+8 <= LEN(code));
		SYSTEM.PUT(SYSTEM.ADR(code)+pc, n)
	END;
	INC(pc, 8)
END Put8;
	
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)
	
PROCEDURE EmitREX(reg, rsize: INTEGER);
	CONST W = 8; R = 4; X = 2; B = 1;
	VAR rex: INTEGER;
BEGIN
	rex := 40H;
	IF rsize = 8 THEN rex := rex + W END;
	IF reg >= reg_R8 THEN rex := rex + R END;
	IF (mem.rm >= reg_R8)
	OR (mem.mod # 3) & (mem.rm = reg_SP) & (mem.bas >= reg_R8)
	THEN rex := rex + B
	END;
	IF (mem.mod # 3) & (mem.rm = reg_SP) & (mem.idx >= reg_R8)
	THEN rex := rex + X
	END;
	IF (rex # 40H)
	OR (rsize = 1)
		& ((reg IN {reg_SP..reg_DI})
		OR (mem.mod = 3) & (mem.rm IN {reg_SP..reg_DI}))
	THEN Put1(rex)
	END
END EmitREX;

PROCEDURE Emit16bitPrefix(rsize: INTEGER);
BEGIN IF rsize = 2 THEN Put1(66H) END
END Emit16bitPrefix;

PROCEDURE HandleMultibytesOpcode(VAR op: INTEGER);
BEGIN
	IF op MOD 256 = 0FH THEN
		Put1(0FH); op := op DIV 256;
		IF (op MOD 256 = 38H) OR (op MOD 256 = 3AH) THEN
			Put1(op); op := op DIV 256
		END
	END
END HandleMultibytesOpcode;

PROCEDURE EmitModRM(reg: INTEGER);
BEGIN
	Put1(mem.mod * 64 + reg MOD 8 * 8 + mem.rm MOD 8);
	IF mem.mod # 3 THEN
		IF mem.rm IN {reg_SP, reg_R12} THEN
			Put1(mem.scl * 64 + mem.idx MOD 8 * 8 + mem.bas MOD 8)
		END;
		IF (mem.mod = 0) & (mem.rm IN {reg_BP, reg_R13})
		OR (mem.mod = 0) & (mem.rm IN {reg_SP, reg_R12})
			& (mem.bas IN {reg_BP, reg_R13}) 
		OR (mem.mod = 2) THEN Put4(mem.disp)
		ELSIF mem.mod = 1 THEN Put1(mem.disp)
		END
	END
END EmitModRM;

(* -------------------------------------------------------------------------- *)
	
PROCEDURE EmitRegRm(op, reg, rsize: INTEGER);
	CONST w = 1;
	VAR org: INTEGER;
BEGIN
	Emit16bitPrefix(rsize); EmitREX(reg, rsize);
	org := op; HandleMultibytesOpcode(op);
	
	IF (rsize > 1) & (org < LEA) THEN op := op + w END;
	Put1(op); EmitModRM(reg)
END EmitRegRm;

PROCEDURE EmitRm(op, rsize: INTEGER);
	CONST w = 1;
	VAR op3bits, org: INTEGER;
BEGIN
	Emit16bitPrefix(rsize); EmitREX(0, rsize);
	org := op; HandleMultibytesOpcode(op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF (rsize > 1) & ~ODD(op) & (org # LDMXCSR) & (org # STMXCSR)
	THEN op := op + w
	END;
	Put1(op); EmitModRM(op3bits)
END EmitRm;

PROCEDURE EmitRmImm(op, rsize, imm: INTEGER);
	CONST w = 1; s = 2;
	VAR op3bits: INTEGER;
BEGIN
	Emit16bitPrefix(rsize);
	IF op MOD 256 # IMULi THEN EmitREX(0, rsize)
	ELSE EmitREX(op DIV 256, rsize)
	END;
	HandleMultibytesOpcode(op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF rsize > 1 THEN
		IF (op = 0C0H) OR (op = 0BAH) THEN rsize := 1
		ELSIF (imm >= -128) & (imm <= 127) & (op = 80H) THEN
			op := op + s; rsize := 1
		END;
		IF ~ODD(op) & (op # 0BAH) THEN op := op + w END
	END;
	Put1(op); EmitModRM(op3bits);
	
	IF rsize = 1 THEN Put1(imm)
	ELSIF rsize = 2 THEN Put2(imm) ELSE Put4(imm)
	END
END EmitRmImm;

PROCEDURE EmitBare(op: INTEGER);
BEGIN WHILE op > 0 DO Put1(op); op := op DIV 256 END
END EmitBare;

PROCEDURE EmitXmmRm(op, xreg, rsize: INTEGER);
	VAR prefix: INTEGER;
BEGIN
	prefix := op MOD 256; op := op DIV 256;
	IF prefix # 0 THEN Put1(prefix) END;
	EmitREX(xreg, rsize); HandleMultibytesOpcode(op);
	Put1(op MOD 256); EmitModRM(xreg)
END EmitXmmRm;

PROCEDURE EmitMOVZX(reg, rmsize: INTEGER);
	VAR rsize, op: INTEGER;
BEGIN rsize := 4; op := 0B6H;
	IF rmsize = 1 THEN
		IF (mem.mod = 3) & (mem.rm IN {reg_SP..reg_DI})
		THEN rsize := 8
		END
	ELSIF rmsize = 2 THEN INC(op)
	ELSE ASSERT(FALSE)
	END;
	EmitREX(reg, rsize); Put1(0FH); Put1(op); EmitModRM(reg)
END EmitMOVZX;

PROCEDURE EmitMOVSX(reg, rmsize: INTEGER);
	VAR op: INTEGER;
BEGIN
	IF rmsize = 1 THEN op := 0BE0FH
	ELSIF rmsize = 2 THEN op := 0BF0FH
	ELSIF rmsize = 4 THEN op := 63H
	ELSE ASSERT(FALSE)
	END;
	EmitREX(reg, 8); HandleMultibytesOpcode(op);
	Put1(op); EmitModRM(reg)
END EmitMOVSX;

PROCEDURE EmitCMPXCHG(reg, rsize: INTEGER);
BEGIN
	Put1(0F0H); (* LOCK prefix *)
	Emit16bitPrefix(rsize); EmitREX(reg, rsize);
	Put1(0FH); IF rsize > 1 THEN Put1(0B1H) ELSE Put1(0B0H) END;
	EmitModRM(reg)
END EmitCMPXCHG;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetRm_reg(reg: INTEGER);
BEGIN mem.rm := reg; mem.mod := 3
END SetRm_reg;

PROCEDURE SetRm_regI(reg, disp: INTEGER);
BEGIN
	mem.rm := reg; mem.disp := disp;
	IF (disp >= -128) & (disp <= 127) THEN
		IF (disp = 0) & ~(reg IN {reg_BP, reg_R13})
		THEN mem.mod := 0 ELSE mem.mod := 1
		END
	ELSE mem.mod := 2
	END;
	IF reg IN {reg_SP, reg_R12} THEN
		mem.bas := reg_SP; mem.idx := reg_SP; mem.scl := 0
	END
END SetRm_regI;

PROCEDURE SetRm_RIP(disp: INTEGER);
BEGIN mem.rm := reg_BP; mem.disp := disp; mem.mod := 0
END SetRm_RIP;

PROCEDURE SetRm_regX(reg, idx, scl, disp: INTEGER);
BEGIN
	mem.rm := reg_SP; mem.disp := disp; ASSERT(idx # reg_SP);
	mem.bas := reg; mem.idx := idx; mem.scl := scl;
	IF (disp >= -128) & (disp <= 127) THEN
		IF (disp = 0) & ~(reg IN {reg_BP, reg_R13})
		THEN mem.mod := 0 ELSE mem.mod := 1
		END
	ELSE mem.mod := 2
	END
END SetRm_regX;

PROCEDURE SetRmOperand(x: Item);
BEGIN
	IF x.mode = mSP THEN
		mem.rm := reg_SP; mem.bas := reg_SP; mem.idx := reg_SP;
		mem.scl := 0; mem.disp := x.a;
		IF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1 ELSE mem.mod := 2 END
	ELSIF x.mode = mBP THEN
		mem.rm := reg_BP; mem.disp := x.a;
		IF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1 ELSE mem.mod := 2 END
	ELSIF x.mode = mIP THEN mem.rm := reg_BP; mem.disp := x.a; mem.mod := 0
	ELSIF x.mode = mBX THEN mem.rm := reg_B; mem.disp := x.a;
		IF x.a = 0 THEN mem.mod := 0 
		ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
		ELSE mem.mod := 2
		END
	ELSIF x.mode = mRegI THEN SetRm_regI(x.r, x.a)
	ELSIF x.mode = mReg THEN SetRm_reg(x.r)
	ELSIF x.mode = mProc THEN mem.rm := reg_B; mem.disp := x.a;
		IF x.a = 0 THEN mem.mod := 0 
		ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
		ELSE mem.mod := 2
		END
	END
END SetRmOperand;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRR(op, reg, rsize, rm: INTEGER);
BEGIN SetRm_reg(rm); EmitRegRm(op, reg, rsize)
END EmitRR;

PROCEDURE EmitRI(op, rm, rsize, imm: INTEGER);
BEGIN
	SetRm_reg(rm); IF op = IMULi THEN op := op + rm * 256 END;
	EmitRmImm(op, rsize, imm)
END EmitRI;

PROCEDURE EmitR(op, rm, rsize: INTEGER);
BEGIN SetRm_reg(rm); EmitRm(op, rsize)
END EmitR;

PROCEDURE EmitXX(op, xreg, rsize, rm: INTEGER);
BEGIN SetRm_reg(rm); EmitXmmRm(op, xreg, rsize)
END EmitXX;

(* -------------------------------------------------------------------------- *)

PROCEDURE MoveRI(rm, rsize, imm: INTEGER);
	CONST w = 8;
	VAR op: INTEGER;
BEGIN
	SetRm_reg(rm); Emit16bitPrefix(rsize);
	EmitREX(0, rsize); op := 0B0H + rm MOD 8;
	IF rsize > 1 THEN op := op + w END; Put1(op);
	IF rsize = 1 THEN Put1(imm) ELSIF rsize = 2 THEN Put2(imm)
	ELSIF rsize = 4 THEN Put4(imm) ELSE Put8(imm)
	END
END MoveRI;

PROCEDURE PushR(rm: INTEGER);
BEGIN SetRm_reg(rm); EmitREX(0, 4); Put1(50H + rm MOD 8)
END PushR;

PROCEDURE PopR(rm: INTEGER);
BEGIN SetRm_reg(rm); EmitREX(0, 4); Put1(58H + rm MOD 8)
END PopR;

PROCEDURE Jmp4(disp: INTEGER);
BEGIN Put1(0E9H); Put4(disp)
END Jmp4;

PROCEDURE Jmp1(disp: INTEGER);
BEGIN Put1(0EBH); Put1(disp)
END Jmp1;

PROCEDURE Jcc4(cond, disp: INTEGER);
BEGIN Put1(0FH); Put1(80H + cond); Put4(disp)
END Jcc4;

PROCEDURE Jcc1(cond, disp: INTEGER);
BEGIN Put1(70H + cond); Put1(disp)
END Jcc1;

PROCEDURE CallNear(disp: INTEGER);
BEGIN Put1(0E8H); Put4(disp)
END CallNear;

PROCEDURE SetccRm(cond: INTEGER);
BEGIN EmitREX(0, 1); Put1(0FH); Put1(90H + cond); EmitModRM(0)
END SetccRm;

PROCEDURE EmitRep(op, rsize, z: INTEGER);
	CONST w = 1;
BEGIN
	Put1(0F2H + z); (* REP prefix *)
	Emit16bitPrefix(rsize); EmitREX(0, rsize);
	IF (rsize > 1) & ~ODD(op) THEN op := op + w END;
	Put1(op)
END EmitRep;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Pass 1 *)

PROCEDURE CheckTypeSize(VAR sz: INTEGER);
BEGIN
	IF sz > MaxSize THEN S.Mark('type too big'); sz := 8 END
END CheckTypeSize;

PROCEDURE SetTypeSize*(tp: B.Type);
	VAR size, align, falg: INTEGER;
		ident: B.Ident; ftype, btype: B.Type;
BEGIN
	IF tp.size = 0 THEN btype := tp.base;
		IF tp.form = B.tPtr THEN tp.size := 8; tp.align := 8
		ELSIF tp.form = B.tProc THEN
			tp.size := 8; tp.align := 8; ident := tp.fields; size := 0;
			WHILE ident # NIL DO
				ftype := ident.obj.type; ident.obj(B.Var).adr := size + 16;
				IF B.IsOpenArray(ftype) & ~ftype.notag
				OR (ftype.form = B.tRec) & (ident.obj(B.Par).varpar)
				THEN INC(size, 16) ELSE INC(size, 8)
				END;
				ident := ident.next
			END;
			tp.parblksize := size
		ELSIF (tp.form = B.tArray) & (tp.len >= 0) THEN
			SetTypeSize(btype); tp.align := btype.align;
			tp.size := btype.size * tp.len; CheckTypeSize(tp.size)
		ELSIF tp.form = B.tRec THEN
			IF btype # NIL THEN SetTypeSize(btype);
				align := btype.align; size := btype.size0
			ELSE size := 0; align := 0
			END;
			ident := tp.fields;
			WHILE ident # NIL DO
				ftype := ident.obj.type; SetTypeSize(ftype);
				IF ftype.align > align THEN align := ftype.align END;
				IF ~tp.union THEN
					Align(size, ftype.align); ident.obj(B.Field).off := size;
					INC(size, ftype.size); CheckTypeSize(size);
				ELSE
					ident.obj(B.Field).off := 0;
					IF ftype.size > size THEN size := ftype.size END
				END;
				ident := ident.next
			END;
			tp.size0 := size; Align(size, align);
			CheckTypeSize(size); tp.size := size; tp.align := align
		ELSE ASSERT(FALSE)
		END
	END
END SetTypeSize;

PROCEDURE SetGlobalVarSize*(x: B.Object);
BEGIN
	Align(varSize, x.type.align); INC(varSize, x.type.size);
	x(B.Var).adr := -varSize;
	IF varSize > MaxSize THEN varSize := 8;
		S.Mark('global var size limit reached')
	END
END SetGlobalVarSize;

PROCEDURE SetProcVarSize*(proc: B.Proc; x: B.Object);
	VAR size: INTEGER;
BEGIN size := proc.locblksize;
	Align(size, x.type.align); INC(size, x.type.size);
	x(B.Var).adr := -size; proc.locblksize := size;
	IF size > MaxLocBlkSize THEN proc.locblksize := 8;
		S.Mark('local var size limit reached')
	END
END SetProcVarSize;

PROCEDURE AllocImport*(x: B.Object; module: B.Module);
	VAR p: B.Ident; adr: INTEGER;
BEGIN
	NEW(p); p.obj := x; p.next := module.impList;
	module.impList := p; adr := staticSize; INC(staticSize, 8);
	IF x IS B.Var THEN x(B.Var).adr := adr
	ELSIF x IS B.Proc THEN x(B.Proc).adr := adr
	ELSIF x.class = B.cType THEN
		ASSERT(x.type.form = B.tRec); x.type.adr := adr
	END
END AllocImport;

PROCEDURE AllocImportModules;
	VAR size: INTEGER; imod: B.Module;
		str: ARRAY 512 OF CHAR;
BEGIN
	Align(staticSize, 16); imod := B.modList;
	WHILE imod # NIL DO
		IF imod.import OR (imod.impList # NIL) THEN
			B.ModIdToStr(imod.id, str); size := (B.StrLen(str)+5)*2;
			imod.adr := staticSize; INC(staticSize, size)
		END;
		imod := imod.next
	END
END AllocImportModules;

PROCEDURE AllocStaticData;
	VAR p: B.StrList; q: B.TypeList;
		x: B.Object; y: B.Str; strSize, tdSize, align: INTEGER;
BEGIN
	Align(staticSize, 16); p := B.strList;
	WHILE p # NIL DO
		y := p.obj; strSize := 2*y.len;
		y.adr := staticSize; INC(staticSize, strSize); p := p.next
	END;
	Align(staticSize, 16); q := B.recList;
	WHILE q # NIL DO
		tdSize := (24 + 8*(B.MaxExt + q.type.nTraced)) DIV 16 * 16;
		q.type.adr := staticSize; INC(staticSize, tdSize); q := q.next
	END;
	AllocImportModules;
	IF staticSize + varSize > MaxSize THEN
		S.Mark('static variables size too big'); ASSERT(FALSE)
	END
END AllocStaticData;

PROCEDURE ScanNode(node: B.Node);
	VAR left, right: B.Object;
		fpar: B.Ident; e: INTEGER; 
	
	PROCEDURE ScanPar(node: B.Node; fpar: B.Ident; n: INTEGER);
		VAR vpar, open: BOOLEAN;
			i: INTEGER; ftype: B.Type;
	BEGIN (* ScanPar *)
		ftype := fpar.obj.type;
		IF node.left IS B.Node THEN
			ScanNode(node.left(B.Node));
			node.regUsed := node.left(Node).regUsed;
			node.xRegUsed := node.left(Node).xRegUsed
		END;
		open := B.IsOpenArray(ftype) & ~ftype.notag;
		vpar := fpar.obj(B.Par).varpar; 
		IF open OR vpar & (ftype.form = B.tRec) THEN i := 2 ELSE i := 1 END;
		WHILE i > 0 DO 
			IF (ftype.form # B.tReal) OR vpar THEN
				IF n = 0 THEN INCL(node.regUsed, reg_C)
				ELSIF n = 1 THEN INCL(node.regUsed, reg_D)
				ELSIF n = 2 THEN INCL(node.regUsed, reg_R8)
				ELSIF n = 3 THEN INCL(node.regUsed, reg_R9)
				END
			ELSE INCL(node.xRegUsed, n)
			END;
			DEC(i); INC(n)
		END;
		IF node.right # NIL THEN
			ScanPar(node.right(B.Node), fpar.next, n);
			node.regUsed := node.regUsed + node.right(Node).regUsed;
			node.xRegUsed := node.xRegUsed + node.right(Node).xRegUsed
		END
	END ScanPar;
	
BEGIN (* ScanNode *)
	left := node.left; right := node.right;
	IF node.op # S.call THEN
		IF (left # NIL) & (left IS B.Node) THEN
			ScanNode(left(B.Node));
			IF node.op # S.semicolon THEN
				node.regUsed := left(Node).regUsed;
				node.xRegUsed := left(Node).xRegUsed
			END
		END;
		IF (right # NIL) & (right IS B.Node) THEN
			ScanNode(right(B.Node));
			IF node.op # S.semicolon THEN
				node.regUsed := right(Node).regUsed;
				node.xRegUsed := right(Node).xRegUsed
			END
		END;
		IF node.op = S.times THEN
			IF (node.type = B.intType) & (right IS B.Const) THEN
				e := log2(right(B.Const).val);
				IF e >= 0 THEN
					right := B.NewConst(B.intType, e);
					node.op := S.sfLSL; node.right := right
				END
			END
		ELSIF (node.op = S.div) OR (node.op = S.mod) THEN e := -1;
			IF right IS B.Const THEN e := log2(right(B.Const).val) END;
			IF e = -1 THEN node.regUsed := node.regUsed + {reg_A, reg_D} END
		ELSIF (node.op >= S.sfLSL) & (node.op <= S.sfROR) THEN
			IF ~(right IS B.Const) THEN INCL(node.regUsed, reg_C) END
		ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN
			IF B.IsStr(left.type) THEN
				node.regUsed := node.regUsed + {reg_SI, reg_DI}
			END
		ELSIF node.op = S.upto THEN INCL(node.regUsed, reg_C)
		ELSIF node.op = S.becomes THEN
			IF left.type.form IN {B.tArray, B.tRec} THEN
				node.regUsed := node.regUsed + {reg_SI, reg_DI}
			END
		ELSIF node.op = S.sfCAS THEN INCL(node.regUsed, reg_A)
		END
	ELSIF node.op = S.call THEN
		IF left IS B.Node THEN
			ScanNode(left(B.Node));
			node.regUsed := left(Node).regUsed;
			node.xRegUsed := left(Node).xRegUsed
		END;
		node.regUsed := node.regUsed + {0 .. 2, 8 .. 11};
		node.xRegUsed := node.xRegUsed + {0 .. 5};
		IF right # NIL THEN
			fpar := left.type.fields; ScanPar(right(B.Node), fpar, 0);
			node.regUsed := node.regUsed + right(Node).regUsed;
			node.xRegUsed := node.xRegUsed + right(Node).xRegUsed
		END
	END
END ScanNode;

PROCEDURE ScanProc(proc: B.Proc);
BEGIN
	proc.adr := -1; proc.fix := -1;
	IF curProc # NIL THEN NEW(curProc.next); curProc := curProc.next
	ELSIF curProc = NIL THEN NEW(procList); curProc := procList
	END;
	curProc.obj := proc;
	IF proc.statseq # NIL THEN ScanNode(proc.statseq) END;
	IF proc.return # NIL THEN
		IF proc.return IS B.Node THEN ScanNode(proc.return(B.Node)) END
	END
END ScanProc;

PROCEDURE ScanDeclaration(decl: B.Ident; lev: INTEGER);
	VAR ident: B.Ident; obj: B.Object;
BEGIN ident := decl;
	IF lev = 0 THEN Align(staticSize, 16); modPtrTable := staticSize END;
	WHILE ident # NIL DO obj := ident.obj;
		IF obj IS B.Proc THEN
			ScanDeclaration(obj(B.Proc).decl, lev+1); ScanProc(obj(B.Proc))
		ELSIF (lev = 0) & (obj IS B.Var) & ~(obj IS B.Str) THEN
			IF obj.type.nTraced > 0 THEN
				INC(staticSize, obj.type.nTraced*8)
			END
		END;
		ident := ident.next
	END;
	IF lev = 0 THEN INC(staticSize, 8) (* for -1 at the end of table *) END
END ScanDeclaration;

PROCEDURE NewProc(VAR proc: Proc; statseq: Node);
BEGIN
	proc := B.NewProc(); proc.statseq := statseq;
	proc.locblksize := 0; proc.adr := -1; proc.fix := -1;
	IF curProc # NIL THEN NEW(curProc.next); curProc := curProc.next
	ELSIF curProc = NIL THEN NEW(procList); curProc := procList
	END;
	curProc.obj := proc
END NewProc;

PROCEDURE Pass1(VAR modinit: B.Node);
	VAR fixAmount: INTEGER; obj: B.Proc;
		str, str2: ARRAY 512 OF CHAR;
BEGIN
	B.ModIdToStr(B.modid, str); modidStr := B.NewStr2(str);
	errFmtStr := B.NewStr2('Error code: %d; Source pos: %d');
	err2FmtStr := B.NewStr2('Module key of %s is mismatched');
	err3FmtStr := B.NewStr2('Unknown exception; Pc: %x');
	err4FmtStr := B.NewStr2('Cannot load module %s (not exist?)');
	rtlName := B.NewStr2(B.RtlName); user32name := B.NewStr2('USER32.DLL');
	
	str2 := 'Error in module '; B.Append(str, str2);
	err5FmtStr := B.NewStr2(str2);
	
	AllocStaticData; ScanDeclaration(B.universe.first, 0);
	baseOffset := (-staticSize) DIV 4096 * 4096;
	
	IF modinit # NIL THEN
		ScanNode(modinit(B.Node)); NewProc(modInitProc, modinit)
	END;
	NewProc(dllInitProc, NIL); NewProc(trapProc, NIL); NewProc(trapProc2, NIL)
END Pass1;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Pass 2 *)

PROCEDURE negated(cond: INTEGER): INTEGER;
BEGIN IF ODD(cond) THEN DEC(cond) ELSE INC(cond) END; RETURN cond
END negated;

PROCEDURE Jump(node, link: Node; cond: INTEGER);
	VAR jmpSz: INTEGER;
BEGIN
	IF pass = 2 THEN
		node.jmpPc := pc; node.link := link;
		IF cond = ccAlways THEN INC(pc, 5); node.jmpSz := 5
		ELSIF cond # ccNever THEN INC(pc, 6); node.jmpSz := 6
		ELSE node.jmpSz := 0
		END
	ELSIF pass = 3 THEN
		node.jmpPc := pc; node.link := link; jmpSz := node.jmpSz;
		IF cond = ccAlways THEN
			IF jmpSz = 5 THEN Jmp4(0)
			ELSIF jmpSz = 2 THEN Jmp1(0)
			ELSE ASSERT(jmpSz = 0)
			END
		ELSIF cond # ccNever THEN
			IF jmpSz = 6 THEN Jcc4(cond, 0)
			ELSIF jmpSz = 2 THEN Jcc1(cond, 0)
			ELSE ASSERT(jmpSz = 0)
			END
		END
	ELSE ASSERT(FALSE)
	END
END Jump;

PROCEDURE BJump(dst, cond: INTEGER);
	VAR off: INTEGER;
BEGIN off := dst - pc;
	IF cond = ccAlways THEN
		IF off-2 >= -128 THEN Jmp1(off-2) ELSE Jmp4(off-5) END
	ELSIF cond # ccNever THEN
		IF off-2 >= -128 THEN Jcc1(cond, off-2)
		ELSE Jcc4(cond, off-6)
		END
	END
END BJump;

PROCEDURE ValidOff(off, sz: INTEGER): BOOLEAN;
	RETURN (off < 0) & (off >= LSL(-1, sz-1))
	OR (off >= 0) & (off < LSL(1, sz-1))
END ValidOff;

PROCEDURE FixDisp(p, disp, dispSz: INTEGER);
	VAR i: INTEGER;
BEGIN
	IF disp < 0 THEN ASSERT(disp >= LSL(-1, dispSz*8-1))
	ELSIF disp >= 0 THEN ASSERT(disp < LSL(1, dispSz*8-1))
	END; i := 0;
	WHILE i < dispSz DO code[p+i] := ASR(disp, i*8); INC(i) END
END FixDisp;

PROCEDURE Fixup(jmpPc, dst: INTEGER);
	VAR op: BYTE; off: INTEGER;
BEGIN
	IF pass = 2 THEN (* do nothing *)
	ELSIF pass = 3 THEN op := code[jmpPc];
		IF op = 0E9H (* jmp near *) THEN
			off := dst - (jmpPc + 5); FixDisp(jmpPc+1, off, 4)
		ELSIF op = 0EBH (* jmp short *) THEN
			off := dst - (jmpPc + 2); FixDisp(jmpPc+1, off, 1)
		ELSIF op = 0FH THEN (* jcc near *)
			ASSERT(code[jmpPc+1] DIV 10H = 8);
			off := dst - (jmpPc + 6); FixDisp(jmpPc+2, off, 4)
		ELSIF op DIV 10H = 7 THEN (* jcc short *)
			off := dst - (jmpPc + 2); FixDisp(jmpPc+1, off, 1)
		ELSE ASSERT(FALSE)
		END
	ELSE ASSERT(FALSE)
	END
END Fixup;

PROCEDURE FixLinkWith(L: Node; dst: INTEGER);
	VAR off: INTEGER;
BEGIN
	WHILE L # NIL DO
		off := dst - (L.jmpPc + L.jmpSz);
		IF pass = 2 THEN
			IF L.jmpSz # 0 THEN
				IF off = 0 THEN L.jmpSz := 0
				ELSIF (off >= -128) & (off < 128) THEN L.jmpSz := 2
				END
			END
		ELSIF pass = 3 THEN
			IF L.jmpSz > 0 THEN Fixup(L.jmpPc, dst) END
		ELSE ASSERT(FALSE)
		END;
		L := L.link
	END
END FixLinkWith;

PROCEDURE FixLink(L: Node);
BEGIN FixLinkWith(L, pc)
END FixLink;

PROCEDURE merged(L0, L1: Node): Node;
	VAR L2, L3: Node;
BEGIN 
	IF L0 # NIL THEN L3 := L0;
		REPEAT L2 := L3; L3 := L3.link UNTIL L3 = NIL;
		L2.link := L1; L1 := L0
	END;
    RETURN L1
END merged;

PROCEDURE SetCond(VAR x: Item; c: INTEGER);
BEGIN x.mode := mCond; x.aLink := NIL; x.bLink := NIL; x.c := c
END SetCond;

PROCEDURE OpToCc(op: INTEGER): INTEGER;
BEGIN
	IF op = S.eql THEN op := ccZ ELSIF op = S.neq THEN op := ccNZ
	ELSIF op = S.lss THEN op := ccB ELSIF op = S.gtr THEN op := ccA
	ELSIF op = S.leq THEN op := ccBE ELSE op := ccAE
	END;
	RETURN op
END OpToCc;

PROCEDURE IntOpToCc(op: INTEGER): INTEGER;
BEGIN
	IF op = S.eql THEN op := ccZ ELSIF op = S.neq THEN op := ccNZ
	ELSIF op = S.lss THEN op := ccL ELSIF op = S.gtr THEN op := ccG
	ELSIF op = S.leq THEN op := ccLE ELSE op := ccGE
	END;
	RETURN op
END IntOpToCc;

PROCEDURE CallProc(proc: B.Proc);
	VAR L, adr: INTEGER;
BEGIN
	IF pass = 2 THEN INC(pc, 5)
	ELSIF pass = 3 THEN adr := proc.adr;
		IF adr >= 0 THEN CallNear(adr-pc-5)
		ELSE CallNear(proc.fix); proc.fix := pc - 4
		END
	ELSE ASSERT(FALSE)
	END
END CallProc;

PROCEDURE LoadProc(reg: INTEGER; obj: B.Proc);
	VAR adr: INTEGER;
BEGIN
	IF pass = 2 THEN SetRm_RIP(0); EmitRegRm(LEA, reg, 8)
	ELSIF pass = 3 THEN
		SetRm_RIP(0); EmitRegRm(LEA, reg, 8); adr := obj.adr;
		IF adr >= 0 THEN FixDisp(pc-4, adr-pc, 4)
		ELSE FixDisp(pc-4, obj.fix, 4); obj.fix := pc - 4
		END
	ELSE ASSERT(FALSE)
	END
END LoadProc;

PROCEDURE WriteDebug(pos, spos, trapno: INTEGER);
BEGIN
	IF pass = 3 THEN
		Files.WriteInt(rider, pos + LSL(spos, 30) + LSL(trapno, 60))
	END
END WriteDebug;

PROCEDURE Trap(cond, trapno: INTEGER);
	VAR L: INTEGER;
BEGIN
	IF ~(cond IN {ccAlways, ccNever}) THEN
		L := pc; Jcc1(negated(cond), 0); EmitBare(UD2);
		WriteDebug(pc-2, sPos, trapno); Fixup(L, pc)
	ELSIF cond = ccAlways THEN
		EmitBare(UD2); WriteDebug(pc-2, sPos, trapno)
	END
END Trap;

PROCEDURE ModKeyTrap(cond, errno: INTEGER; imod: B.Module);
	VAR L: INTEGER;
BEGIN
	L := pc; Jcc1(negated(cond), 0);
	SetRm_regI(reg_B, imod.adr); EmitRegRm(LEA, reg_C, 8);
	MoveRI(reg_D, 1, errno); CallProc(trapProc2); Fixup(L, pc)
END ModKeyTrap;

(* -------------------------------------------------------------------------- *)

PROCEDURE ResetMkItmStat;
BEGIN
	MkItmStat.avoid := {}; MkItmStat.bestReg := 255;
	MkItmStat.xAvoid := {}; MkItmStat.bestXReg := 255
END ResetMkItmStat;

PROCEDURE ResetMkItmStat2(VAR oldStat: MakeItemState);
BEGIN oldStat := MkItmStat;
	MkItmStat.avoid := {}; MkItmStat.bestReg := 255;
	MkItmStat.xAvoid := {}; MkItmStat.bestXReg := 255
END ResetMkItmStat2;

PROCEDURE SetAlloc(reg: BYTE);
BEGIN INCL(allocReg, reg); INCL(curProc.obj.usedReg, reg)
END SetAlloc;

PROCEDURE SetAllocX(reg: BYTE);
BEGIN INCL(allocXReg, reg); INCL(curProc.obj.usedXReg, reg)
END SetAllocX;

PROCEDURE AllocReg(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.avoid + allocReg + {reg_SP, reg_BP, reg_B};
	IF (MkItmStat.bestReg = 255) OR (MkItmStat.bestReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 3) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 3 THEN reg := 8;
			WHILE (reg < 12) & (reg IN cantAlloc) DO INC(reg) END;
			IF reg >= 12 THEN reg := 6;
				WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
				IF reg >= 16 THEN S.Mark('Reg stack overflow') END
			END
		END
	ELSE reg := MkItmStat.bestReg
	END;
	ASSERT(reg < 16); SetAlloc(reg);
	RETURN reg
END AllocReg;

PROCEDURE AllocReg2(avoid: SET): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := avoid + allocReg + {reg_SP, reg_BP, reg_B};
	reg := 0; WHILE (reg < 3) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 3 THEN reg := 8;
		WHILE (reg < 12) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 12 THEN reg := 6;
			WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
			IF reg >= 16 THEN S.Mark('Reg stack overflow') END
		END
	END;
	ASSERT(reg < 16); SetAlloc(reg);
	RETURN reg
END AllocReg2;

PROCEDURE SetAvoid(reg: BYTE);
BEGIN INCL(MkItmStat.avoid, reg)
END SetAvoid;

PROCEDURE SetBestReg(reg: BYTE);
BEGIN MkItmStat.bestReg := reg
END SetBestReg;

PROCEDURE AllocXReg(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.xAvoid + allocXReg;
	IF (MkItmStat.bestXReg = -1) OR (MkItmStat.bestXReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END
	ELSE reg := MkItmStat.bestXReg
	END;
	SetAllocX(reg);
	RETURN reg
END AllocXReg;

PROCEDURE AllocXReg2(avoid: SET): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := avoid + allocXReg;
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	SetAllocX(reg);
	RETURN reg
END AllocXReg2;

PROCEDURE FreeReg(reg: BYTE);
BEGIN EXCL(allocReg, reg)
END FreeReg;

PROCEDURE FreeXReg(reg: BYTE);
BEGIN EXCL(allocXReg, reg)
END FreeXReg;

PROCEDURE FreeReg2(x: Item);
BEGIN
	IF x.mode IN {mReg, mRegI} THEN FreeReg(x.r)
	ELSIF x.mode = mXReg THEN FreeXReg(x.r)
	END
END FreeReg2;

PROCEDURE AllocStack(size: INTEGER): INTEGER;
	VAR adr: INTEGER;
BEGIN
	INC(stack, size); adr := -stack;
	IF stack > curProc.obj.stack THEN curProc.obj.stack := stack END;
	RETURN adr
END AllocStack;

PROCEDURE FreeStack(size: INTEGER);
BEGIN DEC(stack, size)
END FreeStack;

(* -------------------------------------------------------------------------- *)

PROCEDURE RelocReg(VAR reg: BYTE; newReg: BYTE);
BEGIN
	EmitRR(MOVd, newReg, 8, reg);
	FreeReg(reg); reg := newReg; SetAlloc(reg)
END RelocReg;

PROCEDURE RelocXReg(VAR reg: BYTE; newReg: BYTE);
BEGIN
	SetRm_reg(reg); EmitXmmRm(MOVSD, newReg, 8);
	FreeXReg(reg); reg := newReg; SetAllocX(reg)
END RelocXReg;

PROCEDURE RefToRegI(VAR x: Item);
	VAR reg: BYTE;
BEGIN
	IF x.ref & (x.mode # mProc) THEN
		ASSERT(x.mode IN {mSP, mIP, mBP, mBX});
		reg := AllocReg(); SetRmOperand(x); EmitRegRm(MOVd, reg, 8);
		x.mode := mRegI; x.r := reg; x.a := x.b; x.ref := FALSE
	END
END RefToRegI;

PROCEDURE LoadImm(r: BYTE; size, imm: INTEGER);
BEGIN
	IF imm = 0 THEN EmitRR(XOR, r, 4, r)
	ELSIF size <= 4 THEN MoveRI(r, size, imm)
	ELSIF (imm > 0) & (imm < 100000000H) THEN MoveRI(r, 4, imm)
	ELSIF SmallConst(imm) THEN EmitRR(XOR, r, 4, r); EmitRI(ADDi, r, 8, imm)
	ELSE MoveRI(r, 8, imm)
	END
END LoadImm;

PROCEDURE Load(VAR x: Item);
	VAR r, r2: BYTE; size, L, L2: INTEGER;
BEGIN RefToRegI(x);
	IF x.type.form # B.tReal THEN
		IF x.mode # mReg THEN size := x.type.size;
			IF x.mode # mRegI THEN r := AllocReg() ELSE r := x.r END;
			IF x.mode = mImm THEN LoadImm(r, size, x.a)
			ELSIF x.mode IN {mRegI, mSP, mIP, mBP, mBX} THEN SetRmOperand(x);
				IF x.type = B.strType THEN
					ASSERT(x.strlen <= 2); EmitMOVZX(r, 2)
				ELSIF size >= 4 THEN EmitRegRm(MOVd, r, size)
				ELSE EmitMOVZX(r, size)
				END
			ELSIF x.mode = mProc THEN
				IF ~x.ref THEN LoadProc(r, x.obj(B.Proc))
				ELSE SetRmOperand(x); EmitRegRm(MOVd, r, 8); x.ref := FALSE
				END
			ELSIF x.mode = mCond THEN
				L := pc; Jcc1(negated(x.c), 0);
				FixLink(x.bLink); LoadImm(r, 4, 1); L2 := pc; Jmp1(0);
				FixLink(x.aLink); Fixup(L, pc); EmitRR(XOR, r, 4, r);
				Fixup(L2, pc)
			ELSE ASSERT(FALSE)
			END;
			x.mode := mReg; x.r := r
		END
	ELSIF x.mode # mXReg THEN r := AllocXReg();
		IF x.mode = mImm THEN
			IF x.a = 0 THEN SetRm_reg(r); EmitXmmRm(XORPS, r, 4)
			ELSE r2 := AllocReg2({});
				LoadImm(r2, x.type.size, x.a); SetRm_reg(r2);
				EmitXmmRm(SseMOVD, r, x.type.size); FreeReg(r2)
			END
		ELSE SetRmOperand(x); EmitXmmRm(MOVSD, r, 4)
		END;
		FreeReg2(x); x.mode := mXReg; x.r := r
	END
END Load;

PROCEDURE LoadAdr(VAR x: Item);
	VAR r: BYTE;
BEGIN
	RefToRegI(x); SetRmOperand(x);
	IF x.mode = mRegI THEN r := x.r ELSE r := AllocReg() END;
	IF (x.mode # mRegI) OR (x.a # 0) THEN EmitRegRm(LEA, r, 8) END;
	x.r := r; x.mode := mReg
END LoadAdr;

PROCEDURE ArrayLen(VAR x: Item; obj: B.Object);
BEGIN
	IF obj IS B.Str THEN x.mode := mImm; x.a := obj(B.Str).len
	ELSIF B.IsOpenArray(obj.type) THEN MakeItem0(x, obj); INC(x.a, 8)
	ELSIF B.IsNormalArray(obj.type) THEN x.mode := mImm; x.a := obj.type.len
	ELSE ASSERT(FALSE)
	END;
	x.type := B.card32Type; x.ref := FALSE
END ArrayLen;

PROCEDURE SizeOf(VAR x: Item; obj: B.Object);
	VAR size, e: INTEGER;
BEGIN
	IF obj IS B.Str THEN x.mode := mImm; x.a := obj(B.Str).len*2
	ELSIF B.IsOpenArray(obj.type) THEN size := obj.type.base.size;
		IF size = 0 THEN x.mode := mImm; x.a := 0
		ELSE ArrayLen(x, obj); Load(x); e := log2(size);
			IF e > 0 THEN EmitRI(SHLi, x.r, 8, e)
			ELSIF e < 0 THEN EmitRI(IMULi, x.r, 8, size)
			END
		END
	ELSE x.mode := mImm; x.a := obj.type.size
	END;
	x.type := B.card32Type; x.ref := FALSE
END SizeOf;

PROCEDURE TypeTag(VAR x: Item);
BEGIN
	IF x.type.form = B.tPtr THEN
		Load(x); x.mode := mRegI;
		EmitRR(TEST, x.r, 8, x.r); Trap(ccZ, nilTrap);
		IF B.Flag.handle THEN x.a := 8 ELSE x.a := -16 END
	ELSIF x.mode = mBP THEN x.a := x.a + 8; x.ref := FALSE
	ELSE ASSERT(FALSE)
	END;
	x.type := B.intType
END TypeTag;

PROCEDURE TypeTag2(VAR tag: Item; x: Item);
BEGIN tag := x;
	IF (x.type.form = B.tPtr) & (x.mode IN {mReg, mRegI}) THEN
		tag.r := AllocReg(); SetRmOperand(x);
		EmitRegRm(MOVd, tag.r, 8); tag.mode := mReg
	END; TypeTag(tag)
END TypeTag2;

PROCEDURE TypeDesc(VAR x: Item; tp: B.Type);
BEGIN
	IF tp.form = B.tRec THEN x.a := tp.adr ELSE ASSERT(FALSE) END;
	x.mode := mBX; x.type := B.intType; x.b := 0; x.ref := tp.mod # NIL;
	SetAlloc(reg_B)
END TypeDesc;

PROCEDURE AvoidUsedBy(obj: B.Object);
	VAR node: Node;
BEGIN
	IF (obj # NIL) & (obj IS Node) THEN node := obj(Node);
		IF (node.regUsed # {}) OR (node.xRegUsed # {}) THEN
			MkItmStat.avoid := MkItmStat.avoid + node.regUsed;
			MkItmStat.xAvoid := MkItmStat.xAvoid + node.xRegUsed
		END
	END
END AvoidUsedBy;

PROCEDURE LoadLeftRight(VAR x, y: Item; node: Node);
	VAR oldStat: MakeItemState;
BEGIN
	oldStat := MkItmStat; AvoidUsedBy(node.right);
	MakeItem0(x, node.left); Load(x);
	ResetMkItmStat; MakeItem0(y, node.right); Load(y); MkItmStat := oldStat
END LoadLeftRight;

PROCEDURE LoadLeftRight2(VAR x, y: Item; node: Node);
BEGIN
	AvoidUsedBy(node.right); MakeItem0(x, node.left); Load(x);
	ResetMkItmStat; MakeItem0(y, node.right); Load(y)
END LoadLeftRight2;

PROCEDURE Add(VAR x: Item; node: Node);
	VAR y: Item; form: INTEGER;
BEGIN form := node.type.form;
	LoadLeftRight(x, y, node); SetRmOperand(y);
	IF form = B.tInt THEN EmitRegRm(ADDd, x.r, 8)
	ELSIF form = B.tSet THEN EmitRegRm(ORd, x.r, 8)
	ELSIF node.type = B.realType THEN EmitXmmRm(ADDSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	FreeReg2(y)
END Add;

PROCEDURE Subtract(VAR x: Item; node: Node);
	VAR y: Item; form: INTEGER;
BEGIN form := node.type.form;
	IF node.right # NIL THEN
		LoadLeftRight(x, y, node); SetRmOperand(y);
		IF form = B.tInt THEN EmitRegRm(SUBd, x.r, 8)
		ELSIF form = B.tSet THEN
			Load(y); EmitR(NOT, y.r, 8); EmitRR(ANDd, x.r, 8, y.r)
		ELSIF node.type = B.realType THEN EmitXmmRm(SUBSD, x.r, 4)
		ELSE ASSERT(FALSE)
		END;
		FreeReg2(y)
	ELSE MakeItem0(x, node.left); Load(x);
		IF node.type.form = B.tInt THEN EmitR(NEG, x.r, 8)
		ELSIF node.type.form = B.tSet THEN EmitR(NOT, x.r, 8)
		ELSIF node.type = B.realType THEN y.r := AllocXReg2({});
			SetRm_reg(y.r); EmitXmmRm(XORPS, y.r, 4);
			SetRm_reg(x.r); EmitXmmRm(SUBSD, y.r, 4);
			SetRm_reg(y.r); EmitXmmRm(MOVSD, x.r, 4); FreeXReg(y.r)
		END
	END
END Subtract;

PROCEDURE Multiply(VAR x: Item; node: Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tInt THEN EmitRR(IMUL, x.r, 8, y.r)
	ELSIF node.type.form = B.tSet THEN EmitRR(ANDd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(MULSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	FreeReg2(y)
END Multiply;

PROCEDURE IntDiv(VAR x: Item; node: Node);
	VAR oldStat: MakeItemState; e, n, L: INTEGER; r: BYTE;
		right: B.Object; y: Item;
BEGIN
	e := -1; right := node.right;
	IF right IS B.Const THEN n := right(B.Const).val; e := log2(n) END;
	IF (e >= 0) THEN
		MakeItem0(x, node.left); Load(x);
		IF node.op = S.div THEN
			IF e = 1 THEN EmitR(SAR1, x.r, 8)
			ELSIF e > 1 THEN EmitRI(SARi, x.r, 8, e)
			END
		ELSIF node.op = S.mod THEN
			IF ~SmallConst(n-1) THEN
				r := AllocReg(); LoadImm(r, 8, n-1);
				EmitRR(ANDd, x.r, 8, r); FreeReg(r)
			ELSIF n > 1 THEN EmitRI(ANDi, x.r, 4, n-1)
			ELSE EmitRR(XOR, x.r, 4, x.r)
			END
		END
	ELSE
		ResetMkItmStat2(oldStat); AvoidUsedBy(node.right);
		SetBestReg(reg_A); MakeItem0(x, node.left); Load(x);
		ResetMkItmStat; SetAvoid(reg_A); SetAvoid(reg_D);
		MakeItem0(y, node.right); Load(y);
		IF x.r # reg_A THEN RelocReg(x.r, reg_A) END; SetAlloc(reg_D);
		
		EmitBare(CQO); EmitR(IDIVa, y.r, 8);
		EmitRR(TEST, reg_D, 8, reg_D); L := pc; Jcc1(ccGE, 0);
		IF node.op = S.div THEN EmitRI(SUBi, reg_A, 8, 1)
		ELSE EmitRR(ADDd, reg_D, 8, y.r)
		END;
		Fixup(L, pc);
		
		IF node.op = S.div THEN FreeReg(reg_D)
		ELSE FreeReg(reg_A); x.r := reg_D
		END;
		FreeReg(y.r); MkItmStat := oldStat
	END
END IntDiv;

PROCEDURE Divide(VAR x: Item; node: Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tSet THEN EmitRR(XORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(DIVSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	FreeReg2(y)
END Divide;

PROCEDURE LoadCond(VAR x: Item; obj: B.Object);
	VAR oldStat: MakeItemState;
BEGIN ResetMkItmStat2(oldStat); MakeItem0(x, obj);
	IF x.mode # mCond THEN 
		IF x.mode = mImm THEN SetCond(x, ccNever - x.a)
		ELSE Load(x); EmitRR(TEST, x.r, 4, x.r); FreeReg(x.r); SetCond(x, ccNZ)
		END
	END;
	MkItmStat := oldStat
END LoadCond;

PROCEDURE And(VAR x: Item; node: Node);
	VAR y: Item;
BEGIN
	LoadCond(x, node.left); Jump(node, x.aLink, negated(x.c));
	FixLink(x.bLink); LoadCond(y, node.right);
	x.aLink := merged(y.aLink, node); x.bLink := y.bLink; x.c := y.c
END And;

PROCEDURE Or(VAR x: Item; node: Node);
	VAR y: Item;
BEGIN
	LoadCond(x, node.left); Jump(node, x.bLink, x.c);
	FixLink(x.aLink); LoadCond(y, node.right);
	x.bLink := merged(y.bLink, node); x.aLink := y.aLink; x.c := y.c
END Or;

PROCEDURE Not(VAR x: Item; node: Node);
	VAR t: Node;
BEGIN
	LoadCond(x, node.left); x.c := negated(x.c);
	t := x.aLink; x.aLink := x.bLink; x.bLink := t
END Not;

PROCEDURE Compare(VAR x: Item; node: Node);
	VAR cx, r: BYTE; first, L: INTEGER;
		y, len: Item; tp: B.Type; oldStat: MakeItemState;
BEGIN
	ResetMkItmStat2(oldStat); tp := node.left.type;
	IF tp.form IN B.typScalar THEN
		IF tp.form # B.tReal THEN
			LoadLeftRight2(x, y, node); EmitRR(CMPd, x.r, 8, y.r);
			FreeReg(x.r); FreeReg2(y); SetCond(x, IntOpToCc(node.op))
		ELSIF tp = B.realType THEN
			LoadLeftRight2(x, y, node); SetRmOperand(y);
			EmitXmmRm(COMISD, x.r, 4); FreeXReg(x.r); FreeReg2(y);
			SetCond(x, IntOpToCc(node.op))
		ELSE ASSERT(FALSE)
		END
	ELSIF B.IsStr(tp) THEN
		SetBestReg(reg_SI); AvoidUsedBy(node.right); SetAvoid(reg_DI);
		MakeItem0(x, node.left); LoadAdr(x); ResetMkItmStat;
		SetBestReg(reg_DI); MakeItem0(y, node.right); LoadAdr(y);
		IF y.r # reg_DI THEN RelocReg(y.r, reg_DI) END;
		IF x.r # reg_SI THEN RelocReg(x.r, reg_SI) END;
		cx := AllocReg2({}); EmitRR(XOR, cx, 4, cx);
		
		first := pc; EmitR(INC_, cx, 4);
		
		ArrayLen(len, node.left);
		IF len.mode = mImm THEN EmitRI(CMPi, cx, 4, len.a)
		ELSE SetRmOperand(len); EmitRegRm(CMPd, cx, 4); FreeReg2(len)
		END;
		Trap(ccA, stringTrap);
		
		ArrayLen(len, node.right);
		IF len.mode = mImm THEN EmitRI(CMPi, cx, 4, len.a)
		ELSE SetRmOperand(len); EmitRegRm(CMPd, cx, 4); FreeReg2(len)
		END;
		Trap(ccA, stringTrap);
		
		EmitBare(CMPSW); L := pc; Jcc1(ccNZ, 0);
		SetRm_regI(reg_SI, -2); EmitRmImm(CMPi, 2, 0); BJump(first, ccNZ);
		Fixup(L, pc); FreeReg(reg_DI); FreeReg(reg_SI);
		SetCond(x, OpToCc(node.op))
	ELSE ASSERT(FALSE)
	END;
	MkItmStat := oldStat
END Compare;

PROCEDURE MemberTest(VAR x: Item; node: Node);
	VAR y: Item; oldStat: MakeItemState;
BEGIN ResetMkItmStat2(oldStat);
	IF node.left IS B.Const THEN
		MakeItem0(x, node.left); MakeItem0(y, node.right); RefToRegI(y);
		SetRmOperand(y); EmitRmImm(BTi, 8, x.a MOD 64); FreeReg2(y)
	ELSE
		LoadLeftRight2(x, y, node);
		EmitRR(BT, x.r, 8, y.r); FreeReg(x.r); FreeReg(y.r)
	END;
	SetCond(x, ccC); MkItmStat := oldStat
END MemberTest;

PROCEDURE TypeTest(VAR x: Item; node: Node);
	VAR y, tag: Item; oldStat: MakeItemState; tp: B.Type;
BEGIN
	ResetMkItmStat2(oldStat); tp := node.right.type;
	IF tp.form = B.tPtr THEN tp := tp.base END;
	MakeItem0(x, node.left); TypeTag(x); Load(x);
	TypeDesc(y, tp); LoadAdr(y); SetRm_regI(x.r, tp.len * 8);
	EmitRegRm(CMP, y.r, 8); FreeReg(x.r); FreeReg(y.r);
	SetCond(x, ccZ); MkItmStat := oldStat
END TypeTest;

PROCEDURE Deref(VAR x: Item; node: Node);
BEGIN
	MakeItem0(x, node.left); Load(x);
	EmitRR(TEST, x.r, 8, x.r); Trap(ccZ, nilTrap);
	IF B.Flag.handle THEN SetRm_regI(x.r, 0); EmitRegRm(MOVd, x.r, 8) END;
	x.mode := mRegI; x.a := 0
END Deref;

PROCEDURE Field(VAR x: Item; node: Node);
	VAR off: INTEGER;
BEGIN
	MakeItem0(x, node.left); off := node.right(B.Field).off;
	IF x.ref THEN INC(x.b, off) ELSE INC(x.a, off) END
END Field;

PROCEDURE TypeCheck(VAR x: Item; node: Node);
	VAR tag, y: Item; oldStat: MakeItemState; tp: B.Type;
BEGIN
	MakeItem0(x, node.left); ResetMkItmStat2(oldStat);
	tp := node.right.type; IF tp.form = B.tPtr THEN tp := tp.base END;
	TypeDesc(y, tp); LoadAdr(y); TypeTag2(tag, x); Load(tag);
	SetRm_regI(tag.r, tp.len * 8); EmitRegRm(CMP, y.r, 8);
	FreeReg(tag.r); FreeReg(y.r); Trap(ccNZ, typeTrap); MkItmStat := oldStat
END TypeCheck;

PROCEDURE Index(VAR x: Item; node: Node);
	VAR idx, size, align, e: INTEGER; len, y: Item;
		bType: B.Type; oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; AvoidUsedBy(node.right);
	MakeItem0(x, node.left); bType := x.type.base; size := bType.size;
	IF node.right IS B.Const THEN idx := node.right(B.Const).val;
		IF B.IsOpenArray(x.type) & ~x.type.notag THEN
			ArrayLen(len, node.left); SetRmOperand(len);
			EmitRmImm(CMPi, 4, idx); Trap(ccBE, arrayTrap)
		END;
		IF x.ref THEN INC(x.b, idx*size) ELSE INC(x.a, idx*size) END
	ELSE RefToRegI(x);
		IF x.mode # mRegI THEN LoadAdr(x); x.mode := mRegI; x.a := 0 END;
		ResetMkItmStat; MakeItem0(y, node.right); Load(y);
		IF ~x.type.notag THEN ArrayLen(len, node.left);
			IF len.mode = mImm THEN EmitRI(CMPi, y.r, 4, len.a)
			ELSE SetRmOperand(len); EmitRegRm(CMPd, y.r, 4)
			END; Trap(ccAE, arrayTrap)
		END;
		IF size > 0 THEN e := log2(size);
			IF (e >= 0) & (e <= 3) THEN
				SetRm_regX(x.r, y.r, e, 0); EmitRegRm(LEA, x.r, 8)
			ELSIF e > 3 THEN
				EmitRI(SHLi, y.r, 8, e); EmitRR(ADDd, x.r, 8, y.r)
			ELSE EmitRI(IMULi, y.r, 8, size); EmitRR(ADDd, x.r, 8, y.r)
			END 
		END;
		FreeReg(y.r)
	END;
	MkItmStat := oldStat
END Index;

PROCEDURE RecordCast(VAR x: Item; node: Node);
BEGIN
	MakeItem0(x, node.left); Load(x); x.mode := mRegI; x.a := 0
END RecordCast;

PROCEDURE SingletonSet(VAR x: Item; node: Node);
	VAR r: INTEGER; oldStat: MakeItemState;
BEGIN
	r := AllocReg(); ResetMkItmStat2(oldStat); MakeItem0(x, node.left);
	Load(x); EmitRR(XOR, r, 4, r); EmitRR(BTS, x.r, 8, r);
	FreeReg(x.r); x.r := r; MkItmStat := oldStat
END SingletonSet;

PROCEDURE RangeSet(VAR x: Item; node: Node);
	VAR r, r2: INTEGER; oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; SetAvoid(reg_C);
	r := AllocReg(); r2 := AllocReg2({reg_C}); ResetMkItmStat;
	SetBestReg(reg_C); MakeItem0(x, node.left); Load(x);
	IF x.r # reg_C THEN RelocReg(x.r, reg_C) END;
	LoadImm(r, 8, -1); EmitR(SHLcl, r, 8); FreeReg(reg_C);
	SetBestReg(reg_C); MakeItem0(x, node.right); Load(x);
	IF x.r # reg_C THEN RelocReg(x.r, reg_C) END;
	LoadImm(r2, 8, -2); EmitR(SHLcl, r2, 8); FreeReg(reg_C);
	EmitRR(XORd, r, 8, r2); FreeReg(r2); x.r := r; MkItmStat := oldStat
END RangeSet;

PROCEDURE ParReg(n: INTEGER): BYTE;
BEGIN
	IF n > 3 THEN n := 255
	ELSIF n = 0 THEN n := reg_C ELSIF n = 1 THEN n := reg_D
	ELSIF n = 2 THEN n := reg_R8 ELSIF n = 3 THEN n := reg_R9
	END;
	RETURN n
END ParReg;

PROCEDURE LoadParam(VAR x: Item; par: Node; n: INTEGER; ref: BOOLEAN);
	VAR obj: B.Object; tp: B.Type;
BEGIN
	IF n < 4 THEN AvoidUsedBy(par.right) END;
	obj := par.left; tp := obj.type;
	IF ~ref THEN
		IF tp.form # B.tReal THEN SetBestReg(ParReg(n))
		ELSIF n < 4 THEN MkItmStat.bestXReg := n
		END; MakeItem0(x, obj);
		IF tp.form # B.tRec THEN Load(x)
		ELSIF tp.size >= 8 THEN x.type := B.intType; Load(x)
		ELSIF tp.size >= 4 THEN x.type := B.card32Type; Load(x)
		ELSIF tp.size >= 2 THEN x.type := B.card16Type; Load(x)
		ELSIF tp.size = 1 THEN x.type := B.byteType; Load(x)
		END
	ELSE SetBestReg(ParReg(n)); MakeItem0(x, obj); LoadAdr(x)
	END;
	IF n >= 4 THEN SetRm_regI(reg_SP, n*8);
		IF x.mode = mReg THEN EmitRegRm(MOV, x.r, 8); FreeReg(x.r)
		ELSE EmitXmmRm(MOVSDd, x.r, 8); FreeXReg(x.r)
		END
	END
END LoadParam;

PROCEDURE Parameter(par: Node; fpar: B.Ident; n: INTEGER);
	VAR varpar: BOOLEAN; ftype: B.Type; x, y: Item; i: INTEGER; r: BYTE;
BEGIN
	ResetMkItmStat; i := 1;
	ftype := fpar.obj.type; varpar := fpar.obj(B.Par).varpar;
	IF ftype.form = B.tArray THEN LoadParam(x, par, n, TRUE);
		IF B.IsOpenArray(ftype) & ~ftype.notag THEN INC(i) END
	ELSIF ftype = B.strType THEN LoadParam(x, par, n, TRUE)
	ELSIF ftype.form = B.tRec THEN
		IF varpar THEN LoadParam(x, par, n, TRUE); INC(i)
		ELSIF (ftype.size < 9) & (ftype.size IN {0, 1, 2, 4, 8}) THEN
			LoadParam(x, par, n, FALSE)
		ELSE LoadParam(x, par, n, TRUE)
		END
	ELSE LoadParam(x, par, n, varpar)
	END;
	IF par.right # NIL THEN Parameter(par.right(Node), fpar.next, n+i) END;
	IF n < 4 THEN
		IF x.mode = mReg THEN r := ParReg(n);
			IF x.r # r THEN RelocReg(x.r, r) END
		ELSIF x.r # n THEN RelocXReg(x.r, n)
		END
	END;
	IF i = 2 THEN ResetMkItmStat; SetBestReg(ParReg(n+1));
		IF ftype.form = B.tArray THEN
			IF ftype.base # B.byteType THEN ArrayLen(y, par.left)
			ELSE SizeOf(y, par.left)
			END; Load(y)
		ELSIF (par.left IS B.Par) & par.left(B.Par).varpar THEN
			MakeItem0(y, par.left); TypeTag(y); Load(y)
		ELSE TypeDesc(y, par.left.type); LoadAdr(y)
		END;
		IF n > 2 THEN
			SetRm_regI(reg_SP, n*8+8); EmitRegRm(MOV, y.r, 8); FreeReg(y.r)
		ELSIF ParReg(n+1) # y.r THEN ASSERT(FALSE)
		END
	END
END Parameter;

PROCEDURE Call(VAR x: Item; node: Node);
	VAR y: Item; oldStat: MakeItemState; L: INTEGER;
		oldAlloc, oldXAlloc: SET; procType: B.Type;
BEGIN
	procType := node.left.type; ResetMkItmStat2(oldStat);
	IF procType.base = NIL THEN allocReg := {}; allocXReg := {} END;
	oldAlloc := allocReg; oldXAlloc := allocXReg;
	IF curProc.obj.homeSpace < 32 THEN curProc.obj.homeSpace := 32 END;
	IF curProc.obj.homeSpace < procType.parblksize THEN
		curProc.obj.homeSpace := procType.parblksize
	END;
	
	IF node.left IS B.Proc THEN MakeItem0(x, node.left)
	ELSE AvoidUsedBy(node.right); MakeItem0(x, node.left);
		Load(x); EmitRR(TEST, x.r, 8, x.r); Trap(ccZ, nilProcTrap)
	END;
	IF node.right # NIL THEN
		Parameter(node.right(Node), procType.fields, 0)
	END;
	IF (x.mode = mReg) OR x.ref THEN SetRmOperand(x); EmitRm(CALL, 4)
	ELSE CallProc(x.obj(B.Proc))
	END;
	MkItmStat := oldStat; allocReg := oldAlloc; allocXReg := oldXAlloc;
	IF procType.base # NIL THEN
		IF procType.base.form # B.tReal THEN
			x.mode := mReg; x.r := reg_A; SetAlloc(reg_A)
		ELSE x.mode := mXReg; x.r := 0; SetAllocX(0)
		END;
		x.ref := FALSE
	ELSE x.mode := mNothing
	END
END Call;

PROCEDURE StdFunc(VAR x: Item; node: Node);
	VAR id, op, L, valSize: INTEGER; r: BYTE; obj1, obj2, obj3: B.Object;
		oldStat: MakeItemState; y, z: Item; valType: B.Type;
BEGIN id := node.op; obj1 := node.left; obj2 := node.right;
	IF id = S.sfABS THEN MakeItem0(x, obj1); Load(x);
		IF x.type.form = B.tInt THEN
			EmitRR(TEST, x.r, 8, x.r); L := pc; Jcc1(ccGE, 0);
			EmitR(NEG, x.r, 8); Fixup(L, pc)
		ELSIF x.type = B.realType THEN
			r := AllocReg2({}); SetRm_reg(r);
			EmitXmmRm(SseMOVDd, x.r, 8); EmitRI(BTRi, r, 8, 63);
			SetRm_reg(r); EmitXmmRm(SseMOVD, x.r, 8); FreeReg(r)
		END
	ELSIF id = S.sfODD THEN
		ResetMkItmStat2(oldStat); MakeItem0(x, obj1); Load(x);
		EmitRI(ANDi, x.r, 4, 1); FreeReg(x.r); SetCond(x, ccNZ);
		MkItmStat := oldStat
	ELSIF id = S.sfLEN THEN (* x is open array *)
		ArrayLen(x, obj1); Load(x)
	ELSIF (id >= S.sfLSL) & (id <= S.sfROR) THEN
		oldStat := MkItmStat; AvoidUsedBy(obj2); SetAvoid(reg_C);
		MakeItem0(x, obj1); Load(x); ResetMkItmStat;
		SetBestReg(reg_C); MakeItem0(y, obj2); Load(y);
		IF y.r # reg_C THEN RelocReg(y.r, reg_C) END;
		IF id = S.sfLSL THEN op := SHLcl
		ELSIF id = S.sfROR THEN op := RORcl
		ELSIF id = S.sfASR THEN op := SARcl
		END;
		SetRm_reg(x.r); EmitRm(op, 8); FreeReg(reg_C); MkItmStat := oldStat
	ELSIF id = S.sfFLOOR THEN
		oldStat := MkItmStat; r := AllocReg(); ResetMkItmStat;
		MakeItem0(x, obj1); Load(x); EmitRI(SUBi, reg_SP, 8, 8);
		SetRm_regI(reg_SP, 0); EmitRm(STMXCSR, 4);
		EmitRmImm(BTSi, 4, 13); EmitRm(LDMXCSR, 4);
		SetRm_reg(x.r); EmitXmmRm(CVTSD2SI, r, 8); FreeXReg(x.r);	
		SetRm_regI(reg_SP, 0); EmitRmImm(BTSi, 4, 13);
		EmitRm(LDMXCSR, 4); EmitRI(ADDi, reg_SP, 8, 8);
		x.mode := mReg; x.r := r; MkItmStat := oldStat
	ELSIF id = S.sfFLT THEN
		oldStat := MkItmStat; r := AllocXReg(); ResetMkItmStat;
		MakeItem0(x, obj1); Load(x); SetRm_reg(x.r);
		EmitXmmRm(CVTSI2SD, r, 8); FreeReg(x.r);
		x.mode := mXReg; x.r := r; MkItmStat := oldStat
	ELSIF id = S.sfORD THEN MakeItem0(x, obj1); Load(x)
	ELSIF id = S.sfCHR THEN MakeItem0(x, obj1); RefToRegI(x);
		IF x.type = B.byteType THEN Load(x)
		ELSIF x.mode IN {mReg, mRegI} THEN
			SetRmOperand(x); EmitMOVZX(x.r, 2); x.mode := mReg
		ELSE SetRmOperand(x); r := AllocReg();
			EmitMOVZX(r, 2); x.mode := mReg; x.r := r
		END
	ELSIF id = S.sfADR THEN MakeItem0(x, obj1); LoadAdr(x)
	ELSIF id = S.sfBIT THEN
		ResetMkItmStat2(oldStat); AvoidUsedBy(obj2);
		MakeItem0(x, obj1); Load(x); ResetMkItmStat;
		MakeItem0(y, obj2); SetRm_regI(x.r, 0);
		IF y.mode = mImm THEN EmitRmImm(BTi, 8, y.a MOD 64)
		ELSE Load(y); EmitRegRm(BT, y.r, 8); FreeReg(y.r)
		END;
		FreeReg(x.r); SetCond(x, ccC)
	ELSIF id = S.sfVAL THEN
		valType := node.type; valSize := valType.size;
		IF valType = obj1.type THEN MakeItem0(x, obj1); Load(x)
		ELSIF (valType = B.realType) & (obj1.type.form # B.tReal) THEN
			oldStat := MkItmStat; r := AllocXReg(); ResetMkItmStat;
			MakeItem0(x, obj1); Load(x); SetRm_reg(x.r);
			EmitXmmRm(SseMOVD, r, 8); FreeReg(x.r);
			x.mode := mXReg; x.r := r; MkItmStat := oldStat
		ELSIF (obj1.type = B.realType) & (valType.form # B.tReal) THEN
			oldStat := MkItmStat; r := AllocReg(); ResetMkItmStat;
			MakeItem0(x, obj1); Load(x); SetRm_reg(r);
			EmitXmmRm(SseMOVDd, x.r, 8); FreeXReg(x.r);
			x.mode := mReg; x.r := r; MkItmStat := oldStat;
			IF valSize = 4 THEN EmitRR(MOV, x.r, 4, x.r)
			ELSIF valSize < 4 THEN SetRm_reg(x.r); EmitMOVZX(x.r, valSize)
			END
		ELSIF (obj1.type.form # B.tReal) & (valType.form # B.tReal) THEN
			MakeItem0(x, obj1); RefToRegI(x);
			IF valSize >= obj1.type.size THEN Load(x)
			ELSIF x.mode IN {mReg, mRegI} THEN SetRmOperand(x);
				IF valSize = 4 THEN EmitRegRm(MOVd, x.r, 4)
				ELSE EmitMOVZX(x.r, valSize)
				END
			ELSIF x.mode # mCond THEN
				r := AllocReg(); SetRmOperand(x);
				IF valSize = 4 THEN EmitRegRm(MOVd, r, 4)
				ELSE EmitMOVZX(r, valSize)
				END; x.r := r
			ELSE ASSERT(FALSE)
			END;
			x.mode := mReg
		ELSE ASSERT(FALSE)
		END
	ELSIF id = S.sfNtCurrentTeb THEN
		x.mode := mReg; x.r := AllocReg();
		mem.mod := 0; mem.rm := reg_SP; mem.bas := reg_BP;
		mem.idx := reg_SP; mem.scl := 0; mem.disp := 30H;
		(* GS prefix *) Put1(65H); EmitREX(x.r, 8);
		Put1(MOVd+1); EmitModRM(x.r)
	ELSIF id = S.sfCAS THEN
		obj3 := obj2(B.Node).right; obj2 := obj2(B.Node).left;
		oldStat := MkItmStat; AvoidUsedBy(obj2); AvoidUsedBy(obj3);
		SetAvoid(reg_A); MakeItem0(x, obj1); RefToRegI(x); ResetMkItmStat;
		AvoidUsedBy(obj3); SetBestReg(reg_A); MakeItem0(y, obj2); Load(y);
		ResetMkItmStat; SetAvoid(reg_A); MakeItem0(z, obj3); Load(z);
		IF y.r # reg_A THEN RelocReg(y.r, reg_A) END;
		
		SetRmOperand(x); EmitCMPXCHG(z.r, x.type.size);
		FreeReg(z.r); FreeReg2(x); MkItmStat := oldStat;
		x.mode := mReg; x.r := reg_A
	ELSE ASSERT(FALSE)
	END
END StdFunc;

PROCEDURE Becomes(node: Node);
	VAR x, y, z: Item; cx, rsize, first, L: INTEGER;
BEGIN
	IF ~(node.left.type.form IN {B.tArray, B.tRec}) THEN
		AvoidUsedBy(node.right); MakeItem0(x, node.left); ResetMkItmStat;
		MakeItem0(y, node.right); Load(y); RefToRegI(x); SetRmOperand(x);
		IF y.type = B.realType THEN EmitXmmRm(MOVSDd, y.r, 4)
		ELSE EmitRegRm(MOV, y.r, x.type.size)
		END
	ELSE
		AvoidUsedBy(node.right); SetAvoid(reg_SI); SetBestReg(reg_DI);
		MakeItem0(x, node.left); LoadAdr(x); ResetMkItmStat;
		SetBestReg(reg_SI); MakeItem0(y, node.right); LoadAdr(y);
		IF y.r # reg_SI THEN RelocReg(y.r, reg_SI) END;
		IF x.r # reg_DI THEN RelocReg(x.r, reg_DI) END;
		IF y.type = B.strType THEN cx := y.strlen;
			IF B.IsOpenArray(x.type) THEN
				ArrayLen(z, node.left); SetRmOperand(z);
				EmitRmImm(CMPi, 4, cx); Trap(ccB, stringTrap)			
			END;
			SetAlloc(reg_C); LoadImm(reg_C, 4, cx); EmitRep(MOVSrep, 2, 1)
		ELSIF B.IsStr(x.type) THEN
			SetAlloc(reg_A); SetAlloc(reg_C);
			EmitRR(XOR, reg_C, 4, reg_C); EmitRR(XOR, reg_A, 4, reg_A);
			first := pc; EmitRI(ADDi, reg_C, 4, 1);
			
			ArrayLen(z, node.left);
			IF z.mode = mImm THEN EmitRI(CMPi, reg_C, 4, z.a)
			ELSE SetRmOperand(z); EmitRegRm(CMPd, reg_C, 4)
			END; Trap(ccA, stringTrap);
			ArrayLen(z, node.right);
			IF z.mode = mImm THEN EmitRI(CMPi, reg_C, 4, z.a)
			ELSE SetRmOperand(z); EmitRegRm(CMPd, reg_C, 4)
			END; Trap(ccA, stringTrap);
			
			EmitBare(LODSW); EmitBare(STOSW);
			EmitRR(TEST, reg_A, 4, reg_A); BJump(first, ccNZ)
		ELSIF B.IsOpenArray(y.type) THEN
			SetBestReg(reg_C); ArrayLen(z, node.right); Load(z);
			IF z.r # reg_C THEN RelocReg(z.r, reg_C) END;
			
			ArrayLen(z, node.left);
			IF z.mode = mImm THEN EmitRI(CMPi, reg_C, 4, z.a)
			ELSE SetRmOperand(z); EmitRegRm(CMPd, reg_C, 4)
			END; Trap(ccA, stringTrap);
			
			rsize := y.type.base.align; cx := y.type.base.size DIV rsize;
			EmitRI(IMULi, reg_C, 8, cx); EmitRep(MOVSrep, rsize, 1)
		ELSE
			cx := x.type.size DIV x.type.align;
			SetAlloc(reg_C); LoadImm(reg_C, 4, cx);
			EmitRep(MOVSrep, x.type.align, 1)
		END
	END;
END Becomes;

PROCEDURE If(node: Node);
	VAR x, y: Item; then: Node;
BEGIN
	LoadCond(x, node.left); Jump(node, x.aLink, negated(x.c));
	FixLink(x.bLink); then := node.right(Node);
	MakeItem0(y, then.left); Jump(then, NIL, ccAlways); FixLink(node);	
	IF then.right # NIL THEN MakeItem0(y, then.right) END; FixLink(then)
END If;

PROCEDURE While(node: Node; first: INTEGER);
	VAR x, y: Item; do: Node;
BEGIN
	IF first < 0 THEN first := pc END;
	LoadCond(x, node.left); Jump(node, x.aLink, negated(x.c));
	FixLink(x.bLink); do := node.right(Node);
	MakeItem0(y, do.left); BJump(first, ccAlways); FixLink(node);
	IF do.right # NIL THEN While(do.right(Node), first) END
END While;

PROCEDURE Repeat(node: Node);
	VAR x, y: Item; first: INTEGER;
BEGIN
	first := pc; MakeItem0(x, node.left);
	LoadCond(y, node.right); Jump(node, y.aLink, negated(y.c));
	FixLinkWith(node, first); FixLink(y.bLink)
END Repeat;

PROCEDURE For(node: Node);
	VAR i, b, e: Item; control, beg, end: Node; by: B.Object;
		inc, op, opI, first: INTEGER; r, cc: BYTE;
BEGIN
	control := node.left(Node); beg := control.right(Node);
	end := beg.right(Node); by := end.right;
	
	ASSERT(control.left IS B.Var); MakeItem0(b, beg.left);
	IF (b.mode = mImm) & SmallConst(b.a) THEN
		MakeItem0(i, control.left); RefToRegI(i);
		SetRmOperand(i); EmitRmImm(MOVi, 8, b.a); FreeReg2(i)
	ELSE Load(b); MakeItem0(i, control.left); RefToRegI(i);
		SetRmOperand(i); EmitRegRm(MOV, b.r, 8); FreeReg2(i); FreeReg(b.r)
	END;	
	IF by = NIL THEN inc := 1 ELSE inc := by(Node).left(B.Const).val END;
	IF inc >= 0 THEN cc := ccG; opI := ADDi; op := ADD
	ELSE cc := ccL; opI := SUBi; op := SUB
	END;
	
	first := pc; AvoidUsedBy(end.left);
	MakeItem0(i, control.left); RefToRegI(i);
	IF (end.left IS B.Const) & SmallConst(end.left(B.Const).val) THEN
		SetRmOperand(i); EmitRmImm(CMPi, 8, end.left(B.Const).val)
	ELSE
		ResetMkItmStat; MakeItem0(e, end.left); Load(e);
		SetRmOperand(i); EmitRegRm(CMP, e.r, 8); FreeReg(e.r)
	END;
	FreeReg2(i); Jump(node, NIL, cc);
	
	MakeItem0(i, node.right); (* Statement sequence *)
	MakeItem0(i, control.left); RefToRegI(i); SetRmOperand(i);
	IF inc = 1 THEN EmitRm(INC_, 8) ELSIF inc = -1 THEN EmitRm(DEC_, 8)
	ELSIF SmallConst(inc) THEN EmitRmImm(opI, 8, inc)
	ELSE r := AllocReg(); LoadImm(r, 8, inc); EmitRegRm(op, r, 8)
	END;
	BJump(first, ccAlways); FixLink(node)
END For;

PROCEDURE Case(node: Node);
	VAR tv: B.TempVar;
	
	PROCEDURE TypeCase(node0: B.Object);
		VAR x, y: Item; node, colon: B.Node;
			obj: B.Object; tp, org: B.Type;
	BEGIN node := node0(B.Node);
		IF node.left IS B.Node THEN
			tp := node.left(B.Node).right.type;
			obj := node.left(B.Node).left; org := obj.type
		END;
		LoadCond(x, node.left); Jump(node, x.aLink, negated(x.c));
		FixLink(x.bLink); IF obj # NIL THEN obj.type := tp END;
		colon := node.right(Node); MakeItem0(y, colon.left);
		Jump(colon, NIL, ccAlways); FixLink(node);
		IF obj # NIL THEN obj.type := org END;
		IF colon.right # NIL THEN TypeCase(colon.right) END; FixLink(colon)
	END TypeCase;
	
	PROCEDURE NumericCase(node0: B.Object);
		VAR x, y: Item; node, colon: B.Node;
	BEGIN node := node0(B.Node); 
		LoadCond(x, node.left); Jump(node, x.aLink, negated(x.c));
		FixLink(x.bLink); colon := node.right(Node); MakeItem0(y, colon.left);
		Jump(colon, NIL, ccAlways); FixLink(node);
		IF colon.right # NIL THEN NumericCase(colon.right) END; FixLink(colon)
	END NumericCase;
	
BEGIN (* Case *)
	IF node.left = NIL (* type case *) THEN
		IF node.right # NIL THEN TypeCase(node.right) END
	ELSE tv := node.left(B.Node).left(B.TempVar);
		IF ~tv.inited THEN tv.adr := AllocStack(8); tv.inited := TRUE END;
		Becomes(node.left(B.Node));
		IF node.right # NIL THEN NumericCase(node.right) END;
		FreeStack(8)
	END
END Case;

PROCEDURE StdProc(node: Node);
	VAR id, size: INTEGER; r, r2: BYTE;
		x, y, z: Item; obj1, obj2, obj3: B.Object;
BEGIN
	id := node.op; obj1 := node.left; obj2 := node.right;
	IF id = S.spINC THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x);
		IF obj2 = NIL THEN SetRmOperand(x); EmitRm(INC_, x.type.size)
		ELSE SetRmOperand(x); r := AllocReg();
			EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
			MakeItem0(y, obj2); Load(y); EmitRR(ADDd, r, x.type.size, y.r);
			SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
		END
	ELSIF id = S.spDEC THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x);
		IF obj2 = NIL THEN SetRmOperand(x); EmitRm(DEC_, x.type.size)
		ELSE SetRmOperand(x); r := AllocReg();
			EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
			MakeItem0(y, obj2); Load(y); EmitRR(SUBd, r, x.type.size, y.r);
			SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
		END
	ELSIF id = S.spINCL THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); SetRmOperand(x);
		r := AllocReg(); EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
		MakeItem0(y, obj2); Load(y); EmitRR(BTS, y.r, x.type.size, r);
		SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
	ELSIF id = S.spEXCL THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); SetRmOperand(x);
		r := AllocReg(); EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
		MakeItem0(y, obj2); Load(y); EmitRR(BTR, y.r, x.type.size, r);
		SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
	ELSIF id = S.spNEW THEN
		SetBestReg(reg_C); MakeItem0(x, obj1); LoadAdr(x);
		IF x.r # reg_C THEN RelocReg(x.r, reg_C) END;
		SetBestReg(reg_D); TypeDesc(y, obj1.type.base);
		LoadAdr(y); SetRm_regI(reg_B, adrOfNEW); EmitRm(CALL, 4);
		SetAlloc(reg_B);
		IF curProc.obj.homeSpace < 32 THEN curProc.obj.homeSpace := 32 END
	ELSIF id = S.spASSERT THEN
		LoadCond(x, obj1); Jump(node, x.bLink, x.c); FixLink(x.aLink);
		EmitBare(UD2); WriteDebug(pc-2, sPos, assertTrap); FixLink(node)
	ELSIF id = S.spPACK THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); r := AllocReg();
		SetRmOperand(x); EmitRegRm(MOVd, r, 8); ResetMkItmStat;
		MakeItem0(y, obj2); Load(y); EmitRI(SHLi, y.r, 8, 52);
		EmitRR(ADDd, r, 8, y.r); SetRmOperand(x); EmitRegRm(MOV, r, 8)
	ELSIF id = S.spUNPK THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); r := AllocReg();
		SetRmOperand(x); EmitRegRm(MOVd, r, 8); ResetMkItmStat;
		MakeItem0(y, obj2); RefToRegI(y); r2 := AllocReg();
		EmitRR(MOVd, r2, 8, r); EmitRI(SHRi, r2, 8, 52);
		EmitRI(SUBi, r2, 4, 1023); SetRmOperand(y);
		EmitRegRm(MOV, r2, 8); EmitRI(SHLi, r2, 8, 52);
		EmitRR(SUBd, r, 8, r2); SetRmOperand(x); EmitRegRm(MOV, r, 8)
	ELSIF id = S.spGET THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); Load(x);
		SetRm_regI(x.r, 0); EmitRegRm(MOVd, x.r, obj2.type.size);
		ResetMkItmStat; MakeItem0(y, obj2); RefToRegI(y); SetRmOperand(y);
		EmitRegRm(MOV, x.r, y.type.size)
	ELSIF id = S.spPUT THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); Load(x);
		ResetMkItmStat; MakeItem0(y, obj2); Load(y); SetRm_regI(x.r, 0);
		EmitRegRm(MOV, y.r, y.type.size)
	ELSIF id = S.spCOPY THEN
		obj3 := obj2(Node).right; obj2 := obj2(Node).left;
		AvoidUsedBy(obj2); AvoidUsedBy(obj3); SetAvoid(reg_DI);
		SetAvoid(reg_C); SetBestReg(reg_SI); MakeItem0(x, obj1); Load(x);
		ResetMkItmStat; AvoidUsedBy(obj3); SetAvoid(reg_C);
		SetBestReg(reg_DI); MakeItem0(y, obj2); Load(y);
		ResetMkItmStat; SetBestReg(reg_C); MakeItem0(z, obj3); size := 1;
		IF z.mode = mImm THEN
			IF z.a <= 0 THEN z.a := 0; size := 0
			ELSIF z.a MOD 8 = 0 THEN z.a := z.a DIV 8; size := 8
			ELSIF z.a MOD 4 = 0 THEN z.a := z.a DIV 4; size := 4
			ELSIF z.a MOD 2 = 0 THEN z.a := z.a DIV 2; size := 2
			END
		END;
		IF size > 0 THEN Load(z);
			IF z.r # reg_C THEN RelocReg(z.r, reg_C) END;
			IF y.r # reg_DI THEN RelocReg(y.r, reg_DI) END;
			IF x.r # reg_SI THEN RelocReg(x.r, reg_SI) END;
			EmitRep(MOVSrep, size, 1)
		END
	ELSIF id = S.spLoadLibraryW THEN
		MkItmStat.avoid := {0, 1, 2, 8 .. 11};
		AvoidUsedBy(obj2); MakeItem0(x, obj1); ResetMkItmStat;
		SetBestReg(reg_C); MakeItem0(y, obj2); LoadAdr(y);
		IF y.r # reg_C THEN RelocReg(y.r, reg_C) END;
		
		SetRm_regI(reg_B, LoadLibraryW); EmitRm(CALL, 4);
		FreeReg(reg_C); SetAlloc(reg_A); SetAlloc(reg_B);
		
		RefToRegI(x); SetRmOperand(x); EmitRegRm(MOV, reg_A, 8);
		IF curProc.obj.homeSpace < 32 THEN curProc.obj.homeSpace := 32 END
	ELSIF id = S.spGetProcAddress THEN
		obj3 := obj2(Node).right; obj2 := obj2(Node).left;
		MkItmStat.avoid := {0, 1, 2, 8 .. 11}; AvoidUsedBy(obj2);
		AvoidUsedBy(obj3); MakeItem0(x, obj1); ResetMkItmStat;
		AvoidUsedBy(obj3); SetAvoid(reg_D); SetBestReg(reg_C);
		MakeItem0(y, obj2); Load(y); ResetMkItmStat;
		SetBestReg(reg_D); MakeItem0(z, obj3); Load(z);
		IF z.r # reg_D THEN RelocReg(y.r, reg_D) END;
		IF y.r # reg_C THEN RelocReg(y.r, reg_C) END;
		
		SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
		FreeReg(reg_C); FreeReg(reg_D); SetAlloc(reg_A); SetAlloc(reg_B);
		
		RefToRegI(x); SetRmOperand(x); EmitRegRm(MOV, reg_A, 8);
		IF curProc.obj.homeSpace < 32 THEN curProc.obj.homeSpace := 32 END
	ELSIF id = S.spINT3 THEN
		EmitBare(INT3)
	ELSIF id = S.spPAUSE THEN
		EmitBare(PAUSE)
	ELSE ASSERT(FALSE)
	END
END StdProc;

PROCEDURE OpImm(VAR x: Item; node: Node);
	VAR imm, cond: INTEGER; oldStat: MakeItemState;
BEGIN imm := node.right(B.Const).val;
	IF node.op = S.plus THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type.form = B.tInt THEN EmitRI(ADDi, x.r, 8, imm)
		ELSIF x.type = B.setType THEN EmitRI(ORi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF node.op = S.minus THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type.form = B.tInt THEN EmitRI(SUBi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF node.op = S.times THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type.form = B.tInt THEN EmitRI(IMULi, x.r, 8, imm)
		ELSIF x.type = B.setType THEN EmitRI(ANDi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF node.op = S.rdiv THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type = B.setType THEN EmitRI(XORi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN
		oldStat := MkItmStat; ResetMkItmStat; MakeItem0(x, node.left);
		IF x.type.size # 8 THEN Load(x) ELSE RefToRegI(x) END;
		MkItmStat := oldStat; SetRmOperand(x);
		IF imm # 0 THEN EmitRmImm(CMPi, 8, imm)
		ELSIF x.mode = mReg THEN EmitRegRm(TEST, x.r, 8)
		ELSE EmitRmImm(CMPi, 8, 0)
		END;
		FreeReg2(x); SetCond(x, IntOpToCc(node.op))
	ELSIF node.op = S.becomes THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); RefToRegI(x);
		SetRmOperand(x); EmitRmImm(MOVi, x.type.size, imm);
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSIF (node.op >= S.sfLSL) & (node.op <= S.sfROR) THEN
		MakeItem0(x, node.left); Load(x);
		IF imm = 0 THEN (* do nothing *)
		ELSIF imm = 1 THEN
			IF node.op = S.sfLSL THEN EmitR(SHL1, x.r, 8)
			ELSIF node.op = S.sfASR THEN EmitR(SAR1, x.r, 8)
			ELSE EmitR(ROR1, x.r, 8)
			END
		ELSIF node.op = S.sfLSL THEN EmitRI(SHLi, x.r, 8, imm MOD 64)
		ELSIF node.op = S.sfASR THEN EmitRI(SARi, x.r, 8, imm MOD 64)
		ELSE EmitRI(RORi, x.r, 8, imm MOD 64)
		END
	ELSIF (node.op = S.spINC) OR (node.op = S.spDEC) THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); RefToRegI(x); SetRmOperand(x);
		IF node.op = S.spINC THEN
			IF imm = 1 THEN EmitRm(INC_, x.type.size)
			ELSE EmitRmImm(ADDi, x.type.size, imm)
			END
		ELSIF imm = 1 THEN EmitRm(DEC_, x.type.size)
		ELSE EmitRmImm(SUBi, x.type.size, imm)
		END;
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSIF (node.op = S.spINCL) OR (node.op = S.spEXCL) THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); RefToRegI(x); SetRmOperand(x);
		IF node.op = S.spINCL THEN EmitRmImm(BTSi, x.type.size, imm MOD 64)
		ELSE EmitRmImm(BTRi, x.type.size, imm MOD 64)
		END;
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSIF node.op = S.spPUT THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); Load(x); SetRm_regI(x.r, 0);
		EmitRmImm(MOVi, node.right.type.size, imm);
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSE ASSERT(FALSE)
	END
END OpImm;

PROCEDURE MakeItem(VAR x: Item; obj: B.Object);
	VAR objv: B.Var; node: Node; size, form: INTEGER;
		flag: BOOLEAN; const: INTEGER;
BEGIN
	x.type := obj.type; x.ref := FALSE; x.a := 0; x.b := 0; x.c := 0;
	IF obj IS B.Const THEN x.mode := mImm; x.a := obj(B.Const).val
	ELSIF obj IS B.Var THEN x.obj := obj;
		objv := obj(B.Var); x.a := objv.adr; form := objv.type.form;
		IF objv.lev <= 0 THEN x.mode := mBX ELSE x.mode := mBP END;
		IF objv.lev < 0 THEN x.ref := TRUE END;
		IF objv IS B.Str THEN x.mode := mBX; x.strlen := objv(B.Str).len
		ELSIF objv IS B.Par THEN
			size := objv.type.size;
			x.ref := objv(B.Par).varpar OR (form = B.tArray)
				OR (form = B.tRec) & ((size > 8) OR (size IN {3, 5, 6, 7}))
		ELSIF objv IS B.TempVar THEN x.mode := mBP
		END;
		IF x.mode = mBX THEN SetAlloc(reg_B) END
	ELSIF obj IS B.Proc THEN
		x.mode := mProc; x.a := obj(B.Proc).adr;
		IF obj(B.Proc).lev >= 0 THEN x.obj := obj
		ELSE x.ref := TRUE; SetAlloc(reg_B)
		END
	ELSIF obj.class = B.cType THEN ASSERT(FALSE)
	ELSIF obj IS Node THEN
		node := obj(Node); sPos := node.sPos; x.mode := mNothing;
		IF (node.right # NIL) & (node.right IS B.Const) THEN
			const := node.right(B.Const).val;
			flag := (node.op >= S.sfLSL) & (node.op <= S.sfROR)
				OR (node.op = S.spINCL) OR (node.op = S.spEXCL)
				OR SmallConst(const)
					& (node.right.type.form # B.tReal)
					& ((node.right.type # B.setType) OR (node.op # S.minus))
					& (node.op # S.div) & (node.op # S.mod)
					& (node.op # S.and) & (node.op # S.or)
					& (node.op # S.in) & (node.op # S.lbrak)
					& ((node.op # S.becomes) OR (const # 0))
		ELSE flag := FALSE
		END;
		IF flag THEN OpImm(x, node)
		ELSIF node.op = S.plus THEN Add(x, node)
		ELSIF node.op = S.minus THEN Subtract(x, node)
		ELSIF node.op = S.times THEN Multiply(x, node)
		ELSIF (node.op = S.div) OR (node.op = S.mod) THEN IntDiv(x, node)
		ELSIF node.op = S.rdiv THEN Divide(x, node)
		ELSIF node.op = S.and THEN And(x, node)
		ELSIF node.op = S.or THEN Or(x, node)
		ELSIF node.op = S.not THEN Not(x, node)
		ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN Compare(x, node)
		ELSIF node.op = S.in THEN MemberTest(x, node)
		ELSIF node.op = S.is THEN TypeTest(x, node)
		ELSIF node.op = S.arrow THEN Deref(x, node)
		ELSIF node.op = S.period THEN Field(x, node)
		ELSIF node.op = S.lparen THEN TypeCheck(x, node)
		ELSIF node.op = S.lbrak THEN Index(x, node)
		ELSIF node.op = S.lbrace THEN RecordCast(x, node)
		ELSIF node.op = S.bitset THEN SingletonSet(x, node)
		ELSIF node.op = S.upto THEN RangeSet(x, node)
		ELSIF node.op = S.call THEN Call(x, node)
		ELSIF (node.op >= S.begSf) & (node.op <= S.endSf) THEN StdFunc(x, node)
		ELSIF (node.op = S.becomes)
		OR (node.op >= S.if) & (node.op <= S.for)
		OR (node.op >= S.begSp) & (node.op <= S.endSp) THEN
			ResetMkItmStat; allocReg := {}; allocXReg := {};
			IF node.op = S.becomes THEN Becomes(node)
			ELSIF node.op = S.if THEN If(node)
			ELSIF node.op = S.while THEN While(node, -1)
			ELSIF node.op = S.repeat THEN Repeat(node)
			ELSIF node.op = S.for THEN For(node)
			ELSIF node.op = S.case THEN Case(node)
			ELSE StdProc(node)
			END;
			ResetMkItmStat; allocReg := {}; allocXReg := {}
		ELSIF node.op = S.semicolon THEN
			IF node.left # NIL THEN MakeItem(x, node.left) END;
			IF node.right # NIL THEN MakeItem(x, node.right) END
		ELSE ASSERT(FALSE)
		END;
		IF x.mode # mNothing THEN x.type := node.type;
			IF (x.mode IN {mReg, mRegI}) & (x.r IN MkItmStat.avoid) THEN
				RelocReg(x.r, AllocReg())
			ELSIF (x.mode = mXReg) & (x.r IN MkItmStat.xAvoid) THEN
				RelocReg(x.r, AllocXReg())
			END
		END
	ELSE ASSERT(FALSE)
	END
END MakeItem;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE BeginProc;
	VAR proc: Proc;
BEGIN proc := curProc.obj;
	Align(proc.locblksize, 8); stack := proc.locblksize;
	IF pass = 2 THEN
		proc.usedReg := {}; proc.usedXReg := {};
		proc.homeSpace := 0; proc.stack := stack
	ELSIF pass = 3 THEN proc.adr := pc
	ELSE ASSERT(FALSE)
	END
END BeginProc;

PROCEDURE FinishProc;
	VAR proc: Proc; L, L2, adr: INTEGER;
BEGIN proc := curProc.obj;
	IF pass = 3 THEN
		WHILE pc MOD 16 # 0 DO Put1(90H) END;
		proc.lim := pc; L := proc.fix; adr := proc.adr;
		WHILE L # -1 DO
			L2 := code[L] + LSL(code[L+1], 8);
			INC(L2, LSL(code[L+2], 16) + LSL(code[L+3], 24));
			FixDisp(L, adr-L-4, 4); L := ASR(LSL(L2, 32), 32)
		END
	END
END FinishProc;

PROCEDURE Procedure;
	VAR locblksize, homeSpace, nSave, nSaveX, n, i, j, L: INTEGER;
		r: BYTE; x: Item; obj: B.Proc;
		param, ident: B.Ident; pType: B.Type;
BEGIN
	BeginProc; obj := curProc.obj;
	IF pass = 3 THEN
		PushR(reg_BP); EmitRR(MOVd, reg_BP, 8, reg_SP);
		nSave := 0; nSaveX := 0; r := 0;
		WHILE r < 16 DO
			IF r IN obj.usedReg*{3 .. 7, 12 .. 15} THEN INC(nSave) END;
			IF r IN obj.usedXReg*{6 .. 15} THEN INC(nSaveX) END;
			INC(r)
		END;
		n := ((obj.stack + nSave*8 + 8) DIV 16 + nSaveX) * 16;
		Align(obj.homeSpace, 16); homeSpace := obj.homeSpace;
		IF n + homeSpace # 0 THEN
			IF n + homeSpace >= 4096 THEN
				EmitRR(XOR, reg_A, 4, reg_A); EmitRI(SUBi, reg_A, 8, 4096);
				L := pc; SetRm_regX(reg_SP, reg_A, 0, 0);
				EmitRegRm(MOV, reg_A, 1); EmitRI(SUBi, reg_A, 8, 4096);
				EmitRI(CMPi, reg_A, 8, -(n + homeSpace)); BJump(L, ccGE)
			END;
			EmitRI(SUBi, reg_SP, 8, n + homeSpace)
		END;
		
		r := 0; i := 0; j := 0;
		WHILE r < 16 DO
			IF r IN obj.usedReg*{3 .. 7, 12 .. 15} THEN
				SetRm_regI(reg_BP, -n+nSaveX*16 + i*8);
				EmitRegRm(MOV, r, 8); INC(i)
			END;
			IF r IN obj.usedXReg*{6 .. 15} THEN
				SetRm_regI(reg_BP, -n + j*16);
				EmitXmmRm(MOVAPSd, r, 4); INC(j)
			END;
			INC(r)
		END;

		(* Load the base of current module to RBX *)
		IF reg_B IN obj.usedReg THEN
			SetRm_RIP(baseOffset-pc-7); EmitRegRm(LEA, reg_B, 8)
		END;
		
		IF obj.type # NIL THEN
			param := obj.type.fields; i := 0;
			WHILE (param # NIL) & (i < 4) DO
				pType := param.obj.type; SetRm_regI(reg_BP, 16+i*8);
				IF pType.form # B.tReal THEN
					EmitRegRm(MOV, ParReg(i), 8);
					IF (pType.form = B.tRec) & (param.obj(B.Par).varpar)
					OR B.IsOpenArray(pType) & ~pType.notag THEN INC(i);
						IF i < 4 THEN
							SetRm_regI(reg_BP, 16+i*8);
							EmitRegRm(MOV, ParReg(i), 8)
						END
					END
				ELSE EmitXmmRm(MOVSDd, i, 4)
				END;
				INC(i); param := param.next
			END
		END
	END;
	
	IF (obj.statseq # NIL) OR (obj.return # NIL) THEN
		IF obj.nProc + obj.nPtr > 0 THEN
			EmitRR(XOR, reg_A, 4, reg_A);
			LoadImm(reg_C, 8, -obj.locblksize DIV 8);
			L := pc; SetRm_regX(reg_BP, reg_C, 3, 0);
			EmitRegRm(MOV, reg_A, 8); EmitRI(ADDi, reg_C, 8, 1); BJump(L, ccL)
		END;
		IF obj.statseq # NIL THEN MakeItem(x, obj.statseq) END;
		IF obj.return # NIL THEN ResetMkItmStat;
			MakeItem(x, obj.return); Load(x);
			IF x.r # 0 THEN
				IF x.mode = mReg THEN RelocReg(x.r, 0) ELSE RelocXReg(x.r, 0)
				END
			END
		END
	END;
	
	IF pass = 3 THEN r := 0; i := 0; j := 0;
		WHILE r < 16 DO
			IF r IN obj.usedReg*{3 .. 7, 12 .. 15} THEN
				SetRm_regI(reg_BP, -n+nSaveX*16 + i*8);
				EmitRegRm(MOVd, r, 8); INC(i)
			END;
			IF r IN obj.usedXReg*{6 .. 15} THEN
				SetRm_regI(reg_BP, -n + j*16);
				EmitXmmRm(MOVAPS, r, 4); INC(j)
			END;
			INC(r)
		END;
		EmitRR(MOVd, reg_SP, 8, reg_BP); PopR(reg_BP); EmitBare(RET)
	END;
	FinishProc
END Procedure;

PROCEDURE ImportRTL;
	VAR L: INTEGER;
BEGIN
	SetRm_regI(reg_B, rtlName.adr); EmitRegRm(LEA, reg_C, 8);
	SetRm_regI(reg_B, LoadLibraryW); EmitRm(CALL, 4);
	EmitRR(TEST, reg_A, 8, reg_A); Trap(ccZ, rtlTrap);
	EmitRR(MOVd, reg_SI, 8, reg_A);
	
	LoadImm(reg_A, 4, 0077654EH); (* New *)
	SetRm_regI(reg_SP, 32); EmitRegRm(MOV, reg_A, 4);
	EmitRR(MOVd, reg_C, 8, reg_SI);
	SetRm_regI(reg_SP, 32); EmitRegRm(LEA, reg_D, 8);
	SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
	EmitRR(TEST, reg_A, 8, reg_A); Trap(ccZ, rtlTrap);
	SetRm_regI(reg_B, adrOfNEW); EmitRegRm(MOV, reg_A, 8);
	
	LoadImm(reg_A, 8, 746C6148H); (* Halt *) 
	SetRm_regI(reg_SP, 32); EmitRegRm(MOV, reg_A, 8);
	EmitRR(MOVd, reg_C, 8, reg_SI);
	SetRm_regI(reg_SP, 32); EmitRegRm(LEA, reg_D, 8);
	SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
	EmitRR(TEST, reg_A, 8, reg_A); Trap(ccZ, rtlTrap);
	SetRm_regI(reg_B, ExitProcess); EmitRegRm(MOV, reg_A, 8);
	
	LoadImm(reg_A, 8, 7265747369676552H); (* Register *)
	SetRm_regI(reg_SP, 32); EmitRegRm(MOV, reg_A, 8);
	SetRm_regI(reg_SP, 40); EmitRmImm(MOVi, 1, 0);
	EmitRR(MOVd, reg_C, 8, reg_SI);
	SetRm_regI(reg_SP, 32); EmitRegRm(LEA, reg_D, 8);
	SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
	EmitRR(TEST, reg_A, 8, reg_A); Trap(ccZ, rtlTrap);
	
	EmitRR(MOVd, reg_C, 8, reg_B);
	SetRm_reg(reg_A); EmitRm(CALL, 4)
END ImportRTL;

PROCEDURE DLLInit;
	VAR i, j, adr, expno, L: INTEGER;
		imod: B.Module; key: B.ModuleKey;
		ident: B.Ident; x: B.Object; t: B.TypeList; tp: B.Type;
BEGIN
	BeginProc;
	IF pass = 3 THEN
		IF B.Flag.debug THEN EmitBare(INT3) END;
		IF ~B.Flag.main THEN
			EmitRI(CMPi, reg_D, 4, 1); L := pc; Jcc1(ccZ, 0);
			EmitBare(RET); Fixup(L, pc)
		END;
		
		PushR(reg_SI); PushR(reg_DI);
		PushR(reg_B); EmitRI(SUBi, reg_SP, 8, 64);
		(* Load the base of current module to RBX *)
		SetRm_RIP(baseOffset-pc-7); EmitRegRm(LEA, reg_B, 8);
		
		(* Import USER32.DLL *)
		SetRm_regI(reg_B, user32name.adr); EmitRegRm(LEA, reg_C, 8); 
		SetRm_regI(reg_B, LoadLibraryW); EmitRm(CALL, 4);
		EmitRR(MOVd, reg_SI, 8, reg_A);
		
		EmitRR(MOVd, reg_C, 8, reg_A);
		LoadImm(reg_A, 8, 66746E6972707377H); (* RAX := 'wsprintf' *)
		SetRm_regI(reg_SP, 32); EmitRegRm(MOV, reg_A, 8);
		LoadImm(reg_A, 4, 0000000000000057H); (* RAX := 'W' *)
		SetRm_regI(reg_SP, 40); EmitRegRm(MOV, reg_A, 8);
		SetRm_regI(reg_SP, 32); EmitRegRm(LEA, reg_D, 8);
		SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
		SetRm_regI(reg_B, wsprintfW); EmitRegRm(MOV, reg_A, 8);
		
		EmitRR(MOVd, reg_C, 8, reg_SI);
		LoadImm(reg_A, 8, 426567617373654DH); (* RAX := 'MessageB' *)
		SetRm_regI(reg_SP, 32); EmitRegRm(MOV, reg_A, 8);
		LoadImm(reg_A, 4, 000000000057786FH); (* RAX := 'oxW' *)
		SetRm_regI(reg_SP, 40); EmitRegRm(MOV, reg_A, 8);
		SetRm_regI(reg_SP, 32); EmitRegRm(LEA, reg_D, 8);
		SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
		SetRm_regI(reg_B, MessageBoxW); EmitRegRm(MOV, reg_A, 8);
		
		(* Register Trap Handler *)
		EmitRR(XOR, reg_C, 4, reg_C); LoadProc(reg_D, trapProc);
		SetRm_regI(reg_B, AddVectoredExceptionHandler); EmitRm(CALL, 4);
		
		(* Set pointer to Module Pointer Table and import RTL *)
		SetRm_regI(reg_B, modPtrTable); EmitRegRm(LEA, reg_A, 8);
		SetRm_regI(reg_B, adrOfPtrTable); EmitRegRm(MOV, reg_A, 8);
		IF B.Flag.rtl THEN ImportRTL END;
		
		(* Import modules, if there are any *)
		imod := B.modList;
		WHILE imod # NIL DO
			IF imod.import OR (imod.impList # NIL) THEN
				SetRm_regI(reg_B, imod.adr); EmitRegRm(LEA, reg_C, 8);
				SetRm_regI(reg_B, LoadLibraryW); EmitRm(CALL, 4);
				EmitRR(TEST, reg_A, 8, reg_A); ModKeyTrap(ccZ, 1, imod);
				EmitRR(MOVd, reg_SI, 8, reg_A);
				
				(* Check module key *)
				key := imod.key;
				LoadImm(reg_A, 8, key[0]); SetRm_regI(reg_SI, 400H-16);
				EmitRegRm(CMPd, reg_A, 8); ModKeyTrap(ccNZ, 0, imod);
				LoadImm (reg_A, 8, key[1]); SetRm_regI(reg_SI, 400H-8);
				EmitRegRm(CMPd, reg_A, 8); ModKeyTrap(ccNZ, 0, imod);
			
				ident := imod.impList;
				WHILE ident # NIL DO x := ident.obj;
					IF x.class = B.cType THEN
						ASSERT(x.type.form = B.tRec);
						adr := x.type.adr; expno := x.type.expno
					ELSIF x IS B.Var THEN
						adr := x(B.Var).adr; expno := x(B.Var).expno
					ELSIF x IS B.Proc THEN
						adr := x(B.Proc).adr; expno := x(B.Proc).expno
					END;
					ASSERT(adr >= 128);
					EmitRR(MOVd, reg_C, 8, reg_SI); LoadImm(reg_D, 4, expno);
					SetRm_regI(reg_B, GetProcAddress); EmitRm(CALL, 4);
					SetRm_regI(reg_B, adr); EmitRegRm(MOV, reg_A, 8);
					ident := ident.next
				END
			END;
			imod := imod.next
		END;
		
		(* Fill value into type descriptors *)
		t := B.recList;
		WHILE t # NIL DO
			tp := t.type; adr := tp.adr; SetRm_regI(reg_B, adr);
			IF SmallConst(tp.size) THEN EmitRmImm(MOVi, 8, tp.size)
			ELSE ASSERT(FALSE)
			END;
			WHILE tp.len >= 1 DO
				SetRm_regI(reg_B, tp.adr); ASSERT(tp.adr >= 128);
				IF tp.mod = NIL THEN EmitRegRm(LEA, reg_A, 8)
				ELSE EmitRegRm(MOVd, reg_A, 8)
				END;
				SetRm_regI(reg_B, adr + tp.len*8);
				EmitRegRm(MOV, reg_A, 8); tp := tp.base
			END;
			t := t.next
		END;
		
		IF modInitProc # NIL THEN CallProc(modInitProc) END;

		IF B.Flag.main THEN EmitRR(XOR, reg_C, 4, reg_C);
			SetRm_regI(reg_B, ExitProcess); EmitRm(CALL, 4)
		ELSE
			LoadImm(reg_A, 4, 1); EmitRI(ADDi, reg_SP, 8, 64);
			PopR(reg_B); PopR(reg_DI); PopR(reg_SI); EmitBare(RET)
		END
	END;
	FinishProc
END DLLInit;

PROCEDURE TrapHandler;
	VAR first, L: INTEGER;
BEGIN
	BeginProc;
	IF pass = 3 THEN
		EmitRR(XOR, reg_A, 4, reg_A);
		SetRm_regI(reg_C, 0); EmitRegRm(MOVd, reg_C, 8);
		SetRm_regI(reg_C, 16); EmitRegRm(MOVd, reg_D, 8);
		SetRm_RIP(-pc-7); EmitRegRm(LEA, reg_C, 8);
		EmitRR(CMPd, reg_D, 8, reg_C); Jcc1(ccAE, 1); EmitBare(RET);
		SetRm_RIP(trapProc.adr - pc - 7); EmitRegRm(LEA, reg_C, 8);
		EmitRR(CMPd, reg_D, 8, reg_C); Jcc1(ccB, 1); EmitBare(RET);
		SetRm_regI(reg_D, 0); EmitRmImm(CMPi, 1, INT3);
		Jcc1(ccNZ, 1); EmitBare(RET);
	
		EmitRI(SUBi, reg_SP, 8, 2064 + 64 + 8);
		(* Load the base of current module to RBX *)
		SetRm_RIP(baseOffset-pc-7); EmitRegRm(LEA, reg_B, 8);
		
		EmitRR(MOVd, reg_R12, 8, reg_D); LoadImm(reg_C, 4, 6);
		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_R8, 8);
		SetRm_regI(reg_B, GetModuleHandleExW); EmitRm(CALL, 4);
		
		SetRm_regI(reg_SP, 64); EmitRegRm(MOVd, reg_SI, 8);
		SetRm_regI(reg_SI, 400H-24); EmitRegRm(MOVd, reg_DI, 8);
		EmitRR(ADDd, reg_DI, 8, reg_SI); EmitRR(SUBd, reg_R12, 8, reg_SI);
		SetRm_regI(reg_SI, 400H-32); EmitRegRm(SUBd, reg_R12, 8);
		
		first := pc; EmitRI(ADDi, reg_DI, 8, 8); SetRm_regI(reg_DI, -8);
		EmitRmImm(CMPi, 8, -1); L := pc; Jcc1(ccZ, 0); SetRm_regI(reg_DI, -8);
		EmitRegRm(MOVd, reg_C, 4); EmitRI(ANDi, reg_C, 4, 40000000H-1);
		EmitRR(CMPd, reg_C, 8, reg_R12); BJump(first, ccNZ);
		
		SetRm_regI(reg_DI, -8); EmitRegRm(MOVd, reg_R8, 8);
		EmitRI(SHRi, reg_R8, 8, 60);
		SetRm_regI(reg_DI, -8); EmitRegRm(MOVd, reg_R9, 8);
		EmitRI(SHRi, reg_R9, 8, 30); EmitRI(ANDi, reg_R9, 4, 40000000H-1);

		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_C, 8);
		SetRm_regI(reg_B, errFmtStr.adr); EmitRegRm(LEA, reg_D, 8);
		SetRm_regI(reg_B, wsprintfW); EmitRm(CALL, 4);
		
		EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_D, 8);
		SetRm_regI(reg_B, err5FmtStr.adr); EmitRegRm(LEA, reg_R8, 8);
		EmitRR(XOR, reg_R9, 4, reg_R9);
		SetRm_regI(reg_B, MessageBoxW); EmitRm(CALL, 4);
		
		EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_B, ExitProcess); EmitRm(CALL, 4);
		
		Fixup(L, pc);
		SetRm_regI(reg_SI, 400H-32); EmitRegRm(ADDd, reg_R12, 8);
		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_C, 8);
		SetRm_regI(reg_B, err3FmtStr.adr); EmitRegRm(LEA, reg_D, 8);
		EmitRR(MOVd, reg_R8, 8, reg_R12);
		SetRm_regI(reg_B, wsprintfW); EmitRm(CALL, 4);
		
		EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_D, 8);
		SetRm_regI(reg_B, err5FmtStr.adr); EmitRegRm(LEA, reg_R8, 8);
		EmitRR(XOR, reg_R9, 4, reg_R9);
		SetRm_regI(reg_B, MessageBoxW); EmitRm(CALL, 4);
		
		EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_B, ExitProcess); EmitRm(CALL, 4)
	END;
	FinishProc
END TrapHandler;

PROCEDURE ModKeyTrapHandler;
	VAR L, L2: INTEGER;
BEGIN
	BeginProc;
	IF pass = 3 THEN
		EmitRR(MOVd, reg_R13, 8, reg_C);
		PopR(reg_A); EmitRI(SUBi, reg_SP, 8, 2064 + 64);
		
		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_C, 8);
		EmitRR(TEST, reg_D, 1, reg_D); L := pc; Jcc1(ccNZ, 0);
		SetRm_regI(reg_B, err2FmtStr.adr); EmitRegRm(LEA, reg_D, 8);
		L2 := pc; Jmp1(0); Fixup(L, pc);
		SetRm_regI(reg_B, err4FmtStr.adr); EmitRegRm(LEA, reg_D, 8);
		Fixup(L2, pc); EmitRR(MOVd, reg_R8, 8, reg_R13);
		SetRm_regI(reg_B, wsprintfW); EmitRm(CALL, 4);
		
		EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_SP, 64); EmitRegRm(LEA, reg_D, 8);
		SetRm_regI(reg_B, err5FmtStr.adr); EmitRegRm(LEA, reg_R8, 8);
		EmitRR(XOR, reg_R9, 4, reg_R9);
		SetRm_regI(reg_B, MessageBoxW); EmitRm(CALL, 4);
		
		EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_B, ExitProcess); EmitRm(CALL, 4)
	END;
	FinishProc
END ModKeyTrapHandler;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Const folding during parsing phase *)
	
PROCEDURE CheckSetElement*(x: B.Object);
	VAR val: INTEGER;
BEGIN
	IF x IS B.Const THEN val := x(B.Const).val ELSE val := 0 END;
	IF (val < 0) OR (val > 63) THEN
		S.Mark('Set element must be >= 0 and <= 63')
	END
END CheckSetElement;

PROCEDURE ConstSingletonSet*(x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN
	IF x IS B.Const THEN val := x(B.Const).val MOD 64 ELSE val := 0 END;
	x := B.NewConst(B.setType, ORD({val}));
	RETURN x
END ConstSingletonSet;

PROCEDURE ConstRangeSet*(x, y: B.Object): B.Object;
	VAR beg, end: INTEGER;
BEGIN
	IF x IS B.Const THEN beg := x(B.Const).val MOD 64 ELSE beg := 0 END;
	IF y IS B.Const THEN end := y(B.Const).val MOD 64 ELSE end := 0 END;
	x := B.NewConst(B.setType, ORD({beg..end}));
	RETURN x
END ConstRangeSet;

PROCEDURE NegateConst*(x0: B.Object): B.Const;
	VAR x: B.Const; type: B.Type; val: INTEGER;
BEGIN
	type := x0.type; IF x0 IS B.Const THEN val := x0(B.Const).val END;
	x := B.NewConst(type, val);
	IF x.type = B.byteType THEN x.type := B.intType END;
	IF x.type = B.intType THEN x.val := -x.val
	ELSIF x.type = B.realType THEN
		x.val := SYSTEM.VAL(INTEGER, -SYSTEM.VAL(REAL, x.val))
	ELSIF x.type = B.setType THEN x.val := ORD(-SYSTEM.VAL(SET, x.val))
	ELSIF x.type = B.boolType THEN x.val := (x.val + 1) MOD 2
	END;
	RETURN x
END NegateConst;

PROCEDURE AbsConst*(x: B.Object): B.Object;
	VAR type: B.Type; val: INTEGER; val2: SET;
BEGIN type := x.type; val := x(B.Const).val;
	IF type = B.intType THEN
		IF val < 0 THEN val := -val END
	ELSIF type = B.byteType THEN type := B.intType
	ELSIF type = B.realType THEN
		val2 := SYSTEM.VAL(SET, val); EXCL(val2, 63); val := ORD(val2)
	END;
	x := B.NewConst(type, val)
	RETURN x
END AbsConst;

PROCEDURE OddConst*(x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN val := x(B.Const).val; x := B.NewConst(B.boolType, val MOD 2);
	RETURN x
END OddConst;

PROCEDURE ShiftConst*(fid: INTEGER; x, y: B.Object): B.Object;
	VAR xval, yval: INTEGER;
BEGIN xval := x(B.Const).val; yval := y(B.Const).val;
	IF fid = S.sfLSL THEN xval := LSL(xval, yval)
	ELSIF fid = S.sfASR THEN xval := ASR(xval, yval)
	ELSIF fid = S.sfROR THEN xval := ROR(xval, yval)
	END;
	x := B.NewConst(B.intType, xval);
	RETURN x
END ShiftConst;

PROCEDURE FloorConst*(x: B.Object): B.Object;
	VAR val, fraction, exp, p: INTEGER; sign: BOOLEAN;
BEGIN
	IF x.type = B.realType THEN
		val := x(B.Const).val; fraction := val MOD 10000000000000H;
		exp := val DIV 10000000000000H MOD 800H; sign := val < 0;
		IF exp = 0 (* subnormal *) THEN val := 0
		ELSIF exp = 7FFH (* Inf or NaN *) THEN S.Mark('Float too large')
		ELSE DEC(exp, 1023); INC(fraction, 10000000000000H); p := 52;
			IF exp < 0 THEN val := 0 ELSIF exp = 0 THEN val := 1
			ELSE WHILE (p > 0) & (exp > 0) DO DEC(p); DEC(exp) END;
				IF exp = 0 THEN val := ASR(fraction, p)
				ELSIF exp <= 11 THEN val := LSL(fraction, exp)
				ELSE S.Mark('Float too large')
				END
			END;
			IF sign THEN val := -val END
		END
	END;
	x := B.NewConst(B.intType, val);
	RETURN x
END FloorConst;

PROCEDURE FltConst*(x: B.Object): B.Object;
	CONST n52 = 10000000000000H;
	VAR val, exp, r: INTEGER; sign: BOOLEAN;
BEGIN val := x(B.Const).val;
	IF val = MinInt THEN val := -3C20000000000000H
	ELSE exp := 0; sign := val < 0; IF sign THEN val := -val END; r := 0;
		WHILE val < n52 DO val := val * 2; DEC(exp) END;
		WHILE val >= n52 * 2 DO
			INC(r, LSL(val MOD 2, exp)); val := val DIV 2; INC(exp)
		END;
		IF (exp > 0) & (r >= LSL(1, exp-1)) THEN INC(val);
			IF val >= n52 * 2 THEN val := val DIV 2; INC(exp) END
		END;
		INC(exp, 1023); val := val MOD n52 + exp * n52;
		IF sign THEN val := ORD(SYSTEM.VAL(SET, val) + {63}) END
	END;
	x := B.NewConst(B.realType, val);
	RETURN x
END FltConst;

PROCEDURE TypeTransferConst*(type: B.Type; x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN
	IF x IS B.Str THEN val := ORD(B.strbuf[x(B.Str).bufpos])
	ELSE val := x(B.Const).val
	END;
	IF type # x.type THEN
		IF type.size = 1 THEN val := val MOD 100H
		ELSIF type.size = 2 THEN val := val MOD 10000H
		ELSIF type.size = 4 THEN val := val MOD 100000000H
		END;
		x := B.NewConst(type, val)
	END;
	RETURN x
END TypeTransferConst;
	
PROCEDURE FoldConst*(op: INTEGER; x, y: B.Object): B.Object;
	VAR val, xval, yval, i, k: INTEGER; type: B.Type;
		r1, r2: REAL; xstr, ystr: B.Str; ch1, ch2: CHAR;
BEGIN
	IF (op >= S.eql) & (op <= S.in) THEN
		IF (x IS B.Const) & (y IS B.Const) & (x.type # B.realType) THEN
			xval := x(B.Const).val; yval := y(B.Const).val;
			IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
			OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
			OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
			OR (op = S.in) & (xval IN SYSTEM.VAL(SET,yval))
			THEN val := 1 ELSE val := 0
			END
		ELSIF (x IS B.Const) & (y IS B.Const) & (x.type = B.realType) THEN
			xval := x(B.Const).val; yval := y(B.Const).val;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF (op = S.eql) & (r1 = r2) OR (op = S.neq) & (r1 # r2)
			OR (op = S.gtr) & (r1 > r2) OR (op = S.geq) & (r1 >= r2)
			OR (op = S.lss) & (r1 < r2) OR (op = S.leq) & (r1 <= r2)
			THEN val := 1 ELSE val := 0
			END
		ELSIF (x IS B.Str) & (y IS B.Str) THEN
			xstr := x(B.Str); ystr := y(B.Str);
			IF (xstr.bufpos >= 0) & (ystr.bufpos >= 0) THEN
				i := xstr.bufpos; k := ystr.bufpos;
				ch1 := B.strbuf[i]; ch2 := B.strbuf[k];
				WHILE (ch1 = ch2) & (ch1 # 0X) DO
					INC(i); INC(k); ch1 := B.strbuf[i]; ch2 := B.strbuf[k] 
				END;
				IF (op = S.eql) & (ch1 = ch2) OR (op = S.neq) & (ch1 # ch2)
				OR (op = S.gtr) & (ch1 > ch2) OR (op = S.geq) & (ch1 >= ch2)
				OR (op = S.lss) & (ch1 < ch2) OR (op = S.leq) & (ch1 <= ch2)
				THEN val := 1 ELSE val := 0
				END 
			END
		END;
		type := B.boolType
	ELSIF (x IS B.Const) & (y IS B.Const) THEN
		xval := x(B.Const).val; yval := y(B.Const).val;
		IF x.type.form = B.tInt THEN type := B.intType;
			IF op = S.plus THEN val := xval + yval
			ELSIF op = S.minus THEN val := xval - yval
			ELSIF op = S.times THEN val := xval * yval
			ELSIF (op = S.div) OR (op = S.mod) THEN
				IF yval <= 0 THEN S.Mark('invalid divisor')
				ELSIF op = S.div THEN val := xval DIV yval
				ELSE val := xval MOD yval
				END
			END
		ELSIF x.type = B.setType THEN type := B.setType;
			IF op = S.plus THEN
				val := ORD(SYSTEM.VAL(SET, xval) + SYSTEM.VAL(SET, yval))
			ELSIF op = S.minus THEN
				val := ORD(SYSTEM.VAL(SET, xval) - SYSTEM.VAL(SET, yval))
			ELSIF op = S.times THEN
				val := ORD(SYSTEM.VAL(SET, xval) * SYSTEM.VAL(SET, yval))
			ELSIF op = S.rdiv THEN
				val := ORD(SYSTEM.VAL(SET, xval) / SYSTEM.VAL(SET, yval))
			END
		ELSIF x.type = B.realType THEN type := B.realType;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF op = S.plus THEN val := SYSTEM.VAL(INTEGER, r1 + r2)
			ELSIF op = S.minus THEN val := SYSTEM.VAL(INTEGER, r1 - r2)
			ELSIF op = S.times THEN val := SYSTEM.VAL(INTEGER, r1 * r2)
			ELSIF op = S.rdiv THEN val := SYSTEM.VAL(INTEGER, r1 / r2)
			END
		ELSIF x.type = B.boolType THEN type := B.boolType;
			IF op = S.or THEN
				IF (xval = 1) OR (yval = 1) THEN val := 1 ELSE val := 0 END
			ELSIF op = S.and THEN
				IF (xval = 1) & (yval = 1) THEN val := 1 ELSE val := 0 END
			END
		END
	END;
	x := B.NewConst(type, val);
	RETURN x
END FoldConst;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init*;
	VAR fname: ARRAY 128 OF CHAR;
BEGIN
	varSize := 0; staticSize := 128;
	procList := NIL; curProc := NIL;
	B.intType.size := 8; B.intType.align := 8;
	B.byteType.size := 1; B.byteType.align := 1;
	B.charType.size := 2; B.charType.align := 2;
	B.boolType.size := 1; B.boolType.align := 1;
	B.setType.size := 8; B.setType.align := 8;
	B.realType.size := 8; B.realType.align := 8;
	B.nilType.size := 8; B.nilType.align := 8;
	B.card16Type.size := 2; B.card16Type.align := 2;
	B.card32Type.size := 4; B.card32Type.align := 4;
	
	adrOfNEW := 120;
	adrOfPtrTable := 112;
	wsprintfW := 104;
	MessageBoxW := 96;
	
	AddVectoredExceptionHandler := 32;
	GetModuleHandleExW := 24;
	ExitProcess := 16;
	LoadLibraryW := 8;
	GetProcAddress := 0;

	debug := Files.New('.DebugInfo'); Files.Set(rider, debug, 0)
END Init;

PROCEDURE Generate*(VAR modinit: B.Node);
BEGIN
	(* Pass 1 *)
	pass := 1; pc := 0; Pass1(modinit);
	
	(* Pass 2 *)
	pass := 2; curProc := procList; pc := 0;
	WHILE curProc # NIL DO
		IF curProc.obj = trapProc THEN TrapHandler
		ELSIF curProc.obj = trapProc2 THEN ModKeyTrapHandler
		ELSIF curProc.obj = dllInitProc THEN DLLInit
		ELSE Procedure
		END;
		curProc := curProc.next
	END;
	
	(* Pass 3 *)
	pass := 3; curProc := procList; pc := 0;
	WHILE curProc # NIL DO
		IF curProc.obj = trapProc THEN TrapHandler
		ELSIF curProc.obj = trapProc2 THEN ModKeyTrapHandler
		ELSIF curProc.obj = dllInitProc THEN DLLInit
		ELSE Procedure
		END;
		curProc := curProc.next
	END;
	
	Linker.Link(
		debug, code,
		pc, dllInitProc.adr, staticSize, varSize, modPtrTable
	)
END Generate;

PROCEDURE Cleanup*;
BEGIN
	debug := NIL; Files.Set(rider, NIL, 0);
	
	procList := NIL; curProc := NIL; modInitProc := NIL;
	trapProc := NIL; trapProc2 := NIL; dllInitProc := NIL;
	
	errFmtStr := NIL; err2FmtStr := NIL; err3FmtStr := NIL;
	err4FmtStr := NIL; err5FmtStr := NIL; modidStr := NIL;
	rtlName := NIL; user32name := NIL
END Cleanup;

BEGIN
	MakeItem0 := MakeItem
END Generator.