MODULE Base;

IMPORT
	SYSTEM, S := Scn;

CONST
	MaxExt* = 7;
	true* = 1; false* = 0;
	MaxInt = 9223372036854775807; MinInt = -MaxInt-1;

	(* Type form *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3; tReal* = 4;
	tPtr* = 5; tProc* = 6; tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;

	tStructs* = {tArray, tRec};
	tScalars* = {tInt, tBool, tSet, tChar, tReal, tPtr, tProc, tNil};

	tEqls* = {tBool, tSet, tPtr, tProc, tNil};
	tCmps* = {tInt, tReal, tChar, tStr};
	tAdds* = {tInt, tReal, tSet};
	tTimes* = {tInt, tReal, tSet};
	tRdivs* = {tReal, tSet};

	(* Op code *)
	opCall* = 100H; opPar* = 101H; opSproc* = 102H; opBitset* = 104H;
	opABS* = 110H; opODD* = 111H; opLEN* = 112H;
	opLSL* = 113H; opASR* = 114H; opROR* = 115H;
	opFLOOR* = 116H; opFLT* = 117H; opORD* = 118H; opCHR* = 119H;
	opADR* = 120H; opBIT* = 121H; opVAL* = 122H; opSIZE* = 123H;
	
	opINC* = 130H; opDEC* = 131H; opINCL* = 132H; opEXCL* = 133H;
	opNEW* = 134H; opASSERT* = 135H; opPACK* = 136H; opUNPK* = 137H;
	opGET* = 138H; opPUT* = 139H; opCOPY* = 140H;
	
	(* Flags *)
	flExport* = 0; flUsed* = 1; flUntagged* = 2; flUntraced* = 3;
	flUnion* = 4;

TYPE
	ModuleKey* = ARRAY 2 OF INTEGER;
	ModuleId* = RECORD context*, name*: S.Ident END ;

	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
	Node* = POINTER TO NodeDesc;
	Ident* = POINTER TO IdentDesc;
	Scope* = POINTER TO ScopeDesc;
	
	ObjDesc = RECORD flags*: SET; ident*: Ident; type*: Type END ;
	Const* = POINTER TO RECORD (ObjDesc) value*: INTEGER END ;
	TypeObj* = POINTER TO RECORD (ObjDesc) END ;
	Var* = POINTER TO RECORD (ObjDesc) lev*: INTEGER; ronly*: BOOLEAN END ;
	Par* = POINTER TO RECORD (Var) varpar*: BOOLEAN END ;
	Str* = POINTER TO RECORD (Var) bufpos*, len*: INTEGER END ;
	Field* = POINTER TO RECORD (ObjDesc) END ;
	SProc* = POINTER TO RECORD (ObjDesc) id*: INTEGER END ;

	Proc* = POINTER TO RECORD (ObjDesc)
		lev*: INTEGER;
		decl*: Ident; statseq*: Node; return*: Object
	END;

	ObjList* = POINTER TO RECORD obj*: Object; next*: ObjList END ;
	TypeList* = POINTER TO RECORD type*: Type; next*: TypeList END ;
	ProcList* = POINTER TO RECORD obj*: Proc; next*: ProcList END ;
	StrList* = POINTER TO RECORD obj*: Str; next*: StrList END ;
	
	Module* = POINTER TO RECORD (ObjDesc)
		id*: ModuleId; first*: Ident
	END ;

	NodeDesc = RECORD (ObjDesc)
		ronly*: BOOLEAN;
		op*, spos*: INTEGER;
		left*, right*: Object
	END ;

	IdentDesc = RECORD
		flags*: SET; spos*: INTEGER;
		name*: S.Ident; obj*: Object; next*: Ident
	END ;
	ScopeDesc = RECORD first*: Ident; dsc*: Scope END ;

	TypeDesc = RECORD
		flags*: SET;
		form*, len*: INTEGER;
		nPtr*, nProc*, nTraced*: INTEGER;
		fields*: Ident; base*: Type
	END ;

	CurrentModule* = POINTER TO RECORD
		id*: ModuleId;
		init*: Node; universe*: Scope;
		strbuf*: ARRAY 10000H OF CHAR; strbufSize*: INTEGER;
		strList*: StrList; recList*: TypeList;
		system*: BOOLEAN
	END ;

VAR
	externalIdentNotFound*: Ident; guard*: Object;
	mod*: CurrentModule;

	curLev*: INTEGER;
	topScope*: Scope;

	intType*, boolType*, setType*, realType*, byteType*: Type;
	charType*, strType*: Type;
	nilType*: Type;

(* Symbols table *)

PROCEDURE OpenScope*;
	VAR scp: Scope;
BEGIN
	NEW(scp); scp.dsc := topScope; topScope := scp
END OpenScope;

PROCEDURE CloseScope*;
BEGIN
	topScope := topScope.dsc
END CloseScope;

PROCEDURE IncLev*(x: INTEGER);
BEGIN
	curLev := curLev + x
END IncLev;

PROCEDURE NewIdent0*(VAR ident: Ident; name: S.Ident);
	VAR prev, x: Ident;
BEGIN x := topScope.first;
	NEW(ident); ident.flags := {}; ident.name := name;
	WHILE x # NIL DO
		IF x # NIL THEN S.Mark('duplicated ident') END ;
		prev := x; x := x.next
	END ;
	IF prev # NIL THEN prev.next := ident ELSE topScope.first := ident END
END NewIdent0;

PROCEDURE NewIdent*(VAR ident: Ident);
BEGIN NewIdent0(ident, S.id); ident.spos := S.symPos
END NewIdent;

PROCEDURE InitNewObject(x: Object);
BEGIN
	x.flags := {}
END InitNewObject;

PROCEDURE NewConst*(t: Type; val: INTEGER): Const;
	VAR c: Const;
BEGIN
	NEW(c); InitNewObject(c);
	c.type := t; c.value := val;
	RETURN c
END NewConst;

PROCEDURE NewTypeObj*(): TypeObj;
	VAR t: TypeObj;
BEGIN
	NEW(t); InitNewObject(t);
	RETURN t
END NewTypeObj;

PROCEDURE NewVar*(t: Type): Var;
	VAR v: Var;
BEGIN
	NEW(v); InitNewObject(v);
	v.type := t; v.lev := curLev; v.ronly := FALSE;
	RETURN v
END NewVar;

PROCEDURE NewPar*(proc: Type; t: Type; varpar: BOOLEAN): Par;
	VAR p: Par;
BEGIN
	NEW(p); InitNewObject(p);
	p.type := t; p.lev := curLev;
	p.varpar := varpar; INC(proc.len);
	p.ronly := ~varpar & (t.form IN tStructs);
	RETURN p
END NewPar;

PROCEDURE NewField*(VAR rec: TypeDesc; t: Type): Field;
	VAR f: Field;
BEGIN
	NEW(f); InitNewObject(f); f.type := t;
	INC(rec.nPtr, t.nPtr);
	INC(rec.nProc, t.nProc);
	INC(rec.nTraced, t.nTraced)
	RETURN f
END NewField;

PROCEDURE NewStr*(str: ARRAY OF CHAR; slen: INTEGER): Str;
	VAR x: Str; i: INTEGER; p: StrList;
BEGIN
	NEW(x); InitNewObject(x); x.ronly := TRUE;
	x.type := strType; x.lev := curLev; x.len := slen;
	IF x.lev >= -1 (* not imported str, need to alloc buffer *) THEN 
		IF mod.strbufSize + slen >= LEN(mod.strbuf) THEN
			S.Mark('too many strings'); x.bufpos := -1
		ELSE x.bufpos := mod.strbufSize; INC(mod.strbufSize, slen);
			FOR i := 0 TO slen-1 DO mod.strbuf[x.bufpos+i] := str[i] END ;
			NEW(p); p.obj := x; p.next := mod.strList; mod.strList := p
		END
	ELSE x.bufpos := -1
	END ;
	RETURN x
END NewStr;

PROCEDURE NewProc*(): Proc;
	VAR x: Proc;
BEGIN
	NEW(x); InitNewObject(x); x.lev := curLev;
	RETURN x
END NewProc;

(* types *)

PROCEDURE NewType*(VAR t: Type; form: INTEGER);
BEGIN
	NEW(t); t.form := form; t.flags := {};
	t.nPtr := 0; t.nProc := 0; t.nTraced := 0
END NewType;

PROCEDURE NewArray*(VAR t: Type; len: INTEGER);
BEGIN
	NewType(t, tArray); t.len := len;
END NewArray;

PROCEDURE CompleteArray*(VAR t: TypeDesc);
BEGIN
	IF t.base.form = tArray THEN CompleteArray(t.base^) END ;
	t.nPtr := t.len * t.base.nPtr;
	t.nTraced := t.len * t.base.nTraced;
	t.nProc := t.len * t.base.nProc
END CompleteArray;

PROCEDURE NewRecord*(VAR t: Type);
	VAR p: TypeList;
BEGIN
	NewType(t, tRec); t.len := 0;
	IF curLev >= 0 THEN
		NEW(p); p.type := t; p.next := mod.recList; mod.recList := p
	ELSIF curLev = -1 THEN ASSERT(FALSE)
	END
END NewRecord;

PROCEDURE ExtendRecord*(VAR rec: TypeDesc);
	VAR base: Type;
BEGIN
	IF rec.base # NIL THEN base := rec.base;
		rec.len := base.len + 1; rec.nPtr := base.nPtr;
		rec.nTraced := base.nTraced; rec.nProc := base.nProc
	END
END ExtendRecord;

PROCEDURE NewPointer*(VAR t: Type);
BEGIN
	NewType(t, tPtr); t.nPtr := 1; t.nTraced := 1
END NewPointer;

PROCEDURE NewProcType*(VAR t: Type);
BEGIN
	NewType(t, tProc); t.len := 0; t.nProc := 1;
END NewProcType;

(* Import module *)

PROCEDURE NewSystemModule*(ident: Ident);
END NewSystemModule;

PROCEDURE NewModule0*(ident: Ident; id: ModuleId);
END NewModule0;

PROCEDURE NewModule*(ident: Ident; name: S.Ident);
END NewModule;

(* Constanst folding *)

PROCEDURE CheckSetElement(VAR x: INTEGER);
BEGIN
	IF (x >= 0) & (x <= 63) THEN (*ok*)
	ELSE x := 0; S.Mark('set element must be >= 0 and <= 63')
	END
END CheckSetElement;

PROCEDURE ConstSingletonSet*(x: Object): Object;
	VAR val: INTEGER;
BEGIN
	val := x(Const).value; CheckSetElement(val);
	x := NewConst(setType, ORD({val}));
	RETURN x
END ConstSingletonSet;

PROCEDURE ConstRangeSet*(x, y: Object): Object;
	VAR beg, end: INTEGER;
BEGIN
	beg := x(Const).value; CheckSetElement(beg);
	end := y(Const).value; CheckSetElement(end);
	x := NewConst(setType, ORD({beg..end}));
	RETURN x
END ConstRangeSet;

PROCEDURE NegateConst*(x0: Object): Const;
	VAR x: Const; t: Type; v: INTEGER;
BEGIN t := x0.type; v := x0(Const).value;
	IF t = byteType THEN t := intType END ;
	IF t = intType THEN v := -v
	ELSIF t = realType THEN v := SYSTEM.VAL(INTEGER, -SYSTEM.VAL(REAL, v))
	ELSIF t = setType THEN v := ORD(-SYSTEM.VAL(SET, v))
	ELSIF t = boolType THEN v := (v + 1) MOD 2
	ELSE ASSERT(FALSE)
	END ;
	x := NewConst(t, v);
	RETURN x
END NegateConst;

PROCEDURE AbsConst*(x: Object): Object;
	VAR t: Type; v: INTEGER; v2: SET;
BEGIN t := x.type; v := x(Const).value;
	IF t = intType THEN
		IF v < 0 THEN v := -v END
	ELSIF t = byteType THEN t := intType
	ELSIF t = realType THEN
		v2 := SYSTEM.VAL(SET, v); EXCL(v2, 63); v := ORD(v2)
	END ;
	x := NewConst(t, v)
	RETURN x
END AbsConst;

PROCEDURE OddConst*(x: Object): Object;
	VAR v: INTEGER;
BEGIN
	v := x(Const).value; x := NewConst(boolType, v MOD 2);
	RETURN x
END OddConst;

PROCEDURE ShiftConst*(fid: INTEGER; x, y: Object): Object;
	VAR xv, yv: INTEGER;
BEGIN xv := x(Const).value; yv := y(Const).value;
	IF fid = opLSL THEN xv := LSL(xv, yv)
	ELSIF fid = opASR THEN xv := ASR(xv, yv)
	ELSIF fid = opROR THEN xv := ROR(xv, yv)
	ELSE ASSERT(FALSE)
	END ;
	x := NewConst(intType, xv);
	RETURN x
END ShiftConst;

PROCEDURE FloorConst*(x: Object): Object;
	VAR v, frac, exp, p: INTEGER; sign: BOOLEAN;
BEGIN
	IF x.type = realType THEN
		v := x(Const).value; frac := v MOD 10000000000000H;
		exp := v DIV 10000000000000H MOD 800H; sign := v < 0;
		IF exp = 0 (* subnormal *) THEN v := 0
		ELSIF exp = 7FFH (* Inf or NaN *) THEN S.Mark('float num too large')
		ELSE DEC(exp, 1023); INC(frac, 10000000000000H); p := 52;
			IF exp < 0 THEN v := 0 ELSIF exp = 0 THEN v := 1
			ELSE
				WHILE (p > 0) & (exp > 0) DO DEC(p); DEC(exp) END ;
				IF exp = 0 THEN v := ASR(frac, p)
				ELSIF exp <= 11 THEN v := LSL(frac, exp)
				ELSE S.Mark('float num too large')
				END
			END ;
			IF sign THEN v := -v END
		END
	END ;
	x := NewConst(intType, v);
	RETURN x
END FloorConst;

PROCEDURE FltConst*(x: Object): Object;
	CONST n52 = 10000000000000H;
	VAR v, exp, r: INTEGER; sign: BOOLEAN;
BEGIN v := x(Const).value;
	IF v = MinInt THEN v := -3C20000000000000H
	ELSIF v # 0 THEN exp := 52; r := 0;
		IF v < 0 THEN v := -v; sign := TRUE ELSE sign := FALSE END ;
		WHILE v < n52 DO v := v * 2; DEC(exp) END ;
		WHILE v >= n52 * 2 DO
			INC(r, LSL(v MOD 2, exp)); v := v DIV 2; INC(exp)
		END ;
		IF (exp > 0) & (r >= LSL(1, exp-1)) THEN INC(v);
			IF v >= n52 * 2 THEN v := v DIV 2; INC(exp) END
		END;
		INC(exp, 1023); v := v MOD n52 + exp * n52;
		IF sign THEN v := ORD(SYSTEM.VAL(SET, v) + {63}) END
	END ;
	x := NewConst(realType, v);
	RETURN x
END FltConst;

PROCEDURE OrdConst*(x: Const): Object;
BEGIN
	x := NewConst(intType, x.value);
	RETURN x
END OrdConst;

PROCEDURE IntToCharConst*(x: Const): Object;
BEGIN
	IF (x.value >= 0) OR (x.value <= 256) THEN (*ok*)
	ELSE S.Mark('char value should be 0 to 256')
	END ;
	x := NewConst(charType, x.value)
	RETURN x
END IntToCharConst;
	
PROCEDURE FoldConst*(op: INTEGER; x, y: Object): Object;
	VAR val, xval, yval, i, k: INTEGER; type: Type;
		r1, r2: REAL; xstr, ystr: Str; ch1, ch2: CHAR;
BEGIN
	IF (op >= S.eql) & (op <= S.in) THEN
		IF (x IS Const) & (y IS Const) & (x.type # realType) THEN
			xval := x(Const).value; yval := y(Const).value;
			IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
			OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
			OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
			OR (op = S.in) & (xval IN SYSTEM.VAL(SET,yval))
			THEN val := true ELSE val := false
			END
		ELSIF (x IS Const) & (y IS Const) & (x.type = realType) THEN
			xval := x(Const).value; yval := y(Const).value;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF (op = S.eql) & (r1 = r2) OR (op = S.neq) & (r1 # r2)
			OR (op = S.gtr) & (r1 > r2) OR (op = S.geq) & (r1 >= r2)
			OR (op = S.lss) & (r1 < r2) OR (op = S.leq) & (r1 <= r2)
			THEN val := true ELSE val := false
			END
		ELSIF (x IS Str) & (y IS Str) THEN
			xstr := x(Str); ystr := y(Str);
			IF (xstr.bufpos >= 0) & (ystr.bufpos >= 0) THEN
				i := xstr.bufpos; k := ystr.bufpos;
				ch1 := mod.strbuf[i]; ch2 := mod.strbuf[k];
				WHILE (ch1 = ch2) & (ch1 # 0X) DO
					INC(i); INC(k); ch1 := mod.strbuf[i]; ch2 := mod.strbuf[k] 
				END ;
				IF (op = S.eql) & (ch1 = ch2) OR (op = S.neq) & (ch1 # ch2)
				OR (op = S.gtr) & (ch1 > ch2) OR (op = S.geq) & (ch1 >= ch2)
				OR (op = S.lss) & (ch1 < ch2) OR (op = S.leq) & (ch1 <= ch2)
				THEN val := true ELSE val := false
				END 
			END
		END ;
		type := boolType
	ELSIF (x IS Const) & (y IS Const) THEN
		xval := x(Const).value; yval := y(Const).value;
		IF x.type.form = tInt THEN type := intType;
			IF op = S.plus THEN val := xval + yval
			ELSIF op = S.minus THEN val := xval - yval
			ELSIF op = S.times THEN val := xval * yval
			ELSIF (op = S.div) OR (op = S.mod) THEN
				IF yval <= 0 THEN S.Mark('invalid divisor')
				ELSIF op = S.div THEN val := xval DIV yval
				ELSE val := xval MOD yval
				END
			END
		ELSIF x.type = setType THEN type := setType;
			IF op = S.plus THEN
				val := ORD(SYSTEM.VAL(SET, xval) + SYSTEM.VAL(SET, yval))
			ELSIF op = S.minus THEN
				val := ORD(SYSTEM.VAL(SET, xval) - SYSTEM.VAL(SET, yval))
			ELSIF op = S.times THEN
				val := ORD(SYSTEM.VAL(SET, xval) * SYSTEM.VAL(SET, yval))
			ELSIF op = S.rdiv THEN
				val := ORD(SYSTEM.VAL(SET, xval) / SYSTEM.VAL(SET, yval))
			END
		ELSIF x.type = realType THEN type := realType;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF op = S.plus THEN val := SYSTEM.VAL(INTEGER, r1 + r2)
			ELSIF op = S.minus THEN val := SYSTEM.VAL(INTEGER, r1 - r2)
			ELSIF op = S.times THEN val := SYSTEM.VAL(INTEGER, r1 * r2)
			ELSIF op = S.rdiv THEN val := SYSTEM.VAL(INTEGER, r1 / r2)
			END
		ELSIF x.type = boolType THEN type := boolType;
			IF op = S.or THEN
				IF (xval = true) OR (yval = true)
				THEN val := true ELSE val := false
				END
			ELSIF op = S.and THEN
				IF (xval = true) & (yval = true)
				THEN val := true ELSE val := false
				END
			END
		END
	END ;
	x := NewConst(type, val);
	RETURN x
END FoldConst;

(* Init *)

PROCEDURE Init*(modid: ModuleId);
BEGIN
	NEW(mod); NEW(mod.universe); mod.id := modid;
	topScope := mod.universe; curLev := 0;

	NEW(guard); NEW(externalIdentNotFound)
END Init;

END Base.