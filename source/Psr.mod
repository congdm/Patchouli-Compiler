MODULE Psr;

IMPORT
	S := Scn, B := Base;

CONST
	msgIvlType = 'invalid type';

TYPE
	UndefPtrList = POINTER TO RECORD
		name: S.Ident; tp: B.Type; next: UndefPtrList
	END;

VAR
	sym: INTEGER;
	undefList: UndefPtrList;
	type0: PROCEDURE(): B.Type;
	expression0: PROCEDURE(): B.Object;

PROCEDURE MarkMissing(sym0: INTEGER);
BEGIN
	IF sym0 = S.ident THEN S.Mark('identifier missing')
	ELSIF sym0 = S.return THEN S.Mark('no RETURN')
	ELSIF sym0 = S.comma THEN S.Mark('no ,')
	ELSIF sym0 = S.semicolon THEN S.Mark('no ;')
	ELSIF sym0 = S.period THEN S.Mark('no .')
	ELSE ASSERT(FALSE)
	END
END MarkMissing;

PROCEDURE MarkSflous(sym0: INTEGER);
BEGIN
	IF sym0 = S.comma THEN S.Mark('superfluous ,')
	ELSIF sym0 = S.semicolon THEN S.Mark('superfluous ;')
	ELSE ASSERT(FALSE)
	END
END MarkSflous;

PROCEDURE Reset(VAR x: B.Object);
BEGIN x := B.NewConst(B.intType, 0)
END Reset;

PROCEDURE Check(expected: INTEGER);
BEGIN
	IF sym = expected THEN S.Get(sym)
	ELSIF expected = S.semicolon THEN S.Mark('no ;')
	ELSIF expected = S.eql THEN S.Mark('no =')
	ELSIF expected = S.colon THEN S.Mark('no :')
	ELSIF expected = S.of THEN S.Mark('no OF')
	ELSIF expected = S.end THEN S.Mark('no END')
	ELSIF expected = S.to THEN S.Mark('no TO')
	ELSIF expected = S.rparen THEN S.Mark('no )')
	ELSIF expected = S.rbrak THEN S.Mark('no ]')
	ELSIF expected = S.rbrace THEN S.Mark('no }')
	ELSIF expected = S.then THEN S.Mark('no THEN')
	ELSIF expected = S.do THEN S.Mark('no DO')
	ELSIF expected = S.until THEN S.Mark('no UNTIL')
	ELSIF expected = S.becomes THEN S.Mark('no :=')
	ELSIF expected = S.period THEN S.Mark('no .')
	ELSIF expected = S.comma THEN S.Mark('no ,')
	ELSE ASSERT(FALSE)
	END
END Check;

PROCEDURE IsStr(t: B.Type): BOOLEAN;
	RETURN (t = B.strType) OR (t.form = B.tArray) & (t.base.form = B.tChar)
END IsStr;

PROCEDURE IsExt0(t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2) OR (t1 # NIL) & IsExt0(t1.base, t2)
END IsExt0;

PROCEDURE IsExt(t1, t2: B.Type): BOOLEAN;
BEGIN
	IF t1.form = B.tPtr THEN t1 := t1.base END;
	IF t2.form = B.tPtr THEN t2 := t2.base END;
	RETURN IsExt0(t1, t2)
END IsExt;

PROCEDURE IsVarPar(x: B.Object): BOOLEAN;
	RETURN (x IS B.Par) & x(B.Par).varpar
END IsVarPar;

PROCEDURE IsConst(x: B.Object): BOOLEAN;
	RETURN (x IS B.Const) OR (x IS B.Str) & (x(B.Str).bufpos >= 0)
END IsConst;

PROCEDURE IsOpenArray(tp: B.Type): BOOLEAN;
	RETURN (tp.form = B.tArray) & (tp.len < 0)
END IsOpenArray;

PROCEDURE CompArray(t1, t2: B.Type): BOOLEAN;
	RETURN (t1.form = B.tArray) & (t2.form = B.tArray)
		& ((t1.base = t2.base) OR CompArray(t1.base, t2.base))
END CompArray;

PROCEDURE SameParType(t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR (t1.form = B.tArray) & (t1.form = B.tArray)
		& (t1.base = t2.base) & (t1.len = -1) & (t2.len = -1)
END SameParType;

PROCEDURE SamePars(p1, p2: B.Ident): BOOLEAN;
	RETURN (p1 # NIL) & (p2 # NIL)
		& (p1.obj(B.Par).varpar = p2.obj(B.Par).varpar)
		& SameParType(p1.obj.type, p2.obj.type)
		& SamePars(p1.next, p2.next)
	OR (p1 = NIL) & (p2 = NIL)
END SamePars;

PROCEDURE SameProc(t1, t2: B.Type): BOOLEAN;
	RETURN (t1.base = t2.base) & (t1.len = t2.len)
		& SamePars(t1.fields, t2.fields)
END SameProc;

PROCEDURE CompTypes(t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR (t1.form = B.tInt) & (t2.form = B.tInt)
	OR IsStr(t1) & IsStr(t2)
	OR (t1.form IN {B.tProc, B.tPtr}) & (t2 = B.nilType)
	OR (t1.form IN {B.tRec, B.tPtr}) & (t1.form = t2.form) & IsExt(t2, t1)
	OR (t1.form = B.tProc) & (t2.form = B.tProc) & SameProc(t1, t2)
END CompTypes;

PROCEDURE CompTypes2(t1, t2: B.Type): BOOLEAN;
	RETURN CompTypes(t1, t2) OR CompTypes(t2, t1)
END CompTypes2;

PROCEDURE IsCharStr(x: B.Object): BOOLEAN;
	RETURN (x IS B.Str) & (x(B.Str).len <= 2) & (x(B.Str).bufpos >= 0)
END IsCharStr;

PROCEDURE CheckInt(x: B.Object);
BEGIN
	IF x.type.form # B.tInt THEN S.Mark('not int') END
END CheckInt;

PROCEDURE CheckInt2(x: B.Object);
BEGIN
	IF x.type # B.intType THEN S.Mark('not INTEGER') END
END CheckInt2;

PROCEDURE CheckBool(x: B.Object);
BEGIN
	IF x.type # B.boolType THEN S.Mark('not bool') END
END CheckBool;

PROCEDURE CheckSet(x: B.Object);
BEGIN
	IF x.type # B.setType THEN S.Mark('not a set value') END
END CheckSet;

PROCEDURE CheckReal(x: B.Object);
BEGIN
	IF x.type.form # B.tReal THEN S.Mark('not real number') END
END CheckReal;

PROCEDURE TypeTestable(x: B.Object): BOOLEAN;
	RETURN (x.type.form = B.tPtr) & (x.type.base # NIL)
	OR (x.type.form = B.tRec) & IsVarPar(x)
END TypeTestable;

PROCEDURE CheckVar(x: B.Object; ronly: BOOLEAN);
	CONST msgRonly = 'read only';
	VAR op: INTEGER;
BEGIN op := S.null;
	IF x IS B.Node THEN op := x(B.Node).op END ;
	IF x IS B.Var THEN
		IF ~ronly & x(B.Var).ronly THEN S.Mark(msgRonly) END
	ELSIF (op = S.arrow) OR (op = S.period)
		OR (op = S.lparen) OR (op = S.lbrak)
	THEN
		IF ~ronly & x(B.Node).ronly THEN S.Mark(msgRonly) END
	ELSE S.Mark('not var')
	END
END CheckVar;

(*
PROCEDURE CheckStrLen(xtype: B.Type; y: B.Object);
BEGIN
	IF (xtype.len >= 0) & (y IS B.Str) & (y(B.Str).len > xtype.len) THEN
		Mark('string longer than dest')
	END
END CheckStrLen;

PROCEDURE CheckPar(fpar: B.Par; x: B.Object);
	CONST msgIvlParType = 'invalid par type';
	VAR xtype, ftype: B.Type; xform, fform: INTEGER;
BEGIN xtype := x.type; ftype := fpar.type;
	IF IsOpenArray(ftype) THEN CheckVar(x, fpar.ronly);
		IF IsOpenArray0(xtype) & ~ftype.notag THEN Mark('untagged open array')
		ELSIF CompArray(ftype, xtype) OR (ftype.base = B.byteType)
		OR IsStr(xtype) & IsStr(ftype) THEN (*valid*)
		ELSE Mark(msgIvlParType)
		END
	ELSIF ~fpar.varpar THEN
		IF ~CompTypes(ftype, xtype) THEN
			IF (ftype = B.charType) & (x IS B.Str) & (x(B.Str).len <= 2)
			THEN (*valid*) ELSE Mark(msgIvlParType)
			END
		ELSIF IsStr(ftype) THEN CheckStrLen(ftype, x)
		END
	ELSIF fpar.varpar THEN
		CheckVar(x, fpar.ronly); xform := xtype.form; fform := ftype.form;
		IF (xtype = ftype) OR CompArray(xtype, ftype) & (ftype.len = xtype.len)
		OR (fform = B.tRec) & (xform = B.tRec) & IsExt0(xtype, ftype)
		THEN (*valid*) ELSE Mark(msgIvlParType)
		END
	END
END CheckPar;
*)

PROCEDURE CheckLeft(x: B.Object; op: INTEGER);
BEGIN
	IF (op >= S.eql) & (op <= S.geq) THEN
		IF (x.type.form IN B.tCmps) OR IsStr(x.type)
		OR (op <= S.neq) & (x.type.form IN B.tEqls) THEN (*valid*)
		ELSE S.Mark(msgIvlType)
		END
	ELSIF op = S.is THEN
		IF TypeTestable(x) THEN (*valid*) ELSE S.Mark(msgIvlType) END
	END
END CheckLeft;

PROCEDURE CheckT(x: B.Object; forms: SET);
BEGIN
	IF x.type.form IN forms THEN (*ok*) ELSE S.Mark(msgIvlType) END
END CheckT;

PROCEDURE StrToChar(x: B.Object): B.Object;
	RETURN B.NewConst(B.charType, ORD(B.mod.strbuf[x(B.Str).bufpos]))
END StrToChar;

PROCEDURE CheckTypeObj(x: B.Object; VAR t: B.Type);
BEGIN t := B.intType;
	IF x IS B.TypeObj THEN
		IF x.type # NIL THEN t := x.type END
	ELSE S.Mark('not a type')
	END
END CheckTypeObj;

PROCEDURE AddUndef(ptr: B.Type; recName: S.Ident);
	VAR undef: UndefPtrList;
BEGIN
	NEW(undef); undef.tp := ptr; undef.name := recName;
	undef.next := undefList; undefList := undef
END AddUndef;

PROCEDURE FixUndef(rec: B.Type; recName: S.Ident);
	VAR undef, prev: UndefPtrList;
BEGIN
	undef := undefList; prev := NIL;
	WHILE (undef # NIL) & (undef.name # recName) DO
		prev := undef; undef := undef.next
	END;
	IF undef # NIL THEN undef.tp.base := rec;
		IF prev # NIL THEN prev.next := undef.next
		ELSE undefList := undef.next
		END
	END
END FixUndef;

PROCEDURE CheckUndef;
BEGIN
	IF undefList # NIL THEN
		undefList := NIL; S.Mark('some pointers didnt have base type')
	END
END CheckUndef;

PROCEDURE NewNode(op: INTEGER; x, y: B.Object; t: B.Type; p: INTEGER): B.Node;
	VAR z: B.Node;
BEGIN
	NEW(z); z.op := op; z.spos := p;
	z.left := x; z.right := y; z.type := t; z.ronly := FALSE;
	RETURN z
END NewNode;

PROCEDURE qualident0(): B.Ident;
	VAR x: B.Ident; id: S.Ident; mod: B.Module;
BEGIN
	id := S.id; x := B.topScope.first; S.Get(sym);
	WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
	IF x = NIL THEN x := B.mod.universe.first;
		WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
		IF (x # NIL) & (x.obj # NIL)
			& (x.obj IS B.Module) & (sym = S.period)
		THEN S.Get(sym);
			IF sym = S.ident THEN
				id := S.id; S.Get(sym);
				mod := x.obj(B.Module); x := mod.first;
				WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
				IF x = NIL THEN x := B.externalIdentNotFound END
			ELSE MarkMissing(S.ident)
			END
		END
	END ;
	RETURN x
END qualident0;

PROCEDURE qualident(): B.Object;
	VAR id: B.Ident; x: B.Object;
BEGIN id := qualident0();
	IF id # NIL THEN
		IF id # B.externalIdentNotFound THEN
			IF id.obj # NIL THEN x := id.obj
			ELSE S.Mark('identifier not defined yet')
			END
		ELSE S.Mark('external identifier not found')
		END
	ELSE S.Mark('identifier not found')
	END ;
	IF x # NIL THEN (*ok*) ELSE Reset(x) END ;
	RETURN x
END qualident;

PROCEDURE Call(x: B.Object): B.Node;
BEGIN
	RETURN NIL
END Call;

PROCEDURE designator(): B.Object;
	VAR x, y: B.Object; fid: S.Ident; f: B.Ident;
		node, next: B.Node; xt, yt, recType: B.Type;
		spos, yval: INTEGER; ronly: BOOLEAN;
BEGIN x := qualident();
    IF ~(x IS B.Module) & ~(x IS B.TypeObj) THEN (*ok*)
    ELSE Reset(x); S.Mark('invalid value')
    END ;
	IF x IS B.Var THEN ronly := x(B.Var).ronly ELSE ronly := FALSE END ;
	WHILE sym = S.period DO
		spos := S.pos; CheckT(x, B.tStructs); S.Get(sym);
		IF sym # S.ident THEN S.Mark('no field?')
		ELSE fid := S.id; recType := x.type;
			IF (recType.form = B.tPtr) & (recType.base # NIL) THEN
				x := NewNode(S.arrow, x, NIL, recType.base, spos);
				x(B.Node).ronly := FALSE; recType := recType.base;
				ronly := FALSE
			END ;
			IF recType.form = B.tRec THEN
				REPEAT f := recType.fields;
					WHILE (f # NIL) & (f.name # fid) DO f := f.next END ;
					IF f # NIL THEN y := f.obj;
						x := NewNode(S.period, x, y, y.type, spos);
						x(B.Node).ronly := ronly
					ELSE recType := recType.base
					END ;
					IF recType = NIL THEN S.Mark('field not found') END
				UNTIL (f # NIL) OR (recType = NIL)
			END ;
			S.Get(sym)
		END
	ELSIF sym = S.lbrak DO
		spos := S.pos; CheckT(x, {B.tArray});
		xt := x.type; S.Get(sym); y := expression0(); CheckInt(y);
		x := NewNode(S.lbrak, x, y, xt.base, spos); x(B.Node).ronly := ronly;
		IF (xt.form = B.tArray) & (xt.len >= 0) THEN
			IF y IS B.Const THEN yval := y(B.Const);
				IF (yval >= 0) & (yval < xt.len) THEN (*ok*)
				ELSE S.Mark('index out of range')
				END
			END
		END ;
		IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END ;
		WHILE sym = S.comma DO
			xt := x.type; spos := S.pos;
			IF xt.form # B.tArray THEN S.Mark('not multi-dimension') END ;
			S.Get(sym); y := expression0(); CheckInt(y);
			x := NewNode(S.lbrak, x, y, xt.base, spos);
			IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END;
			x(B.Node).ronly := ronly
		END ;
		Check(S.rbrak)
	ELSIF sym = S.arrow DO
		CheckT(x, {B.tPtr}); xt := x.type;
		x := NewNode(S.arrow, x, NIL, xt.base, S.pos);
		IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END ;
		x(B.Node).ronly := FALSE; ronly := FALSE; S.Get(sym)
	ELSIF (sym = S.lparen) & ~(x IS B.SProc) & TypeTestable(x) DO
		spos := S.pos; xt := x.type; S.Get(sym);
		IF sym = S.ident THEN y := qualident()
		ELSE Reset(y); Missing(S.ident)
		END ;
		CheckTypeObj(y, yt);
		IF yt.form = xt.form THEN(*ok*)ELSE S.Mark(msgIvlType); yt := xt END ;
		IF yt # xt THEN
			IF IsExt(yt, xt) THEN (*ok*) ELSE S.Mark('not extension') END ;
			x := NewNode(S.lparen, x, y, yt, spos); x(B.Node).ronly := ronly
		END ;
		Check(S.rparen)
	END ;
	RETURN x
END designator;

PROCEDURE StdFunc(f: B.SProc): B.Object;
	VAR par, par2: B.Node; x, y, z: B.Object; t: B.Type;
		spos: INTEGER; ch: CHAR;
BEGIN spos := S.pos; S.Get(sym);
	IF f.id = B.opABS THEN
		y := expression0(); CheckT(y, {B.tInt, B.tReal});
		IF ~(y IS B.Const) THEN
			IF y.type.form = B.tInt THEN t := B.intType ELSE t := y.type END ;
			x := NewNode(B.opABS, y, NIL, t, spos)
		ELSE x := B.AbsConst(y)
		END
	ELSIF f.id = B.opODD THEN y := expression0(); CheckInt(y);
		IF y IS B.Const THEN x := B.OddConst(y)
		ELSE x := NewNode(B.opODD, y, NIL, B.boolType, spos)
		END
	ELSIF f.id = B.opLEN THEN
		y := designator(); CheckT(y, {B.tArray, B.tStr});
		IF (y.type.form = B.tArray) & (y.type.len >= 0) THEN
			x := B.NewConst(B.intType, y.type.len)
		ELSIF (y.type.form = B.tStr) & (y(B.Str).len >= 0) THEN
			x := B.NewConst(B.intType, y(B.Str).len)
		ELSE x := NewNode(B.opLEN, y, NIL, B.intType, spos)
		END
	ELSIF (f.id >= B.opLSL) & (f.id <= B.opROR) THEN
		y := expression0(); CheckInt(y);
		Check(S.comma); z := expression0(); CheckInt(z);
		IF (y IS B.Const) & (z IS B.Const) THEN x := B.ShiftConst(f.id, y, z)
		ELSE x := NewNode(f.id, y, z, B.intType, spos)
		END
	ELSIF f.id = B.opFLOOR THEN y := expression0(); CheckReal(y);
		IF y IS B.Const THEN x := B.FloorConst(y)
		ELSE x := NewNode(B.opFLOOR, y, NIL, B.intType, spos)
		END
	ELSIF f.id = B.opFLT THEN y := expression0(); CheckInt(y);
		IF y IS B.Const THEN x := B.FltConst(y)
		ELSE x := NewNode(B.opFLT, y, NIL, B.realType, spos)
		END
	ELSIF f.id = B.opORD THEN y := expression0();
		IF y.type # B.strType THEN CheckT(y, {B.tSet, B.tBool, B.tChar})
		ELSIF IsCharStr(y) THEN y := StrToChar(y)
		ELSE Reset(y); S.Mark(msgIvlType)
		END ;
		IF y IS B.Const THEN x := B.OrdConst(y(B.Const))
		ELSE x := NewNode(B.opORD, y, NIL, B.intType, spos)
		END
	ELSIF f.id = B.opCHR THEN y := expression0(); CheckInt(y);
		IF y IS B.Const THEN x := B.IntToCharConst(y(B.Const))
		ELSE x := NewNode(B.opCHR, y, NIL, B.charType, spos)
		END
	ELSIF f.id = B.opADR THEN
		y := expression0(); CheckVar(y, TRUE);
		x := NewNode(B.opADR, y, NIL, B.intType, spos)
	ELSIF f.id = B.opSIZE THEN
		y := qualident(); CheckTypeObj(y, t);
		x := NewNode(B.opSIZE, y, NIL, B.intType, spos)
	ELSIF f.id = B.opBIT THEN
		y := expression0(); CheckInt(y);
		Check(S.comma); z := expression0(); CheckInt(z);
		x := NewNode(B.opBIT, y, z, B.boolType, spos)
	ELSIF f.id = B.opVAL THEN y := qualident(); CheckTypeObj(y, t);
		IF t.form IN B.tScalars THEN (*ok*)
		ELSE t := B.intType; S.Mark('not scalar')
		END ;
		Check(S.comma); z := expression0();
		IF (z.type.form IN B.tScalars) OR IsCharStr(z) THEN (*ok*)
		ELSE Reset(z); S.Mark('not scalar')
		END ;
		x := NewNode(B.opVAL, z, NIL, t, spos)
	ELSE ASSERT(FALSE)
	END ;
	Check(S.rparen);
	RETURN x
END StdFunc;

PROCEDURE element(): B.Object;
	VAR x, y: B.Object; spos: INTEGER;
BEGIN
	spos := S.pos; x := expression0(); CheckInt(x);
	IF sym = S.upto THEN
		spos := S.pos; S.Get(sym); y := expression0(); CheckInt(y);
		IF IsConst(x) & IsConst(y) THEN x := B.ConstRangeSet(x, y)
		ELSE x := NewNode(S.upto, x, y, B.setType, spos)
		END ;
	ELSIF IsConst(x) THEN x := B.ConstSingletonSet(x)
	ELSE x := NewNode(B.opBitset, x, NIL, B.setType, spos)
	END ;
	RETURN x
END element;

PROCEDURE set(): B.Object;
	VAR const, x, y: B.Object; node, next: B.Node; spos: INTEGER;
BEGIN
	const := B.NewConst(B.setType, 0); S.Get(sym);
	IF sym # S.rbrace THEN y := element();
		IF ~IsConst(y) THEN x := y
		ELSE const := B.FoldConst(S.plus, const, y)
		END ;
		WHILE sym = S.comma DO
			spos := S.pos; S.Get(sym);
			IF sym # S.rbrace THEN y := element();
				IF IsConst(y) THEN const := B.FoldConst(S.plus, const, y)
				ELSIF x # NIL THEN x := NewNode(S.plus, x, y, B.setType, spos)
				ELSE x := y
				END
			ELSE MarkSflous(S.comma)
			END
		END ;
		IF (const(B.Const).value # 0) & (x # NIL) THEN
			x := NewNode(S.plus, x, const, B.setType, S.pos)
		END
	END ;
	Check(S.rbrace); IF x = NIL THEN x := const END ;
	RETURN x
END set;

PROCEDURE factor(): B.Object;
	CONST msgNotFunc = 'not function';
	VAR x: B.Object; spos: INTEGER;
BEGIN
	IF sym = S.int THEN x := B.NewConst(B.intType, S.ival); S.Get(sym)
	ELSIF sym = S.real THEN x := B.NewConst(B.realType, S.ival); S.Get(sym)
	ELSIF sym = S.string THEN x := B.NewStr(S.str, S.slen); S.Get(sym)
	ELSIF sym = S.nil THEN x := B.NewConst(B.nilType, 0); S.Get(sym)
	ELSIF sym = S.true THEN x := B.NewConst(B.boolType, B.true); S.Get(sym)
	ELSIF sym = S.false THEN x := B.NewConst(B.boolType, B.false); S.Get(sym)
	ELSIF sym = S.lbrace THEN x := set()
	ELSIF sym = S.ident THEN x := designator();
		IF x IS B.SProc THEN
			IF sym = S.lparen THEN
				IF x.type # NIL THEN x := StdFunc(x(B.SProc))
				ELSE Reset(x); S.Mark(msgNotFunc)
				END
			ELSE Reset(x); S.Mark('invalid factor or no (')
			END
		ELSIF (sym = S.lparen) & (x.type.form = B.tProc) THEN
			IF x.type.base = NIL THEN S.Mark(msgNotFunc) END ;
			x := Call(x); IF x.type = NIL THEN x.type := B.intType END
		END
	ELSIF sym = S.lparen THEN S.Get(sym); x := expression0(); Check(S.rparen)
	ELSIF sym = S.not THEN
		spos := S.pos; S.Get(sym); x := factor(); CheckBool(x);
		IF ~IsConst(x) THEN x := NewNode(S.not, x, NIL, B.boolType, spos)
		ELSE x := B.NegateConst(x)
		END
	ELSE Reset(x); S.Mark('invalid factor')
	END ;
	RETURN x
END factor;

PROCEDURE term(): B.Object;
	VAR x, y: B.Object; t: B.Type; op, spos: INTEGER;
BEGIN x := factor();
	WHILE sym = S.times DO
		spos := S.pos; CheckT(x, B.tTimes); S.Get(sym); y := factor();
		IF CompTypes(x.type, y.type) THEN (*ok*)
		ELSE Reset(x); Reset(y); S.Mark(msgIvlType)
		END ;
		IF ~IsConst(x) OR ~IsConst(y) THEN
			IF x.type.form = B.tInt THEN t := B.intType ELSE t := x.type END ;
			x := NewNode(S.times, x, y, t, spos)
		ELSE x := B.FoldConst(S.times, x, y)
		END
	ELSIF sym = S.rdiv DO
		spos := S.pos; CheckT(x, B.tRdivs); S.Get(sym); y := factor();
		IF CompTypes(x.type, y.type) THEN (*ok*)
		ELSE Reset(x); Reset(y); S.Mark(msgIvlType)
		END ;
		IF ~IsConst(x) OR ~IsConst(y) THEN
			x := NewNode(S.rdiv, x, y, x.type, spos)
		ELSE x := B.FoldConst(S.rdiv, x, y)
		END
	ELSIF (sym = S.div) OR (sym = S.mod) DO
		spos := S.pos; op := sym; CheckInt(x);
		S.Get(sym); y := factor(); CheckInt(y);
		IF ~IsConst(x) OR ~IsConst(y) THEN
			x := NewNode(op, x, y, B.intType, spos)
		ELSE x := B.FoldConst(op, x, y)
		END
	ELSIF sym = S.and DO
		spos := S.pos; CheckBool(x); S.Get(sym); y := factor(); CheckBool(y);
		IF ~IsConst(x) OR ~IsConst(y) THEN
			x := NewNode(S.and, x, y, B.boolType, spos)
		ELSE x := B.FoldConst(S.and, x, y)
		END
	END ;
	RETURN x
END term;

PROCEDURE SimpleExpression(): B.Object;
	VAR x, y: B.Object; op, spos: INTEGER; t: B.Type;
BEGIN
	IF sym = S.plus THEN S.Get(sym); x := term()
	ELSIF sym = S.minus THEN
		spos := S.pos; S.Get(sym); x := term();
		IF x.type.form IN B.tAdds THEN (*ok*)
		ELSE Reset(x); S.Mark(msgIvlType)
		END ;
		IF ~IsConst(x) THEN
			IF x.type.form = B.tInt THEN t := B.intType ELSE t := x.type END ;
			x := NewNode(S.minus, x, NIL, t, spos)
		ELSE x := B.NegateConst(x)
		END
	ELSE x := term()
	END ;
	WHILE (sym = S.plus) OR (sym = S.minus) DO
		spos := S.pos; op := sym; CheckT(x, B.tAdds); S.Get(sym); y := term();
		IF CompTypes(x.type, y.type) THEN (*ok*)
		ELSE Reset(x); Reset(y); S.Mark(msgIvlType)
		END ;
		IF ~IsConst(x) OR ~IsConst(y) THEN
			IF x.type.form = B.tInt THEN t := B.intType ELSE t := x.type END ;
			x := NewNode(op, x, y, t, spos)
		ELSE x := B.FoldConst(op, x, y)
		END
	ELSIF sym = S.or DO
		spos := S.pos; CheckBool(x); S.Get(sym); y := term(); CheckBool(y);
		IF ~IsConst(x) OR ~IsConst(y) THEN
			x := NewNode(op, x, y, B.boolType, spos)
		ELSE x := B.FoldConst(S.or, x, y)
		END
	END ;
	RETURN x
END SimpleExpression;

PROCEDURE expression(): B.Object;
	VAR x, y: B.Object; yt: B.Type; op, spos: INTEGER;
BEGIN x := SimpleExpression();
	IF (sym >= S.eql) & (sym <= S.geq) THEN
		spos := S.pos; op := sym;
		CheckLeft(x, sym); S.Get(sym); y := SimpleExpression();
		IF (x.type = y.type) OR CompTypes2(x.type, y.type) THEN (* ok *)
		ELSIF (x.type = B.charType) & IsCharStr(y) THEN y := StrToChar(y)
		ELSIF (y.type = B.charType) & IsCharStr(x) THEN x := StrToChar(x)
		ELSE Reset(x); Reset(y); S.Mark(msgIvlType)
		END ;
		IF ~IsConst(x) OR ~IsConst(y) THEN
			x := NewNode(op, x, y, B.boolType, spos)
		ELSE x := B.FoldConst(op, x, y)
		END
	ELSIF sym = S.in THEN
		spos := S.pos; CheckInt(x);
		S.Get(sym); y := SimpleExpression(); CheckSet(y);
		IF ~IsConst(x) OR ~IsConst(y) THEN
			x := NewNode(S.in, x, y, B.boolType, spos)
		ELSE x := B.FoldConst(S.in, x, y)
		END
	ELSIF sym = S.is THEN
		spos := S.pos; CheckLeft(x, S.is); S.Get(sym); y := B.guard;
		IF sym = S.ident THEN y := qualident(); CheckTypeObj(y, yt);
		ELSE yt := B.intType; MarkMissing(S.ident)
		END ;
		IF x.type.form = yt.form THEN
			IF IsExt(yt, x.type) THEN (*ok*) ELSE S.Mark('not extension') END
		ELSE S.Mark(msgIvlType)
		END ;
		IF x.type # yt THEN x := NewNode(S.is, x, y, B.boolType, spos)
		ELSE x := B.NewConst(B.boolType, B.true)
		END
	END ;
	RETURN x
END expression;

PROCEDURE ConstExpression(): B.Const;
	VAR x: B.Object; c: B.Const;
BEGIN x := expression();
	IF x IS B.Const THEN c := x(B.Const)
	ELSE S.Mark('not a const'); c := B.NewConst(B.intType, 0)
	END ;
	RETURN c
END ConstExpression;

PROCEDURE StatementSequence(): B.Node;
	RETURN NIL
END StatementSequence;

PROCEDURE CheckExport(VAR expo: BOOLEAN);
BEGIN
	IF sym = S.times THEN S.Get(sym);
		IF B.curLev = 0 THEN expo := TRUE
		ELSE S.Mark('not exportable')
		END
	END
END CheckExport;

PROCEDURE IdentList(): B.Ident;
	VAR fst, x: B.Ident;
BEGIN
	B.NewIdent(fst, S.id); S.Get(sym); CheckExport(fst.expo);
	WHILE sym = S.comma DO
		IF sym = S.ident THEN
			B.NewIdent(x, S.id); S.Get(sym); CheckExport(x.expo)
		ELSE MarkSflous(S.comma)
		END
	END ;
	RETURN fst
END IdentList;

PROCEDURE FormalType(): B.Type;
	VAR x: B.Object; t: B.Type;
BEGIN t := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF x IS B.TypeObj THEN t := x.type ELSE S.Mark('not type') END
	ELSIF sym = S.array THEN
		B.NewArray(t, -1); S.Get(sym); Check(S.of);
		IF sym = S.array THEN S.Mark('multi-dim open array not supported') END ;
		t.base := FormalType()
	END ;
	RETURN t
END FormalType;

PROCEDURE FPSection(proc: B.Type);
	VAR first, x: B.Ident; tp: B.Type; varpar: BOOLEAN;
BEGIN
	IF sym = S.var THEN varpar := TRUE; S.Get(sym) ELSE varpar := FALSE END ;
	IF sym = S.ident THEN
		B.NewIdent(first, S.id); S.Get(sym);
		WHILE sym = S.comma DO S.Get(sym);
			IF sym = S.ident THEN
				B.NewIdent(x, S.id); S.Get(sym);
				IF first = NIL THEN first := x END
			ELSE MarkSflous(S.comma)
			END
		END
	ELSE S.Mark('no params?')
	END ;
	Check(S.colon); tp := FormalType(); x := first;
	WHILE x # NIL DO
		x.obj := B.NewPar(proc, tp, varpar); x := x.next
	END
END FPSection;

PROCEDURE FormalParameters(proc: B.Type);
	VAR x: B.Object;
BEGIN S.Get(sym);
	IF (sym = S.ident) OR (sym = S.var) THEN
		B.OpenScope; FPSection(proc);
		WHILE sym = S.semicolon DO S.Get(sym);
			IF (sym = S.ident) OR (sym = S.var) THEN FPSection(proc)
			ELSE S.Mark('param section?')
			END
		END ;
		proc.fields := B.topScope.first; B.CloseScope
	END ;
	Check(S.rparen);
	IF sym = S.colon THEN S.Get(sym);
		IF sym = S.ident THEN x := qualident() ELSE MarkMissing(S.ident) END ;
		IF x IS B.TypeObj THEN
			IF ~(x.type.form IN B.tStructs) THEN proc.base := x.type
			ELSE S.Mark(msgIvlType); proc.base := B.intType
			END
		ELSE S.Mark('not type')
		END
	END
END FormalParameters;

PROCEDURE PointerType(typeObj: B.TypeObj): B.Type;
	VAR ptr, b: B.Type; id: S.Ident; x: B.Ident;
BEGIN
	B.NewPointer(ptr); S.Get(sym); Check(S.to);
	IF typeObj # NIL THEN typeObj.type := ptr END ;
	IF sym = S.ident THEN id := S.id; x := qualident0();
		IF x # NIL THEN
			IF x.obj # NIL THEN CheckTypeObj(x.obj, b);
				IF b.form = B.tRec THEN ptr.base := b
				ELSE S.Mark('not a record type')
				END
			ELSIF x = B.externalIdentNotFound THEN
				S.Mark('external identifier not found')
			ELSE S.Mark('identifier not defined yet')
			END
		ELSE (* undefined base type *) AddUndef(ptr, id)
		END
	ELSIF sym = S.record THEN ptr.base := type0()
	ELSE S.Mark('base type?')
	END ;
	RETURN ptr
END PointerType;

PROCEDURE length(): INTEGER;
	VAR x: B.Const;
BEGIN x := ConstExpression();
	IF x.value < 0 THEN S.Mark('Invalid array length'); x.value := 0 END ;
	RETURN x.value
END length;

PROCEDURE BaseType(): B.Type;
	VAR b: B.Type; x: B.Object;
BEGIN
	IF sym = S.ident THEN
		x := qualident(); CheckTypeObj(x, b);
		IF b.form = B.tRec THEN (* ok *)
		ELSIF b.form = B.tPtr THEN
			IF b.base # NIL THEN b := b.base
			ELSE S.Mark('this type is not defined yet')
			END
		ELSE b := NIL; S.Mark('not record type')
		END ;
		IF (b # NIL) & (b.len >= B.MaxExt) THEN
			b := NIL; S.Mark('max extension limit reached')
		END
	ELSE MarkMissing(S.ident)
	END ;
	RETURN b
END BaseType;

PROCEDURE FieldList;
	VAR f: B.Ident; tp: B.Type;
BEGIN
	f := IdentList(); Check(S.colon); tp := type0();
	WHILE f # NIL DO f.obj := B.NewField(tp); f := f.next END
END FieldList;

PROCEDURE type(): B.Type;
	VAR t, arr: B.Type; x: B.Object; len: INTEGER;
BEGIN t := B.intType;
	IF sym = S.ident THEN
		x := qualident(); CheckTypeObj(x, t); S.Get(sym)
	ELSIF sym = S.array THEN
		S.Get(sym); len := length(); B.NewArray(t, len); arr := t;
		WHILE sym = S.comma DO S.Get(sym);
			IF sym <= S.ident THEN
				len := length(); B.NewArray(arr.base, len); arr := arr.base
			ELSE MarkSflous(S.comma)
			END
		END ;
		Check(S.of); arr.base := type()
	ELSIF sym = S.record THEN
		S.Get(sym); B.NewRecord(t);
		IF sym = S.lparen THEN
			S.Get(sym); t.base := BaseType(); Check(S.rparen);
			IF t.base # NIL THEN t.len := t.base.len + 1 END
		END ;
		B.OpenScope;
		IF sym = S.ident THEN FieldList;
			WHILE sym = S.semicolon DO S.Get(sym);
				IF sym = S.ident THEN FieldList;
				ELSE MarkSflous(S.semicolon)
				END
			END
		END ;
		t.fields := B.topScope.first; B.CloseScope; Check(S.end)
	ELSIF sym = S.pointer THEN t := PointerType(NIL)
	ELSIF sym = S.procedure THEN
	END ;
	RETURN t
END type;

PROCEDURE CloneParameters(VAR proc: B.Type);
	VAR p, id: B.Ident; x: B.Par;
BEGIN p := proc.fields;
	WHILE p # NIL DO
		B.NewIdent(id, p.name); NEW(x); id.spos := p.spos;
		id.obj := x; x^ := p.obj(B.Par)^; INC(x.lev); p := p.next
	END
END CloneParameters;

PROCEDURE DeclarationSequence;
	VAR id: B.Ident; x, ret: B.Object; tp: B.Type;
BEGIN
	IF sym = S.const THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(id, S.id); S.Get(sym);
			CheckExport(id.expo); Check(S.eql);
			id.obj := ConstExpression(); Check(S.semicolon)
		END
	END ;
	IF sym = S.type THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(id, S.id); S.Get(sym);
			CheckExport(id.expo); Check(S.eql); x := B.NewTypeObj();
			IF sym # S.pointer THEN x.type := type();
				IF x.type.form = B.tRec THEN FixUndef(x.type, id.name) END
			ELSE x.type := PointerType(x(B.TypeObj))
			END ;
			Check(S.semicolon)
		END ;
		CheckUndef
	END ;
	IF sym = S.var THEN S.Get(sym);
		WHILE sym = S.ident DO
			id := IdentList(); Check(S.colon); tp := type();
			WHILE id # NIL DO id.obj := B.NewVar(tp); id := id.next END ;
			Check(S.semicolon)
		END
	END ;
	WHILE sym = S.procedure DO S.Get(sym);
		IF sym # S.ident THEN id := NIL; S.Mark('proc name?')
		ELSE B.NewIdent(id, S.id); S.Get(sym); CheckExport(id.expo)
		END ;
		x := B.NewProc(); B.NewProcType(tp); x.type := tp;
		IF sym = S.lparen THEN FormalParameters(tp) END ;
		Check(S.semicolon); B.OpenScope; B.IncLev(1); CloneParameters(tp);
		IF id # NIL THEN id.obj := x END ;

		DeclarationSequence; x(B.Proc).decl := B.topScope.first;
		IF sym = S.begin THEN
			S.Get(sym); x(B.Proc).statseq := StatementSequence()
		END ;
		IF sym = S.return THEN
			IF tp.base = NIL THEN S.Mark('not function proc') END;
			S.Get(sym); ret := expression(); x(B.Proc).return := ret;
			IF ret.type.form IN B.tStructs THEN S.Mark(msgIvlType) END
		ELSIF tp.base # NIL THEN MarkMissing(S.return)
		END ;
		B.CloseScope; B.IncLev(-1); Check(S.end);
		IF sym = S.ident THEN
			IF (id # NIL) & (id.name # S.id) THEN
				S.Mark('wrong procedure identifier')
			END ;
			S.Get(sym)
		ELSIF id # NIL THEN MarkMissing(S.ident)
		END ;
		Check(S.semicolon)
	END
END DeclarationSequence;

PROCEDURE ImportList;
END ImportList;

PROCEDURE Module*;
	VAR modid: S.Ident;
BEGIN S.Get(sym);
	IF sym = S.ident THEN modid := S.id
	ELSE modid := 0X; MarkMissing(S.ident)
	END ;
	IF S.errcnt = 0 THEN
		B.Init(modid); Check(S.semicolon);
		IF sym = S.import THEN ImportList END
	END ;
	IF S.errcnt = 0 THEN
		DeclarationSequence;
		IF sym = S.begin THEN
			S.Get(sym); B.mod.init := StatementSequence()
		END ;
		Check(S.end);
		IF sym = S.ident THEN
			IF S.id # modid THEN S.Mark('wrong module name') END ;
			S.Get(sym)
		ELSE MarkMissing(S.ident)
		END ;
		Check(S.period)
	END
END Module;

BEGIN
	type0 := type; expression0 := expression
END Psr.