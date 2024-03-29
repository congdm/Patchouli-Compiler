MODULE Psr;

IMPORT
	Sys, S := Scn, B := Base;

CONST
	msgIvlType = 'invalid type';

TYPE
	Parser* = B.Parser;

VAR	
	type0: PROCEDURE(psr: Parser): B.Type;
	expression0: PROCEDURE(psr: Parser): B.Object;
	StatementSequence0: PROCEDURE(psr: Parser): B.Node;	

(* -------------------------------------------------------------------------- *)

PROCEDURE NewConst(psr: Parser; t: B.Type; ival: Sys.Int): B.Const;
	VAR x: B.Const;
BEGIN
	NEW(x); x.class := B.cConst; x.type := t; x.ival := ival;
	RETURN x
END NewConst;

PROCEDURE NewConstR(psr: Parser; rval: Sys.Real): B.Const;
	VAR x: B.Const;
BEGIN
	NEW(x); x.class := B.cConst; x.type := psr.mod.realType; x.rval := rval
	RETURN x
END NewConstR;

PROCEDURE NewStr(psr: Parser; str: S.Str; slen: Sys.Int): B.Str;
BEGIN
	RETURN NIL
END NewStr;

PROCEDURE NewType(psr: Parser; form: INTEGER): B.Type;
BEGIN
	RETURN NIL
END NewType;

PROCEDURE NewPar(psr: Parser; proc, type: B.Type; varpar: BOOLEAN): B.Par;
BEGIN
	RETURN NIL
END NewPar;

PROCEDURE NewField(psr: Parser; rec, ftype: B.Type): B.Field;
BEGIN
	RETURN NIL
END NewField;

PROCEDURE NewTypeObj(psr: Parser; tp: B.Type): B.Object;
BEGIN
	RETURN NIL
END NewTypeObj;

PROCEDURE NewVar(psr: Parser; tp: B.Type): B.Var;
BEGIN
	RETURN NIL
END NewVar;

PROCEDURE NewProc(psr: Parser): B.Proc;
BEGIN
	RETURN NIL
END NewProc;

PROCEDURE Reset(psr: Parser; VAR x: B.Object);
BEGIN x := NewConst(psr, psr.mod.intType, Sys.ZeroInt)
END Reset;

(* -------------------------------------------------------------------------- *)
(* Error checking procedures *)

PROCEDURE GetSym(psr: Parser);
BEGIN S.Get(psr.scn); psr.sym := psr.scn.sym
END GetSym;

PROCEDURE Mark(psr: Parser; msg: ARRAY OF CHAR);
BEGIN S.Mark(psr.scn, msg)
END Mark;

PROCEDURE MarkMissing(psr: Parser; sym0: INTEGER);
BEGIN
	IF sym0 = S.ident THEN Mark(psr, 'identifier missing')
	ELSIF sym0 = S.return THEN Mark(psr, 'no RETURN')
	ELSIF sym0 = S.comma THEN Mark(psr, 'no ,')
	ELSIF sym0 = S.semicolon THEN Mark(psr, 'no ;')
	ELSIF sym0 = S.period THEN Mark(psr, 'no .')
	ELSE ASSERT(FALSE)
	END
END MarkMissing;

PROCEDURE MarkSflous(psr: Parser; sym0: INTEGER);
BEGIN
	IF sym0 = S.comma THEN Mark(psr, 'superfluous ,')
	ELSIF sym0 = S.semicolon THEN Mark(psr, 'superfluous ;')
	ELSE ASSERT(FALSE)
	END
END MarkSflous;

PROCEDURE Check(psr: Parser; expected: INTEGER);
BEGIN
	IF psr.sym = expected THEN GetSym(psr)
	ELSIF expected = S.semicolon THEN Mark(psr, 'no ;')
	ELSIF expected = S.eql THEN Mark(psr, 'no =')
	ELSIF expected = S.colon THEN Mark(psr, 'no :')
	ELSIF expected = S.of THEN Mark(psr, 'no OF')
	ELSIF expected = S.end THEN Mark(psr, 'no END')
	ELSIF expected = S.to THEN Mark(psr, 'no TO')
	ELSIF expected = S.rparen THEN Mark(psr, 'no )')
	ELSIF expected = S.rbrak THEN Mark(psr, 'no ]')
	ELSIF expected = S.rbrace THEN Mark(psr, 'no }')
	ELSIF expected = S.then THEN Mark(psr, 'no THEN')
	ELSIF expected = S.do THEN Mark(psr, 'no DO')
	ELSIF expected = S.until THEN Mark(psr, 'no UNTIL')
	ELSIF expected = S.becomes THEN Mark(psr, 'no :=')
	ELSIF expected = S.period THEN Mark(psr, 'no .')
	ELSIF expected = S.comma THEN Mark(psr, 'no ,')
	ELSE ASSERT(FALSE)
	END
END Check;

PROCEDURE IsStrType(psr: Parser; t: B.Type): BOOLEAN;
	RETURN (t.form = B.tStr) OR (t.form = B.tArray) & (t.base.form = B.tChar)
END IsStrType;

PROCEDURE IsExt0(psr: Parser; t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2) OR (t1 # NIL) & IsExt0(psr, t1.base, t2)
END IsExt0;

PROCEDURE IsExt(psr: Parser; t1, t2: B.Type): BOOLEAN;
BEGIN
	IF t1.form = B.tPtr THEN t1 := t1.base END ;
	IF t2.form = B.tPtr THEN t2 := t2.base END ;
	RETURN IsExt0(psr, t1, t2)
END IsExt;

PROCEDURE IsVarPar(psr: Parser; x: B.Object): BOOLEAN;
	RETURN (x IS B.Par) & x(B.Par).varpar
END IsVarPar;

PROCEDURE IsConst(psr: Parser; x: B.Object): BOOLEAN;
	RETURN (x IS B.Const) OR (x IS B.Str)
END IsConst;

PROCEDURE CompArray(psr: Parser; t1, t2: B.Type): BOOLEAN;
	RETURN (t1.form = B.tArray) & (t2.form = B.tArray)
		& ((t1.base = t2.base) OR CompArray(psr, t1.base, t2.base))
END CompArray;

PROCEDURE SameParType(psr: Parser; t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR t1.isOpenArray & t2.isOpenArray & (t1.base = t2.base)
END SameParType;

PROCEDURE SamePars(psr: Parser; p1, p2: B.Ident): BOOLEAN;
	RETURN (p1 # NIL) & (p2 # NIL)
		& (p1.obj(B.Par).varpar = p2.obj(B.Par).varpar)
		& SameParType(psr, p1.obj.type, p2.obj.type)
		& SamePars(psr, p1.next, p2.next)
	OR (p1 = NIL) & (p2 = NIL)
END SamePars;

PROCEDURE SameProc(psr: Parser; t1, t2: B.Type): BOOLEAN;
	RETURN (t1.base = t2.base) & (t1.nfpars = t2.nfpars)
		& SamePars(psr, t1.fields, t2.fields)
END SameProc;

PROCEDURE CompTypes(psr: Parser; t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR (t1.form = B.tInt) & (t2.form = B.tInt)
	OR IsStrType(psr, t1) & IsStrType(psr, t2)
	OR (t1.form IN {B.tProc, B.tPtr}) & (t2 = psr.mod.nilType)
	OR (t1.form IN {B.tRec, B.tPtr}) & (t1.form = t2.form) & IsExt(psr, t2, t1)
	OR (t1.form = B.tProc) & (t2.form = B.tProc) & SameProc(psr, t1, t2)
END CompTypes;

PROCEDURE CompTypes2(psr: Parser; t1, t2: B.Type): BOOLEAN;
	RETURN CompTypes(psr, t1, t2) OR CompTypes(psr, t2, t1)
END CompTypes2;

PROCEDURE IsCharStr(psr: Parser; x: B.Object): BOOLEAN;
	RETURN (x IS B.Str) & (Sys.CmpIntByte(x(B.Str).len, 2) <= 0)
END IsCharStr;

PROCEDURE CheckInt(psr: Parser; x: B.Object);
BEGIN
	IF x.type.form # B.tInt THEN Mark(psr, 'not int') END
END CheckInt;

PROCEDURE CheckInt2(psr: Parser; x: B.Object);
BEGIN
	IF x.type # psr.mod.intType THEN Mark(psr, 'not INTEGER') END
END CheckInt2;

PROCEDURE CheckBool(psr: Parser; x: B.Object);
BEGIN
	IF x.type.form # B.tBool THEN Mark(psr, 'not boolean') END
END CheckBool;

PROCEDURE CheckSet(psr: Parser; x: B.Object);
BEGIN
	IF x.type.form # B.tSet THEN Mark(psr, 'not a set value') END
END CheckSet;

PROCEDURE CheckReal(psr: Parser; x: B.Object);
BEGIN
	IF x.type.form # B.tReal THEN Mark(psr, 'not real number') END
END CheckReal;

PROCEDURE TypeTestable(psr: Parser; x: B.Object): BOOLEAN;
	RETURN (x.type.form = B.tPtr) & (x.type.base # NIL)
	OR (x.type.form = B.tRec) & IsVarPar(psr, x)
END TypeTestable;

PROCEDURE CheckVar(psr: Parser; x: B.Object; ronly: BOOLEAN);
	CONST msgRonly = 'read only';
	VAR op: INTEGER;
BEGIN op := S.null;
	IF x IS B.Node THEN op := x(B.Node).op END ;
	IF x IS B.Var THEN
		IF ~ronly & x(B.Var).ronly THEN Mark(psr, msgRonly) END
	ELSIF (op = S.arrow) OR (op = S.period)
		OR (op = S.lparen) OR (op = S.lbrak)
	THEN
		IF ~ronly & x(B.Node).ronly THEN Mark(psr, msgRonly) END
	ELSE Mark(psr, 'not var')
	END
END CheckVar;

PROCEDURE CheckStrLen(psr: Parser; xt: B.Type; y: B.Object);
BEGIN
	IF ~xt.isOpenArray & (y IS B.Str)
		& (Sys.CmpInt(y(B.Str).len, xt.len) > 0)
	THEN Mark(psr, 'string longer than dest')
	END
END CheckStrLen;

PROCEDURE CheckPar(psr: Parser; fpar: B.Par; x: B.Object);
	CONST msgIvlParType = 'invalid par type';
	VAR xt, ftype: B.Type; xform, fform: INTEGER;
BEGIN xt := x.type; ftype := fpar.type;
	IF ftype.isOpenArray THEN CheckVar(psr, x, fpar.ronly);
		IF CompArray(psr, ftype, xt)
		OR IsStrType(psr, xt) & IsStrType(psr, ftype) THEN (*ok*)
		ELSIF psr.mod.system & (ftype.base = psr.mod.byteType) THEN (*ok*)
		ELSE Mark(psr, msgIvlParType)
		END
	ELSIF ~fpar.varpar THEN
		IF CompTypes(psr, ftype, xt) THEN
			IF IsStrType(psr, ftype) THEN CheckStrLen(psr, ftype, x) END
		ELSIF (ftype = psr.mod.charType) & IsCharStr(psr, x) THEN (*valid*)
		ELSE Mark(psr, msgIvlParType)
		END
	ELSIF fpar.varpar THEN
		CheckVar(psr, x, fpar.ronly); xform := xt.form; fform := ftype.form;
		IF (xt = ftype)
		OR CompArray(psr, xt, ftype) & (Sys.CmpInt(ftype.len, xt.len) = 0)
		OR (fform = B.tRec) & (xform = B.tRec) & IsExt0(psr, xt, ftype)
		THEN (*valid*) ELSE Mark(psr, msgIvlParType)
		END
	END
END CheckPar;

PROCEDURE CheckLeftCmp(psr: Parser; x: B.Object; op: INTEGER);
BEGIN
	IF (x.type.form IN B.tCmps) OR IsStrType(psr, x.type)
	OR (op <= S.neq) & (x.type.form IN B.tEqls) THEN (*valid*)
	ELSE Mark(psr, msgIvlType)
	END
END CheckLeftCmp;

PROCEDURE CheckLeftIs(psr: Parser; x: B.Object);
BEGIN IF TypeTestable(psr, x) THEN (*valid*) ELSE Mark(psr, msgIvlType) END
END CheckLeftIs;

PROCEDURE CheckT(psr: Parser; x: B.Object; forms: SET);
BEGIN
	IF x.type.form IN forms THEN (*ok*) ELSE Mark(psr, msgIvlType) END
END CheckT;

PROCEDURE GetType(psr: Parser; x: B.Object): B.Type;
	VAR t: B.Type;
BEGIN
	IF x.class = B.cType THEN t := x.type
	ELSE Mark(psr, 'not a type'); t := psr.mod.intType
	END ;
	RETURN t
END GetType;

PROCEDURE CheckLev(psr: Parser; x: B.Var);
BEGIN
	IF x.lev <= 0 THEN (*ok*)
	ELSIF x.lev = psr.mod.curLev THEN (*ok*)
	ELSE Mark(psr, 'access to non-local & non-global vars not allowed')
	END
END CheckLev;

PROCEDURE CheckAssignment(psr: Parser; xtype: B.Type; y: B.Object);
BEGIN
	IF xtype = y.type THEN (*ok*)
	ELSIF CompTypes(psr, xtype, y.type) THEN
		IF IsStrType(psr, xtype) THEN CheckStrLen(psr, xtype, y) END
	ELSIF CompArray(psr, xtype, y.type) & y.type.isOpenArray THEN (*ok*)
	ELSE Mark(psr, 'invalid assignment')
	END ;
END CheckAssignment;

PROCEDURE CheckArrayIndex(psr: Parser; x, y: B.Object);
BEGIN
END CheckArrayIndex;

(* -------------------------------------------------------------------------- *)
(* undef pointers list *)

PROCEDURE AddUndef(psr: Parser; ptr: B.Type; recName: S.Ident);
	VAR undef: B.UndefPtrList;
BEGIN
	NEW(undef); undef.tp := ptr; undef.name := recName;
	undef.next := psr.undefList; psr.undefList := undef
END AddUndef;

PROCEDURE FixUndef(psr: Parser; rec: B.Type; recName: S.Ident);
	VAR undef, prev: B.UndefPtrList;
BEGIN
	undef := psr.undefList; prev := NIL;
	WHILE (undef # NIL) & (undef.name # recName) DO
		prev := undef; undef := undef.next
	END;
	IF undef # NIL THEN
		undef.tp.base := rec;
		IF prev # NIL THEN prev.next := undef.next
		ELSE psr.undefList := undef.next
		END
	END
END FixUndef;

PROCEDURE CheckUndef(psr: Parser);
BEGIN
	IF psr.undefList # NIL THEN
		psr.undefList := NIL;
		Mark(psr, 'some pointer types didnt have defined base type')
	END
END CheckUndef;

(* new parse tree node *)

PROCEDURE NewNode(psr: Parser; op: INTEGER; x, y: B.Object): B.Node;
	VAR z: B.Node;
BEGIN
	NEW(z); z.op := op; z.ronly := FALSE;
	z.left := x; z.right := y; z.spos := psr.scn.spos;
	RETURN z
END NewNode;

PROCEDURE NewNode2(
	psr: Parser; op: INTEGER; x, y: B.Object; t: B.Type; p: Sys.Int
): B.Node;
	VAR z: B.Node;
BEGIN
	z := NewNode(psr, op, x, y); z.spos := p; z.type := t;
	RETURN z
END NewNode2;

(* qualident & designator *)

PROCEDURE FindIdent(psr: Parser): B.Ident;
	VAR x: B.Ident; scope: B.Scope;
BEGIN scope := psr.mod.topScope;
	WHILE (scope # NIL) & (x = NIL) DO x := scope.first; 
		WHILE (x # NIL) & (x.name # psr.scn.id) DO x := x.next END ;
		IF x # NIL THEN (*found*) x.used := TRUE
		ELSE scope := scope.dsc
		END
	END ;
	RETURN x
END FindIdent;

PROCEDURE qualident0(psr: Parser): B.Ident;
	VAR x: B.Ident; id: S.Ident; mod: B.ExtModule;
BEGIN
	x := FindIdent(psr); GetSym(psr);
	IF (x # NIL) & (x.obj # NIL)
		& (x.obj IS B.ExtModule) & (psr.sym = S.period)
	THEN GetSym(psr);
		IF psr.sym = S.ident THEN
			id := psr.scn.id; GetSym(psr);
			mod := x.obj(B.ExtModule); x := mod.first;
			WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
			IF x # NIL THEN (*found*) x.used := TRUE
			ELSE x := psr.externalIdentNotFound
			END
		ELSE MarkMissing(psr, S.ident)
		END
	END ;
	RETURN x
END qualident0;

PROCEDURE qualident(psr: Parser): B.Object;
	VAR id: B.Ident; x: B.Object;
BEGIN id := qualident0(psr);
	IF id # NIL THEN
		IF id # psr.externalIdentNotFound THEN
			IF id.obj # NIL THEN x := id.obj
			ELSE Mark(psr, 'identifier not defined yet')
			END
		ELSE Mark(psr, 'external identifier not found')
		END
	ELSE Mark(psr, 'identifier not found')
	END ;
	IF x # NIL THEN
		IF x IS B.Var THEN CheckLev(psr, x(B.Var)) END
	ELSE Reset(psr, x)
	END ;
	RETURN x
END qualident;

PROCEDURE Call(psr: Parser; x: B.Object): B.Node;
	VAR call, last: B.Node; proc: B.Type;
		fpar: B.Ident; nacts: INTEGER;
		
	PROCEDURE Parameter(psr: Parser; VAR last: B.Node; fpar: B.Ident);
		VAR y: B.Object; par: B.Node; spos: Sys.Int;
	BEGIN
		spos := psr.scn.spos; y := expression0(psr);
		IF fpar # NIL THEN CheckPar(psr, fpar.obj(B.Par), y) END ;
		par := NewNode2(psr, B.opPar, y, NIL, NIL, spos);
		last.right := par; last := par
	END Parameter;
		
BEGIN (* Call *)
	proc := x.type;
	call := NewNode2(psr, B.opCall, x, NIL, proc.base, psr.scn.spos);
	IF psr.sym = S.lparen THEN GetSym(psr);
		IF psr.sym # S.rparen THEN last := call;
			fpar := proc.fields; Parameter(psr, last, fpar); nacts := 1;
			WHILE psr.sym = S.comma DO GetSym(psr);
				IF fpar # NIL THEN fpar := fpar.next END ;
				IF psr.sym # S.rparen THEN
					Parameter(psr, last, fpar); INC(nacts)
				ELSE MarkSflous(psr, S.comma)
				END
			END ;
			IF nacts = proc.nfpars THEN (*valid*)
			ELSIF nacts > proc.nfpars THEN Mark(psr, 'too many params')
			ELSE Mark(psr, 'not enough params')
			END ;
			Check(psr, S.rparen)
		ELSIF psr.sym = S.rparen THEN
			IF proc.nfpars # 0 THEN Mark(psr, 'need params') END ;
			GetSym(psr)
		END
	ELSIF proc.nfpars # 0 THEN Mark(psr, 'need params')
	END ;
	RETURN call
END Call;

PROCEDURE designator(psr: Parser): B.Object;
	VAR x, y: B.Object; fid: S.Ident; f: B.Ident;
		node, next: B.Node; xt, yt, recType: B.Type;
		spos: Sys.Int; ronly: BOOLEAN;
BEGIN x := qualident(psr);
    IF ~(x IS B.ExtModule) & ~(x.class = B.cType) THEN (*ok*)
    ELSE Reset(psr, x); Mark(psr, 'invalid value')
    END ;
	IF x IS B.Var THEN ronly := x(B.Var).ronly ELSE ronly := FALSE END ;
	WHILE psr.sym = S.period DO
		spos := psr.scn.spos; CheckT(psr, x, B.tStructs); GetSym(psr);
		IF psr.sym # S.ident THEN Mark(psr, 'no field?')
		ELSE fid := psr.scn.id; recType := x.type;
			IF (recType.form = B.tPtr) & (recType.base # NIL) THEN
				x := NewNode2(psr, S.arrow, x, NIL, recType.base, spos);
				x(B.Node).ronly := FALSE; recType := recType.base;
				ronly := FALSE
			END ;
			IF recType.form = B.tRec THEN
				REPEAT f := recType.fields;
					WHILE (f # NIL) & (f.name # fid) DO f := f.next END ;
					IF f # NIL THEN y := f.obj;
						x := NewNode2(psr, S.period, x, y, y.type, spos);
						x(B.Node).ronly := ronly
					ELSE recType := recType.base
					END ;
					IF recType = NIL THEN Mark(psr, 'field not found') END
				UNTIL (f # NIL) OR (recType = NIL)
			END ;
			GetSym(psr)
		END
	ELSIF psr.sym = S.lbrak DO
		spos := psr.scn.spos; CheckT(psr, x, {B.tArray});
		xt := x.type; GetSym(psr); y := expression0(psr);
		CheckInt(psr, y); CheckArrayIndex(psr, x, y);
		x := NewNode2(psr, S.lbrak, x, y, xt.base, spos);
		IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END ;
		x(B.Node).ronly := ronly;
		WHILE psr.sym = S.comma DO spos := psr.scn.spos;
			IF xt.form # B.tArray THEN Mark(psr, 'not multi-dimension') END ;
			xt := x.type; GetSym(psr); y := expression0(psr);
			CheckInt(psr, y); CheckArrayIndex(psr, x, y);
			x := NewNode2(psr, S.lbrak, x, y, xt.base, spos);
			IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END ;
			x(B.Node).ronly := ronly
		END ;
		Check(psr, S.rbrak)
	ELSIF psr.sym = S.arrow DO
		CheckT(psr, x, {B.tPtr}); xt := x.type;
		x := NewNode2(psr, S.arrow, x, NIL, xt.base, psr.scn.spos);
		IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END ;
		x(B.Node).ronly := FALSE; ronly := FALSE; GetSym(psr)
	ELSIF (psr.sym = S.lparen) & ~(x IS B.SFunc) & TypeTestable(psr, x) DO
		spos := psr.scn.spos; xt := x.type; GetSym(psr);
		IF psr.sym = S.ident THEN y := qualident(psr)
		ELSE Reset(psr, y); MarkMissing(psr, S.ident)
		END ;
		yt := GetType(psr, y);
		IF yt.form = xt.form THEN (*ok*)
		ELSE Mark(psr, msgIvlType); yt := xt
		END ;
		IF yt # xt THEN
			IF IsExt(psr, yt, xt) THEN (*ok*)
			ELSE Mark(psr, 'not extension')
			END ;
			x := NewNode2(psr, S.lparen, x, y, yt, spos);
			x(B.Node).ronly := ronly
		END ;
		Check(psr, S.rparen)
	END ;
	RETURN x
END designator;

(* expressions *)

PROCEDURE OpAbs(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpAbs;

PROCEDURE OpOdd(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpOdd;

PROCEDURE OpShift(psr: Parser; op: INTEGER; x, y: B.Const): B.Const;
	RETURN NIL
END OpShift;

PROCEDURE OpFloor(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpFloor;

PROCEDURE OpFlt(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpFlt;

PROCEDURE OpChr(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpChr;

PROCEDURE OpOrdChar(psr: Parser; x: B.Str): B.Const;
	RETURN NIL
END OpOrdChar;

PROCEDURE OpRangeSet(psr: Parser; x, y: B.Const): B.Const;
	RETURN NIL
END OpRangeSet;

PROCEDURE OpSingletonSet(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpSingletonSet;

PROCEDURE OpAdd(psr: Parser; op: INTEGER; x, y: B.Const): B.Const;
	RETURN NIL
END OpAdd;

PROCEDURE OpNegateBool(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpNegateBool;

PROCEDURE OpMultiply(psr: Parser; x, y: B.Const): B.Const;
	RETURN NIL
END OpMultiply;

PROCEDURE OpRDivide(psr: Parser; x, y: B.Const): B.Const;
	RETURN NIL
END OpRDivide;

PROCEDURE OpIntDiv(psr: Parser; op: INTEGER; x, y: B.Const): B.Const;
	RETURN NIL
END OpIntDiv;

PROCEDURE OpAnd(psr: Parser; x, y: B.Const): B.Const;
	RETURN NIL
END OpAnd;

PROCEDURE OpNegate(psr: Parser; x: B.Const): B.Const;
	RETURN NIL
END OpNegate;

PROCEDURE OpOr(psr: Parser; x, y: B.Const): B.Const;
	RETURN NIL
END OpOr;

PROCEDURE OpCompare(psr: Parser; op: INTEGER; x, y: B.Object): B.Const;
	RETURN NIL
END OpCompare;

PROCEDURE OpIn(psr: Parser; x, y: B.Const): B.Const;
	RETURN NIL
END OpIn;

PROCEDURE StdFunc(psr: Parser; f: B.SFunc): B.Object;
	VAR par, par2: B.Node; x, y, z: B.Object; t: B.Type;
		spos: Sys.Int; ch: CHAR;
BEGIN spos := psr.scn.spos; GetSym(psr);
	IF f.id = B.opABS THEN
		y := expression0(psr); CheckT(psr, y, {B.tInt, B.tReal});
		IF ~(y IS B.Const) THEN
			IF y.type.form = B.tInt THEN t := psr.mod.intType ELSE t := y.type END ;
			x := NewNode2(psr, B.opABS, y, NIL, t, spos)
		ELSE x := OpAbs(psr, y(B.Const))
		END
	ELSIF f.id = B.opODD THEN y := expression0(psr); CheckInt(psr, y);
		IF ~(y IS B.Const) THEN
			x := NewNode2(psr, B.opODD, y, NIL, psr.mod.boolType, spos)
		ELSE x := OpOdd(psr, y(B.Const))
		END
	ELSIF f.id = B.opLEN THEN
		y := designator(psr); CheckT(psr, y, {B.tArray, B.tStr});
		IF (y.type.form = B.tArray) & ~y.type.isOpenArray THEN
			x := NewConst(psr, psr.mod.intType, y.type.len)
		ELSIF y.type.form = B.tStr THEN
			x := NewConst(psr, psr.mod.intType, y(B.Str).len)
		ELSE x := NewNode2(psr, B.opLEN, y, NIL, psr.mod.intType, spos)
		END
	ELSIF (f.id >= B.opLSL) & (f.id <= B.opROR) THEN
		y := expression0(psr); CheckInt(psr, y);
		Check(psr, S.comma); z := expression0(psr); CheckInt(psr, z);
		IF ~(y IS B.Const) OR ~(z IS B.Const) THEN
			x := NewNode2(psr, f.id, y, z, psr.mod.intType, spos)
		ELSE x := OpShift(psr, f.id, y(B.Const), z(B.Const))
		END
	ELSIF f.id = B.opFLOOR THEN y := expression0(psr); CheckReal(psr, y);
		IF ~(y IS B.Const) THEN
			x := NewNode2(psr, B.opFLOOR, y, NIL, psr.mod.intType, spos)
		ELSE x := OpFloor(psr, y(B.Const))
		END
	ELSIF f.id = B.opFLT THEN y := expression0(psr); CheckInt(psr, y);
		IF ~(y IS B.Const) THEN
			x := NewNode2(psr, B.opFLT, y, NIL, psr.mod.realType, spos)
		ELSE x := OpFlt(psr, y(B.Const))
		END
	ELSIF f.id = B.opORD THEN y := expression0(psr);
		IF y.type.form # B.tStr THEN CheckT(psr, y, {B.tSet, B.tBool, B.tChar})
		ELSIF ~IsCharStr(psr, y) THEN Reset(psr, y); Mark(psr, msgIvlType)
		END ;
		IF ~(y IS B.Const) & ~(y IS B.Str) THEN
			x := NewNode2(psr, B.opORD, y, NIL, psr.mod.intType, spos)
		ELSIF y IS B.Const THEN
			x := NewConst(psr, psr.mod.intType, y(B.Const).ival)
		ELSE x := OpOrdChar(psr, y(B.Str))
		END
	ELSIF f.id = B.opCHR THEN y := expression0(psr); CheckInt(psr, y);
		IF ~(y IS B.Const) THEN
			x := NewNode2(psr, B.opCHR, y, NIL, psr.mod.charType, spos)
		ELSE x := OpChr(psr, y(B.Const))
		END
	ELSE x := psr.SystemStdFunc(psr, f)
	END ;
	Check(psr, S.rparen);
	RETURN x
END StdFunc;

PROCEDURE element(psr: Parser): B.Object;
	VAR x, y: B.Object; spos: Sys.Int;
BEGIN
	spos := psr.scn.spos; x := expression0(psr); CheckInt(psr, x);
	IF psr.sym = S.upto THEN spos := psr.scn.spos;
		GetSym(psr); y := expression0(psr); CheckInt(psr, y);
		IF (x IS B.Const) & (y IS B.Const) THEN
			x := OpRangeSet(psr, x(B.Const), y(B.Const))
		ELSE x := NewNode2(psr, S.upto, x, y, psr.mod.setType, spos)
		END ;
	ELSIF x IS B.Const THEN x := OpSingletonSet(psr, x(B.Const))
	ELSE x := NewNode2(psr, B.opBitset, x, NIL, psr.mod.setType, spos)
	END ;
	RETURN x
END element;

PROCEDURE set(psr: Parser): B.Object;
	VAR const: B.Const; x, y: B.Object; node, next: B.Node; spos: Sys.Int;
BEGIN
	const := NewConst(psr, psr.mod.setType, Sys.ZeroInt); GetSym(psr);
	IF psr.sym # S.rbrace THEN y := element(psr);
		IF ~(y IS B.Const) THEN x := y
		ELSE const := OpAdd(psr, S.plus, const, y(B.Const))
		END ;
		WHILE psr.sym = S.comma DO
			spos := psr.scn.spos; GetSym(psr);
			IF psr.sym # S.rbrace THEN y := element(psr);
				IF y IS B.Const THEN
					const := OpAdd(psr, S.plus, const, y(B.Const))
				ELSIF x # NIL THEN
					x := NewNode2(psr, S.plus, x, y, psr.mod.setType, spos)
				ELSE x := y
				END
			ELSE MarkSflous(psr, S.comma)
			END
		END ;
		IF (Sys.CmpInt(const(B.Const).ival, Sys.ZeroInt) # 0) & (x # NIL) THEN
			x := NewNode2(psr, S.plus, x, const, psr.mod.setType, psr.scn.spos)
		END
	END ;
	Check(psr, S.rbrace); IF x = NIL THEN x := const END ;
	RETURN x
END set;

PROCEDURE factor(psr: Parser): B.Object;
	CONST msgNotFunc = 'not function';
	VAR x: B.Object; spos: Sys.Int;
BEGIN
	IF psr.sym = S.int THEN
		x := NewConst(psr, psr.mod.intType, psr.scn.ival); GetSym(psr)
	ELSIF psr.sym = S.real THEN
		x := NewConstR(psr, psr.scn.rval); GetSym(psr)
	ELSIF psr.sym = S.string THEN
		x := NewStr(psr, psr.scn.str, psr.scn.strlen); GetSym(psr)
	ELSIF psr.sym = S.nil THEN
		x := NewConst(psr, psr.mod.nilType, Sys.ZeroInt); GetSym(psr)
	ELSIF psr.sym = S.true THEN
		x := NewConst(psr, psr.mod.boolType, Sys.OneInt); GetSym(psr)
	ELSIF psr.sym = S.false THEN
		x := NewConst(psr, psr.mod.boolType, Sys.ZeroInt); GetSym(psr)
	ELSIF psr.sym = S.lbrace THEN x := set(psr)
	ELSIF psr.sym = S.ident THEN x := designator(psr);
		IF x IS B.SFunc THEN
			IF psr.sym = S.lparen THEN
				IF x.type # NIL THEN x := StdFunc(psr, x(B.SFunc))
				ELSE Reset(psr, x); Mark(psr, msgNotFunc)
				END
			ELSE Reset(psr, x); MarkMissing(psr, S.lparen)
			END
		ELSIF (psr.sym = S.lparen) & (x.type.form = B.tProc) THEN
			IF x.type.base = NIL THEN Mark(psr, msgNotFunc) END ;
			x := Call(psr, x);
			IF x.type = NIL THEN x.type := psr.mod.intType END
		END
	ELSIF psr.sym = S.lparen THEN
		GetSym(psr); x := expression0(psr); Check(psr, S.rparen)
	ELSIF psr.sym = S.not THEN
		spos := psr.scn.spos; GetSym(psr); x := factor(psr); CheckBool(psr, x);
		IF ~(x IS B.Const) THEN
			x := NewNode2(psr, S.not, x, NIL, psr.mod.boolType, spos)
		ELSE x := OpNegateBool(psr, x(B.Const))
		END
	ELSE Reset(psr, x); Mark(psr, 'invalid factor')
	END ;
	RETURN x
END factor;

PROCEDURE term(psr: Parser): B.Object;
	VAR x, y: B.Object; t: B.Type; op: INTEGER; spos: Sys.Int;
BEGIN x := factor(psr);
	WHILE psr.sym = S.times DO spos := psr.scn.spos;
		CheckT(psr, x, B.tTimes); GetSym(psr); y := factor(psr);
		IF CompTypes(psr, x.type, y.type) THEN (*ok*)
		ELSE Reset(psr, x); Reset(psr, y); Mark(psr, msgIvlType)
		END ;
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			IF x.type.form = B.tInt THEN t := psr.mod.intType ELSE t := x.type
			END ;
			x := NewNode2(psr, S.times, x, y, t, spos)
		ELSE x := OpMultiply(psr, x(B.Const), y(B.Const))
		END
	ELSIF psr.sym = S.rdiv DO spos := psr.scn.spos;
		CheckT(psr, x, B.tRdivs); GetSym(psr); y := factor(psr);
		IF CompTypes(psr, x.type, y.type) THEN (*ok*)
		ELSE Reset(psr, x); Reset(psr, y); Mark(psr, msgIvlType)
		END ;
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			x := NewNode2(psr, S.rdiv, x, y, x.type, spos)
		ELSE x := OpRDivide(psr, x(B.Const), y(B.Const))
		END
	ELSIF (psr.sym = S.div) OR (psr.sym = S.mod) DO
		spos := psr.scn.spos; op := psr.sym; CheckInt(psr, x);
		GetSym(psr); y := factor(psr); CheckInt(psr, y);
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			x := NewNode2(psr, op, x, y, psr.mod.intType, spos)
		ELSE x := OpIntDiv(psr, op, x(B.Const), y(B.Const))
		END
	ELSIF psr.sym = S.and DO spos := psr.scn.spos;
		CheckBool(psr, x); GetSym(psr); y := factor(psr); CheckBool(psr, y);
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			x := NewNode2(psr, S.and, x, y, psr.mod.boolType, spos)
		ELSE x := OpAnd(psr, x(B.Const), y(B.Const))
		END
	END ;
	RETURN x
END term;

PROCEDURE SimpleExpression(psr: Parser): B.Object;
	VAR x, y: B.Object; op: INTEGER; spos: Sys.Int; t: B.Type;
BEGIN
	IF psr.sym = S.plus THEN GetSym(psr); x := term(psr)
	ELSIF psr.sym = S.minus THEN
		spos := psr.scn.spos; GetSym(psr); x := term(psr);
		IF x.type.form IN B.tAdds THEN (*ok*)
		ELSE Reset(psr, x); Mark(psr, msgIvlType)
		END ;
		IF ~(x IS B.Const) THEN
			IF x.type.form = B.tInt THEN t := psr.mod.intType
			ELSE t := x.type
			END ;
			x := NewNode2(psr, S.minus, x, NIL, t, spos)
		ELSE x := OpNegate(psr, x(B.Const))
		END
	ELSE x := term(psr)
	END ;
	WHILE (psr.sym = S.plus) OR (psr.sym = S.minus) DO
		spos := psr.scn.spos; op := psr.sym;
		CheckT(psr, x, B.tAdds); GetSym(psr); y := term(psr);
		IF CompTypes(psr, x.type, y.type) THEN (*ok*)
		ELSE Reset(psr, x); Reset(psr, y); Mark(psr, msgIvlType)
		END ;
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			IF x.type.form = B.tInt THEN t := psr.mod.intType ELSE t := x.type
			END ;
			x := NewNode2(psr, op, x, y, t, spos)
		ELSE x := OpAdd(psr, op, x(B.Const), y(B.Const))
		END
	ELSIF psr.sym = S.or DO spos := psr.scn.spos;
		CheckBool(psr, x); GetSym(psr); y := term(psr); CheckBool(psr, y);
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			x := NewNode2(psr, op, x, y, psr.mod.boolType, spos)
		ELSE x := OpOr(psr, x(B.Const), y(B.Const))
		END
	END ;
	RETURN x
END SimpleExpression;

PROCEDURE expression(psr: Parser): B.Object;
	VAR x, y: B.Object; yt: B.Type; op: INTEGER; spos: Sys.Int;
BEGIN x := SimpleExpression(psr);
	IF (psr.sym >= S.eql) & (psr.sym <= S.geq) THEN
		spos := psr.scn.spos; op := psr.sym;
		CheckLeftCmp(psr, x, op); GetSym(psr); y := SimpleExpression(psr);
		IF (x.type = y.type) OR CompTypes2(psr, x.type, y.type) THEN (* ok *)
		ELSE Reset(psr, x); Reset(psr, y); Mark(psr, msgIvlType)
		END ;
		IF ~IsConst(psr, x) OR ~IsConst(psr, y) THEN
			x := NewNode2(psr, op, x, y, psr.mod.boolType, spos)
		ELSE x := OpCompare(psr, op, x, y)
		END
	ELSIF psr.sym = S.in THEN
		spos := psr.scn.spos; CheckInt(psr, x);
		GetSym(psr); y := SimpleExpression(psr); CheckSet(psr, y);
		IF ~(x IS B.Const) OR ~(y IS B.Const) THEN
			x := NewNode2(psr, S.in, x, y, psr.mod.boolType, spos)
		ELSE x := OpIn(psr, x(B.Const), y(B.Const))
		END
	ELSIF psr.sym = S.is THEN spos := psr.scn.spos;
		CheckLeftIs(psr, x); GetSym(psr);
		IF psr.sym = S.ident THEN y := qualident(psr); yt := GetType(psr, y);
		ELSE yt := psr.mod.intType; MarkMissing(psr, S.ident)
		END ;
		IF x.type.form = yt.form THEN
			IF IsExt(psr, yt, x.type) THEN (*ok*)
			ELSE Mark(psr, 'not extension')
			END
		ELSE Mark(psr, msgIvlType)
		END ;
		IF x.type # yt THEN
			x := NewNode2(psr, S.is, x, y, psr.mod.boolType, spos)
		ELSE x := NewConst(psr, psr.mod.boolType, Sys.OneInt)
		END
	END ;
	RETURN x
END expression;

PROCEDURE ConstExpression(psr: Parser): B.Const;
	VAR x: B.Object;
BEGIN x := expression(psr);
	IF IsConst(psr, x) THEN (*ok*)
	ELSE Mark(psr, 'not a const'); Reset(psr, x) END ;
	RETURN x
END ConstExpression;

(* statements *)

PROCEDURE StdProc(psr : Parser; f: B.SProc): B.Node;
	VAR x, y: B.Object; node: B.Node;
BEGIN
	IF (f.id >= B.opINC) & (f.id <= B.opEndOfStdProc) THEN
		IF (f.id = B.opINC) OR (f.id = B.opDEC) THEN
			x := designator(psr); CheckInt(psr, x); CheckVar(psr, x, FALSE);
			IF psr.sym = S.comma THEN GetSym(psr);
				y := expression(psr); CheckInt(psr, y);
				node := NewNode(psr, f.id, x, y)
			ELSE node := NewNode(psr, f.id, x, NIL)
			END
		ELSIF (f.id = B.opINCL) OR (f.id = B.opEXCL) THEN
			x := designator(psr); CheckSet(psr, x); CheckVar(psr, x, FALSE);
			Check(psr, S.comma); y := expression(psr); CheckInt(psr, y);
			node := NewNode(psr, f.id, x, y)
		ELSIF f.id = B.opNEW THEN
			x := designator(psr); CheckT(psr, x, {B.tPtr});
			CheckVar(psr, x, FALSE);
			node := NewNode(psr, B.opNEW, x, NIL)
		ELSIF f.id = B.opASSERT THEN
			x := expression(psr); CheckBool(psr, x);
			node := NewNode(psr, B.opASSERT, x, NIL)
		ELSIF f.id = B.opPACK THEN
			x := designator(psr); CheckReal(psr, x);
			CheckVar(psr, x, FALSE); Check(psr, S.comma);
			y := expression(psr); CheckInt(psr, y);
			node := NewNode(psr, B.opPACK, x, y)
		ELSIF f.id = B.opUNPK THEN
			x := designator(psr); CheckReal(psr, x);
			CheckVar(psr, x, FALSE); Check(psr, S.comma);
			y := designator(psr); CheckInt(psr, y); CheckVar(psr, y, FALSE);
			node := NewNode(psr, B.opUNPK, x, y)
		ELSE ASSERT(FALSE)
		END
	ELSE node := psr.SystemStdProc(psr, f)
	END ;
	RETURN node
END StdProc;

PROCEDURE If(psr: Parser; lev: INTEGER): B.Node;
	VAR if, then: B.Node; x: B.Object;
BEGIN
	GetSym(psr); x := expression(psr); CheckBool(psr, x); Check(psr, S.then);
	then := NewNode(psr, S.then, StatementSequence0(psr), NIL);
	if := NewNode(psr, S.if, x, then);
	IF psr.sym = S.elsif THEN then.right := If(psr, lev+1)
	ELSIF psr.sym = S.else THEN
		GetSym(psr); then.right := StatementSequence0(psr)
	END ;
	IF lev = 0 THEN Check(psr, S.end) END ;
	RETURN if
END If;

PROCEDURE While(psr: Parser; lev: INTEGER): B.Node;
	VAR while, do: B.Node; x: B.Object;
BEGIN
	GetSym(psr); x := expression(psr); CheckBool(psr, x); Check(psr, S.do);
	do := NewNode(psr, S.do, StatementSequence0(psr), NIL);
	while := NewNode(psr, S.while, x, do);
	IF psr.sym = S.elsif THEN do.right := While(psr, lev+1) END ;
	IF lev = 0 THEN Check(psr, S.end) END ;
	RETURN while
END While;

PROCEDURE For(psr: Parser): B.Node;
	VAR for, control, becomes, to, by: B.Node; x: B.Object;

	PROCEDURE ident(psr: Parser): B.Object;
		VAR ident: B.Ident; x: B.Object;
	BEGIN ident := FindIdent(psr);
		IF ident # NIL THEN x := ident.obj;
			IF x # NIL THEN
				IF x IS B.Var THEN CheckLev(psr, x(B.Var)) END
			ELSE Reset(psr, x); Mark(psr, 'undefined identifier')
			END
		ELSE Reset(psr, x); Mark(psr, 'identifier not found')
		END ;
		GetSym(psr);
		RETURN x
	END ident;

BEGIN
	for := NewNode(psr, S.for, NIL, NIL); GetSym(psr);
	IF psr.sym = S.ident THEN x := ident(psr)
	ELSE MarkMissing(psr, S.ident)
	END ;
	IF x # NIL THEN CheckInt2(psr, x); CheckVar(psr, x, FALSE) END ;
	Check(psr, S.becomes); becomes := NewNode(psr, S.becomes, NIL, NIL);
	x := expression(psr); CheckInt2(psr, x); becomes.left := x;
	Check(psr, S.to); to := NewNode(psr, S.to, NIL, NIL);
	x := expression(psr); CheckInt2(psr, x); to.left := x;
	IF psr.sym = S.by THEN
		by := NewNode(psr, S.by, NIL, NIL); GetSym(psr);
		x := ConstExpression(psr); CheckInt(psr, x);
		by.left := x; to.right := by
	END ;
	for.left := control; control.right := becomes; becomes.right := to;
	Check(psr, S.do); for.right := StatementSequence0(psr); Check(psr, S.end);
	RETURN for
END For;

PROCEDURE Case(psr: Parser): B.Node;
	VAR x, y: B.Object; case: B.Node;
		xform: INTEGER; isTypeCase: BOOLEAN;
		
	PROCEDURE TypeCase(psr: Parser; x: B.Object): B.Node;
		VAR bar, colon, is: B.Node; y: B.Object; org, yt: B.Type;
	BEGIN
		IF psr.sym = S.ident THEN
			bar := NewNode(psr, S.bar, NIL, NIL); org := x.type;
			y := qualident(psr); yt := GetType(psr, y);
			IF org.form = yt.form THEN
				IF IsExt(psr, yt, org) THEN (*ok*)
				ELSE Mark(psr, 'not extension'); yt := org
				END
			ELSE Mark(psr, msgIvlType); yt := org
			END ;
			is := NewNode2(psr, S.is, x, y, psr.mod.boolType, psr.scn.spos);
			
			IF yt # org THEN x.type := yt END ;
			colon := NewNode(psr, S.colon, is, NIL); Check(psr, S.colon);
			colon.right := StatementSequence0(psr);
			x.type := org; bar.left := colon;
			IF psr.sym = S.bar THEN
				GetSym(psr); bar.right := TypeCase(psr, x)
			END
		ELSIF psr.sym = S.bar THEN GetSym(psr); bar := TypeCase(psr, x)
		END ;
		RETURN bar
	END TypeCase;
	
	PROCEDURE label(psr: Parser; x: B.Object; VAR y: B.Object);
		CONST errMsg = 'invalid value';
		VAR xform: INTEGER;
	BEGIN xform := x.type.form;
		IF psr.sym = S.int THEN y := factor(psr);
			IF xform = B.tInt THEN (*ok*) ELSE Mark(psr, errMsg) END
		ELSIF psr.sym = S.string THEN y := factor(psr);
			IF IsCharStr(psr, y) & (xform = B.tChar) THEN (*ok*)
			ELSE Mark(psr, errMsg)
			END
		ELSIF psr.sym = S.ident THEN y := qualident(psr);
			IF xform = B.tInt THEN CheckInt(psr, y)
			ELSIF (xform = B.tChar) & IsCharStr(psr, y) THEN (*ok*)
			ELSE Mark(psr, errMsg)
			END
		ELSE Mark(psr, 'need integer or char value')
		END
	END label;
	
	PROCEDURE LabelRange(psr: Parser; x: B.Object): B.Object;
		VAR y, z, cond: B.Object; spos: Sys.Int;
	BEGIN label(psr, x, y); spos := psr.scn.spos;
		IF psr.sym # S.upto THEN
			cond := NewNode2(psr, S.eql, x, y, psr.mod.boolType, spos)
		ELSE
			y := NewNode2(psr, S.geq, x, y, psr.mod.boolType, spos);
			GetSym(psr); label(psr, x, z);
			z := NewNode2(psr, S.leq, x, z, psr.mod.boolType, psr.scn.spos);
			cond := NewNode2(psr, S.and, y, z, psr.mod.boolType, spos)
		END ;
		RETURN cond
	END LabelRange;
	
	PROCEDURE NumericCase(psr: Parser; x: B.Object): B.Node;
		VAR bar, colon: B.Node; cond, y: B.Object; spos: Sys.Int;
	BEGIN
		IF (psr.sym = S.int) OR (psr.sym = S.string)
		OR (psr.sym = S.ident) THEN
			bar := NewNode(psr, S.bar, NIL, NIL); cond := LabelRange(psr, x);
			WHILE psr.sym = S.comma DO
				spos := psr.scn.spos; GetSym(psr); y := LabelRange(psr, x);
				cond := NewNode2(psr, S.or, cond, y, psr.mod.boolType, spos)
			END ;
			colon := NewNode(psr, S.colon, cond, NIL); Check(psr, S.colon);
			colon.right := StatementSequence0(psr); bar.left := colon;
			IF psr.sym = S.bar THEN
				GetSym(psr); bar.right := NumericCase(psr, x)
			END
		ELSIF psr.sym = S.bar THEN GetSym(psr); bar := NumericCase(psr, x)
		END
		RETURN bar
	END NumericCase;
		
BEGIN (* Case *)
	case := NewNode(psr, S.case, NIL, NIL); GetSym(psr);
	x := expression(psr); xform := x.type.form; isTypeCase := FALSE;
	IF (xform IN {B.tInt, B.tChar}) OR IsCharStr(psr, x) THEN (*ok*)
	ELSIF TypeTestable(psr, x) THEN isTypeCase := TRUE
	ELSE Reset(psr, x); Mark(psr, 'invalid case expression')
	END ;
	Check(psr, S.of); case.left := x;
	IF isTypeCase THEN case.right := TypeCase(psr, x)
	ELSE case.right := NumericCase(psr, y)
	END ;
	Check(psr, S.end);
	RETURN case
END Case;

PROCEDURE StatementSequence(psr: Parser): B.Node;
	VAR seq, st, prev, becomes, repeat: B.Node; x, y: B.Object;
BEGIN
	seq := NewNode(psr, S.semicolon, NIL, NIL); st := seq;
	REPEAT (*sync*)
		IF (psr.sym = S.ident) OR (psr.sym >= S.semicolon)
		OR (psr.sym >= S.if) & (psr.sym <= S.for) THEN (*valid*)
		ELSE
			Mark(psr, 'statement?');
			REPEAT GetSym(psr)
			UNTIL (psr.sym = S.ident) OR (psr.sym >= S.semicolon)
		END ;
		
		IF psr.sym = S.ident THEN x := designator(psr);
			IF psr.sym = S.becomes THEN
				becomes := NewNode(psr, S.becomes, NIL, NIL);
				CheckVar(psr, x, FALSE); becomes.left := x; GetSym(psr);
				y := expression(psr); CheckAssignment(psr, x.type, y);
				becomes.right := y; st.left := becomes
			ELSIF psr.sym = S.eql THEN
				Mark(psr, 'should be :='); GetSym(psr); y := expression(psr)
			ELSIF (x.type # NIL) & (x.type.form = B.tProc) THEN
				IF x.type.base = NIL THEN (*ok*)
				ELSE Mark(psr, 'not proper procedure')
				END ;
				st.left := Call(psr, x)
			ELSIF x IS B.SProc THEN st.left := StdProc(psr, x(B.SProc))
			ELSE Mark(psr, 'invalid statement')
			END
		ELSIF psr.sym = S.if THEN st.left := If(psr, 0)
		ELSIF psr.sym = S.while THEN st.left := While(psr, 0)
		ELSIF psr.sym = S.repeat THEN GetSym(psr);
			repeat := NewNode(psr, S.repeat, StatementSequence0(psr), NIL);
			Check(psr, S.until); x := expression(psr); CheckBool(psr, x);
			repeat.right := x; st.left := repeat
		ELSIF psr.sym = S.for THEN st.left := For(psr)
		ELSIF psr.sym = S.case THEN st.left := Case(psr)
		END ;
		IF psr.sym <= S.semicolon THEN Check(psr, S.semicolon); prev := st END
	UNTIL psr.sym > S.semicolon;
	RETURN seq
END StatementSequence;

(* declarations *)

PROCEDURE CheckExport(psr: Parser; VAR exp: BOOLEAN);
BEGIN
	IF psr.sym = S.times THEN GetSym(psr);
		IF psr.mod.curLev = 0 THEN exp := TRUE
		ELSE Mark(psr, 'not exportable')
		END
	END
END CheckExport;

PROCEDURE OpenScope(psr: Parser);
	VAR scp: B.Scope;
BEGIN NEW(scp); scp.dsc := psr.mod.topScope; psr.mod.topScope := scp
END OpenScope;

PROCEDURE CloseScope(psr: Parser);
BEGIN psr.mod.topScope := psr.mod.topScope.dsc
END CloseScope;

PROCEDURE IncLev(psr: Parser; x: INTEGER);
BEGIN INC(psr.mod.curLev, x)
END IncLev;

PROCEDURE NewIdent(psr: Parser; name: S.Ident): B.Ident;
	VAR id, prev, x: B.Ident;
BEGIN
	NEW(id); id.export := FALSE; id.used := FALSE;
	id.name := name; id.spos := psr.scn.spos;
	x := psr.mod.topScope.first;
	WHILE x # NIL DO
		IF x # NIL THEN Mark(psr, 'duplicated ident') END ;
		prev := x; x := x.next
	END ;
	IF prev # NIL THEN prev.next := id ELSE psr.mod.topScope.first := id END ;
	RETURN id
END NewIdent;

PROCEDURE IdentList(psr: Parser): B.Ident;
	VAR fst, x: B.Ident;
BEGIN fst := NewIdent(psr, psr.scn.id);
	GetSym(psr); CheckExport(psr, fst.export);
	WHILE psr.sym = S.comma DO
		IF psr.sym = S.ident THEN
			x := NewIdent(psr, psr.scn.id);
			GetSym(psr); CheckExport(psr, x.export)
		ELSE MarkSflous(psr, S.comma)
		END
	END ;
	RETURN fst
END IdentList;

PROCEDURE ParseFormalArrayFlags(psr: Parser; tp: B.Type);
BEGIN ASSERT(FALSE)
END ParseFormalArrayFlags;

PROCEDURE FormalType(psr: Parser): B.Type;
	VAR x: B.Object; tp: B.Type;
BEGIN tp := psr.mod.intType;
	IF psr.sym = S.ident THEN x := qualident(psr); tp := GetType(psr, x)
	ELSIF psr.sym = S.array THEN
		tp := NewType(psr, B.tArray); GetSym(psr);
		ParseFormalArrayFlags(psr, tp); Check(psr, S.of);
		IF psr.sym = S.array THEN
			Mark(psr, 'multidim open array is not supported')
		END ;
		tp.base := FormalType(psr)
	END ;
	RETURN tp
END FormalType;

PROCEDURE FPSection(psr: Parser; proc: B.Type);
	VAR first, id: B.Ident; x: B.Par;
		tp: B.Type; varpar: BOOLEAN;
BEGIN
	IF psr.sym # S.var THEN varpar := FALSE
	ELSE varpar := TRUE; GetSym(psr)
	END ;
	IF psr.sym = S.ident THEN
		first := NewIdent(psr, psr.scn.id); GetSym(psr);
		WHILE psr.sym = S.comma DO GetSym(psr);
			IF psr.sym = S.ident THEN
				id := NewIdent(psr, psr.scn.id); GetSym(psr);
				IF first = NIL THEN first := id END
			ELSE MarkSflous(psr, S.comma)
			END
		END
	ELSE Mark(psr, 'no params?')
	END ;
	Check(psr, S.colon); tp := FormalType(psr); id := first;
	WHILE id # NIL DO
		x := NewPar(psr, proc, tp, varpar); INC(proc.nfpars);
		x.ronly := ~varpar & (tp.form IN B.tStructs);
		x.ident := id; id.obj := x; id := id.next
	END
END FPSection;

PROCEDURE FormalParameters(psr: Parser; proc: B.Type);
	VAR tp: B.Type; x: B.Object;
BEGIN GetSym(psr);
	IF (psr.sym = S.ident) OR (psr.sym = S.var) THEN
		OpenScope(psr); FPSection(psr, proc);
		WHILE psr.sym = S.semicolon DO GetSym(psr);
			IF (psr.sym = S.ident) OR (psr.sym = S.var) THEN
				FPSection(psr, proc)
			ELSE Mark(psr, 'param section?')
			END
		END ;
		proc.fields := psr.mod.topScope.first; CloseScope(psr)
	END ;
	Check(psr, S.rparen);
	IF psr.sym = S.colon THEN GetSym(psr);
		IF psr.sym = S.ident THEN
			x := qualident(psr); tp := GetType(psr, x);
			IF ~(tp.form IN B.tStructs) THEN proc.base := tp
			ELSE Mark(psr, msgIvlType); proc.base := psr.mod.intType
			END
		ELSE MarkMissing(psr, S.ident)
		END
	END
END FormalParameters;

PROCEDURE ParsePointerFlags(psr: Parser; ptr: B.Type);
BEGIN ASSERT(FALSE)
END ParsePointerFlags;

PROCEDURE PointerType(psr: Parser; defobj: B.Object): B.Type;
	VAR ptr, bt: B.Type; name: S.Ident; x: B.Ident;
BEGIN
	ptr := NewType(psr, B.tPtr); GetSym(psr);
	ParsePointerFlags(psr, ptr); Check(psr, S.to);
	IF defobj # NIL THEN defobj.type := ptr END ;
	IF psr.sym = S.ident THEN name := psr.scn.id; x := qualident0(psr);
		IF x # NIL THEN
			IF x.obj # NIL THEN bt := GetType(psr, x.obj);
				IF bt.form = B.tRec THEN ptr.base := bt
				ELSE Mark(psr, 'not a record type')
				END
			ELSIF x = psr.externalIdentNotFound THEN
				Mark(psr, 'cannot find this identifier')
			ELSE Mark(psr, 'identifier is not defined yet')
			END
		ELSE (* undefined base type *) AddUndef(psr, ptr, name)
		END
	ELSIF psr.sym = S.record THEN ptr.base := type0(psr)
	ELSE Mark(psr, 'base type?')
	END ;
	RETURN ptr
END PointerType;

PROCEDURE FieldList(psr: Parser; rec: B.Type);
	VAR first, field: B.Ident; ft: B.Type;
BEGIN first := NewIdent(psr, psr.scn.id);
	GetSym(psr); CheckExport(psr, first.export);
	WHILE psr.sym = S.comma DO GetSym(psr);
		IF psr.sym = S.ident THEN
			field := NewIdent(psr, psr.scn.id);
			GetSym(psr); CheckExport(psr, field.export);
			IF first = NIL THEN first := field END
		ELSIF psr.sym < S.ident THEN MarkMissing(psr, S.ident)
		ELSE MarkSflous(psr, S.comma)
		END
	END;
	Check(psr, S.colon); ft := type0(psr); field := first;
	WHILE field # NIL DO
		field.obj := NewField(psr, rec, ft); field := field.next
	END
END FieldList;

PROCEDURE BaseType(psr: Parser): B.Type;
	VAR tp, rec: B.Type; x: B.Object;
BEGIN
	IF psr.sym = S.ident THEN
		x := qualident(psr); tp := GetType(psr, x);
		IF tp.form = B.tRec THEN (*ok*)
		ELSIF tp.form = B.tPtr THEN tp := tp.base;
			IF tp # NIL THEN (*ok*)
			ELSE Mark(psr, 'record base type is undefined')
			END
		ELSE tp := NIL; Mark(psr, 'not a record type')
		END
	ELSE MarkMissing(psr, S.ident)
	END ;
	RETURN tp
END BaseType;

PROCEDURE ArrayType(psr: Parser): B.Type;
	VAR tp, bt: B.Type; len: Sys.Int;
	
	PROCEDURE length(psr: Parser; VAR len: Sys.Int);
		VAR x: B.Object;
	BEGIN x := ConstExpression(psr);
		IF x.type.form = B.tInt THEN len := x(B.Const).ival
		ELSE Mark(psr, 'array length must be int'); len := Sys.ZeroInt
		END ;
		IF Sys.SignInt(len) THEN Mark(psr, 'invalid array length') END
	END length;
	
BEGIN (* ArrayType *)
	length(psr, len);
	IF psr.sym = S.comma THEN GetSym(psr);
		WHILE psr.sym = S.comma DO MarkSflous(psr, S.comma); GetSym(psr) END ;
		bt := ArrayType(psr)	
	ELSE Check(psr, S.of); bt := type0(psr)
	END ;
	tp := NewType(psr, B.tArray); tp.len := len; tp.base := bt;
	RETURN tp
END ArrayType;

PROCEDURE ParseRecordFlags(psr: Parser; ptr: B.Type);
BEGIN ASSERT(FALSE)
END ParseRecordFlags;

PROCEDURE type(psr: Parser): B.Type;
	VAR tp, ft, bt: B.Type; flds: B.Ident; x: B.Object;
BEGIN
	IF psr.sym = S.ident THEN
		x := qualident(psr); tp := GetType(psr, x); GetSym(psr)
	ELSIF psr.sym = S.array THEN
		GetSym(psr); tp := ArrayType(psr)
	ELSIF psr.sym = S.record THEN
		GetSym(psr); tp := NewType(psr, B.tRec); ParseRecordFlags(psr, tp);
		IF psr.sym = S.lparen THEN
			GetSym(psr); bt := BaseType(psr);
			IF bt # NIL THEN tp.base := bt; tp.lev := bt.lev+1 END ;
			Check(psr, S.rparen)
		END ;
		OpenScope(psr);
		IF psr.sym = S.ident THEN FieldList(psr, tp);
			WHILE psr.sym = S.semicolon DO GetSym(psr);
				IF psr.sym = S.ident THEN FieldList(psr, tp);
				ELSE MarkSflous(psr, S.semicolon)
				END
			END
		END ;
		tp.fields := psr.mod.topScope.first;
		CloseScope(psr); Check(psr, S.end)
	ELSIF psr.sym = S.pointer THEN tp := PointerType(psr, NIL)
	ELSIF psr.sym = S.procedure THEN
		GetSym(psr); tp := NewType(psr, B.tProc);
		IF psr.sym = S.lparen THEN FormalParameters(psr, tp) END
	END ;
	RETURN tp
END type;

PROCEDURE CloneParameters(psr: Parser; VAR proc: B.Proc);
	VAR p, id: B.Ident; x: B.Par;
BEGIN p := proc.type.fields;
	WHILE p # NIL DO
		id := NewIdent(psr, p.name);
		NEW(x); id.obj := x; x^ := p.obj(B.Par)^;
		id.spos := p.spos; id.obj := x; INC(x.lev);
		x.ident := id; p := p.next
	END
END CloneParameters;

PROCEDURE DeclarationSequence(psr: Parser; owner: B.Object);
	VAR id: B.Ident; x: B.Object; tp, rtype: B.Type;
BEGIN
	IF psr.sym = S.const THEN GetSym(psr);
		WHILE psr.sym = S.ident DO
			id := NewIdent(psr, psr.scn.id);
			GetSym(psr); CheckExport(psr, id.export);
			Check(psr, S.eql); x := ConstExpression(psr); id.obj := x;
			IF x.ident = NIL THEN x.ident := id END ;
			Check(psr, S.semicolon)
		END
	END ;
	IF psr.sym = S.type THEN GetSym(psr);
		WHILE psr.sym = S.ident DO
			id := NewIdent(psr, psr.scn.id); GetSym(psr);
			CheckExport(psr, id.export); Check(psr, S.eql);
			IF psr.sym # S.pointer THEN
				tp := type(psr); x := NewTypeObj(psr, tp);
				id.obj := x; x.ident := id;
				IF tp.form = B.tRec THEN FixUndef(psr, tp, id.name) END
			ELSE
				x := NewTypeObj(psr, psr.mod.intType);
				id.obj := x; x.ident := id; tp := PointerType(psr, x);
				IF tp.obj = NIL THEN tp.obj := x END
			END ;
			Check(psr, S.semicolon)
		END ;
		CheckUndef(psr)
	END ;
	IF psr.sym = S.var THEN GetSym(psr);
		WHILE psr.sym = S.ident DO
			id := IdentList(psr); Check(psr, S.colon); tp := type(psr);
			WHILE id # NIL DO
				x := NewVar(psr, tp);
				id.obj := x; x.ident := id; id := id.next
			END ;
			Check(psr, S.semicolon)
		END
	END ;
	WHILE psr.sym = S.procedure DO GetSym(psr);
		IF psr.sym # S.ident THEN id := NIL; Mark(psr, 'proc name?')
		ELSE id := NewIdent(psr, psr.scn.id);
			GetSym(psr); CheckExport(psr, id.export)
		END ;
		x := NewProc(psr); tp := NewType(psr, B.tProc); x.type := tp;
		IF id # NIL THEN id.obj := x; x.ident := id END ;
		
		IF psr.sym = S.lparen THEN FormalParameters(psr, tp) END ;
		Check(psr, S.semicolon);
		OpenScope(psr); IncLev(psr, 1); CloneParameters(psr, x(B.Proc));

		DeclarationSequence(psr, x); x(B.Proc).decl := psr.mod.topScope.first;
		
		IF psr.sym = S.begin THEN
			GetSym(psr); x(B.Proc).statseq := StatementSequence(psr)
		END ;
		
		rtype := tp.base;
		IF psr.sym = S.return THEN
			IF rtype # NIL THEN (*ok*)
			ELSE rtype := psr.mod.intType; Mark(psr, 'not function proc')
			END ;
			GetSym(psr); x(B.Proc).return := expression(psr);
			CheckAssignment(psr, rtype, x(B.Proc).return)
		ELSIF rtype # NIL THEN MarkMissing(psr, S.return)
		END ;
		
		CloseScope(psr); IncLev(psr, -1); Check(psr, S.end);
		IF psr.sym = S.ident THEN
			IF (id # NIL) & (id.name # psr.scn.id) THEN
				Mark(psr, 'wrong procedure identifier')
			END ;
			GetSym(psr)
		ELSIF id # NIL THEN MarkMissing(psr, S.ident)
		END ;
		Check(psr, S.semicolon)
	END
END DeclarationSequence;

PROCEDURE ModuleId(psr: Parser; VAR modid: B.ModuleId);
BEGIN
	modid.context := 0X; modid.name := psr.scn.id; GetSym(psr);
	IF psr.sym = S.period (* my previous syntax *) THEN GetSym(psr);
		IF psr.sym = S.ident THEN
			modid.context := modid.name; modid.name := psr.scn.id
		ELSE MarkMissing(psr, S.ident)
		END
	ELSIF psr.sym = S.in (* A2 Oberon syntax *) THEN GetSym(psr);
		IF psr.sym = S.ident THEN modid.context := psr.scn.id
		ELSE MarkMissing(psr, S.ident)
		END
	END
END ModuleId;

PROCEDURE import(psr: Parser);
	VAR ident: B.Ident; id: B.ModuleId; name: S.Ident;
BEGIN
	name := psr.scn.id; ident := NewIdent(psr, name); GetSym(psr);
	IF psr.sym = S.becomes THEN GetSym(psr);
		IF psr.sym = S.ident THEN ModuleId(psr, id)
		ELSE MarkMissing(psr, S.ident)
		END
	END;
	IF ~psr.scn.hasError THEN
		(*
		IF name = 'SYSTEM' THEN B.NewSystemModule(ident)
		ELSIF name # 0X THEN B.NewModule(ident, name)
		ELSE B.NewModule0(ident, id)
		END
		*)
	END
END import;

PROCEDURE ImportList(psr: Parser);
BEGIN GetSym(psr);
	IF psr.sym = S.ident THEN import(psr) ELSE MarkMissing(psr, S.ident) END ;
	WHILE psr.sym = S.comma DO GetSym(psr);
		IF psr.sym = S.ident THEN import(psr)
		ELSE MarkMissing(psr, S.ident)
		END
	END ;
	Check(psr, S.semicolon)
END ImportList;

PROCEDURE Module*(psr: Parser);
	VAR mod: B.Module;
BEGIN
	(* mod := B.Init(); *) psr.mod := mod; GetSym(psr);
	IF psr.sym = S.ident THEN ModuleId(psr, mod.id)
	ELSE MarkMissing(psr, S.ident)
	END ;
	IF ~psr.scn.hasError THEN
		Check(psr, S.semicolon);
		IF psr.sym = S.import THEN ImportList(psr) END
	END ;
	IF ~psr.scn.hasError THEN
		DeclarationSequence(psr, NIL);
		IF psr.sym = S.begin THEN
			GetSym(psr); psr.mod.init := StatementSequence(psr)
		END ;
		Check(psr, S.end);
		IF psr.sym = S.ident THEN
			IF psr.scn.id # mod.id.name THEN Mark(psr, 'wrong module name')
			END ;
			GetSym(psr)
		ELSE MarkMissing(psr, S.ident)
		END ;
		Check(psr, S.period)
	END
END Module;

BEGIN
	type0 := type; expression0 := expression;
	StatementSequence0 := StatementSequence
END Psr.