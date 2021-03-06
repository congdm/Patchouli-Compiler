MODULE Psr;

IMPORT
	S := Scn, B := Base;

CONST
	msgIvlType = 'invalid type';

TYPE
	UndefPtrList = POINTER TO RECORD
		name: S.Ident; tp: B.Type; next: UndefPtrList
	END ;
	
	TypeCastList = POINTER TO RECORD
		node: B.Node; type: B.Type; next: TypeCastList
	END ;
	
	IArch* = RECORD
		(* const creation *)
		Const*: PROCEDURE(type: B.Type): B.Object;
		ZeroIntConst*: PROCEDURE(): B.Object;
		Str*: PROCEDURE(str: S.Str; slen: INTEGER): B.Object;
		NilConst*: PROCEDURE(): B.Object;
		BoolConst*: PROCEDURE(v: BOOLEAN): B.Object;
		CharConst*: PROCEDURE(ch: CHAR): B.Object;
		
		(* object creation *)
		Par*: PROCEDURE(pt, ft: B.Type; varpar: BOOLEAN): B.Object;
		Var*: PROCEDURE(t: B.Type; owner: B.Proc): B.Object;
		Proc*: PROCEDURE(): B.Object;
		ClonePar*: PROCEDURE(org: B.Object): B.Object;
		Node*: PROCEDURE(op: INTEGER): B.Object;
		
		(* type creation *)
		ArrayType*: PROCEDURE(len: B.Object; bt: B.Type): B.Type;
		RecordType*: PROCEDURE(): B.Type;
		ParseRecordFlags*: PROCEDURE(t: B.Type; VAR sym: INTEGER);
		ExtendRecordType*: PROCEDURE(t: B.Type);
		NewRecordFields*: PROCEDURE(t: B.Type; fst: B.Ident; ft: B.Type);
		AllocRecordDesc*: PROCEDURE(t: B.Type);
		PointerType*: PROCEDURE(): B.Type;
		ParsePointerFlags*: PROCEDURE(t: B.Type; VAR sym: INTEGER);
		SetPointerBaseType*: PROCEDURE(t, bt: B.Type);
		ProcType*: PROCEDURE(): B.Type;
		FormalArrayType*: PROCEDURE(): B.Type;
		ParseFormalArrayFlags*: PROCEDURE(t: B.Type; VAR sym: INTEGER);
		
		(* set/get properties *)
		SetBaseType*: PROCEDURE(t, bt: B.Type);
		GetBaseType*: PROCEDURE(t: B.Type): B.Type;
	END ;

VAR
	mod: B.Module; sym: INTEGER;
	undefList: UndefPtrList;
	externalIdentNotFound: B.Ident;
	typeCastStack: TypeCastList;
	
	type0: PROCEDURE(): B.Type;
	expression0: PROCEDURE(): B.Object;
	StatementSequence0: PROCEDURE(): B.Node;	
	arch: IArch;

(* -------------------------------------------------------------------------- *)

PROCEDURE InitObject*(x: B.Object);
BEGIN
	x.isType := FALSE;
	x.isConst := FALSE;
	x.isStr := FALSE;
	x.isVar := FALSE;
	x.isParam := FALSE;
	x.isProc := FALSE;
	x.isField := FALSE;
	x.isSProc := FALSE;
	
	x.isCharStr := FALSE;
	x.isExtSProc := FALSE;
	
	x.ronly := FALSE;
	x.varpar := FALSE;
	x.lev := mod.curLev;
	x.spos := S.symPos
END InitObject;

PROCEDURE InitType*(x: B.Type; form: INTEGER);
BEGIN
	x.predef := FALSE;
	x.isOpenArray := FALSE;
	x.form := form;
	x.lev := 0;
	x.nfpar := 0
END InitType;

(* -------------------------------------------------------------------------- *)
(* Error checking procedures *)

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

PROCEDURE IsStrType(t: B.Type): BOOLEAN;
	RETURN (t = B.strType) OR (t.form = B.tArray) & (t.base.form = B.tChar)
END IsStrType;

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
	OR IsStrType(t1) & IsStrType(t2)
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

PROCEDURE CheckStrLen(xt: B.Type; y: B.Object);
BEGIN
	IF (xt.len >= 0) & (y IS B.Str) & (y(B.Str).len > xt.len) THEN
		S.Mark('string longer than dest')
	END
END CheckStrLen;

PROCEDURE CheckPar(fpar: B.Par; x: B.Object);
	CONST msgIvlParType = 'invalid par type';
	VAR xt, ftype: B.Type; xform, fform: INTEGER;
BEGIN xt := x.type; ftype := fpar.type;
	IF IsOpenArray(ftype) THEN CheckVar(x, fpar.ronly);
		IF CompArray(ftype, xt) OR IsStrType(xt) & IsStrType(ftype) THEN (*ok*)
		ELSIF B.mod.system & (ftype.base = B.byteType) THEN (*ok*)
		ELSE S.Mark(msgIvlParType)
		END
	ELSIF ~fpar.varpar THEN
		IF CompTypes(ftype, xt) THEN
			IF IsStrType(ftype) THEN CheckStrLen(ftype, x) END
		ELSIF (ftype = B.charType) & IsCharStr(x) THEN (*valid*)
		ELSE S.Mark(msgIvlParType)
		END
	ELSIF fpar.varpar THEN
		CheckVar(x, fpar.ronly); xform := xt.form; fform := ftype.form;
		IF (xt = ftype) OR CompArray(xt, ftype) & (ftype.len = xt.len)
		OR (fform = B.tRec) & (xform = B.tRec) & IsExt0(xt, ftype)
		THEN (*valid*) ELSE S.Mark(msgIvlParType)
		END
	END
END CheckPar;

PROCEDURE CheckLeft(x: B.Object; op: INTEGER);
BEGIN
	IF (op >= S.eql) & (op <= S.geq) THEN
		IF (x.type.form IN B.tCmps) OR IsStrType(x.type)
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

PROCEDURE CheckType(x: B.Object);
BEGIN
	IF x.isType THEN (*ok*) ELSE S.Mark('not a type'); x := B.intType END
END CheckType;

PROCEDURE CheckLev(x: B.Object);
BEGIN
	IF x.lev <= 0 THEN (*ok*)
	ELSIF x.lev = mod.curLev THEN (*ok*)
	ELSE S.Mark('access to non-local & non-global vars not allowed')
	END
END CheckLev;

PROCEDURE CheckAssignment(xtype: B.Type; y: B.Node);
BEGIN
	IF xtype = y.type THEN (*ok*)
	ELSIF CompTypes(xtype, y.type) THEN
		IF IsStrType(xtype) THEN arch.CheckStrLen(xtype, y.type) END
	ELSIF CompArray(xtype, y.type) & y.type.isOpenArray THEN (*ok*)
	ELSE arch.CheckAssignment(xtype, y)
	END ;
END CheckAssignment;

(* -------------------------------------------------------------------------- *)
(* undef pointers list *)

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
	IF undef # NIL THEN
		arch.SetPointerBaseType(undef.tp, rec);
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

(* new parse tree node *)

PROCEDURE NewNode*(VAR x: B.Node; op: INTEGER);
BEGIN
	NEW(x); x.op := op; x.ronly := FALSE; x.spos := S.symPos
END NewNode;

PROCEDURE Node*(op: INTEGER): B.Node;
	VAR x: B.Node;
BEGIN
	NEW(x); x.op := op; x.ronly := FALSE; x.spos := S.symPos
	RETURN x
END Node;

PROCEDURE NewNode2*(VAR x: B.Node; op: INTEGER; t: B.Type)
BEGIN
	NEW(x); x.op := op; x.ronly := FALSE; x.spos := S.symPos;
	x.type := t
END NewNode2;

PROCEDURE NewNode0(op: INTEGER; x, y: B.Object): B.Node;
	VAR z: B.Node;
BEGIN
	NEW(z); z.op := op; z.ronly := FALSE;
	z.left := x; z.right := y; z.spos := S.symPos;
	RETURN z
END NewNode0;

PROCEDURE NewNode(op: INTEGER; x, y: B.Object; t: B.Type; p: INTEGER): B.Node;
	VAR z: B.Node;
BEGIN
	z := NewNode0(op, x, y); z.spos := p; z.type := t;
	RETURN z
END NewNode;

PROCEDURE NewNode2(op: INTEGER; x, y: B.Object; p: INTEGER): B.Node;
	VAR z: B.Node;
BEGIN
	z := NewNode0(op, x, y); z.spos := p;
	RETURN z
END NewNode2;

(* qualident & designator *)

PROCEDURE FindIdent(): B.Ident;
	VAR x: B.Ident; scope: B.Scope;
BEGIN scope := B.topScope;
	WHILE (scope # NIL) & (x = NIL) DO x := scope.first; 
		WHILE (x # NIL) & (x.name # S.id) DO x := x.next END ;
		IF x # NIL THEN (*found*) INC(x.nUsed)
		ELSE scope := scope.dsc
		END
	END ;
	RETURN x
END FindIdent;

PROCEDURE qualident0(): B.Ident;
	VAR x: B.Ident; id: S.Ident; mod: B.Module;
BEGIN
	x := FindIdent(); S.Get(sym);
	IF (x # NIL) & (x.obj # NIL)
		& (x.obj IS B.Module) & (sym = S.period)
	THEN S.Get(sym);
		IF sym = S.ident THEN
			id := S.id; S.Get(sym);
			mod := x.obj(B.Module); x := mod.first;
			WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
			IF x # NIL THEN (*found*) INC(x.nUsed)
			ELSE x := externalIdentNotFound
			END
		ELSE MarkMissing(S.ident)
		END
	END ;
	RETURN x
END qualident0;

PROCEDURE ObjToNode(x: B.Object): B.Node;
	VAR node: B.Node;
BEGIN
	node := arch.Node(S.null); node.obj := x;
	IF x.isVar THEN
		node.var := TRUE;
		node.ronly := x.ronly;
		node.varpar := x.varpar
	END ;
	RETURN node
END ObjToNode;

PROCEDURE qualident(): B.Object;
	VAR id: B.Ident; x: B.Object;
BEGIN id := qualident0();
	IF id # NIL THEN
		IF id # externalIdentNotFound THEN
			IF id.obj # NIL THEN x := id.obj
			ELSE S.Mark('identifier not defined yet')
			END
		ELSE S.Mark('external identifier not found')
		END
	ELSE S.Mark('identifier not found')
	END ;
	IF x # NIL THEN
		IF x.isVar THEN CheckLev(x) END
	ELSE Reset(x)
	END ;
	RETURN x
END qualident;

PROCEDURE Call(x: B.Object): B.Node;
	VAR call, last: B.Node; proc: B.Type;
		fpar: B.Ident; nact: INTEGER;
		
	PROCEDURE Parameter(VAR last: B.Node; fpar: B.Ident);
		VAR y: B.Object; par: B.Node; spos: INTEGER;
	BEGIN
		spos := S.pos; y := expression0();
		IF fpar # NIL THEN CheckPar(fpar.obj(B.Par), y) END ;
		par := NewNode(B.opPar, y, NIL, NIL, spos);
		last.right := par; last := par
	END Parameter;
		
BEGIN (* Call *)
	proc := x.type; call := NewNode(B.opCall, x, NIL, proc.base, S.pos);
	IF sym = S.lparen THEN S.Get(sym);
		IF sym # S.rparen THEN last := call;
			fpar := proc.fields; Parameter(last, fpar); nact := 1;
			WHILE sym = S.comma DO S.Get(sym);
				IF fpar # NIL THEN fpar := fpar.next END ;
				IF sym # S.rparen THEN Parameter(last, fpar); INC(nact)
				ELSE MarkSflous(S.comma)
				END
			END ;
			IF nact = proc.len THEN (*valid*)
			ELSIF nact > proc.len THEN S.Mark('too many params')
			ELSE S.Mark('not enough params')
			END ;
			Check(S.rparen)
		ELSIF sym = S.rparen THEN
			IF proc.len # 0 THEN S.Mark('need params') END ;
			S.Get(sym)
		END
	ELSIF proc.len # 0 THEN S.Mark('need params')
	END ;
	RETURN call
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
			IF y IS B.Const THEN yval := y(B.Const).value;
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
			IF xt.base # NIL THEN (*ok*) ELSE x.type := xt END ;
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
		ELSE Reset(y); MarkMissing(S.ident)
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

(* expressions *)

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
	IF sym = S.int THEN x := arch.Const(B.intType); S.Get(sym)
	ELSIF sym = S.real THEN x := arch.Const(B.realType); S.Get(sym)
	ELSIF sym = S.string THEN
		x := arch.Str(S.str, S.slen); x.ronly := TRUE; S.Get(sym)
	ELSIF sym = S.nil THEN x := arch.NilConst(); S.Get(sym)
	ELSIF sym = S.true THEN x := arch.BoolConst(TRUE); S.Get(sym)
	ELSIF sym = S.false THEN x := arch.BoolConst(FALSE); S.Get(sym)
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
	VAR x: B.Object;
BEGIN x := expression();
	IF x.const THEN (*ok*) ELSE S.Mark('not a const'); Reset(x) END ;
	RETURN x
END ConstExpression;

(* statements *)

PROCEDURE StdProc(f: B.Object): B.Node;
	VAR x, y, z, std, t: B.Node;
BEGIN
	IF ~f.isExtSProc THEN
		std := Node(f.value); Check(S.lparen);
		IF (f.id = B.opINC) OR (f.id = B.opDEC) THEN
			x := designator(); CheckInt(x); CheckVar(x, FALSE);
			IF sym = S.comma THEN
				S.Get(sym); y := expression(); CheckInt(y)
			END
		ELSIF (f.id = B.opINCL) OR (f.id = B.opEXCL) THEN
			x := designator(); CheckSet(x);
			CheckVar(x, FALSE); Check(S.comma);
			y := expression(); CheckInt(y)
		ELSIF f.id = B.opNEW THEN
			x := designator(); CheckPtr(x); CheckVar(x, FALSE)
		ELSIF f.id = B.opASSERT THEN
			x := expression(); CheckBool(x)
		ELSIF f.id = B.opPACK THEN
			x := designator(); CheckReal(x);
			CheckVar(x, FALSE); Check(S.comma);
			y := expression(); CheckInt(y)
		ELSIF f.id = B.opUNPK THEN
			x := designator(); CheckReal(x);
			CheckVar(x, FALSE); Check(S.comma);
			y := designator(); CheckInt(y); CheckVar(y, FALSE)
		ELSIF f.id = B.opGET THEN
			x := expression(); CheckInt(x); Check(S.comma);
			y := designator(); CheckVar(y, FALSE); CheckNonStruct(y)
		ELSIF f.id = B.opPUT THEN
			x := expression(); CheckInt(x); Check(S.comma);
			y := expression(); CheckNonStruct(y)
		ELSIF f.id = B.opCOPY THEN
			x := expression(); CheckInt(x); Check(S.comma);
			t := Node(S.comma);
			y := expression(); CheckInt(y); Check(S.comma);
			z := expression(); CheckInt(z);
			t.right := y; t.left := x; y := t
		ELSE S.Mark('unsupported std procedure')
		END ;
		std.left := x; std.right := y; Check(S.rparen)
	ELSE std := arch.StdProc(f, sym)
	END ;
	RETURN std
END StdProc;

PROCEDURE If(lev: INTEGER): B.Node;
	VAR if, then, x: B.Node;
BEGIN
	if := Node(S.if); S.Get(sym);
	x := expression(); CheckBool(x);
	then := Node(S.then); Check(S.then);
	then.left := StatementSequence0();	
	if.left := x; if.right := then;
	IF sym = S.elsif THEN then.right := If(lev+1)
	ELSIF sym = S.else THEN S.Get(sym); then.right := StatementSequence0()
	ELSE then.right := NIL
	END ;
	IF lev = 0 THEN Check(S.end) END ;
	RETURN if
END If;

PROCEDURE While(lev: INTEGER): B.Node;
	VAR while, do, x: B.Node;
BEGIN
	while := Node(S.while); S.Get(sym);
	x := expression(); CheckBool(x);
	do := Node(S.do); Check(S.do);
	do.left := StatementSequence0();
	while.left := x; while.right := do;
	IF sym = S.elsif THEN do.right := While(lev+1) ELSE do.right := NIL END ;
	IF lev = 0 THEN Check(S.end) END ;
	RETURN while
END While;

PROCEDURE Repeat(): B.Node;
	VAR repeat, x: B.Node;
BEGIN
	repeat := Node(S.repeat); S.Get(sym);
	repeat.left := StatementSequence0(); Check(S.until);
	x := expression(); CheckBool(x); repeat.right := x;
	RETURN repeat
END Repeat;

PROCEDURE For(): B.Node;
	VAR for, control, becomes, to, by, x: B.Node;

	PROCEDURE ident(): B.Node;
		VAR obj: B.Object; ident: B.Ident; x: B.Node;
	BEGIN ident := FindIdent();
		IF ident # NIL THEN obj := ident.obj;
			IF obj # NIL THEN
				IF obj.isVar THEN CheckLev(obj) END ;
				x := ObjToNode(obj)
			ELSE Reset(x); S.Mark('undefined identifier')
			END
		ELSE Reset(x); S.Mark('identifier not found')
		END ;
		S.Get(sym);
		RETURN x
	END ident;

BEGIN
	for := Node(S.for); control := Node(S.null); S.Get(sym);
	IF sym = S.ident THEN
		x := ident(); CheckInt2(x); CheckVar(x, FALSE); control.left := x
	ELSE MarkMissing(S.ident)
	END ;
	Check(S.becomes); becomes := Node(S.becomes);
	x := expression(); CheckInt2(x); becomes.left := x;
	Check(S.to); to := Node(S.to);
	x := expression(); CheckInt2(x); to.left := x;
	IF sym = S.by THEN
		by := Node(S.by); S.Get(sym);
		x := ConstExpression(); CheckInt(x);
		by.left := x; to.right := by
	END ;
	for.left := control; control.right := becomes; becomes.right := to;
	Check(S.do); for.right := StatementSequence0(); Check(S.end);
	RETURN for
END For;

PROCEDURE Case(): B.Node;
	VAR x, y, case: B.Node;
		xform: INTEGER; isTypeCase: BOOLEAN;
		
	PROCEDURE TypeCase(x: B.Node): B.Node;
		VAR bar, colon, is, y: B.Node; obj: B.Object;
			org, yt: B.Type; tc: TypeCastList; spos: INTEGER;
	BEGIN
		IF sym = S.ident THEN
			bar := Node(S.bar); org := x.type;
			obj := qualident(); CheckTypeObj(obj, yt);
			IF org.form = yt.form THEN
				IF IsExt(yt, org) THEN (*ok*)
				ELSE S.Mark('not extension'); yt := org
				END
			ELSE S.Mark(msgIvlType); yt := org
			END ;
			y := TypeToNode(yt); is := Op(S.is, x, y, S.symPos);
			
			IF yt # org THEN
				NEW(tc); tc.node := x; tc.type := yt;
				tc.next := typeCastStack; typeCastStack := tc
			END ;
			colon := Node(S.colon); Check(S.colon);
			y := StatementSequence0(); typeCastStack := typeCastStack.next;
			colon.left := is; colon.right := y; bar.left := colon;
			IF sym = S.bar THEN S.Get(sym); bar.right := TypeCase(x) END
		ELSIF sym = S.bar THEN S.Get(sym); bar := TypeCase(x)
		END ;
		RETURN bar
	END TypeCase;
	
	PROCEDURE label(x: B.Node; VAR y: B.Node);
		CONST errMsg = 'invalid value';
		VAR xform: INTEGER;
	BEGIN xform := x.type.form;
		IF sym = S.int THEN y := factor();
			IF xform # B.tInt THEN S.Mark(errMsg) END
		ELSIF sym = S.string THEN
			y := factor(); CheckChar(y);
			IF xform = B.tInt THEN S.Mark(errMsg) END
		ELSIF sym = S.ident THEN y := ObjToNode(qualident());
			IF xform = B.tInt THEN CheckInt(y) ELSE CheckChar(y) END
		ELSE S.Mark('need integer or char value')
		END
	END label;
	
	PROCEDURE LabelRange(x: B.Node): B.Node;
		VAR y, z, cond: B.Node; spos: INTEGER;
	BEGIN label(x, y);
		IF sym # S.upto THEN cond := Op(S.eql, x, y, S.symPos)
		ELSE spos := S.symPos;
			y := Op(S.geq, x, y, S.symPos);
			S.Get(sym); label(x, z);
			z := Op(S.leq, x, y, S.symPos);
			cond := Op(S.and, x, y, spos)
		END ;
		RETURN cond
	END LabelRange;
	
	PROCEDURE NumericCase(x: B.Node): B.Node;
		VAR bar, colon, label, y, z: B.Node; spos: INTEGER;
	BEGIN
		IF (sym = S.int) OR (sym = S.string) OR (sym = S.ident) THEN
			label := LabelRange(x);
			WHILE sym = S.comma DO
				spos := S.symPos; y := label; S.Get(sym); z := LabelRange(x);
				label := Op(S.or, x, y, spos)
			END ;
			colon := arch.Node(S.colon); Check(S.colon);
			colon.left := label; colon.right := StatementSequence0();
			bar := arch.Node(S.bar); bar.left := colon;
			IF sym = S.bar THEN S.Get(sym); bar.right := NumericCase(x) END
		ELSIF sym = S.bar THEN S.Get(sym); bar := NumericCase(x)
		END
		RETURN bar
	END NumericCase;
		
BEGIN (* Case *)
	case := arch.Node(S.case); S.Get(sym);
	x := expression(); xform := x.type.form; isTypeCase := FALSE;
	IF (xform IN {B.tInt, B.tChar}) OR IsCharStr(x.type) THEN (*ok*)
	ELSIF TypeTestable(x) THEN isTypeCase := TRUE
	ELSE Reset(x); S.Mark('invalid case expression')
	END ;
	Check(S.of); case.left := x;
	IF isTypeCase THEN case.right := TypeCase(x)
	ELSE case.right := NumericCase(y)
	END ;
	Check(S.end);
	RETURN case
END Case;

PROCEDURE StatementSequence(): B.Node;
	VAR seq, st, prev, becomes, x, y: B.Node;
BEGIN
	REPEAT (*sync*)
		IF (sym = S.ident) OR (sym >= S.semicolon)
		OR (sym >= S.if) & (sym <= S.for) THEN (*valid*)
		ELSE
			S.Mark('statement?');
			REPEAT S.Get(sym) UNTIL (sym = S.ident) OR (sym >= S.semicolon)
		END ;
		
		st := arch.Node(S.semicolon);
		IF prev = NIL THEN seq := st ELSE prev.right := st END ;
		IF sym = S.ident THEN x := designator();
			IF sym = S.becomes THEN
				becomes := arch.Node(S.becomes);
				CheckVar(x, FALSE); becomes.left := x; S.Get(sym);
				y := expression(); CheckAssignment(x.type, y);
				becomes.right := y; st.left := becomes
			ELSIF sym = S.eql THEN
				S.Mark('should be :='); S.Get(sym); y := expression()
			ELSIF (x.type # NIL) & (x.type.form = B.tProc) THEN
				IF x.type.base = NIL THEN (*ok*)
				ELSE S.Mark('not proper procedure')
				END ;
				st.left := Call(x)
			ELSIF (x.obj # NIL) & x.obj.isSProc THEN st.left := StdProc(x.obj)
			ELSE S.Mark('invalid statement')
			END
		ELSIF sym = S.if THEN st.left := If(0)
		ELSIF sym = S.while THEN st.left := While(0)
		ELSIF sym = S.repeat THEN st.left := Repeat()
		ELSIF sym = S.for THEN st.left := For()
		ELSIF sym = S.case THEN st.left := Case()
		END ;
		IF sym <= S.semicolon THEN Check(S.semicolon); prev := st END
	UNTIL sym > S.semicolon;
	RETURN seq
END StatementSequence;

(* declarations *)

PROCEDURE CheckExport(VAR exp: BOOLEAN);
BEGIN
	IF sym = S.times THEN S.Get(sym);
		IF B.curLev = 0 THEN exp := TRUE ELSE S.Mark('not exportable') END
	END
END CheckExport;

PROCEDURE OpenScope;
	VAR scp: B.Scope;
BEGIN NEW(scp); scp.dsc := mod.topScope; mod.topScope := scp
END OpenScope;

PROCEDURE CloseScope;
BEGIN mod.topScope := mod.topScope.dsc
END CloseScope;

PROCEDURE IncLev(x: INTEGER);
BEGIN INC(mod.curLev, x)
END IncLev;

PROCEDURE NewIdent(VAR id: B.Ident);
	VAR prev, x: B.Ident;
BEGIN
	x := mod.topScope.first; NEW(id); id.spos := S.symPos;
	id.export := FALSE; id.name := S.id; id.used := FALSE;
	WHILE x # NIL DO
		IF x # NIL THEN S.Mark('duplicated ident') END ;
		prev := x; x := x.next
	END ;
	IF prev # NIL THEN prev.next := id ELSE mod.topScope.first := id END
END NewIdent;

PROCEDURE IdentList(): B.Ident;
	VAR fst, x: B.Ident;
BEGIN
	NewIdent(fst); S.Get(sym); CheckExport(fst.export);
	WHILE sym = S.comma DO
		IF sym = S.ident THEN
			NewIdent(x); S.Get(sym); CheckExport(x.export)
		ELSE MarkSflous(S.comma)
		END
	END ;
	RETURN fst
END IdentList;

PROCEDURE FormalType(): B.Type;
	VAR x: B.Object; tp: B.Type;
BEGIN tp := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF x.isType THEN tp := x.type ELSE S.Mark('not type') END
	ELSIF sym = S.array THEN
		tp := arch.FormalArrayType(); S.Get(sym);
		arch.ParseFormalArrayFlags(tp, sym); Check(S.of);
		IF sym = S.array THEN S.Mark('multidim open array not supported') END ;
		tp.base := FormalType()
	END ;
	RETURN tp
END FormalType;

PROCEDURE FPSection(proc: B.Type);
	VAR first, id: B.Ident; x: B.Object;
		tp: B.Type; var: BOOLEAN;
BEGIN
	IF sym # S.var THEN var := FALSE ELSE var := TRUE; S.Get(sym) END ;
	IF sym = S.ident THEN
		NewIdent(first); S.Get(sym);
		WHILE sym = S.comma DO S.Get(sym);
			IF sym = S.ident THEN
				NewIdent(id); S.Get(sym);
				IF first = NIL THEN first := id END
			ELSE MarkSflous(S.comma)
			END
		END
	ELSE S.Mark('no params?')
	END ;
	Check(S.colon); tp := FormalType(); id := first;
	WHILE id # NIL DO
		x := arch.Par(proc, tp, var); INC(proc.nfpar);
		x.ronly := ~var & (tp.form IN tStructs);
		x.ident := id; id.obj := x; id := id.next
	END
END FPSection;

PROCEDURE FormalParameters(proc: B.Type);
	VAR tp: B.Type; x: B.Object;
BEGIN S.Get(sym);
	IF (sym = S.ident) OR (sym = S.var) THEN
		OpenScope; FPSection(proc);
		WHILE sym = S.semicolon DO S.Get(sym);
			IF (sym = S.ident) OR (sym = S.var) THEN FPSection(proc)
			ELSE S.Mark('param section?')
			END
		END ;
		proc.fields := mod.topScope.first; CloseScope
	END ;
	Check(S.rparen);
	IF sym = S.colon THEN S.Get(sym);
		IF sym = S.ident THEN
			x := qualident(); CheckTypeObj(x, tp);
			IF ~(tp.form IN B.tStructs) THEN proc.base := tp
			ELSE S.Mark(msgIvlType); proc.base := B.intType
			END
		ELSE MarkMissing(S.ident)
		END
	END
END FormalParameters;

PROCEDURE PointerType(defobj: B.Object): B.Type;
	VAR ptr, bt: B.Type; name: S.Ident; x: B.Ident;
BEGIN
	ptr := arch.PointerType(); S.Get(sym);
	arch.ParsePointerFlags(ptr, sym); Check(S.to);
	IF defobj # NIL THEN defobj.type := ptr END ;
	IF sym = S.ident THEN name := S.id; x := qualident0();
		IF x # NIL THEN
			IF x.obj # NIL THEN CheckTypeObj(x.obj, bt);
				IF bt.form = B.tRec THEN arch.SetPointerBaseType(ptr, bt)
				ELSE S.Mark('not a record type')
				END
			ELSIF x = externalIdentNotFound THEN
				S.Mark('identifier does not exist')
			ELSE S.Mark('identifier not defined yet')
			END
		ELSE (* undefined base type *) AddUndef(ptr, name)
		END
	ELSIF sym = S.record THEN bt := type0(); arch.SetPointerBaseType(ptr, bt)
	ELSE S.Mark('base type?')
	END ;
	RETURN ptr
END PointerType;

PROCEDURE BaseType(): B.Type;
	VAR tp, rec: B.Type; x: B.Object;
BEGIN
	IF sym = S.ident THEN
		x := qualident(); CheckTypeObj(x, tp);
		IF tp.form = B.tRec THEN (*ok*)
		ELSIF tp.form = B.tPtr THEN tp := tp.base;
			IF tp # NIL THEN (*ok*)
			ELSE S.Mark('the record of this pointer type is not defined yet')
			END
		ELSE tp := NIL; S.Mark('not record type')
		END ;
		IF (tp # NIL) & (tp.lev >= B.MaxExtension) THEN
			tp := NIL; S.Mark('max extension limit reached')
		END
	ELSE MarkMissing(S.ident)
	END ;
	RETURN tp
END BaseType;

PROCEDURE ArrayType(): B.Type;
	VAR bt: B.Type; len: B.Const;
BEGIN
	len := ConstExpression();
	IF sym = S.comma THEN S.Get(sym);
		WHILE sym = S.comma DO MarkSflous(S.comma); S.Get(sym) END ;
		bt := ArrayType()	
	ELSE Check(S.of); bt := type()
	END ;
	RETURN arch.ArrayType(len, bt)
END ArrayType;

PROCEDURE type(): B.Type;
	VAR tp, ft, bt: B.Type; flds: B.Ident; x: B.Object;
BEGIN tp := B.intType;
	IF sym = S.ident THEN
		x := qualident(); CheckTypeObj(x, tp); S.Get(sym)
	ELSIF sym = S.array THEN
		S.Get(sym); tp := ArrayType()
	ELSIF sym = S.record THEN
		S.Get(sym); tp := arch.RecordType();
		arch.ParseRecordFlags(tp, sym);
		IF sym = S.lparen THEN
			S.Get(sym); bt := BaseType(); tp.base := bt;
			IF bt # NIL THEN
				tp.lev := bt.lev+1; arch.ExtendRecordType(tp)
			END ;
			Check(S.rparen)
		END ;
		OpenScope;
		IF sym = S.ident THEN
			flds := IdentList(); Check(S.colon);
			ft := type(); arch.NewRecordFields(tp, flds, ft);
			WHILE sym = S.semicolon DO S.Get(sym);
				IF sym = S.ident THEN
					flds := IdentList(); Check(S.colon);
					ft := type(); arch.NewRecordFields(tp, flds, ft)
				ELSE MarkSflous(S.semicolon)
				END
			END
		END ;
		tp.fields := mod.topScope.first; CloseScope; Check(S.end)
	ELSIF sym = S.pointer THEN tp := PointerType(NIL)
	ELSIF sym = S.procedure THEN
		S.Get(sym); tp := arch.ProcType();
		IF sym = S.lparen THEN FormalParameters(tp) END
	END ;
	RETURN tp
END type;

PROCEDURE CloneParameters(VAR proc: B.Type);
	VAR p, id: B.Ident; x: B.Object;
BEGIN p := proc.fields;
	WHILE p # NIL DO
		NewIdent(id, p.name); x := arch.ClonePar(p.obj);
		id.spos := p.spos; id.obj := x; INC(x.lev);
		x.ident := id; p := p.next
	END
END CloneParameters;

PROCEDURE DeclarationSequence(owner: B.Object);
	VAR id: B.Ident; x: B.Object; tp, rtype: B.Type;
BEGIN
	IF sym = S.const THEN S.Get(sym);
		WHILE sym = S.ident DO
			NewIdent(id); S.Get(sym); CheckExport(id.export);
			Check(S.eql); x := ConstExpression(); id.obj := x;
			IF x.ident = NIL THEN x.ident := id END ;
			Check(S.semicolon)
		END
	END ;
	IF sym = S.type THEN S.Get(sym);
		WHILE sym = S.ident DO
			NewIdent(id); S.Get(sym);
			CheckExport(id.export); Check(S.eql);
			IF sym # S.pointer THEN
				tp := type();
				x := arch.TypeObj(tp);
				id.obj := x; x.ident := id;
				IF x.form = B.tRec THEN FixUndef(x, id.name) END
			ELSE
				x := arch.TypeObj(B.intType);
				id.obj := x; x.ident := id; tp := PointerType(x);
				IF tp.obj = NIL THEN tp.obj := x END
			END ;
			Check(S.semicolon)
		END ;
		CheckUndef
	END ;
	IF sym = S.var THEN S.Get(sym);
		WHILE sym = S.ident DO
			id := IdentList(); Check(S.colon); tp := type();
			WHILE id # NIL DO
				x := arch.Var(tp, owner);
				id.obj := x; x.ident := id; id := id.next
			END ;
			Check(S.semicolon)
		END
	END ;
	WHILE sym = S.procedure DO S.Get(sym);
		IF sym # S.ident THEN id := NIL; S.Mark('proc name?')
		ELSE NewIdent(id); S.Get(sym); CheckExport(id.export)
		END ;
		x := arch.Proc(); tp := arch.ProcType(); x.type := tp;
		IF id # NIL THEN id.obj := x; x.ident := id END ;
		
		IF sym = S.lparen THEN FormalParameters(tp) END ;
		Check(S.semicolon); OpenScope; IncLev(1); CloneParameters(tp);

		DeclarationSequence(x); x.dsc := mod.topScope.first;
		
		IF sym = S.begin THEN
			S.Get(sym); x.statseq := StatementSequence()
		END ;
		
		rtype := tp.base;
		IF sym = S.return THEN
			IF rtype # NIL THEN (*ok*)
			ELSE rtype := B.intType; S.Mark('not function proc')
			END ;
			S.Get(sym); x.return := expression();
			CheckAssignment(rtype, x.return)
		ELSIF rtype # NIL THEN MarkMissing(S.return)
		END ;
		
		CloseScope; IncLev(-1); Check(S.end);
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

PROCEDURE ModuleId(VAR modid: B.ModuleId);
BEGIN
	modid.context := 0X; modid.name := S.id; S.Get(sym);
	IF sym = S.period (* my previous syntax *) THEN S.Get(sym);
		IF sym = S.ident THEN
			modid.context := modid.name; modid.name := S.id
		ELSE MarkMissing(S.ident)
		END
	ELSIF sym = S.in (* A2 Oberon syntax *) THEN S.Get(sym);
		IF sym = S.ident THEN modid.context := S.id
		ELSE MarkMissing(S.ident)
		END
	END
END ModuleId;

PROCEDURE import;
	VAR ident: B.Ident; id: B.ModuleId; name: S.Ident;
BEGIN
	B.NewIdent(ident); name := S.id; S.Get(sym);
	IF sym = S.becomes THEN S.Get(sym);
		IF sym = S.ident THEN ModuleId(id) ELSE MarkMissing(S.ident) END
	END;
	IF S.errcnt = 0 THEN
		(*
		IF name = 'SYSTEM' THEN B.NewSystemModule(ident)
		ELSIF name # 0X THEN B.NewModule(ident, name)
		ELSE B.NewModule0(ident, id)
		END
		*)
	END
END import;

PROCEDURE ImportList;
BEGIN S.Get(sym);
	IF sym = S.ident THEN import ELSE MarkMissing(S.ident) END ;
	WHILE sym = S.comma DO S.Get(sym);
		IF sym = S.ident THEN import ELSE MarkMissing(S.ident) END
	END ;
	Check(S.semicolon)
END ImportList;

PROCEDURE Module*(arch0: IArch);
	VAR modid: B.ModuleId;
BEGIN S.Get(sym);
	IF sym = S.ident THEN ModuleId(modid) ELSE MarkMissing(S.ident) END ;
	IF S.errcnt = 0 THEN
		mod := B.mod; NEW(mod.universe); mod.id := modid;
		mod.topScope := mod.universe; mod.curLev := 0;
		arch := arch0; Check(S.semicolon);
		IF sym = S.import THEN ImportList END
	END ;
	IF S.errcnt = 0 THEN
		DeclarationSequence;
		IF sym = S.begin THEN
			S.Get(sym); mod.init := StatementSequence()
		END ;
		Check(S.end);
		IF sym = S.ident THEN
			IF S.id # modid.name THEN S.Mark('wrong module name') END ;
			S.Get(sym)
		ELSE MarkMissing(S.ident)
		END ;
		Check(S.period)
	END
END Module;

BEGIN
	type0 := type; expression0 := expression;
	StatementSequence0 := StatementSequence;
	NEW(externalIdentNotFound)
END Psr.