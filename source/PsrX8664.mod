MODULE PsrX8664;

IMPORT
	Sys, S := Scn, B := Base, B64 := BaseX8664, P := Psr;
	
TYPE
	Parser = B.Parser;
	
PROCEDURE Align(VAR a: INTEGER; align: INTEGER);
BEGIN
	IF a > 0 THEN a := (a + align - 1) DIV align * align
	ELSIF a < 0 THEN a := a DIV align * align
	END
END Align;

(* -------------------------------------------------------------------------- *)
(* Object creation *)

PROCEDURE NewConst(psr: Parser; t: B.Type; ival: Sys.Int): B.Const;
	VAR x: B64.Const;
BEGIN
	NEW(x); B.InitConst(x); x.type := t; x.ival := ival;
	RETURN x
END NewConst;

PROCEDURE NewConstR(psr: Parser; rval: Sys.Real): B.Const;
	VAR x: B64.Const;
BEGIN
	NEW(x); B.InitConst(x); x.type := psr.mod.realType; x.rval := rval;
	RETURN x
END NewConstR;

PROCEDURE NewStr(psr: Parser; str: S.Str; slen: INTEGER): B.Str;
	VAR x: B64.Str; mod: B64.Module; pos, i: INTEGER;
BEGIN
	NEW(x); B.InitStr(x); mod := psr.mod(B64.Module); x.len := slen;
	IF (mod.strbufSize + slen) > LEN(mod.strbuf) THEN
		S.Mark(psr.scn, 'too many strings'); x.pos := -1
	ELSE
		pos := mod.strbufSize; x.pos := pos; INC(mod.strbufSize, slen);
		FOR i := 0 TO slen-1 DO
			mod.strbuf[pos+i] := ORD(str[i])
		END
	END ;
	RETURN x
END NewStr;

(* -------------------------------------------------------------------------- *)
(* Const folding *)

PROCEDURE CloneConst(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const;
BEGIN
	IF x.type.form # B.tReal THEN
		res := NewConst(psr, x.type, x(B64.Const).ival)
	ELSE res := NewConstR(psr, x(B64.Const).rval)
	END ;
	RETURN res
END CloneConst;

PROCEDURE OpAbs(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const;
BEGIN
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	IF x.type.form = B.tInt THEN
		IF Sys.SignInt(x(B64.Const).ival) THEN
			Sys.NegInt(res(B64.Const).ival); res.type := psr.mod.intType
		END
	ELSIF x.type.form = B.tReal THEN Sys.AbsReal(res(B64.Const).rval)
	ELSE ASSERT(FALSE)
	END ;
	RETURN res
END OpAbs;

PROCEDURE OpOdd(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const;
BEGIN ASSERT(x.type.form = B.tInt);
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	Sys.ModIntByte(res(B64.Const).ival, 2); res.type := psr.mod.boolType;
	RETURN res
END OpOdd;

PROCEDURE OpShift(psr: Parser; op: INTEGER; x, y: B.Const): B.Const;
	VAR res: B.Const; shfCnt: Sys.Int;
BEGIN ASSERT(x.type.form = B.tInt); ASSERT(y.type.form = B.tInt);
	IF ~x.named THEN res := x
	ELSIF ~y.named THEN res := y ELSE res := CloneConst(psr, x)
	END ;
	IF ~Sys.SignInt(y(B64.Const).ival) THEN shfCnt := y(B64.Const).ival
	ELSE S.Mark(psr.scn, 'Shift count should be non-negative')
	END ;
	IF op = B.opLSL THEN Sys.LShiftLeft(res(B64.Const).ival, shfCnt)
	ELSIF op = B.opASR THEN Sys.AShiftRight(res(B64.Const).ival, shfCnt)
	ELSIF op = B.opROR THEN Sys.RotRight(res(B64.Const).ival, shfCnt)
	ELSE ASSERT(FALSE)
	END ;
	res.type := psr.mod.intType;
	RETURN res
END OpShift;

PROCEDURE OpFloor(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const;
BEGIN ASSERT(x.type.form = B.tReal);
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	Sys.FloorReal(res(B64.Const).rval);
	RETURN res
END OpFloor;

PROCEDURE OpFlt(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const;
BEGIN ASSERT(x.type.form = B.tInt);
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	Sys.IntToReal(x(B64.Const).ival, res(B64.Const).rval);
	res.type := psr.mod.realType;
	RETURN res
END OpFlt;

PROCEDURE OpChr(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const;
BEGIN ASSERT(x.type.form = B.tInt);
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	res.type := psr.mod.charType;
	RETURN res
END OpChr;

PROCEDURE OpOrdChar(psr: Parser; x: B.Str): B.Const;
	VAR res: B.Const; ival: Sys.Int;
BEGIN
	IF x(B64.Str).pos >= 0 THEN
		Sys.ByteToInt(psr.mod(B64.Module).strbuf[x(B64.Str).pos], ival)
	END ;
	res := psr.NewConst(psr, psr.mod.intType, ival);
	RETURN res
END OpOrdChar;

PROCEDURE OpRangeSet(psr: Parser; x, y: B.Const): B.Const;
	VAR res: B.Const; low, hi, shfCnt: Sys.Int;
BEGIN ASSERT(x.type.form = B.tInt); ASSERT(y.type.form = B.tInt);
	IF ~x.named THEN res := x
	ELSIF ~y.named THEN res := y ELSE res := CloneConst(psr, x)
	END ;
	IF ~Sys.SignInt(x(B64.Const).ival) & ~Sys.SignInt(y(B64.Const).ival)
		& (Sys.CmpIntByte(x(B64.Const).ival, Sys.SizeInt*8) < 0)
		& (Sys.CmpIntByte(y(B64.Const).ival, Sys.SizeInt*8) < 0) THEN (*ok*)
	ELSE S.Mark(psr.scn, 'out of limit value')
	END ;
	low := Sys.IntMinusOne;
	Sys.LShiftLeft(low, x(B64.Const).ival);
	Sys.ByteToInt(Sys.SizeInt*8 - 1, shfCnt);
	Sys.SubInt(shfCnt, y(B64.Const).ival);
	hi := Sys.IntMinusOne; Sys.LShiftRight(hi, shfCnt);
	Sys.AndInt(low, hi); res(B64.Const).ival := low;
	res.type := psr.mod.setType;
	RETURN res
END OpRangeSet;

PROCEDURE OpSingletonSet(psr: Parser; x: B.Const): B.Const;
	VAR res: B.Const; shfCnt: Sys.Int;
BEGIN ASSERT(x.type.form = B.tInt);
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	IF ~Sys.SignInt(x(B64.Const).ival)
		& (Sys.CmpIntByte(x(B64.Const).ival, Sys.SizeInt*8) < 0)
	THEN (*ok*) ELSE S.Mark(psr.scn, 'out of limit value')
	END ;
	shfCnt := x(B64.Const).ival; res(B64.Const).ival := Sys.IntOne;
	Sys.LShiftLeft(res(B64.Const).ival, shfCnt); res.type := psr.mod.setType;
	RETURN res
END OpSingletonSet;

PROCEDURE OpAdd(psr: Parser; op: INTEGER; x, y: B.Const): B.Const;
	VAR res: B.Const;
BEGIN
	IF ~x.named THEN res := x ELSE res := CloneConst(psr, x) END ;
	IF x.type.form = B.tInt THEN
		Sys.AddInt(res(B64.Const).ival, y(B64.Const).ival);
		res.type := psr.mod.intType
	ELSIF x.type.form = B.tSet THEN
		Sys.OrInt(res(B64.Const).ival, y(B64.Const).ival);
		res.type := psr.mod.setType
	ELSIF x.type.form = B.tReal THEN
		Sys.AddReal(res(B64.Const).rval, y(B64.Const).rval);
		res.type := psr.mod.realType
	ELSE ASSERT(FALSE)
	END ;
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

(*
(* -------------------------------------------------------------------------- *)
(* Type *)

PROCEDURE ArrayType(len: B.Object; bt: B.Type): B.Type;
	VAR x, b: B64.Type;
	
	PROCEDURE ArrayLength(x: B64.Type; len: B64.Object);
	BEGIN
		IF len.value >= 0 THEN x.len := len.value
		ELSE S.Mark('invalid length'); x.len := 0
		END
	END ArrayLength;
	
BEGIN (* ArrayType *)
	NEW(x); P.InitType(x, B.tArray);
	b := bt(B64.Type); x.base := b;
	ArrayLength(x, len);
	x.size := b.size * x.len;
	x.align := b.align;
	x.nPtr := b.nPtr * x.len;
	x.nTraced := b.nTraced * x.len;
	x.nProc := b.nProc * x.len;
	RETURN x
END ArrayType;

PROCEDURE RecordType(): B.Type;
	VAR x: B64.Type;
BEGIN
	NEW(x); P.InitType(x, B.tRec);
	x.union := FALSE; x.untagged := FALSE;
	x.size := 0; x.align := 0; x.adr := -1;
	x.nPtr := 0; x.nTraced := 0; x.nProc := 0;
	RETURN x
END RecordType;

PROCEDURE ParseRecordFlags(t: B.Type; VAR sym: INTEGER);
	VAR brak: BOOLEAN;
	
	PROCEDURE flag(rec: B64.Type; VAR sym: INTEGER);
	BEGIN
		IF S.id = 'union' THEN
			IF ~mod.system THEN S.Mark('must import SYSTEM') END ;
			rec.union := TRUE; rec.untagged := TRUE
		ELSIF S.id = 'untagged' THEN
			IF ~mod.system THEN S.Mark('must import SYSTEM') END ;
			rec.untagged := TRUE
		ELSE S.Mark('invalid flag')
		END ;
		S.Get(sym)
	END flag;
	
BEGIN (* ParseRecordFlags *)
	IF (sym = S.lbrak) OR (sym = S.lbrace) THEN
		brak := (sym = S.lbrak); S.Get(sym);
		IF sym = S.ident THEN flag(t(B64.Type), sym) END ;
		WHILE sym = S.comma DO S.Get(sym);
			IF sym = S.ident THEN flag(t(B64.Type), sym)
			ELSE MarkSflous(S.comma)
			END
		END ;
		IF brak THEN Check(S.rbrak) ELSE Check(S.rbrace) END
	END
END ParseRecordFlags;

PROCEDURE ExtendRecordType(t: B.Type);
	VAR bt, rec: B64.Type;
BEGIN
	rec := t(B64.Type); bt := t.base(B64.Type);
	IF bt.untagged THEN S.Mark('untagged record cannot be base type')
	ELSIF rec.untagged THEN S.Mark('untagged record cannot have base type')
	ELSE
		rec.align := bt.align;
		rec.size := bt.size;
		rec.nPtr := bt.nPtr;
		rec.nTraced := bt.nTraced;
		rec.nProc := bt.nProc 
	END
END ExtendRecordType;

PROCEDURE NewRecordFields(t: B.Type; fst: B.Ident; ft: B.Type);
	VAR rec, ftype: B64.Type; f: B64.Field;
		size, align, i: INTEGER;
BEGIN
	rec := t(B64.Type); ftype := ft(B64.Type);
	IF fst # NIL THEN
		align := rec.align; size := rec.size;
		IF ftype.align > align THEN align := ftype.align END ;
		Align(size, ftype.align); i := 0;
		WHILE fst # NIL DO
			NEW(f); fst.obj := f; f.type := ftype; INC(i);
			IF ~rec.union THEN f.off := size; INC(size, ftype.size)
			ELSE f.off := 0
			END ;
			fst := fst.next
		END ;
		IF rec.union & (ftype.size > size) THEN size := ftype.size END ;
		Align(size, align); rec.size := size; rec.align := align;
		rec.nPtr := rec.nPtr + ftype.nPtr * i;
		rec.nProc := rec.nProc + ftype.nProc * i;
		rec.nTraced := rec.nTraced + ftype.nTraced * i
	END
END NewRecordFields;

PROCEDURE AllocRecordDesc(t: B.Type);
	VAR rec: B64.Type; tdSize: INTEGER;
BEGIN
	rec := t(B64.Type);
	IF ~rec.untagged THEN
		tdSize := (24 + 8*(B.MaxExt + rec.nPtr)) DIV 16 * 16;
		rec.adr := mod.tdescTableSize; INC(mod.tdescTableSize, tdSize)
	END
END AllocRecordDesc;

PROCEDURE PointerType(): B.Type;
	VAR x: B64.Type;
BEGIN
	NEW(x); x.untraced := FALSE; x.unsafe := FALSE;
	x.size := 8; x.align := 8;
	x.nPtr := 1; x.nTraced := 1; x.nProc := 0;
	RETURN x
END PointerType;

PROCEDURE ParsePointerFlags(t: B.Type; VAR sym: INTEGER);
	VAR brak: BOOLEAN;
	
	PROCEDURE flag(ptr: B64.Type; VAR sym: INTEGER);
	BEGIN
		IF (S.id = 'UNSAFE') OR (S.id = 'unsafe') THEN
			IF ~mod.system THEN S.Mark('must import SYSTEM') END ;
			ptr.unsafe := TRUE; ptr.nTraced := 0
		ELSIF (S.id = 'UNTRACED') OR (S.id = 'untraced') THEN
			IF ~mod.system THEN S.Mark('must import SYSTEM') END ;
			ptr.nTraced := 0
		ELSE S.Mark('invalid flag')
		END ;
		S.Get(sym)
	END flag;
	
BEGIN (* ParsePointerFlags *)
	IF (sym = S.lbrak) OR (sym = S.lbrace) THEN
		brak := (sym = S.lbrak); S.Get(sym);
		IF sym = S.ident THEN flag(t(B64.Type), sym) END ;
		WHILE sym = S.comma DO S.Get(sym);
			IF sym = S.ident THEN flag(t(B64.Type), sym)
			ELSE MarkSflous(S.comma)
			END
		END ;
		IF brak THEN Check(S.rbrak) ELSE Check(S.rbrace) END
	END
END ParsePointerFlags;

PROCEDURE SetPointerBaseType(t, bt: B.Type);
	VAR ptr, rec: B64.Type;
BEGIN
	ptr := t(B64.Type); rec := bt(B64.Type);
	IF rec.untagged & ~ptr.unsafe THEN
		S.Mark('pointer must be marked as unsafe to have untagged base type')
	END ;
	ptr.base := rec
END SetPointerBaseType;

PROCEDURE ProcType(): B.Type;
	VAR x: B64.Type;
BEGIN
	NEW(x); x.size := 8; x.align := 8; x.parblksize := 0;
	x.nPtr := 0; x.nTraced := 0; x.nProc := 1;
	RETURN x
END ProcType;

PROCEDURE FormalArrayType(): B.Type;
	VAR x: B64.Type;
BEGIN
	NEW(x); x.len := -1; x.size := 16; x.align := 8;
	x.nPtr := 0; x.nTraced := 0; x.nProc := 0;
	RETURN x
END FormalArrayType;

PROCEDURE ParseFormalArrayFlags(t: B.Type; VAR sym: INTEGER);
	VAR brak: BOOLEAN;
	
	PROCEDURE flag(arr: B64.Type; VAR sym: INTEGER);
	BEGIN
		IF S.id = 'untagged' THEN
			arr.untagged := TRUE; arr.size := 8;
			IF ~mod.system THEN S.Mark('must import SYSTEM') END
		ELSE S.Mark('invalid flag')
		END ;
		S.Get(sym)
	END flag;
	
BEGIN (* ParseFormalArrayFlags *)
	IF (sym = S.lbrak) OR (sym = S.lbrace) THEN
		brak := (sym = S.lbrak); S.Get(sym);
		IF sym = S.ident THEN flag(t(B64.Type), sym) END ;
		WHILE sym = S.comma DO S.Get(sym);
			IF sym = S.ident THEN flag(t(B64.Type), sym)
			ELSE MarkSflous(S.comma)
			END
		END ;
		IF brak THEN Check(S.rbrak) ELSE Check(S.rbrace) END
	END
END ParseFormalArrayFlags;

(* -------------------------------------------------------------------------- *)

PROCEDURE Module*(): B64.Module;
BEGIN
	NEW(mod); B.mod := mod;
	NEW(mod.strbuf); mod.strbuf.size := 0;
	mod.tdescTableSize := 0;
	P.Module(arch);
	RETURN mod
END Module;

BEGIN
	arch.Const := Const;
	arch.ZeroIntConst := ZeroIntConst;
	arch.Str := Str;
	arch.NilConst := NilConst;
	arch.BoolConst := BoolConst;
	arch.Par := Par;
	arch.Var := Var;
	arch.Proc := Proc;
	arch.ClonePar := ClonePar;
	
	arch.CheckArrayLen := CheckArrayLen;
	arch.ArrayType := ArrayType;
	arch.RecordType := RecordType;
	arch.ParseRecordFlags := ParseRecordFlags;
	arch.ExtendRecordType := ExtendRecordType;
	arch.NewRecordFields := NewRecordFields;
	arch.AllocRecordDesc := AllocRecordDesc;
	arch.PointerType := PointerType;
	arch.ParsePointerFlags := arch.ParsePointerFlags;
	arch.SetPointerBaseType := SetPointerBaseType;
	arch.ProcType := ProcType;
	arch.FormalArrayType := FormalArrayType;
	arch.ParseFormalArrayFlags := ParseFormalArrayFlags
*)
END PsrX8664.