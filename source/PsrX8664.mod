MODULE PsrX8664;

IMPORT
	S := Scn, B := Base, B64 := BaseX8664, P := Psr;
	
VAR
	mod*: B64.Module; arch: P.IArch;
	
PROCEDURE Align(VAR a: INTEGER; align: INTEGER);
BEGIN
	IF a > 0 THEN a := (a + align - 1) DIV align * align
	ELSIF a < 0 THEN a := a DIV align * align
	END
END Align;

PROCEDURE ZeroIntConst(): B.Const;
	VAR x: B64.Const;
BEGIN
	NEW(x); x.type := B.intType; x.value := 0;
	RETURN x
END ZeroIntConst;

PROCEDURE Const(t: B.Type): B.Const;
	VAR x: B64.Const;
BEGIN
	NEW(x); x.type := t;
	IF t = B.intType THEN x.value := S.ival
	ELSIF t = B.realType THEN x.value := S.ival
	ELSE ASSERT(FALSE)
	END ;
	RETURN x
END Const;

PROCEDURE Str(str: S.Str; slen: INTEGER): B.Var;
	VAR x: B64.Str; i, adr: INTEGER;
BEGIN
	NEW(x); x.type := B.strType; x.len := slen;
	IF mod.curLev >= -1 (* not imported str, need to alloc buffer *) THEN 
		IF mod.strbuf.size + slen >= LEN(mod.strbuf.buf) THEN
			S.Mark('too many strings'); x.adr := -1
		ELSE
			adr := mod.strbuf.size; x.adr := adr; INC(mod.strbuf.size, slen);
			FOR i := 0 TO slen-1 DO mod.strbuf.buf[adr+i] := str[i] END
		END
	ELSE x.bufpos := -1
	END ;
	RETURN x
END Str;

PROCEDURE NilConst(): B.Const;
	VAR x: B64.Const;
BEGIN
	NEW(x); x.type := B.nilType; x.value := 0;
	RETURN x
END NilConst;

PROCEDURE BoolConst(v: BOOLEAN): B.Const;
	VAR x: B64.Const;
BEGIN
	NEW(x); x.type := B.boolType;
	IF v THEN x.value := 1 ELSE x.value := 0 END ;
	RETURN x
END BoolConst;

PROCEDURE CheckArrayLen(len: B.Const);
	VAR x: B64.Const;
BEGIN
	x := len(B64.Const);
	IF x.value >= 0 THEN (*ok*) ELSE S.Mark('invalid array length') END
END CheckArrayLen;

PROCEDURE ArrayType(len: B.Const; bt: B.Type): B.Type;
	VAR x, b: B64.Type;
BEGIN NEW(x);
	x.len := len(B64.Const).value;
	b := bt(B64.Type); x.base := bt;
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
	NEW(x); x.size := 0; x.align := 0; x.adr := mod.tdescOff;
	x.nPtr := 0; x.nTraced := 0; x.nProc := 0;
	RETURN x
END RecordType;

PROCEDURE ParseRecordFlags(t: B.Type; VAR sym: INTEGER);
	VAR brak: BOOLEAN;
	
	PROCEDURE flag(rec: B64.Type; VAR sym: INTEGER);
	BEGIN
		IF S.id = 'union' THEN
			IF ~mod.system THEN S.Mark('must import SYSTEM') END ;
			rec.union := TRUE; rec.untagged := TRUE; rec.adr := -1
		ELSIF S.id = 'untagged' THEN
			IF ~mod.system THEN S.Mark('must import SYSTEM') END ;
			rec.untagged := TRUE; rec.adr := -1
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
	IF ~bt.union & ~bt.untagged THEN
		rec.align := bt.align; rec.size := bt.size0;
		rec.nPtr := bt.nPtr;
		rec.nTraced := bt.nTraced;
		rec.nProc := bt.nProc
	ELSE S.Mark('not valid base type')
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

PROCEDURE Module*(): B64.Module;
BEGIN
	NEW(mod); B.mod := mod;
	NEW(mod.strbuf); mod.strbuf.size := 0;
	mod.tdescOff := 0;
	P.Module(arch);
	RETURN mod
END Module;

BEGIN
	NEW(arch);
	
	arch.Const := Const;
	arch.ZeroIntConst := ZeroIntConst;
	arch.Str := Str;
	arch.NilConst := NilConst;
	arch.BoolConst := BoolConst;
	
	arch.CheckArrayLen := CheckArrayLen;
	arch.ArrayType := ArrayType;
	arch.RecordType := RecordType;
	arch.ParseRecordFlags := ParseRecordFlags;
	arch.ExtendRecordType := ExtendRecordType;
	arch.NewRecordFields := NewRecordFields
END PsrX8664.