MODULE PsrX8664;

IMPORT
	S := Scn, B := Base, B64 := BaseX8664, P := Psr;
	
VAR
	mod: B64.Module; arch: P.IArch;
	
PROCEDURE Align(VAR a: INTEGER; align: INTEGER);
BEGIN
	IF a > 0 THEN a := (a + align - 1) DIV align * align
	ELSIF a < 0 THEN a := a DIV align * align
	END
END Align;

(* -------------------------------------------------------------------------- *)
(* Object *)

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

PROCEDURE Par(pt, ft: B.Type; varpar: BOOLEAN): B.Var;
	VAR x: B64.Par;
		proc, ftype: B64.Type; parSize: INTEGER;
BEGIN
	proc := pt(B64.Type); ftype := ft(B64.Type);
	NEW(x); x.type := ft; x.varpar := varpar;
	
	x.adr := proc.parblksize + 16; parSize := 8;
	IF ftype.openArray OR (varpar & ftype.form = B.tRec) THEN
		IF ~ftype.untagged THEN parSize := 16 END
	END ;
	INC(proc.parblksize, parSize);
	RETURN x
END Par;

PROCEDURE Var(t: B.Type; owner: B.Proc): B.Var;
	VAR x: B64.Var; vt: B64.Type; proc: B64.Proc;
		blksize: INTEGER;
BEGIN
	NEW(x); vt := t(B64.Type); x.type := t;
	IF owner = NIL THEN
		blksize := mod.varSize; Align(blksize, vt.align);
		INC(blksize, vt.size); x.adr := -blksize;
		IF blksize > B64.MaxSize THEN
			blksize := 0;
			S.Mark('global variables size limit reached')
		END ;
		mod.varSize := blksize
	ELSE
		proc := owner(B64.Proc); blksize := proc.locblksize;
		Align(blksize, vt.align); INC(blksize, vt.size);
		x.adr := -blksize;
		IF blksize > B64.MaxLocBlkSize THEN
			blksize := 0;
			S.Mark('global variables size limit reached')
		END ;
		proc.locblksize := blksize
	END ;
	RETURN x
END Var;

PROCEDURE Proc(): B.Proc;
	VAR x: B64.Proc;
BEGIN
	NEW(x); x.adr := 0; x.locblksize := 0;
	RETURN x
END Proc;

PROCEDURE ClonePar(org: B.Object): B.Var;
	VAR x: B64.Par;
BEGIN
	NEW(x); x^ := org(B64.Par);
	RETURN x
END Proc;

(* -------------------------------------------------------------------------- *)
(* Type *)

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
	NEW(x); x.union := FALSE; x.untagged := FALSE;
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
	NEW(arch);
	
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
END PsrX8664.