MODULE PsrX8664;

IMPORT
	S := Scn, B := Base, B64 := BaseX8664, P := Psr;
	
VAR
	mod*: B64.Module; arch: P.IArch;	
	strbuf: B64.StrBuf;

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
	IF B.mod.curLev >= -1 (* not imported str, need to alloc buffer *) THEN 
		IF strbuf.size + slen >= LEN(strbuf.buf) THEN
			S.Mark('too many strings'); x.adr := -1
		ELSE
			adr := strbuf.size; x.adr := adr; INC(strbuf.size, slen);
			FOR i := 0 TO slen-1 DO strbuf.buf[adr+i] := str[i] END
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
	x.nPtr := b.nPtr * len;
	x.nTraced := b.nTraced * len;
	x.nProc := b.nProc * len;
	RETURN x
END ArrayType;

PROCEDURE Module*(): B64.Module;
BEGIN
	NEW(mod); B.mod := mod;
	NEW(strbuf); strbuf.size := 0;
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
	arch.ArrayType := ArrayType
END PsrX8664.