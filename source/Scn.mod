MODULE Scn;

IMPORT
	Sys;
	
CONST
	StrLen* = 255; IdLen* = 63;
	
	(* Symbols *)
    null* = 0; times* = 1; rdiv* = 2; div* = 3; mod* = 4;
    and* = 5; plus* = 6; minus* = 7; or* = 8; eql* = 9;
    neq* = 10; lss* = 11; leq* = 12; gtr* = 13; geq* = 14;
    in* = 15; is* = 16; arrow* = 17; period* = 18;
    char* = 20; int* = 21; real* = 22; false* = 23; true* = 24;
    nil* = 25; string* = 26; not* = 27; lparen* = 28; lbrak* = 29;
    lbrace* = 30; ident* = 31;
    if* = 32; while* = 34; repeat* = 35; case* = 36; for* = 37;
    comma* = 40; colon* = 41; becomes* = 42; upto* = 43; rparen* = 44;
    rbrak* = 45; rbrace* = 46; then* = 47; of* = 48; do* = 49;
    to* = 50; by* = 51; semicolon* = 52; end* = 53; bar* = 54;
    else* = 55; elsif* = 56; until* = 57; return* = 58;
    array* = 60; record* = 61; pointer* = 62;
	const* = 70; type* = 71; var* = 72; procedure* = 73; begin* = 74; 
	import* = 76; module* = 77;

TYPE
	Str* = ARRAY StrLen+1 OF CHAR;
	Ident* = ARRAY IdLen+1 OF CHAR;

	Scanner* = POINTER TO RECORD
		ch: CHAR; eof, escUpto: BOOLEAN;
		pos*, symPos*, errcnt*: INTEGER;
		sym*: INTEGER; id*: Ident;
		str*: Str; slen*: INTEGER;
		ival*: Sys.Int; rval*: Sys.Double
	END ;
	
PROCEDURE SetInput*(scn: Scanner; fname: ARRAY OF CHAR; pos: INTEGER);
END SetInput;

PROCEDURE ReadCh(scn: Scanner);
END ReadCh;

PROCEDURE Mark*(scn: Scanner; msg: ARRAY OF CHAR);
END Mark;

PROCEDURE String(scn: Scanner; quote: CHAR);
	VAR i: INTEGER;
BEGIN ReadCh(scn); i := 0;
	WHILE (i < StrLen) & (scn.ch # quote) DO
		str[i] := scn.ch; INC(i); ReadCh(scn)
	END ;
	IF scn.ch # quote THEN Mark(scn, 'string too long') END ;
	str[i] := 0X; slen := i; ReadCh(scn)
END String;

PROCEDURE Comment(scn: Scanner; lev: INTEGER);
BEGIN ReadCh(scn);
	REPEAT
		WHILE (scn.ch # '*') & ~scn.eof DO
			IF scn.ch # '(' THEN ReadCh(scn)
			ELSE ReadCh(scn);
				IF scn.ch = '*' THEN Comment(scn, lev+1) END
			END
		END ;
		ReadCh(scn)
	UNTIL scn.eof OR (scn.ch = ')');
	IF ~scn.eof THEN ReadCh(scn) ELSE Mark(scn, 'comment without closure')
	END
END Comment;

PROCEDURE KeywordOrIdent(scn: Scanner);
	VAR i: INTEGER; hasNonCap, validChar: BOOLEAN;
BEGIN
	scn.sym := ident;
	hasNonCap := FALSE; validChar := TRUE; i := 0;
	WHILE validChar & (i < IdLen) DO
		scn.id[i] := scn.ch; INC(i); ReadCh(scn);
		IF (scn.ch >= 'A') & (scn.ch <= 'Z') THEN (* cap char *)
		ELSIF (scn.ch >= '0') & (scn.ch <= '9')
		OR (scn.ch >= 'a') & (scn.ch <= 'z')
		OR (scn.ch = '_') THEN hasNonCap := TRUE
		ELSE validChar := FALSE
		END
	END ;
	scn.id[i] := 0X; IF validChar THEN Mark(scn, 'identifier too long') END ;

	IF hasNonCap OR (i < 2) THEN (* ident *)
	ELSIF i = 3 THEN
		IF scn.id = 'END' THEN scn.sym := end
		ELSIF scn.id = 'VAR' THEN scn.sym := var
		ELSIF scn.id = 'NIL' THEN scn.sym := nil
		ELSIF scn.id = 'FOR' THEN scn.sym := for
		ELSIF scn.id = 'MOD' THEN scn.sym := mod
		ELSIF scn.id = 'DIV' THEN scn.sym := div
		END
	ELSIF i = 4 THEN
		IF scn.id = 'THEN' THEN scn.sym := then
		ELSIF scn.id = 'ELSE' THEN scn.sym := else
		ELSIF scn.id = 'TRUE' THEN scn.sym := true
		ELSIF scn.id = 'TYPE' THEN scn.sym := type
		ELSIF scn.id = 'CASE' THEN scn.sym := case
		END
	ELSIF i = 2 THEN
		IF scn.id = 'IF' THEN scn.sym := if
		ELSIF scn.id = 'OR' THEN scn.sym := or
		ELSIF scn.id = 'OF' THEN scn.sym := of
		ELSIF scn.id = 'DO' THEN scn.sym := do
		ELSIF scn.id = 'IN' THEN scn.sym := in
		ELSIF scn.id = 'IS' THEN scn.sym := is
		ELSIF scn.id = 'BY' THEN scn.sym := by
		END
	ELSIF i = 5 THEN
		IF scn.id = 'ELSIF' THEN scn.sym := elsif
		ELSIF scn.id = 'WHILE' THEN scn.sym := while
		ELSIF scn.id = 'UNTIL' THEN scn.sym := until
		ELSIF scn.id = 'BEGIN' THEN scn.sym := begin
		ELSIF scn.id = 'CONST' THEN scn.sym := const
		ELSIF scn.id = 'FALSE' THEN scn.sym := false
		ELSIF scn.id = 'ARRAY' THEN scn.sym := array
		END
	ELSIF i = 6 THEN
		IF scn.id = 'RECORD' THEN scn.sym := record
		ELSIF scn.id = 'REPEAT' THEN scn.sym := repeat
		ELSIF scn.id = 'RETURN' THEN scn.sym := return
		ELSIF scn.id = 'IMPORT' THEN scn.sym := import
		ELSIF scn.id = 'MODULE' THEN scn.sym := module
		END
	ELSIF i = 7 THEN
		IF scn.id = 'POINTER' THEN scn.sym := pointer END
	ELSIF i = 9 THEN
		IF scn.id = 'PROCEDURE' THEN scn.sym := procedure END
	ELSE (* ident *)
	END
END KeywordOrIdent;

PROCEDURE Identifier(scn: Scanner);
	VAR i: INTEGER; validChar: BOOLEAN;
BEGIN i := 0; validChar := TRUE;
	WHILE (i < IdLen) & validChar DO
		scn.id[i] := scn.ch; INC(i); ReadCh(scn);
		validChar := (scn.ch >= 'a') & (scn.ch <= 'z')
			OR (scn.ch >= 'A') & (scn.ch <= 'Z')
			OR (scn.ch >= '0') & (scn.ch <= '9') OR (scn.ch = '_')
	END ;
	scn.id[i] := 0X; IF validChar THEN Mark(scn, 'identifier too long') END 
END Identifier;

PROCEDURE Real(scn: Scanner; d: Sys.Decimal; intLen: INTEGER);
	VAR f: Sys.Decimal; e: Sys.Int;
		fracLen, k, float, last: INTEGER; negE, expTooBig: BOOLEAN;
BEGIN
	fracLen := 0;
	WHILE (scn.ch >= '0') & (scn.ch <= '9') DO (* fraction *)
		IF fracLen < Sys.MaxFracLen THEN
			f[fracLen] := ORD(scn.scn.ch) - 30H; INC(fracLen)
		ELSE Mark(scn, 'Fraction part is too long'); fracLen := 0
		END ;
		ReadCh(scn)
	END ;
	IF (scn.ch = 'E') OR (scn.ch = 'D') THEN (* scale factor *)
		ReadCh(scn); e := Sys.ZeroInt; 
		IF scn.ch = '-' THEN negE := TRUE; ReadCh(scn)
		ELSE negE := FALSE; IF scn.ch = '+' THEN ReadCh(scn) END
		END ;
		IF (scn.ch >= '0') & (scn.ch <= '9') THEN
			REPEAT
				Sys.MulIntByte(e, 10);
				Sys.AddIntByte(ORD(scn.ch)-30H); ReadCh(scn);
				expTooBig := Sys.CmpInt(e, Sys.MaxRealExp);
			UNTIL (scn.ch < '0') OR (scn.ch > '9') OR expTooBig;
			IF expTooBig THEN Mark(scn, 'Exponent part is too large');
				WHILE (scn.ch < '0') OR (scn.ch > '9') DO ReadCh(scn) END
			END ;
			IF negE THEN Sys.NegInt(e) END
		ELSE Mark(scn, 'Digit?')
		END
	END ;
	scn.sym := real; Sys.DecToReal(d, intLen, f, fracLen, e, scn.rval);
END Real;

PROCEDURE Number(scn: Scanner);
	VAR i, n: INTEGER;
		x: Sys.Real; k2: Sys.Int; h: BYTE;
		d: Sys.Decimal; errormsg: Str;
BEGIN
	scn.ival := Sys.ZeroInt; i := 0; n := 0; k2 := Sys.ZeroInt;
    REPEAT
		IF n < LEN(d) THEN d[n] := ORD(scn.ch) - 30H; INC(n)
		ELSE Mark(scn, 'Too many digits'); n := 0
		END ;
		ReadCh(scn)
    UNTIL (scn.ch < '0') OR (scn.ch > '9') & (scn.ch < 'A') OR (scn.ch > 'F');
    IF (scn.ch = 'H') OR (scn.ch = 'R') OR (scn.ch = 'X') THEN  (* hex *)
		REPEAT h := d[i];
			IF h >= 10 THEN h := h-7 END ;
			Sys.MulIntByte(k2, 10H);
			Sys.AddIntByte(k2, h); (* no overflow check *)
			INC(i)
		UNTIL i = n;
		IF scn.ch = 'X' THEN scn.sym := string;
			IF Sys.CmpInt(k2, Sys.MaxUnicode) > 0 THEN
				Mark(scn, 'Not a valid Unicode codepoint'); k2 := Sys.ZeroInt
			END ;
			Sys.CodepointToStr(k2, str)
		ELSIF scn.ch = 'R' THEN scn.sym := real; rval := Sys.HexToReal(k2)
		ELSE scn.sym := int; ival := k2
		END ;
		ReadCh(scn)
    ELSIF scn.ch = '.' THEN ReadCh(scn);
		IF scn.ch = '.' THEN (* double dot *)
			scn.ch := 7FX; scn.escUpto := TRUE; 
			(* decimal integer *)
			scn.sym := int; Sys.DecToInt(d, n, scn.ival, errormsg);
			IF errormsg = 0X THEN Mark(scn, errormsg) END
		ELSE (* real number *)
			Real(scn, d, n)
		END
    ELSE (* decimal integer *)
		scn.sym := int; Sys.DecToInt(d, n, scn.ival, errormsg);
		IF errormsg = 0X THEN Mark(scn, errormsg) END
    END
END Number;

PROCEDURE Get*(scn: Scanner);
BEGIN
	REPEAT
		WHILE ~scn.eof & (scn.ch <= ' ') DO
			scn.symPos := scn.pos; ReadCh(scn)
		END ;
		IF scn.ch < '0' THEN
			IF scn.ch = 22X (* " *) THEN scn.sym := string; String(scn, 22X)
			ELSIF scn.ch = '#' THEN ReadCh(scn); scn.sym := neq
			ELSIF scn.ch = '&' THEN ReadCh(scn); scn.sym := and
			ELSIF scn.ch = 27X (* ' *)THEN scn.sym := string; String(scn, 27X)
			ELSIF scn.ch = '(' THEN ReadCh(scn);
				IF scn.ch # '*' THEN scn.sym := lparen
				ELSE Comment(scn, 0); scn.sym := null
				END
			ELSIF scn.ch = ')' THEN ReadCh(scn); scn.sym := rparen
			ELSIF scn.ch = '*' THEN ReadCh(scn); scn.sym := times
			ELSIF scn.ch = '+' THEN ReadCh(scn); scn.sym := plus
			ELSIF scn.ch = ',' THEN ReadCh(scn); scn.sym := comma
			ELSIF scn.ch = '-' THEN ReadCh(scn); scn.sym := minus
			ELSIF scn.ch = '.' THEN ReadCh(scn);
				IF scn.ch # '.' THEN scn.sym := period
				ELSE ReadCh(scn); scn.sym := upto
				END
			ELSIF scn.ch = '/' THEN ReadCh(scn); scn.sym := rdiv
			ELSE (* ! % *) ReadCh(scn); scn.sym := null
			END
		ELSIF scn.ch <= '9' THEN Number(scn)
		ELSIF scn.ch < 'A' THEN
			IF scn.ch = ':' THEN ReadCh(scn); scn.sym := colon
			ELSIF scn.ch = ';' THEN ReadCh(scn); scn.sym := semicolon
			ELSIF scn.ch = '<' THEN ReadCh(scn);
				IF scn.ch # '=' THEN scn.sym := lss
				ELSE ReadCh(scn); scn.sym := leq
				END
			ELSIF scn.ch = '=' THEN scn.sym := eql
			ELSIF scn.ch = '>' THEN ReadCh(scn);
				IF scn.ch # '=' THEN scn.sym := gtr
				ELSE ReadCh(scn); scn.sym := geq
				END
			ELSE (* ? @ *) ReadCh(scn); scn.sym := null
			END
		ELSIF scn.ch <= 'Z' THEN KeywordOrIdent(scn)
		ELSIF scn.ch < 'a' THEN
			IF scn.ch = ']' THEN scn.sym := rbrak
			ELSIF scn.ch = '[' THEN scn.sym := lbrak
			ELSIF scn.ch = '^' THEN scn.sym := arrow
			ELSIF scn.ch = '_' THEN Identifier(scn); scn.sym := ident 
			ELSE (* ` *) scn.sym := null
			END ;
			ReadCh(scn)
		ELSIF scn.ch <= 'z' THEN Identifier(scn); scn.sym := ident ELSE
			IF scn.ch = '{' THEN scn.sym := rbrace
			ELSIF scn.ch = '|' THEN scn.sym := bar
			ELSIF scn.ch = 7FX (* escape *) THEN
				IF scn.escUpto THEN
					scn.sym := upto; ReadCh(scn); scn.escUpto := FALSE
				ELSE scn.sym := null
				END
			ELSE (* others *) scn.sym := null 
			END ;
			ReadCh(scn)
		END 
	UNTIL (scn.sym # null) OR scn.eof
END Get;

END Scn.