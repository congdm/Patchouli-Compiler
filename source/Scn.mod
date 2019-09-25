MODULE Scn;

IMPORT
	SYSTEM,
	(* This module isn't platform independent
	   it needs INTEGER = int64 and REAL = double *)
	Files, BigNums;

CONST
	StrLen* = 255; IdLen* = 63;
	MaxInt = 7FFFFFFFFFFFFFFFH; MinInt = 8000000000000000H; maxExp = 308;
	
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

VAR
	ch: CHAR; eof, escUpto: BOOLEAN;
	str*: Str; id*: Ident;
	ival*, rval*, slen*: INTEGER;
	
PROCEDURE Init*(fname: ARRAY OF CHAR; pos: INTEGER);
END Init;

PROCEDURE ReadCh;
END ReadCh;

PROCEDURE String(quote: CHAR);
	VAR i: INTEGER;
BEGIN ReadCh; i := 0;
	WHILE (i < StrLen) & (ch # quote) DO
		str[i] := ch; INC(i); ReadCh
	END ;
	IF ch # quote THEN Mark('string too long') END ;
	str[i] := 0X; slen := i; ReadCh
END String;

PROCEDURE Comment(lev: INTEGER);
BEGIN ReadCh;
	REPEAT
		WHILE (ch # '*') & ~eof DO
			IF ch # '(' THEN ReadCh
			ELSE ReadCh;
				IF ch = '*' THEN Comment(lev+1) END
			END
		END ;
		ReadCh
	UNTIL eof OR (ch = ')');
	IF ~eof THEN ReadCh ELSE Mark('comment without closure') END
END Comment;

PROCEDURE KeyWordOrIdent(VAR sym: INTEGER);
	VAR i: INTEGER; hasNonCap, notValidChar: BOOLEAN;
BEGIN
	hasNonCap := FALSE; notValidChar := FALSE; i := 0; sym := ident;
	WHILE ~notValidChar & (i < IdLen) DO
		id[i] := ch; INC(i); ReadCh;
		IF (ch >= 'A') & (ch <= 'Z') THEN (* cap char *)
		ELSIF (ch >= '0') & (ch <= '9')
		OR (ch >= 'a') & (ch <= 'z')
		OR (ch = '_') THEN hasNonCap := TRUE
		ELSE notValidChar := TRUE
		END
	END ;
	id[i] := 0X; IF ~notValidChar THEN Mark('identifier too long') END ;

	IF hasNonCap OR (i < 2) THEN (* ident *)
	ELSIF i = 3 THEN
		IF id = 'END' THEN sym := end
		ELSIF id = 'VAR' THEN sym := var
		ELSIF id = 'NIL' THEN sym := nil
		ELSIF id = 'FOR' THEN sym := for
		ELSIF id = 'MOD' THEN sym := mod
		ELSIF id = 'DIV' THEN sym := div
		END
	ELSIF i = 4 THEN
		IF id = 'THEN' THEN sym := then
		ELSIF id = 'ELSE' THEN sym := else
		ELSIF id = 'TRUE' THEN sym := true
		ELSIF id = 'TYPE' THEN sym := type
		ELSIF id = 'CASE' THEN sym := case
		END
	ELSIF i = 2 THEN
		IF id = 'IF' THEN sym := if
		ELSIF id = 'OR' THEN sym := or
		ELSIF id = 'OF' THEN sym := of
		ELSIF id = 'DO' THEN sym := do
		ELSIF id = 'IN' THEN sym := in
		ELSIF id = 'IS' THEN sym := is
		ELSIF id = 'BY' THEN sym := by
		END
	ELSIF i = 5 THEN
		IF id = 'ELSIF' THEN sym := elsif
		ELSIF id = 'WHILE' THEN sym := while
		ELSIF id = 'UNTIL' THEN sym := until
		ELSIF id = 'BEGIN' THEN sym := begin
		ELSIF id = 'CONST' THEN sym := const
		ELSIF id = 'FALSE' THEN sym := false
		ELSIF id = 'ARRAY' THEN sym := array
		END
	ELSIF i = 6 THEN
		IF id = 'RECORD' THEN sym := record
		ELSIF id = 'REPEAT' THEN sym := repeat
		ELSIF id = 'RETURN' THEN sym := return
		ELSIF id = 'IMPORT' THEN sym := import
		ELSIF id = 'MODULE' THEN sym := module
		END
	ELSIF i = 7 THEN
		IF id = 'POINTER' THEN sym := pointer END
	ELSIF i = 9 THEN
		IF id = 'PROCEDURE' THEN sym := procedure END
	ELSE (* ident *)
	END
END KeyWordOrIdent;

PROCEDURE Identifier;
	VAR i: INTEGER; validChar: BOOLEAN;
BEGIN i := 0; sym := ident; validChar := TRUE;
	WHILE (i < IdLen) & validChar DO
		id[i] := ch; INC(i); ReadCh;
		validChar := (ch >= 'a') & (ch <= 'z') OR (ch >= 'A') & (ch <= 'Z')
			OR (ch >= '0') & (ch <= '9') OR (ch = '_')
	END ;
	id[i] := 0X; IF ~notValidChar THEN Mark('identifier too long') END 
END Identifier;

PROCEDURE Real(VAR sym: INTEGER; d: ARRAY OF INTEGER; n: INTEGER);
	VAR x, f, max, min, half: BigNums.BigNum;
		i, k, e, float, last: INTEGER; negE: BOOLEAN;
BEGIN
	i := n-1; k := 0; x := BigNums.Zero; f := BigNums.Zero;
	REPEAT
		IF d[i] > 10 THEN Mark('Bad number')
		ELSE BigNums.SetDecimalDigit(x, k, d[i])
		END;
		DEC(i); INC(k)
	UNTIL i < 0;
	i := BigNums.MaxDecimalDigits-1;
	WHILE (ch >= '0') & (ch <= '9') DO (* fraction *)
		IF i > BigNums.MaxDecimalDigits-19 THEN
			BigNums.SetDecimalDigit(f, i, ORD(ch)-30H)
		ELSIF i = BigNums.MaxDecimalDigits-19 THEN Mark('Fraction too long')
		END ;
		DEC(i); ReadCh
	END ;
	IF (ch = 'E') OR (ch = 'D') THEN (* scale factor *)
		ReadCh; e := 0; 
		IF ch = '-' THEN negE := TRUE; ReadCh
		ELSE negE := FALSE; IF ch = '+' THEN ReadCh END
		END ;
		IF (ch >= '0') & (ch <= '9') THEN
			REPEAT e := e*10 + ORD(ch)-30H; ReadCh
			UNTIL (ch < '0') OR (ch > '9') OR (e > maxExp);
			IF e > maxExp THEN Mark('Exponent too large');
				WHILE (ch < '0') OR (ch > '9') DO ReadCh END
			END ;
			IF negE THEN e := -e END
		ELSE Mark('Digit?')
		END ;
		i := BigNums.MaxDecimalDigits-1;
		WHILE e > 0 DO BigNums.MultiplyByTen(x, x);
			BigNums.SetDecimalDigit(x, 0, BigNums.DecimalDigit(f, i));
			BigNums.SetDecimalDigit(f, i, 0); BigNums.MultiplyByTen(f, f);
			DEC(e)
		END ;
		WHILE e < 0 DO
			last := BigNums.DecimalDigit(f, 0); BigNums.DivideByTen(f, f);
			BigNums.SetDecimalDigit(f, i, BigNums.DecimalDigit(x, 0));
			BigNums.DivideByTen(x, x);
			IF (last > 5) OR (last = 5) & ODD(BigNums.DecimalDigit(f, 0)) THEN
				IF BigNums.Compare(f, BigNums.MaxNum) = 0 THEN
					f := BigNums.Zero; BigNums.Add(x, x, BigNums.One)
				ELSE BigNums.Add(f, f, BigNums.One)
				END
			END ;
			INC(e)
		END
	END ;
	e := 52; half := BigNums.Zero;
	i := BigNums.MaxDecimalDigits-1; BigNums.SetDecimalDigit(half, i, 5);
	BigNums.Set0(max, 1FFFFFFFFFFFFFH); BigNums.Set0(min, 10000000000000H);
	IF (BigNums.Compare(x, BigNums.Zero) # 0)
	OR (BigNums.Compare(x, BigNums.Zero) # 0) THEN
		WHILE BigNums.Compare(x, min) < 0 DO BigNums.Add(x, x, x);
			IF BigNums.Compare(f, half) >= 0 THEN
				BigNums.Subtract(f, f, half); BigNums.Add(x, x, BigNums.One)
			END ;
			BigNums.Add(f, f, f); DEC(e)
		END ;
		WHILE BigNums.Compare(x, max) > 0 DO BigNums.DivideByTwo(f, f);
			IF BigNums.ModuloTwo(x) = 1 THEN BigNums.Add(f, f, half) END;
			BigNums.DivideByTwo(x, x); INC(e)
		END ;
		float := BigNums.Get0(x); i := BigNums.Compare(f, half);
		IF (i > 0) OR (i = 0) & ODD(float) THEN INC(float);
			IF float > 1FFFFFFFFFFFFFH THEN float := float DIV 2; INC(e) END
		END ;
		float := float - 10000000000000H + (e+1023)*10000000000000H;
	ELSE float := 0
	END ;
	sym := real; rval := SYSTEM.VAL(REAL, float); ival := float
END Real;

PROCEDURE Number(VAR sym: INTEGER);
    CONST max = MaxInt;
	VAR i, k2, e, n, s, h: INTEGER; x: REAL;
		d: ARRAY 21 OF INTEGER; negE: BOOLEAN;
BEGIN
	ival := 0; i := 0; n := 0; k2 := 0;
    REPEAT
		IF n < LEN(d) THEN d[n] := ORD(ch) - 30H; INC(n)
		ELSE Mark('Too many digits'); n := 0
		END ;
		ReadCh
    UNTIL (ch < '0') OR (ch > '9') & (ch < 'A') OR (ch > 'F');
    IF (ch = 'H') OR (ch = 'R') OR (ch = 'X') THEN  (* hex *)
		REPEAT h := d[i];
			IF h >= 10 THEN h := h-7 END ;
			k2 := k2*10H + h; INC(i) (* no overflow check *)
		UNTIL i = n;
		ival := k2;
		IF ch = 'X' THEN sym := string;
			IF ival >= 10000H THEN Mark('Illegal value'); ival := 0 END ;
			IF ival = 0 THEN str[0] := 0X; slen := 1
			ELSE str[0] := CHR(ival); str[1] := 0X; slen := 2
			END
		ELSIF ch = 'R' THEN sym := real; rval := SYSTEM.VAL(REAL, ival)
		ELSE sym := int
		END ;
		ReadCh
    ELSIF ch = '.' THEN ReadCh;
		IF ch = '.' THEN (* double dot *) ch := 7FX; escUpto := TRUE;
			(* decimal integer *)
			REPEAT
				IF d[i] < 10 THEN
					IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2 * 10 + d[i]
					ELSE Mark('Too large'); k2 := 0
					END
				ELSE Mark('Bad integer')
				END ;
				INC(i)
			UNTIL i = n;
			sym := int; ival := k2
		ELSE (* real number *)
			Real(sym, d, n)
		END
    ELSE (* decimal integer *)
		REPEAT
			IF d[i] < 10 THEN
				IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2*10 + d[i]
				ELSE Mark ('Too large'); k2 := 0
				END
			ELSE Mark('Bad integer')
			END ;
			INC(i)
		UNTIL i = n;
		sym := int; ival := k2
    END
END Number;

PROCEDURE Get*(VAR sym: INTEGER);
BEGIN
	REPEAT
		WHILE ~eof & (ch <= ' ') DO ReadCh END ;
		IF ch < '0' THEN
			IF ch = 22X (* " *) THEN sym := string; String(22X)
			ELSIF ch = '#' THEN ReadCh; sym := neq
			ELSIF ch = '&' THEN ReadCh; sym := and
			ELSIF ch = 27X (* ' *)THEN sym := string; String(27X)
			ELSIF ch = '(' THEN ReadCh;
				IF ch # '*' THEN sym := lparen ELSE Comment(0); sym := null END
			ELSIF ch = ')' THEN ReadCh; sym := rparen
			ELSIF ch = '*' THEN ReadCh; sym := times
			ELSIF ch = '+' THEN ReadCh; sym := plus
			ELSIF ch = ',' THEN ReadCh; sym := comma
			ELSIF ch = '-' THEN ReadCh; sym := minus
			ELSIF ch = '.' THEN ReadCh;
				IF ch # '.' THEN sym := period ELSE ReadCh; sym := upto END
			ELSIF ch = '/' THEN ReadCh; sym := rdiv
			ELSE (* ! % *) ReadCh; sym := null
			END
		ELSIF ch <= '9' THEN Number(sym)
		ELSIF ch < 'A' THEN
			IF ch = ':' THEN ReadCh; sym := colon
			ELSIF ch = ';' THEN ReadCh; sym := semicolon
			ELSIF ch = '<' THEN ReadCh;
				IF ch # '=' THEN sym := lss ELSE ReadCh; sym := leq END
			ELSIF ch = '=' THEN sym := eql
			ELSIF ch = '>' THEN ReadCh;
				IF ch # '=' THEN sym := gtr ELSE ReadCh; sym := geq END
			ELSE (* ? @ *) ReadCh; sym := null
			END
		ELSIF ch <= 'Z' THEN KeyWordOrIdent(sym)
		ELSIF ch < 'a' THEN
			IF ch = ']' THEN sym := rbrak
			ELSIF ch = '[' THEN sym := lbrak
			ELSIF ch = '^' THEN sym := arrow
			ELSIF ch = '_' THEN Identifier 
			ELSE (* ` *) sym := null
			END ;
			ReadCh
		ELSIF ch <= 'z' THEN Identifier ELSE
			IF ch = '{' THEN sym := rbrace
			ELSIF ch = '|' THEN sym := bar
			ELSIF ch = 7FX (* escape *) THEN
				IF escUpto THEN sym := upto; ReadCh; escUpto := FALSE
				ELSE sym := null
				END
			ELSE (* others *) sym := null 
			END ;
			ReadCh
		END 
	UNTIL (sym # null) OR eof
END Get;

END Scn.
