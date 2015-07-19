(*
	Project Oberon, Revised Edition 2013

	Book copyright (C)2013 Niklaus Wirth and Juerg Gutknecht;
	software copyright (C)2013 Niklaus Wirth (NW), Juerg Gutknecht (JG), Paul
	Reed (PR/PDR).

	Permission to use, copy, modify, and/or distribute this software and its
	accompanying documentation (the "Software") for any purpose with or
	without fee is hereby granted, provided that the above copyright notice
	and this permission notice appear in all copies.

	THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHORS DISCLAIM ALL WARRANTIES
	WITH REGARD TO THE SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
	MERCHANTABILITY, FITNESS AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
	AUTHORS BE LIABLE FOR ANY CLAIM, SPECIAL, DIRECT, INDIRECT, OR
	CONSEQUENTIAL DAMAGES OR ANY DAMAGES OR LIABILITY WHATSOEVER, WHETHER IN
	AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
	CONNECTION WITH THE DEALINGS IN OR USE OR PERFORMANCE OF THE SOFTWARE.
*)

MODULE Scanner; (* Modified from ORS module in Project Oberon *)

IMPORT
	SYSTEM, Sys, Console, Base;
  
CONST
	MaxIdLen = Base.MaxIdentLen;
    NKW = 34;  (* Number of keywords *)
    maxExp = 38; stringBufSize = 256;
  
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
    array* = 60; record* = 61; pointer* = 62; address* = 63;
	const* = 70; type* = 71; var* = 72; procedure* = 73; begin* = 74;
	import* = 75; module* = 76;

VAR
	ival*, slen*: INTEGER;
    rval*: REAL;
    id*: Base.IdentStr;
    str*: Base.String;
    errcnt*: INTEGER;

    ch: CHAR; eof: BOOLEAN;
    errpos: INTEGER;
    srcfile : Sys.FileHandle;
    k: INTEGER;
    KWX: ARRAY 10 OF INTEGER;
    keyTab: ARRAY NKW OF
		RECORD sym: INTEGER; id: Base.IdentStr
		END;
  
PROCEDURE Pos*() : INTEGER;
BEGIN
	RETURN Sys.FilePos(srcfile)
END Pos;

PROCEDURE Mark* (msg: ARRAY OF CHAR);
	VAR p: INTEGER;
BEGIN
	p := Pos();
	IF (p > errpos) & (errcnt < 25) THEN
		Console.WriteString ('file pos '); Console.WriteInt (p);
		Console.WriteString (': '); Console.WriteString (msg); Console.WriteLn;
		INC (errcnt);
	END;
	errpos := p + 4
END Mark;

PROCEDURE Read;
	VAR n : INTEGER;
BEGIN
	n := -1; Sys.Read_byte(srcfile, n);
	IF n = -1 THEN eof := TRUE; ch := 0X ELSE ch := CHR(n)
	END
END Read;

PROCEDURE Identifier (VAR sym: INTEGER);
	VAR i, k2: INTEGER;
BEGIN
	i := 0;
	REPEAT
		IF i < Base.MaxIdentLen THEN id[i] := ch; INC(i) END; Read
	UNTIL (ch < '0') OR (ch > '9') & (ch < 'A')
		OR (ch # '_') & (ch > 'Z') & (ch < 'a') OR (ch > 'z');
	id[i] := 0X; 
	IF i < 10 THEN k2 := KWX[i-1];  (* search for keyword *)
		WHILE (id # keyTab[k2].id) & (k2 < KWX[i]) DO INC(k2)
		END;
		IF k2 < KWX[i] THEN sym := keyTab[k2].sym ELSE sym := ident
		END
	ELSE sym := ident
	END
END Identifier;

PROCEDURE String (quoteCh: CHAR);
	VAR i: INTEGER;
BEGIN
	i := 0; Read;
	WHILE ~eof & (ch # quoteCh) DO
		IF i < Base.MaxStrLen THEN str[i] := ch; INC(i)
		ELSE Mark('String too long')
		END;
		Read
	END;
	str[i] := 0X; Read; slen := i + 1
END String;

PROCEDURE HexString;
	VAR i, m, n: INTEGER;
BEGIN
	i := 0; Read;
	WHILE eof & (ch # '$') DO
		WHILE (ch = ' ') OR (ch = 9X) OR (ch = 0DX) DO Read
		END;  
		IF ('0' <= ch) & (ch <= '9') THEN m := ORD(ch) - 30H
		ELSIF ('A' <= ch) & (ch <= 'F') THEN m := ORD(ch) - 37H
		ELSE m := 0; Mark('Hex digit expected')
		END;
		Read;
		IF ('0' <= ch) & (ch <= '9') THEN n := ORD(ch) - 30H
		ELSIF ('A' <= ch) & (ch <= 'F') THEN n := ORD(ch) - 37H
		ELSE n := 0; Mark('Hex digit expected')
		END;
		IF i < Base.MaxStrLen THEN str[i] := CHR(m*10H + n); INC(i)
		ELSE Mark('String too long')
		END;
		Read
    END;
    Read; slen := i  (* no 0X appended! *)
END HexString;

PROCEDURE Ten(e: INTEGER): REAL;
	VAR x, t: REAL;
BEGIN
	x := 1.0; t := 10.0;
    WHILE e > 0 DO
		IF ODD(e) THEN x := t * x END ;
		t := t * t; e := e DIV 2
    END ;
    RETURN x
END Ten;

PROCEDURE Number(VAR sym: INTEGER);
    CONST max = Base.MaxInt;
	VAR i, k2, e, n, s, h: INTEGER; x: REAL;
		d: ARRAY 16 OF INTEGER;
		negE: BOOLEAN;
BEGIN
	ival := 0; i := 0; n := 0; k2 := 0;
    REPEAT
		IF n < 16 THEN d[n] := ORD(ch)-30H; INC(n)
		ELSE Mark('Too many digits'); n := 0
		END;
		Read
    UNTIL (ch < '0') OR (ch > '9') & (ch < 'A') OR (ch > 'F');
    IF (ch = 'H') OR (ch = 'R') OR (ch = 'X') THEN  (* hex *)
		REPEAT h := d[i];
			IF h >= 10 THEN h := h-7 END;
			k2 := k2*10H + h; INC(i) (* no overflow check *)
		UNTIL i = n;
		IF ch = 'X' THEN sym := char;
			IF k2 < 100H THEN ival := k2
			ELSE Mark('Illegal value'); ival := 0
			END
		ELSIF ch = 'R' THEN sym := real; rval := SYSTEM.VAL(REAL, k2)
		ELSE sym := int; ival := k2
		END;
		Read
    ELSIF ch = '.' THEN Read;
		IF ch = '.' THEN (* double dot *) ch := 7FX;  (* decimal integer *)
			REPEAT
				IF d[i] < 10 THEN
					IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2 *10 + d[i]
					ELSE Mark('Too large'); k2 := 0
					END
				ELSE Mark('Bad integer')
				END;
				INC(i)
			UNTIL i = n;
			sym := int; ival := k2
		ELSE (* real number *) x := 0.0; e := 0;
			REPEAT (* integer part *) x := x * 10.0 + FLT(d[i]); INC(i)
			UNTIL i = n;
			WHILE (ch >= '0') & (ch <= '9') DO (* fraction *)
				x := x * 10.0 + FLT(ORD(ch) - 30H); DEC(e); Read
			END;
			IF (ch = 'E') OR (ch = 'D') THEN (* scale factor *)
				Read; s := 0; 
				IF ch = '-' THEN negE := TRUE; Read
				ELSE negE := FALSE; IF ch = '+' THEN Read END
				END;
				IF (ch >= '0') & (ch <= '9') THEN
					REPEAT s := s*10 + ORD(ch)-30H; Read
					UNTIL (ch < '0') OR (ch > '9');
					IF negE THEN e := e-s ELSE e := e+s
					END
				ELSE Mark('Digit?')
				END
			END;
			IF e < 0 THEN
				IF e >= -maxExp THEN x := x / Ten(-e) ELSE x := 0.0 END
			ELSIF e > 0 THEN
				IF e <= maxExp THEN x := Ten(e) * x
				ELSE x := 0.0; Mark('Too large')
				END
			END;
			sym := real; rval := x
		END
    ELSE (* decimal integer *)
		REPEAT
			IF d[i] < 10 THEN
				IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2*10 + d[i]
				ELSE Mark('Too large'); k2 := 0
				END
			ELSE Mark('Bad integer')
			END;
			INC(i)
		UNTIL i = n;
		sym := int; ival := k2
    END
END Number;

PROCEDURE SkipComment (lev: INTEGER);
	VAR exit: BOOLEAN;
	
	PROCEDURE SetPragma;
		VAR pragma: Base.String; i: INTEGER;
	BEGIN
		Read; i := 0;
		WHILE (i < LEN(pragma) - 1) & (ch # '*') & ~eof DO
			pragma[i] := ch; Read; INC (i)
		END;
		pragma[i] := 0X;
		IF ch = '*' THEN Base.SetCompilerFlag (pragma)
		ELSE Mark ('Wrong compiler directive')
		END
	END SetPragma;
	
BEGIN
	IF (ch = '$') & (lev = 0) THEN SetPragma END;
	exit := FALSE;
	WHILE ~eof & ~exit DO
		IF ch = '(' THEN Read;
			IF ch = '*' THEN Read; SkipComment (lev + 1) END
		ELSIF ch = '*' THEN Read;
			IF ch = ')' THEN Read; exit := TRUE END
		ELSE Read
		END
	END
END SkipComment;

PROCEDURE Get*(VAR sym: INTEGER);
BEGIN
    REPEAT
		WHILE ~eof & (ch <= ' ') DO Read END;
		IF ch < 'A' THEN
			IF ch < '0' THEN
				IF (ch = 22X) OR (ch = 27X) THEN String(ch); sym := string
				ELSIF ch = '#' THEN Read; sym := neq
				ELSIF ch = '$' THEN HexString; sym := string
				ELSIF ch = '&' THEN Read; sym := and
				ELSIF ch = '(' THEN Read; 
					IF ch = '*' THEN sym := null; Read; SkipComment (0)
					ELSE sym := lparen
					END
				ELSIF ch = ')' THEN Read; sym := rparen
				ELSIF ch = '*' THEN Read; sym := times
				ELSIF ch = '+' THEN Read; sym := plus
				ELSIF ch = ',' THEN Read; sym := comma
				ELSIF ch = '-' THEN Read; sym := minus
				ELSIF ch = '.' THEN Read;
					IF ch = '.' THEN Read; sym := upto
					ELSE sym := period
					END
				ELSIF ch = '/' THEN Read; sym := rdiv
				ELSE Read; (* ! % *) sym := null
				END
			ELSIF ch < ':' THEN Number(sym)
			ELSIF ch = ':' THEN Read;
				IF ch = '=' THEN Read; sym := becomes ELSE sym := colon END 
			ELSIF ch = ';' THEN Read; sym := semicolon
			ELSIF ch = '<' THEN  Read;
				IF ch = '=' THEN Read; sym := leq ELSE sym := lss END
			ELSIF ch = '=' THEN Read; sym := eql
			ELSIF ch = '>' THEN Read;
				IF ch = '=' THEN Read; sym := geq ELSE sym := gtr END
			ELSE (* ? @ *) Read; sym := null
			END
		ELSIF ch < '[' THEN Identifier(sym)
		ELSIF ch < 'a' THEN
			IF ch = '[' THEN sym := lbrak
			ELSIF ch = ']' THEN  sym := rbrak
			ELSIF ch = '^' THEN sym := arrow
			ELSIF ch = '_' THEN Identifier(sym)
			ELSE (* ` *) sym := null
			END;
			Read
		ELSIF ch < '{' THEN Identifier(sym)
		ELSE
			IF ch = '{' THEN sym := lbrace
			ELSIF ch = '}' THEN sym := rbrace
			ELSIF ch = '|' THEN sym := bar
			ELSIF ch = '~' THEN  sym := not
			ELSIF ch = 7FX THEN  sym := upto
			ELSE sym := null
			END;
			Read
		END
	UNTIL (sym # null) OR eof
END Get;

PROCEDURE Init* (VAR file: Sys.FileHandle; pos: INTEGER);
BEGIN
	srcfile := file; errpos := pos; errcnt := 0; Sys.Seek (file, pos); Read
END Init;

PROCEDURE EnterKW (sym: INTEGER; name: ARRAY OF CHAR);
BEGIN Base.StrCopy(name, keyTab[k].id); keyTab[k].sym := sym; INC(k)
END EnterKW;

BEGIN
	k := 0; KWX[0] := 0; KWX[1] := 0;
	EnterKW(if, 'IF');
	EnterKW(do, 'DO');
	EnterKW(of, 'OF');
	EnterKW(or, 'OR');
	EnterKW(to, 'TO');
	EnterKW(in, 'IN');
	EnterKW(is, 'IS');
	EnterKW(by, 'BY');
	KWX[2] := k;
	EnterKW(end, 'END');
	EnterKW(nil, 'NIL');
	EnterKW(var, 'VAR');
	EnterKW(div, 'DIV');
	EnterKW(mod, 'MOD');
	EnterKW(for, 'FOR');
	KWX[3] := k;
	EnterKW(else, 'ELSE');
	EnterKW(then, 'THEN');
	EnterKW(true, 'TRUE');
	EnterKW(type, 'TYPE');
	EnterKW(case, 'CASE');
	KWX[4] := k;
	EnterKW(elsif, 'ELSIF');
	EnterKW(false, 'FALSE');
	EnterKW(array, 'ARRAY');
	EnterKW(begin, 'BEGIN');
	EnterKW(const, 'CONST');
	EnterKW(until, 'UNTIL');
	EnterKW(while, 'WHILE');
	KWX[5] := k;
	EnterKW(record, 'RECORD');
	EnterKW(repeat, 'REPEAT');
	EnterKW(return, 'RETURN');
	EnterKW(import, 'IMPORT');
	EnterKW(module, 'MODULE');
	KWX[6] := k;
	EnterKW(pointer, 'POINTER');
	EnterKW(address, 'ADDRESS');
	KWX[7] := k;
	KWX[8] := k;
	EnterKW(procedure, 'PROCEDURE');
	KWX[9] := k
END Scanner.