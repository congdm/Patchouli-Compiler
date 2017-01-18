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

MODULE Patchouli Scanner; (* Modified from ORS module in Project Oberon *)
IMPORT
	SYSTEM, Rtl := [Oberon07 Rtl], Files := [Oberon07 Files];
  
CONST
	MaxIdLen* = 63; MaxStrLen* = 255;
	MaxInt = 9223372036854775807; MinInt = -MaxInt - 1;
    NKW = 35;  (* Number of keywords *)
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
    array* = 60; record* = 61; pointer* = 62;
	const* = 70; type* = 71; var* = 72; procedure* = 73; begin* = 74; 
	import* = 76; module* = 77;
	
	call* = 100; par* = 101; sproc* = 102; bitset* = 104;
	
	begSf* = 110;
	sfABS* = 110; sfODD* = 111; sfLEN* = 112;
	sfLSL* = 113; sfASR* = 114; sfROR* = 115;
	sfFLOOR* = 116; sfFLT* = 117; sfORD* = 118; sfCHR* = 119;
	
	sfADR* = 120; sfBIT* = 121; sfVAL* = 122; sfSIZE* = 123;
	sfNtCurrentTeb* = 124;
	endSf* = 129;
	
	spINC* = 130; spDEC* = 131; spINCL* = 132; spEXCL* = 133;
	spNEW* = 134; spASSERT* = 135; spPACK* = 136; spUNPK* = 137;
	spGET* = 138; spPUT* = 139; spCOPY* = 140;
	
	spLoadLibraryW* = 151; spGetProcAddress* = 152; spINT3* = 154;
	
TYPE
	IdStr* = ARRAY MaxIdLen+1 OF CHAR;
	Str* = ARRAY MaxStrLen+1 OF CHAR;
	
	SetCompilerFlagProc* = PROCEDURE(pragma: ARRAY OF CHAR);
	NotifyErrorProc* = PROCEDURE(pos: INTEGER; msg: ARRAY OF CHAR);

VAR
	ival*, slen*: INTEGER;
    rval*: REAL;
    id*: IdStr;
    str*: Str; ansiStr*: BOOLEAN;
    errcnt*: INTEGER;

    ch: CHAR; eof: BOOLEAN;
    errpos: INTEGER;
    k: INTEGER;
    KWX: ARRAY 11 OF INTEGER;
    keyTab: ARRAY NKW OF RECORD sym: INTEGER; id: IdStr END;
	
	bufPos, lastPos, filePos, bufSize: INTEGER;
	buffer: ARRAY 100000H OF BYTE;
	
	SetCompilerFlag: SetCompilerFlagProc;
	NotifyError: NotifyErrorProc;
	
PROCEDURE Pos*() : INTEGER;
	RETURN filePos
END Pos;

PROCEDURE Mark*(msg: ARRAY OF CHAR);
	VAR p: INTEGER;
BEGIN
	p := lastPos;
	IF (p > errpos) & (errcnt < 25) THEN
		IF NotifyError # NIL THEN NotifyError(p, msg) END;
		INC(errcnt)
	END;
	errpos := p + 4
END Mark;

PROCEDURE Read;
	VAR n: INTEGER;
BEGIN
	IF bufPos < bufSize THEN
		ch := CHR(buffer[bufPos] MOD 256); INC(bufPos); INC(filePos)
	ELSE eof := TRUE; ch := 0X
	END
END Read;

PROCEDURE Identifier(VAR sym: INTEGER);
	VAR i, k2: INTEGER;
BEGIN i := 0;
	REPEAT
		IF i <= MaxIdLen THEN id[i] := ch; INC(i) END; Read
	UNTIL (ch < '0') OR (ch > '9') & (ch < 'A')
		OR (ch # '_') & (ch > 'Z') & (ch < 'a') OR (ch > 'z');
	IF i <= MaxIdLen THEN id[i] := 0X
	ELSE Mark('identifier too long'); id[MaxIdLen] := 0X
	END;
	IF i < 11 THEN k2 := KWX[i-1];  (* search for keyword *)
		WHILE (id # keyTab[k2].id) & (k2 < KWX[i]) DO INC(k2) END;
		IF k2 < KWX[i] THEN sym := keyTab[k2].sym ELSE sym := ident END
	ELSE sym := ident
	END
END Identifier;

PROCEDURE String(quoteCh: CHAR);
	VAR i: INTEGER; utf8str: ARRAY MaxStrLen+1 OF BYTE;
BEGIN
	i := 0; Read;
	WHILE ~eof & (ch # quoteCh) DO
		IF i < MaxStrLen THEN utf8str[i] := ORD(ch); INC(i)
		ELSE Mark('String too long')
		END;
		Read
	END;
	Read; utf8str[i] := 0;
	slen := Rtl.Utf8ToUnicode(utf8str, str)
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
		IF i < MaxStrLen THEN str[i] := CHR(m*10H + n); INC(i)
		ELSE Mark('String too long')
		END;
		Read
    END;
    Read; slen := i  (* no 0X appended! *)
END HexString;

PROCEDURE Ten(e: INTEGER): REAL;
	VAR x, t: REAL;
BEGIN
	x := 1.0;
	t := 10.0;
    WHILE e > 0 DO
		IF ODD(e) THEN x := t * x END ;
		t := t * t; e := e DIV 2
    END ;
    RETURN x
END Ten;

PROCEDURE Number(VAR sym: INTEGER);
    CONST max = MaxInt;
	VAR i, k2, e, n, s, h: INTEGER; x: REAL;
		d: ARRAY 21 OF INTEGER;
		negE: BOOLEAN;
BEGIN
	ival := 0; i := 0; n := 0; k2 := 0;
    REPEAT
		IF n < LEN(d) THEN d[n] := ORD(ch) - 30H; INC(n)
		ELSE Mark('Too many digits'); n := 0
		END;
		Read
    UNTIL (ch < '0') OR (ch > '9') & (ch < 'A') OR (ch > 'F');
    IF (ch = 'H') OR (ch = 'R') OR (ch = 'X') THEN  (* hex *)
		REPEAT h := d[i];
			IF h >= 10 THEN h := h-7 END;
			k2 := k2*10H + h; INC(i) (* no overflow check *)
		UNTIL i = n;
		IF ch = 'X' THEN sym := string;
			IF k2 < 10000H THEN ival := k2
			ELSE Mark('Illegal value'); ival := 0
			END;
			IF k2 = 0 THEN str[0] := 0X; slen := 1
			ELSE str[0] := CHR(k2); str[1] := 0X; slen := 2
			END
		ELSIF ch = 'R' THEN sym := real; rval := SYSTEM.VAL(REAL, k2)
		ELSE sym := int; ival := k2
		END;
		Read
    ELSIF ch = '.' THEN Read;
		IF ch = '.' THEN (* double dot *) ch := 7FX;  (* decimal integer *)
			REPEAT
				IF d[i] < 10 THEN
					IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2 * 10 + d[i]
					ELSE Mark('Too large'); k2 := 0
					END
				ELSE Mark('Bad integer')
				END;
				INC(i)
			UNTIL i = n;
			sym := int; ival := k2
		ELSE (* real number *) x := 0.0; e := 0;
			REPEAT (* integer part *)
				x := x * 10.0 + FLT(d[i]); INC(i)
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
			sym := real; rval := x; ival := SYSTEM.VAL(INTEGER, x);
		END
    ELSE (* decimal integer *)
		REPEAT
			IF d[i] < 10 THEN
				IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2*10 + d[i]
				ELSE Mark ('Too large'); k2 := 0
				END
			ELSE Mark('Bad integer')
			END;
			INC(i)
		UNTIL i = n;
		sym := int; ival := k2
    END
END Number;

PROCEDURE SkipComment(lev: INTEGER);
	VAR exit: BOOLEAN;
	
	PROCEDURE SetPragma;
		VAR pragma: Str; i: INTEGER;
	BEGIN Read; i := 0;
		WHILE (i < LEN(pragma) - 1) & (ch # '*') & ~eof DO
			pragma[i] := ch; Read; INC(i)
		END;
		pragma[i] := 0X;
		IF ch = '*' THEN SetCompilerFlag(pragma)
		ELSE Mark('Incorrect compiler directive')
		END
	END SetPragma;
	
BEGIN
	IF (ch = '$') & (lev = 0) THEN SetPragma END;
	exit := FALSE;
	WHILE ~eof & ~exit DO
		IF ch = '(' THEN Read;
			IF ch = '*' THEN Read; SkipComment(lev + 1) END
		ELSIF ch = '*' THEN Read;
			IF ch = ')' THEN Read; exit := TRUE END
		ELSE Read
		END
	END
END SkipComment;

PROCEDURE Get*(VAR sym: INTEGER);
BEGIN
    REPEAT
		WHILE ~eof & (ch <= ' ') DO Read END; lastPos := filePos-1;
		IF ch < 'A' THEN
			IF ch < '0' THEN
				IF (ch = 22X) OR (ch = 27X) THEN String(ch); sym := string
				ELSIF ch = '#' THEN Read; sym := neq
				ELSIF ch = '$' THEN HexString; sym := string
				ELSIF ch = '&' THEN Read; sym := and
				ELSIF ch = '(' THEN Read; 
					IF ch = '*' THEN sym := null; Read; SkipComment(0)
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
		ELSIF ch = '_' THEN Identifier(sym)
		ELSIF ch < 'a' THEN
			IF ch = '[' THEN sym := lbrak
			ELSIF ch = ']' THEN  sym := rbrak
			ELSIF ch = '^' THEN sym := arrow
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

PROCEDURE Init*(f: Files.File; pos: INTEGER);
	VAR r: Files.Rider;
BEGIN
	errpos := pos; errcnt := 0; eof := FALSE;
	Files.Set(r, f, pos); filePos := pos; bufPos := 0;
	Files.ReadBytes(r, buffer, LEN(buffer));
	bufSize := LEN(buffer)-r.res; Read
END Init;

PROCEDURE InstallSetCompilerFlag*(proc: SetCompilerFlagProc);
BEGIN SetCompilerFlag := proc
END InstallSetCompilerFlag;

PROCEDURE InstallNotifyError*(proc: NotifyErrorProc);
BEGIN NotifyError := proc
END InstallNotifyError;

PROCEDURE EnterKW(sym: INTEGER; name: IdStr);
BEGIN keyTab[k].id := name; keyTab[k].sym := sym; INC(k)
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
	KWX[7] := k;
	KWX[8] := k;
	EnterKW(procedure, 'PROCEDURE');
	KWX[9] := k;
	EnterKW(null, 'EXTENSIBLE');
	KWX[10] := k
END Patchouli Scanner.