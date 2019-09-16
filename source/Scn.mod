MODULE Scn;

IMPORT Files;

CONST
	StrLen* = 255; IdLen* = 63;

TYPE
	Str* = ARRAY StrLen+1 OF CHAR;
	Ident* = ARRAY IdLen+1 OF CHAR;

VAR
	ch: CHAR; eof, escUpto: BOOLEAN;
	str*: Str; id*: Ident;
	
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
	str[i] := 0X; ReadCh
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
		ELSIF ch <= '9' THEN (* number *)
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
				IF escUpto THEN sym := upto ELSE sym := null END
			ELSE (* others *) sym := null 
			END ;
			ReadCh
		END 
	UNTIL (sym # null) OR eof
END Get;

END Scn.
