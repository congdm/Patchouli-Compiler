MODULE Scn;

IMPORT Files;

CONST
	StrLen* = 255;

TYPE
	Str = ARRAY StrLen+1 OF CHAR;

VAR
	ch: CHAR; eof, escUpto: BOOLEAN;
	str*: Str;
	
PROCEDURE Init*(fname: ARRAY OF CHAR; pos: INTEGER);
END Init;

PROCEDURE ReadCh;
END ReadCh;

PROCEDURE String(quote: CHAR);
	VAR i: INTEGER;
BEGIN
	ReadCh; i := 0;
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

PROCEDURE Get*(VAR sym: INTEGER);
BEGIN
	REPEAT
		WHILE ~eof & (ch <= ' ') DO ReadCh END ;
		IF ch < '0' THEN
			IF ch = '"' THEN sym := string; String('"')
			ELSIF ch = '#' THEN ReadCh; sym := neq
			ELSIF ch = '&' THEN ReadCh; sym := and
			ELSIF ch = "'" THEN sym := string; String("'")
			ELSIF ch = '(' THEN ReadCh;
				IF ch # '*' THEN sym := lparen ELSE Comment(0) END
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
		ELSIF ch <= 'Z' THEN (* ident or keyword *)
		ELSIF ch < 'a' THEN
			IF ch = ']' THEN sym := rbrak
			ELSIF ch = '[' THEN sym := lbrak
			ELSIF ch = '^' THEN sym := arrow
			ELSIF ch = '_' THEN (* ident C style *)
			ELSE (* ` *) sym := null
			END ;
			ReadCh
		ELSIF ch <= 'z' THEN (* ident *) ELSE
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
