MODULE Scn;

IMPORT Files;

VAR
	ch: CHAR; eof, escUpto: BOOLEAN;
	
PROCEDURE Init*(fname: ARRAY OF CHAR; pos: INTEGER);
END Init;

PROCEDURE ReadCh;
END ReadCh;

PROCEDURE Get*(VAR sym: INTEGER);
BEGIN
	REPEAT
		WHILE ~eof & (ch <= ' ') DO ReadCh END ;
		IF ch < '0' THEN
			IF ch = '"' THEN (* string *)
			ELSIF ch = '#' THEN ReadCh; sym := neq
			ELSIF ch = '&' THEN ReadCh; sym := and
			ELSIF ch = "'" THEN (* string *)
			ELSIF ch = '(' THEN ReadCh;
				IF ch # '*' THEN sym := lparen ELSE (* comment *) END
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
