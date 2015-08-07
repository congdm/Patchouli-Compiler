MODULE Scanner;

IMPORT
	SYSTEM, Sys, Base;
	
CONST
	failed = FALSE; success = TRUE;
	
	MAX_INT = 9223372036854775807; (* 64-bits integer *)
	MAX_MANTISSA = 1152921504606846975; (* 60-bits mantissa *)
	SINGLE_MANTISSA = 16777215; (* 24-bits mantissa *)
	DOUBLE_MANTISSA = 9007199254740991; (* 53-bits mantissa *)
	DOUBLE_MAX_EXPONENT = 1023;
	DOUBLE_MIN_EXPONENT = -1022;
	SINGLE_MAX_EXPONENT = 127;
	SINGLE_MIN_EXPONENT = -126;
	MAX_SCALE = 308;
	MIN_SCALE = -308;
	SINGLE_POSITIVE_INFINITY = 2139095040;
	
	TWO_POWER_30 = 1073741824;
	TWO_POWER_52 = 4503599627370496;
	TWO_POWER_23 = 8388608;
	
	null* = 0;
	times* = 1; slash* = 2; div* = 3; mod* = 4; and* = 5;
	plus* = 6; minus* = 7; or* = 8;
	
	equal* = 9; not_equal* = 10; less* = 11; greater_equal* = 12;
	greater* = 13; less_equal* = 14; in* = 15; is* = 16;
	
	arrow* = 17; period* = 18;
	
	lparen* = 29; lbrak* = 30; lbrace* = 31; not* = 32;
	number* = 34; nil* = 35; true* = 36; false* = 37; string* = 38;
	ident* = 39;
	
	if* = 55; while* = 56; repeat* = 57; for* = 58; case* = 59;
	
	comma* = 60; colon* = 61; becomes* = 62; upto* = 63;
	rparen* = 64; rbrak* = 65; rbrace* = 66;
	then* = 67; of* = 68; do* = 69; to* = 70; by* = 71;
	
	semicolon* = 72; end* = 73; bar* = 74; else* = 75;
	elsif* = 76; until* = 77; return* = 78;
	
	array* = 79; record* = 80; pointer* = 81; address* = 82;
	const* = 83; type* = 84; var* = 85; procedure* = 86;
	begin* = 87; import* = 88; module* = 89;
	
	eof* = 90;
	
	errLargeNumber = 'This number is too large to handle (compiler limit)';
	errLargeChar = 'Value outside character range';

VAR
	val* : LONGINT;
	typeOfVal* : Base.Type;
	haveError* : BOOLEAN;
	id* : Base.String;
	str* : Base.LongString;
	
	numArray : ARRAY 19 OF INTEGER;
	numLen : INTEGER; overflow : BOOLEAN;
	
	srcfile : Sys.FileHandle;
	ch : CHAR;
	charNum*, prevErrorPos : INTEGER;
	eofFlag : BOOLEAN;

PROCEDURE Init* (VAR file : Sys.FileHandle);
BEGIN
	srcfile := file; ch := 0X; charNum := 0; prevErrorPos := -1;
	haveError := FALSE; eofFlag := FALSE
END Init;

PROCEDURE Read_char;
	VAR n : INTEGER;
BEGIN
	n := -1; Sys.Read_byte (srcfile, n);
	IF n = -1 THEN eofFlag := TRUE; ch := 0X ELSE ch := CHR(n); INC (charNum)
	END
END Read_char;

PROCEDURE Seek_back (VAR srcfile: Sys.FileHandle; num: INTEGER);
BEGIN
	Sys.Seek (srcfile, Sys.FilePos(srcfile) - num); DEC (charNum, num)
END Seek_back;
	
PROCEDURE Mark* (s : ARRAY OF CHAR);
BEGIN
	IF charNum > prevErrorPos + 10 THEN
		Sys.Console_WriteInt (charNum);
		Sys.Console_WriteString (': '); Sys.Console_WriteString (s);
		Sys.Console_WriteLn; haveError := TRUE; prevErrorPos := charNum
	END
END Mark;

PROCEDURE Get_word (VAR sym : INTEGER);
	CONST err_toolong = 'Identifier length is too long (compiler limit)';
	VAR i : INTEGER; flag : BOOLEAN;
BEGIN
	i := 0; flag := TRUE;
	WHILE (ch = '_')
		OR (ch >= 'A') & (ch <= 'Z')
		OR (ch >= 'a') & (ch <= 'z')
		OR (ch >= '0') & (ch <= '9')
	DO
		IF flag THEN
			IF i < Base.max_ident_len THEN id[i] := ch; INC (i)
			ELSE Mark (err_toolong); flag := FALSE
			END;
		END;
		Read_char
	END;
	id[i] := 0X;
	
	sym := ident;
	CASE id[0] OF
		'A':
		IF (i = 5) & (id = 'ARRAY') THEN sym := array
		ELSIF (i = 7) & (id = 'ADDRESS') THEN sym := address
		END |
		
		'B':
		IF (i = 2) & (id[1] = 'Y') THEN sym := by
		ELSIF (i = 5) & (id = 'BEGIN') THEN sym := begin
		END |
		
		'C':
		IF (i = 4) & (id = 'CASE') THEN sym := case
		ELSIF (i = 5) & (id = 'CONST') THEN sym := const
		END |
		
		'D':
		IF (i = 2) & (id[1] = 'O') THEN sym := do
		ELSIF (i = 3) & (id[1] = 'I') & (id[2] = 'V') THEN sym := div;
		END |
		
		'E':
		IF (i = 3) & (id[1] = 'N') & (id[2] = 'D') THEN sym := end
		ELSIF (i = 4) & (id = 'ELSE') THEN sym := else
		ELSIF (i = 5) & (id = 'ELSIF') THEN sym := elsif
		END |
		
		'F':
		IF (i = 3) & (id[1] = 'O') & (id[2] = 'R') THEN sym := for
		ELSIF (i = 5) & (id = 'FALSE') THEN sym := false
		END |
		
		'I':
		IF i = 2 THEN
			IF id [1] = 'F' THEN sym := if
			ELSIF id [1] = 'N' THEN sym := in
			ELSIF id [1] = 'S' THEN sym := is
			END
		ELSIF (i = 6) & (id = 'IMPORT') THEN sym := import
		END |
		
		'M':
		IF (i = 3) & (id[1] = 'O') & (id[2] = 'D') THEN sym := mod
		ELSIF (i = 6) & (id = 'MODULE') THEN sym := module
		END |
		
		'O':
		IF i = 2 THEN
			IF id[1] = 'F' THEN sym := of ELSIF id[1] = 'R' THEN sym := or
			END
		END |
		
		'P':
		IF (i = 7) & (id = 'POINTER') THEN sym := pointer
		ELSIF (i = 9) & (id = 'PROCEDURE') THEN sym := procedure
		END |
		
		'R':
		IF i = 6 THEN
			IF id = 'RECORD' THEN sym := record
			ELSIF id = 'REPEAT' THEN sym := repeat
			ELSIF id = 'RETURN' THEN sym := return
			END
		END |
		
		'T':
		IF (i = 2) & (id[1] = 'O') THEN sym := to
		ELSIF i = 4 THEN
			IF id = 'THEN' THEN sym := then
			ELSIF id = 'TRUE' THEN sym := true
			ELSIF id = 'TYPE' THEN sym := type
			END
		END |
		
		'U': IF (i = 5) & (id = 'UNTIL') THEN sym := until END |
		'V': IF (i = 3) & (id[1] = 'A') & (id[2] = 'R') THEN sym := var END |
		'W': IF (i = 5) & (id = 'WHILE') THEN sym := while END
	ELSE (* Do nothing *)
	END;
END Get_word;

PROCEDURE Hex;
	VAR x, i : INTEGER;
BEGIN val := 0; i := 0;
	IF numLen > 16 THEN Mark (errLargeNumber); numLen := 16 END;
	
	WHILE i < numLen DO x := numArray [i];
		IF x > 9 THEN x := x - (ORD ('A') - ORD ('0')) + 10 END;
		val := ASH (val, 4) + x; INC (i)
	END;
	IF ch = 'H' THEN Read_char; typeOfVal := Base.int_type
	ELSIF ch = 'X' THEN Read_char; typeOfVal := Base.char_type;
		IF (val > Base.MAX_CHAR) OR (val < 0) THEN
			Mark (errLargeChar); val := val MOD (Base.MAX_CHAR + 1)
		END
	ELSE Mark ('Hexadecimal number without suffix')
	END
END Hex;

PROCEDURE DecimalInt;
	VAR i, x : INTEGER;
BEGIN val := 0; i := 0;
	WHILE i < numLen - 1 DO val := val * 10 + numArray[i]; INC (i)
	END;
	x := numArray[i]; typeOfVal := Base.int_type;
	IF (numLen # LEN (numArray)) OR (val <= (MAX_INT - x) DIV 10) THEN
		val := val * 10 + x
	ELSE overflow := TRUE
	END;
	IF overflow THEN Mark (errLargeNumber)
	END
END DecimalInt;

PROCEDURE Number;
BEGIN numLen := 0; overflow := FALSE;
	WHILE (ch >= '0') & (ch <= '9') DO
		IF numLen < LEN (numArray) THEN
			numArray [numLen] := ORD (ch) - ORD ('0'); INC (numLen)
		ELSE overflow := TRUE
		END;
		Read_char
	END
END Number;

PROCEDURE Ten (e : LONGINT) : REAL;
	VAR x, t : REAL;
BEGIN x := 1.0; t := 10.0;
    WHILE e > 0 DO
		IF ODD(e) THEN x := t * x END;
		t := t * t; e := e DIV 2
	END;
	RETURN x
END Ten;

PROCEDURE Get_number;
	CONST maxExp = 1023; minExp = -1022;
	VAR	d, f : ARRAY 19 OF INTEGER; e, old_filepos : LONGINT;
		real : REAL; sreal : SHORTREAL; k : INTEGER; negE : BOOLEAN;
BEGIN
	Number;
	IF (ch >= 'A') & (ch <= 'F') THEN
		REPEAT
			IF numLen < LEN (numArray) THEN
				numArray [numLen] := ORD (ch) - ORD ('0'); INC (numLen)
			END;
			Read_char
		UNTIL (ch < '0') OR (ch > 'F') OR (ch > '9') & (ch < 'A'); 
		Hex
	ELSIF ch = '.' THEN DecimalInt; Read_char;
		IF ch # '.' THEN real := val; e := 0;
			WHILE (ch >= '0') & (ch <= '9') DO
				real := real * 10.0 + (ORD(ch) - ORD('0'));
				DEC (e); Read_char
			END;
			IF (ch = 'E') OR (ch = 'D') THEN Read_char;
				IF ch = '-' THEN negE := TRUE; Read_char
				ELSE negE := FALSE; IF ch = '+' THEN Read_char END
				END; Number; DecimalInt;
				IF negE THEN e := e - val ELSE e := e + val END;
			END;
			IF e < 0 THEN
				IF e >= minExp THEN real := real / Ten(-e) ELSE real := 0.0 END
			ELSIF e > 0 THEN
				IF e <= maxExp THEN real := Ten(e) * real
				ELSE real := 0.0; Mark (errLargeNumber)
				END
			END; sreal := SHORT (real); SYSTEM.GET (SYSTEM.ADR (sreal), k);
			typeOfVal := Base.real_type; val := k
		ELSE Seek_back (srcfile, 1)
		END
	ELSIF (ch = 'H') OR (ch = 'X') THEN Hex
	ELSE DecimalInt
	END
END Get_number;
	
PROCEDURE Get_string;
	CONST err_toolong = 'String length is too long (compiler limit)';
	VAR i : INTEGER; flag : BOOLEAN; end_quote_ch : CHAR;
BEGIN
	i := 0; flag := TRUE; end_quote_ch := ch; Read_char;
	WHILE (ch # 0X) & (ch # end_quote_ch) DO
		IF i < Base.max_str_len THEN str[i] := ch; INC (i)
		ELSIF flag THEN Mark (err_toolong); flag := FALSE
		END;
		Read_char
	END;
	str[i] := 0X; Read_char
END Get_string;

PROCEDURE Skip_comment (lev : INTEGER);
	VAR exit : BOOLEAN;
	
	PROCEDURE Set_pragma (VAR ch : CHAR);
		VAR pragma : Base.LongString; i : INTEGER;
	BEGIN
		Read_char; i := 0;
		WHILE (i < LEN(pragma) - 1) & (ch # '*') & ~ eofFlag DO
			pragma[i] := ch; Read_char; i := i + 1
		END;
		pragma[i] := 0X;
		IF ch = '*' THEN Base.Set_compiler_flag (pragma)
		ELSE Mark ('Wrong compiler directive')
		END
	END Set_pragma;
	
BEGIN
	ASSERT (lev >= 0);
	IF (ch = '$') & (lev = 0) THEN Set_pragma (ch) END;
	exit := FALSE;
	WHILE ~ eofFlag & ~ exit DO
		IF ch = '(' THEN Read_char;
			IF ch = '*' THEN Read_char; Skip_comment (lev + 1) END
		ELSIF ch = '*' THEN Read_char;
			IF ch = ')' THEN Read_char; exit := TRUE END
		ELSE Read_char
		END
	END
END Skip_comment;

PROCEDURE Get* (VAR sym : INTEGER);
BEGIN
	WHILE ~ eofFlag & (ch <= ' ') DO Read_char END;
	IF ~eofFlag THEN
		CASE ch OF
			'_', 'A'..'Z', 'a'..'z': Get_word (sym) |
			
			'0'..'9': Get_number;
				IF typeOfVal # Base.char_type THEN sym := number
				ELSE sym := string; str[0] := CHR(val); str[1] := 0X
				END
			|
			
			"'", '"': sym := string; Get_string |
			'*': sym := times; Read_char |
			'/': sym := slash; Read_char |
			'+': sym := plus; Read_char |
			'-': sym := minus; Read_char |
			'=': sym := equal; Read_char |
			'#': sym := not_equal; Read_char |
			'&': sym := and; Read_char |
			'~': sym := not; Read_char |
			
			'<':
			Read_char;
			IF ch = '=' THEN sym := less_equal; Read_char
			ELSE sym := less
			END |
				
			'>':
			Read_char;
			IF ch = '=' THEN sym := greater_equal; Read_char
			ELSE sym := greater
			END |
				
			'.':
			Read_char;
			IF ch = '.' THEN sym := upto; Read_char
			ELSE sym := period
			END |
			
			'^': sym := arrow; Read_char |
			',': sym := comma; Read_char |
			
			':':
			Read_char;
			IF ch = '=' THEN sym := becomes; Read_char
			ELSE sym := colon
			END |
			
			')': sym := rparen; Read_char |
			']': sym := rbrak; Read_char |
			'}': sym := rbrace; Read_char |
			
			'(':
			Read_char;
			IF ch = '*' THEN Read_char; Skip_comment (0); Get (sym)
			ELSE sym := lparen
			END |
			
			'[': sym := lbrak; Read_char |
			'{': sym := lbrace; Read_char |
			';': sym := semicolon; Read_char |
			'|': sym := bar; Read_char
		ELSE sym := null; Read_char
		END
	ELSE sym := eof
	END
END Get;

END Scanner.
