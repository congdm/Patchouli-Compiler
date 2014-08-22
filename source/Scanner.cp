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
	lparen* = 29; lbrak* = 30; lbrace* = 31;
	
	not* = 32; number* = 34; nil* = 35; true* = 36; false* = 37; string* = 38;
	ident* = 39;
	
	if* = 55; while* = 56; repeat* = 57; for* = 58; case* = 59;
	
	comma* = 60; colon* = 61; becomes* = 62; upto* = 63;
	rparen* = 64; rbrak* = 65; rbrace* = 66;
	then* = 67; of* = 68; do* = 69; to* = 70; by* = 71;
	
	semicolon* = 72; end* = 73; bar* = 74; else* = 75;
	elsif* = 76; until* = 77; return* = 78;
	
	array* = 79; record* = 80; pointer* = 81;
	const* = 82; type* = 83; var* = 84; procedure* = 85;
	begin* = 86; module* = 88;
	eof* = 89;
	
	errLargeNumber = 'This number is too large to handle (compiler limit)';

VAR
	val* : LONGINT;
	typeOfVal* : Base.Type;
	haveError* : BOOLEAN;
	id* : Base.String;
	str* : Base.LongString;
	
	max_int : ARRAY 22 OF CHAR;
	max_int_len : INTEGER;
	power_of_10 : ARRAY 28 OF REAL;
	
	srcfile : Sys.FileHandle;
	ch : CHAR;
	charNum, prevCharNum : INTEGER;
	eofFlag : BOOLEAN;

PROCEDURE Init* (VAR file : Sys.FileHandle);
BEGIN
	srcfile := file; ch := 0X; charNum := 0; prevCharNum := 0;
	haveError := FALSE; eofFlag := FALSE
END Init;

PROCEDURE Read_char;
BEGIN
	IF ~ Sys.Read_char (srcfile, ch) THEN eofFlag := TRUE; ch := 0X
	ELSE INC (charNum)
	END
END Read_char;
	
PROCEDURE Mark* (s : ARRAY OF CHAR);
BEGIN
	Sys.Console_WriteInt (charNum);
	Sys.Console_WriteString (': '); Sys.Console_WriteString (s);
	Sys.Console_WriteLn; haveError := TRUE
END Mark;

PROCEDURE Skip_blank_and_comment;
BEGIN
	WHILE ~ eofFlag & (ch <= ' ') DO Read_char END
END Skip_blank_and_comment;

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
		'A': IF (i = 5) & (id = 'ARRAY') THEN sym := array END |
		'B':
		IF (i = 2) & (id[1] = 'Y') THEN sym := by
		ELSIF (i = 5) & (id = 'BEGIN') THEN sym := begin
		END |
		
		'C': IF (i = 5) & (id = 'CONST') THEN sym := const END |
		'D':
		IF (i = 2) & (id[1] = 'O') THEN sym := do
		ELSIF (i = 3) & (id[1] = 'I') & (id[2] = 'V') THEN sym := div;
		END |
		
		'E':
		IF (i = 3) & (id[1] = 'N') & (id[2] = 'D') THEN sym := end
		ELSIF (i = 4) & (id = 'ELSE') THEN sym := else
		ELSIF (i = 5) & (id = 'ELSIF') THEN sym := elsif
		END |
		
		'F': IF (i = 5) & (id = 'FALSE') THEN sym := false END |
		'I':
		IF i = 2 THEN
			IF id [1] = 'F' THEN sym := if
			ELSIF id [1] = 'N' THEN sym := in
			ELSIF id [1] = 'S' THEN sym := is
			END
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
	
PROCEDURE Get_hex_number (hex_int : LONGINT; hex_overflow : BOOLEAN);
	CONST err_no_suffix = 'Hexadecimal number without suffix';
	VAR x : INTEGER;
BEGIN
	WHILE (ch >= 'A') & (ch <= 'F') OR (ch >= '0') & (ch <= '9') DO
		IF ~ hex_overflow THEN
			x := ORD (ch) - ORD ('0');
			IF x > 9 THEN DEC (x, 7) END;
			IF hex_int <= MAX_INT DIV 16 THEN
				hex_int := hex_int * 16;
				IF hex_int <= MAX_INT - x THEN INC (hex_int, x)
				ELSE hex_overflow := TRUE
				END
			ELSE hex_overflow := TRUE
			END
		END;
		Read_char
	END;
	val := hex_int; IF ch = 'H' THEN Read_char ELSE Mark (err_no_suffix) END;
	IF ~ hex_overflow THEN (* Ok *) ELSE Mark (errLargeNumber) END;
	typeOfVal := Base.int_type
END Get_hex_number;
	
PROCEDURE Finish_real_number
(real : LONGINT; scale, exponent : INTEGER; real_overflow : BOOLEAN);
	VAR tail : LONGINT;
		
	PROCEDURE Adjust_scale (real : LONGINT; scale : INTEGER);
		VAR t : INTEGER; single : SHORTREAL; double : REAL;
	BEGIN
		SYSTEM.GET (SYSTEM.ADR(real), double);
		IF scale > 0 THEN
			WHILE scale > LEN(power_of_10) - 1 DO
				double := double * power_of_10[LEN(power_of_10) - 1];
				DEC (scale, LEN(power_of_10) - 1)
			END;
			double := double * power_of_10[scale]
		ELSIF scale < 0 THEN
			WHILE -scale > LEN(power_of_10) - 1 DO
				double := double / power_of_10[LEN(power_of_10) - 1];
				INC (scale, LEN(power_of_10) - 1)
			END;
			double := double / power_of_10[-scale]
		END;
		single := SHORT (double);
		SYSTEM.GET (SYSTEM.ADR(single), t);
		val := t
	END Adjust_scale;
	
BEGIN
	IF real_overflow THEN val := SINGLE_POSITIVE_INFINITY
	ELSE
		tail := 0;
		WHILE real > DOUBLE_MANTISSA DO
			tail := tail DIV 2 + (real MOD 2) * TWO_POWER_52;
			real := real DIV 2; INC (exponent)
		END;
		IF tail < TWO_POWER_30 THEN (* Do nothing *)
		ELSIF ODD(real) OR (tail > TWO_POWER_52) THEN
			INC (real);
			IF real > DOUBLE_MANTISSA THEN real := real DIV 2; INC (exponent)
			END
		END;
		WHILE real * 2 <= DOUBLE_MANTISSA DO
			real := real * 2; DEC (exponent)
		END;
		
		DEC (real, TWO_POWER_52);
		INC (real, (exponent + 52) * TWO_POWER_52);
		
		Adjust_scale (real, scale)
	END;
	typeOfVal := Base.real_type
END Finish_real_number;
	
PROCEDURE Get_fraction
(real : LONGINT; scale : INTEGER; real_overflow : BOOLEAN);
	CONST LIMIT = 1000000000000000000;
	VAR fraction, k : LONGINT;
		exponent : INTEGER;
		real_underflow : BOOLEAN;
BEGIN
	fraction := 0; real_underflow := FALSE;
	IF real = 0 THEN
		WHILE ch = '0' DO Read_char;
			IF scale < MIN_SCALE THEN real_underflow := TRUE
			ELSE DEC (scale)
			END
		END
	END;
	k := LIMIT DIV 10;
	WHILE (ch >= '0') & (ch <= '9') DO
		INC (fraction, (ORD(ch) - ORD('0')) * k);
		k := k DIV 10;
		Read_char
	END;
	
	IF real_underflow THEN val := 0
	ELSE
		exponent := 1023;
		IF scale <= 0 THEN
			WHILE (fraction > 0) & (real <= MAX_MANTISSA) DO
				fraction := fraction * 2;
				IF fraction >= LIMIT THEN
					fraction := fraction - LIMIT;
					real := real * 2 + 1;
				ELSE real := real * 2
				END;
				DEC (exponent);
			END;
			fraction := fraction * 2;
			IF fraction < LIMIT THEN (* Do nothing *)
			ELSIF ODD(real) OR (fraction > LIMIT) THEN INC (real)
			END
		END;
		Finish_real_number (real, scale, exponent, real_overflow)
	END
END Get_fraction;
	
PROCEDURE Get_number2*;
	VAR decimal_int, hex_int, real, limit, old_filepos : LONGINT;
		x, scale : INTEGER;
		is_decimal, decimal_overflow, hex_overflow, real_overflow : BOOLEAN;
BEGIN
	decimal_int := 0; hex_int := 0; real := 0; scale := 0;
	decimal_overflow := FALSE; hex_overflow := FALSE; real_overflow := FALSE;
	WHILE ch = '0' DO Read_char END;
	WHILE (ch >= '0') & (ch <= '9') DO
		x := ORD (ch) - ORD ('0');
		IF ~ decimal_overflow THEN
			IF decimal_int <= MAX_INT DIV 10 THEN
				decimal_int := decimal_int * 10;
				IF decimal_int <= MAX_INT - x THEN INC (decimal_int, x)
				ELSE decimal_overflow := TRUE
				END
			ELSE decimal_overflow := TRUE
			END
		END;
		IF ~ hex_overflow THEN
			IF hex_int <= MAX_INT DIV 16 THEN
				hex_int := hex_int * 16;
				IF hex_int <= MAX_INT - x THEN INC (hex_int, x)
				ELSE hex_overflow := TRUE
				END
			ELSE hex_overflow := TRUE
			END
		END;
		IF ~ real_overflow THEN
			IF real > MAX_MANTISSA THEN
				IF scale = 0 THEN
					IF (x > 5) OR (x = 5) & ODD (real) THEN INC (real) END
				END;
				IF scale < MAX_SCALE THEN INC (scale)
				ELSE real_overflow := TRUE
				END
			ELSE real := real * 10 + x
			END
		END;
		Read_char
	END;
	
	is_decimal := FALSE;
	IF ch = 'H' THEN
		val := hex_int; typeOfVal := Base.int_type; Read_char;
		IF ~ hex_overflow THEN (* Ok *) ELSE Mark (errLargeNumber) END		
	ELSIF (ch >= 'A') & (ch <= 'F') THEN Get_hex_number (hex_int, hex_overflow)
	ELSIF ch = '.' THEN
		old_filepos := Sys.FilePos (srcfile); Read_char;
		IF ch # '.' THEN Get_fraction (real, scale, real_overflow)
		ELSE Sys.Seek (srcfile, old_filepos); is_decimal := TRUE
		END
	ELSE is_decimal := TRUE	
	END;
	
	IF is_decimal THEN
		val := decimal_int; typeOfVal := Base.int_type;
		IF ~ decimal_overflow THEN (* Ok *) ELSE Mark (errLargeNumber) END
	END
END Get_number2;
	
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

PROCEDURE Get* (VAR sym : INTEGER);
BEGIN
	Skip_blank_and_comment;
	IF ~eofFlag THEN
		CASE ch OF
			'_', 'A'..'Z', 'a'..'z': Get_word (sym) |
			'0'..'9': sym := number; Get_number2 |
			"'", '"': sym := string; Get_string |
			'*': sym := times; Read_char; |
			'/': sym := slash; Read_char; |
			'+': sym := plus; Read_char; |
			'-': sym := minus; Read_char; |
			'=': sym := equal; Read_char; |
			'#': sym := not_equal; Read_char; |
			'&': sym := and; Read_char; |
			'~': sym := not; Read_char; |
			
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
			'(': sym := lparen; Read_char |
			'[': sym := lbrak; Read_char |
			'{': sym := lbrace; Read_char |
			';': sym := semicolon; Read_char |
			'|': sym := bar; Read_char
		ELSE sym := null; Read_char
		END
	ELSE sym := eof
	END
END Get;

PROCEDURE Init_module;
	VAR i : INTEGER;
BEGIN
	Sys.Int_to_string (Base.MAX_INT, max_int);
	max_int_len := Base.Str_len (max_int);
	power_of_10[0] := 1.0;
	FOR i := 1 TO LEN(power_of_10) - 1 DO
		power_of_10[i] := power_of_10[i - 1] * 10.0
	END
END Init_module;
	
BEGIN
	Init_module
END Scanner.
