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

VAR
	val* : LONGINT;
	type_of_val* : Base.Type;
	have_error* : BOOLEAN;
	id* : Base.String;
	
	max_int : ARRAY 22 OF CHAR;
	max_int_len : INTEGER;
	power_of_10 : ARRAY 28 OF REAL;
	
	srcfile : Sys.FileHandle;
	ch : CHAR;
	char_num*, _i : INTEGER;
	eof : BOOLEAN;

PROCEDURE Init* (VAR file : Sys.FileHandle);
	BEGIN
	srcfile := file;
	ch := 0X;
	char_num := 0;
	have_error := FALSE;
	eof := FALSE
	END Init;

PROCEDURE Read_char;
	BEGIN
	IF Sys.Read_char (srcfile, ch) = failed THEN
		eof := TRUE;
		ch := 0X;
	ELSE
		INC (char_num)
		END;
	END Read_char;
	
PROCEDURE Mark* (str : ARRAY OF CHAR);
	VAR
		s : ARRAY 22 OF CHAR;
	BEGIN
	Sys.Int_to_string (char_num, s);
	Sys.Console_WriteString (s);
	Sys.Console_WriteString (': ');
	Sys.Console_WriteString (str);
	Sys.Console_WriteLn
	END Mark;

PROCEDURE Skip_blank_and_comment;
	BEGIN
	WHILE ~ eof & (ch <= ' ') DO
		Read_char
		END
	END Skip_blank_and_comment;

PROCEDURE Get_word (VAR sym : INTEGER);
	VAR
		i : INTEGER;
		flag : BOOLEAN;
	BEGIN
	NEW (id);
	i := 0;
	flag := TRUE;
	WHILE (ch = '_') OR (ch >= 'A') & (ch <= 'Z')
	OR (ch >= 'a') & (ch <= 'z')
	OR (ch >= '0') & (ch <= '9') DO
		IF flag THEN
			IF i < Base.max_str_len THEN
				id.content [i] := ch;
				INC (i);
			ELSE
				Mark ('Identifier length is too long (compiler limit)');
				flag := FALSE
				END;
			END;
		Read_char
		END;
	id.len := i;
	id.content [i] := 0X;
	
	sym := Base.sym_ident;
	CASE id.content [0] OF
		'A':
			IF (i = 5) & Base.Str_equal (id, 'ARRAY') THEN
				sym := Base.sym_array;
				END; |
		'B':
			IF (i = 2) & (id.content [1] = 'Y') THEN
				sym := Base.sym_by
			ELSIF (i = 5) & Base.Str_equal (id, 'BEGIN') THEN
				sym := Base.sym_begin;
				END; |
		'C':
			IF (i = 5) & Base.Str_equal (id, 'CONST') THEN
				sym := Base.sym_const;
				END; |
		'D':
			IF (i = 2) & (id.content [1] = 'O') THEN
				sym := Base.sym_do;
			ELSIF (i = 3) & (id.content [1] = 'I') & (id.content [2] = 'V') THEN
				sym := Base.sym_div;
				END; |
		'E':
			IF (i = 3) & (id.content [1] = 'N') & (id.content [2] = 'D') THEN
				sym := Base.sym_end;
			ELSIF (i = 4) & Base.Str_equal (id, 'ELSE') THEN
				sym := Base.sym_else;
			ELSIF (i = 5) & Base.Str_equal (id, 'ELSIF') THEN
				sym := Base.sym_elsif;
				END; |
		'F':
			IF (i = 5) & Base.Str_equal (id, 'FALSE') THEN
				sym := Base.sym_false;
				END; |
		'I':
			IF i = 2 THEN
				IF id.content [1] = 'F' THEN
					sym := Base.sym_if;
				ELSIF id.content [1] = 'N' THEN
					sym := Base.sym_in;
				ELSIF id.content [1] = 'S' THEN
					sym := Base.sym_is;
					END;
				END; |
		'M':
			IF (i = 3) & (id.content [1] = 'O') & (id.content [2] = 'D') THEN
				sym := Base.sym_mod;
			ELSIF Base.Str_equal (id, 'MODULE') THEN
				sym := Base.sym_module;
				END; |
		'N':
			IF (i = 3) & (id.content [1] = 'I') & (id.content [2] = 'L') THEN
				sym := Base.sym_mod;
				END; |
		'O':
			IF i = 2 THEN
				IF id.content [1] = 'F' THEN
					sym := Base.sym_of;
				ELSIF id.content [1] = 'R' THEN
					sym := Base.sym_or;
					END;
				END; |
		'P':
			IF (i = 7) & Base.Str_equal (id, 'POINTER') THEN
				sym := Base.sym_pointer;
			ELSIF (i = 9) & Base.Str_equal (id, 'PROCEDURE') THEN
				sym := Base.sym_procedure;
				END; |
		'R':
			IF i = 6 THEN
				IF Base.Str_equal (id, 'RECORD') THEN
					sym := Base.sym_record;
				ELSIF Base.Str_equal (id, 'REPEAT') THEN
					sym := Base.sym_repeat;
				ELSIF Base.Str_equal (id, 'RETURN') THEN
					sym := Base.sym_return;
					END;
				END; |
		'T':
			IF (i = 2) & (id.content [1] = 'O') THEN
				sym := Base.sym_to;
			ELSIF i = 4 THEN
				IF Base.Str_equal (id, 'THEN') THEN
					sym := Base.sym_then;
				ELSIF Base.Str_equal (id, 'TRUE') THEN
					sym := Base.sym_true
				ELSIF Base.Str_equal (id, 'TYPE') THEN
					sym := Base.sym_type
					END
				END; |
		'U':
			IF (i = 5) & Base.Str_equal (id, 'UNTIL') THEN
				sym := Base.sym_until
				END |
		'V':
			IF (i = 3) & (id.content [1] = 'A') & (id.content [2] = 'R') THEN
				sym := Base.sym_var
				END |
		'W':
			IF (i = 5) & Base.Str_equal (id, 'WHILE') THEN
				sym := Base.sym_while
				END
		ELSE (* Do nothing *)
		END;
	END Get_word;
	
PROCEDURE Get_hex_number (hex_int : LONGINT; hex_overflow : BOOLEAN);
	VAR
		x : INTEGER;
BEGIN
	WHILE (ch >= 'A') & (ch <= 'F') OR (ch >= '0') & (ch <= '9') DO
		IF ~ hex_overflow THEN
			x := ORD (ch) - ORD ('0');
			IF x > 9 THEN DEC (x, 7)
			END;
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
	val := hex_int; 
	IF ch = 'H' THEN Read_char
	ELSE Mark ('Hexadecimal number without suffix')
	END;
	IF hex_overflow THEN Mark ('This number is too large (compiler limit)')
	END;
	type_of_val := Base.int_type
END Get_hex_number;
	
PROCEDURE Finish_real_number
(real : LONGINT; scale, exponent : INTEGER; real_overflow : BOOLEAN);
	VAR
		tail : LONGINT;
		
	PROCEDURE Adjust_scale (real : LONGINT; scale : INTEGER);
		VAR
			t : INTEGER; single : SHORTREAL; double : REAL;
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
	type_of_val := Base.real_type
END Finish_real_number;
	
PROCEDURE Get_fraction
(real : LONGINT; scale : INTEGER; real_overflow : BOOLEAN);
	CONST
		LIMIT = 1000000000000000000;
	VAR
		fraction, k : LONGINT;
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
	VAR
		decimal_int, hex_int, real, limit : LONGINT;
		x, scale : INTEGER;
		decimal_overflow, hex_overflow, real_overflow : BOOLEAN;
BEGIN
	decimal_int := 0; hex_int := 0; real := 0; scale := 0;
	decimal_overflow := FALSE; hex_overflow := FALSE; real_overflow := FALSE;
	WHILE ch = '0' DO
		Read_char
	END;
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
					IF (x > 5) OR (x = 5) & ODD (real) THEN INC (real)
					END
				END;
				IF scale < MAX_SCALE THEN INC (scale) ELSE real_overflow := TRUE
				END
			ELSE real := real * 10 + x
			END
		END;
		Read_char
	END;
	IF ch = 'H' THEN
		val := hex_int;
		IF hex_overflow THEN
			Mark ('This number is too large (compiler limit)')
		END;
		type_of_val := Base.int_type;
		Read_char
	ELSIF (ch >= 'A') & (ch <= 'F') THEN
		Get_hex_number (hex_int, hex_overflow)
	ELSIF ch = '.' THEN
		Read_char; Get_fraction (real, scale, real_overflow)
	ELSE
		val := decimal_int;
		IF decimal_overflow THEN
			Mark ('This number is too large (compiler limit)')
		END;
		type_of_val := Base.int_type;
	END
END Get_number2;

PROCEDURE Get_number;
	VAR
		s : ARRAY 21 OF CHAR;
		i, len : INTEGER;
		flag : BOOLEAN;
	BEGIN
	i := 0; flag := TRUE;
	WHILE (ch = '0') DO Read_char END;
	WHILE (ch >= '0') & (ch <= '9') DO
		IF flag THEN
			IF i < LEN (s) THEN
				s [i] := ch;
				INC (i)
			ELSE
				Mark ('This number is too large to handle (compiler limit)');
				flag := FALSE
				END
			END;
		Read_char
		END;
	
	val := 0;
	IF flag THEN
		len := i;
		IF len = max_int_len THEN
			i := 0;
			WHILE (i < len) & (s [i] = max_int [i]) DO
				INC (i)
				END;
			IF s [i] > max_int [i] THEN
				Mark ('This number is too large to handle (compiler limit)');
				flag := FALSE
				END;
			END;
		IF flag THEN
			i := 0;
			WHILE i < len DO
				val := val * 10 + (ORD (s [i]) - ORD ('0'));
				INC (i)
				END
			END
		END
	END Get_number;
	
PROCEDURE Get_string;
	VAR
		i : INTEGER;
		flag : BOOLEAN;
	BEGIN
	NEW (id);
	i := 0;
	flag := TRUE;
	IF ch = "'" THEN
		Read_char;
		WHILE (ch # 0X) & (ch # "'") DO
			IF flag THEN
				IF i < Base.max_str_len THEN
					id.content [i] := ch;
					INC (i);
				ELSE
					Mark ('String length is too long (compiler limit)');
					flag := FALSE
					END;
				END;
			Read_char
			END
	ELSE
		(* ch = '"' *)
		Read_char;
		WHILE (ch # 0X) & (ch # '"') DO
			IF flag THEN
				IF i < Base.max_str_len THEN
					id.content [i] := ch;
					INC (i);
				ELSE
					Mark ('String length is too long (compiler limit)');
					flag := FALSE
					END;
				END;
			Read_char
			END
		END;
	Read_char;
	id.len := i;
	id.content [i] := 0X
	END Get_string;

PROCEDURE Get* (VAR sym : INTEGER);
	BEGIN
	Skip_blank_and_comment;
	IF eof THEN
		sym := Base.sym_eof;
	ELSE
		IF (ch = '_') OR (ch >= 'A') & (ch <= 'Z')
		OR (ch >= 'a') & (ch <= 'z') THEN
			Get_word (sym);
		ELSIF (ch >= '0') & (ch <= '9') THEN
			Get_number2;
			sym := Base.sym_number;
		ELSE
			CASE ch OF
				'_', 'A'..'Z', 'a'..'z':
					Get_word (sym) |
				'0'..'9':
					sym := Base.sym_number;
					Get_number |
				"'", '"':
					sym := Base.sym_string;
					Get_string |
				'*': sym := Base.sym_times; Read_char; |
				'/': sym := Base.sym_slash; Read_char; |
				'+': sym := Base.sym_plus; Read_char; |
				'-': sym := Base.sym_minus; Read_char; |
				'=': sym := Base.sym_equal; Read_char; |
				'#': sym := Base.sym_not_equal; Read_char; |
				'&': sym := Base.sym_and; Read_char; |
				'~': sym := Base.sym_not; Read_char; |
				'<':
					Read_char;
					IF ch = '=' THEN sym := Base.sym_less_equal; Read_char;
					ELSE sym := Base.sym_less; END; |
				'>':
					Read_char;
					IF ch = '=' THEN sym := Base.sym_greater_equal; Read_char;
					ELSE sym := Base.sym_greater; END; |
				'.':
					Read_char;
					IF ch = '.' THEN sym := Base.sym_upto; Read_char;
					ELSE sym := Base.sym_period; END; |
				'^': sym := Base.sym_arrow; Read_char; |
				',': sym := Base.sym_comma; Read_char; |
				':':
					Read_char;
					IF ch = '=' THEN sym := Base.sym_becomes; Read_char;
					ELSE sym := Base.sym_colon; END; |
				')': sym := Base.sym_rparen; Read_char; |
				']': sym := Base.sym_rbrak; Read_char; |
				'}': sym := Base.sym_rbrace; Read_char; |
				'(': sym := Base.sym_lparen; Read_char; |
				'[': sym := Base.sym_lbrak; Read_char; |
				'{': sym := Base.sym_lbrace; Read_char; |
				';': sym := Base.sym_semicolon; Read_char;
				ELSE sym := Base.sym_null; Read_char;
				END;
			END;
		END;
	END Get;
	
BEGIN
	Sys.Int_to_string (Base.MAX_INT, max_int);
	max_int_len := Base.Str_len (max_int);
	power_of_10[0] := 1.0;
	FOR _i := 1 TO LEN(power_of_10) - 1 DO
		power_of_10[_i] := power_of_10[_i - 1] * 10.0
	END
END Scanner.
