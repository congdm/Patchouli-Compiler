MODULE Scanner;

IMPORT
	Console, Base;

VAR
	val* : INTEGER;
	have_error* : BOOLEAN;
	id* : Base.String;
	
	max_int : ARRAY 22 OF CHAR;
	max_int_len : INTEGER;
	
	srcfile : Base.FileHandle;
	ch : CHAR;
	char_num : INTEGER;
	eof : BOOLEAN;

PROCEDURE Init* (VAR file : Base.FileHandle);
	BEGIN
	srcfile := file;
	ch := 0X;
	char_num := 0;
	have_error := FALSE;
	eof := FALSE;
	END Init;

PROCEDURE Read_char;
	BEGIN
	IF Base.Read_char (srcfile, ch) = Base.failed THEN
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
	Base.Int_to_string (char_num, s);
	Console.WriteString (s);
	Console.WriteString (': ');
	Console.WriteString (str);
	Console.WriteLn
	END Mark;

PROCEDURE Skip_blank_and_comment;
	BEGIN
	WHILE ~ eof & (ch <= ' ') DO
		Read_char;
		END;
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
			IF (i = 5) & Base.Str_equal (id, 'BEGIN') THEN
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
				sym := Base.sym_array;
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

PROCEDURE Get_number;
	VAR
		s : ARRAY 21 OF CHAR;
		i, len : INTEGER;
		flag : BOOLEAN;
	BEGIN
	i := 0;
	flag := TRUE;
	WHILE (ch = '0') DO Read_char END;
	WHILE (ch >= '0') & (ch <= '9') DO
		IF flag THEN
			IF i < max_int_len THEN
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
			Get_number;
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
Base.Int_to_string (Base.MAX_INT, max_int);
max_int_len := Base.Str_len (max_int);
END Scanner.
