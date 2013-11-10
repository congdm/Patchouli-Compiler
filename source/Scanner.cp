MODULE Scanner;

IMPORT
	Base;

VAR
	srcfile : Base.FileHandle;
	ch : CHAR;
	char_num : INTEGER;
	have_error, eof : BOOLEAN;

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
	IF Base.Read_char (srcfile, ch) = failed THEN
		eof := TRUE;
		END;
	END Read_char;

PROCEDURE Get* (VAR sym : INTEGER);
	BEGIN
	WHILE ~ eof & (ch <= ' ') DO
		Read_char;
		END;
	IF eof THEN
		sym := Base.sym_eof;
	ELSE
		IF (ch = '_') OR (ch >= 'A') & (ch <= 'Z')
		OR (ch >= 'a') & (ch <= 'z') THEN
			Get_word (sym);
		ELSIF (ch >= '0') & (ch <= '9') THEN
			Get_number (sym);
		ELSE
			CASE ch OF
				'*' :
				END;
			END;
		END;
	END Get;

END Scanner.
