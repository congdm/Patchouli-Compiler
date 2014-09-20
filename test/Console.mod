MODULE Console;

IMPORT
	SYSTEM, Win := WinApi, Strings;
	
VAR
	stdin, stdout : INTEGER;

PROCEDURE Write* (ch : CHAR);
	VAR nWritten, res : INTEGER;
BEGIN
	res := Win.WriteConsoleW (stdout, SYSTEM.ADR(ch), 1,
		SYSTEM.ADR(nWritten), 0)
END Write;

PROCEDURE WriteLn*;
	VAR str : ARRAY 2 OF CHAR; nWritten, res : INTEGER;
BEGIN
	str[0] := CHR(13); str[1] := CHR(10);
	res := Win.WriteConsoleW (stdout, SYSTEM.ADR(str), 2,
		SYSTEM.ADR(nWritten), 0)
END WriteLn;

PROCEDURE WriteString* (str : ARRAY OF CHAR);
	VAR nWritten, res : INTEGER;
BEGIN
	res := Win.WriteConsoleW (stdout, SYSTEM.ADR(str), Strings.Length(str),
		SYSTEM.ADR(nWritten), 0)
END WriteString;

PROCEDURE IntToString* (x : INTEGER; VAR str : ARRAY OF CHAR);
	CONST MIN_INT = -7FFFFFFFFFFFFFFFH - 1;
	VAR negative : BOOLEAN; s : ARRAY 32 OF CHAR; i, j : INTEGER;
BEGIN
	IF x # MIN_INT THEN 
		IF x < 0 THEN negative := TRUE; x := -x
		ELSE negative := FALSE
		END;
		
		i := 0;
		REPEAT
			s[i] := CHR(x MOD 10 + ORD('0'));
			INC (i); x := x DIV 10
		UNTIL x = 0;
		
		IF negative THEN str[0] := '-'; j := 0;
			WHILE j < i DO str[j + 1] := s[i - 1 - j]; j := j + 1 END;
			str[i + 1] := 0X
		ELSE j := 0;
			WHILE j < i DO str[j] := s[i - 1 - j]; j := j + 1 END;
			str [i] := 0X
		END
	ELSE str[0] := 0X; Strings.Append ('-9223372036854775808', str)
	END
END IntToString;

PROCEDURE WriteInt* (n : INTEGER);
	VAR str : ARRAY 32 OF CHAR;
BEGIN
	IntToString (n, str); WriteString (str)
END WriteInt;

PROCEDURE Read* (VAR ch : CHAR);
	VAR nRead, res : INTEGER;
BEGIN
	res := Win.ReadConsoleW (stdin, SYSTEM.ADR(ch), 1, SYSTEM.ADR(nRead), 0)
END Read;

PROCEDURE Init;
	VAR res : INTEGER;
BEGIN
	res := Win.AllocConsole();
	stdin := Win.GetStdHandle (Win.STD_INPUT_HANDLE);
	stdout := Win.GetStdHandle (Win.STD_OUTPUT_HANDLE)
END Init;

BEGIN
	Init
END Console.