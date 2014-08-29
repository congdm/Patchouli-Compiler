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