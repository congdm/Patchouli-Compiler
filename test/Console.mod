MODULE Console;

IMPORT
	SYSTEM, Win := WinApi;
	
VAR
	stdin, stdout : INTEGER;

PROCEDURE Write* (ch : CHAR);
	VAR nCharWritten, res : INTEGER;
BEGIN
	nCharWritten := 0;
	res := Win.Kernel.WriteConsoleW (stdout, SYSTEM.ADR(ch), 1,
		SYSTEM.ADR(nCharWritten), 0)
END Write;

PROCEDURE WriteLn*;
	VAR str : ARRAY 2 OF CHAR;
		nCharWritten, res : INTEGER;
BEGIN
	str[0] := CHR(13); str[1] := CHR(10);
	res := Win.Kernel.WriteConsoleW (stdout, SYSTEM.ADR(str), 2,
		SYSTEM.ADR(nCharWritten), 0)
END WriteLn;

PROCEDURE Read* (VAR ch : CHAR);
	VAR nCharRead, res : INTEGER;
BEGIN
	nCharRead := 0;
	res := Win.Kernel.ReadConsoleW (stdin, SYSTEM.ADR(ch), 1,
		SYSTEM.ADR(nCharRead), 0)
END Read;

PROCEDURE Init;
	VAR res : INTEGER;
BEGIN
	res := Win.Kernel.AllocConsole();
	stdin := Win.Kernel.GetStdHandle (Win.STD_INPUT_HANDLE);
	stdout := Win.Kernel.GetStdHandle (Win.STD_OUTPUT_HANDLE)
END Init;

BEGIN
	Init
END Console.