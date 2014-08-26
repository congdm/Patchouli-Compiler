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