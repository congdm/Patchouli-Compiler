MODULE Console;

IMPORT
	SYS := SYSTEM, WinBase, Win := WinApi, Strings;
	
CONST
	chSz = SYS.SIZE(CHAR);
	
VAR
	stdin, stdout: WinBase.HANDLE;

PROCEDURE Write* (ch: CHAR);
	VAR nWritten: WinBase.DWORD; r: WinBase.BOOL;
BEGIN
	r := Win.WriteConsoleW(stdout, SYS.ADR(ch), 1, SYS.ADR2(nWritten), 0)
END Write;

PROCEDURE WriteLn*;
	VAR str: ARRAY 2 OF CHAR; nWritten: WinBase.DWORD; r: WinBase.BOOL;
BEGIN
	str[0] := CHR(13); str[1] := CHR(10);
	r := Win.WriteConsoleW(stdout, SYS.ADR(str), 2, SYS.ADR2(nWritten), 0)
END WriteLn;

PROCEDURE WriteString* (str: ARRAY OF CHAR);
	VAR nWritten: WinBase.DWORD; r: WinBase.BOOL; len: INTEGER;
BEGIN
	len := Strings.Length(str);
	r := Win.WriteConsoleW(stdout, SYS.ADR(str), len, SYS.ADR2(nWritten), 0)
END WriteString;

PROCEDURE WriteInt* (n: INTEGER);
	VAR str: ARRAY 32 OF CHAR;
BEGIN Strings.FormatInt (n, str); WriteString (str)
END WriteInt;

PROCEDURE WriteHex* (n: INTEGER);
	VAR str: ARRAY 32 OF CHAR;
BEGIN Strings.FormatHex (n, str); WriteString (str)
END WriteHex;

PROCEDURE WriteReal* (r: REAL);
	VAR str: ARRAY 32 OF CHAR;
BEGIN Strings.FormatReal (r, str); WriteString (str)
END WriteReal;

PROCEDURE Read* (VAR ch: CHAR);
	VAR nRead: WinBase.DWORD; r: WinBase.BOOL;
BEGIN
	r := Win.ReadConsoleW(stdin, SYS.ADR(ch), 1, SYS.ADR2(nRead), NIL)
END Read;

PROCEDURE Init;
	VAR res: WinBase.BOOL;
BEGIN
	res := Win.AllocConsole();
	stdin := Win.GetStdHandle(WinBase.STD_INPUT_HANDLE);
	stdout := Win.GetStdHandle(WinBase.STD_OUTPUT_HANDLE)
END Init;

BEGIN Init
END Console.