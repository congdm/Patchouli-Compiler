MODULE Console;

IMPORT
	SYSTEM, Kernel32, Strings;
	
CONST
	chSz = SYSTEM.SIZE(CHAR);
	
VAR
	stdin, stdout: Kernel32.HANDLE;

PROCEDURE Write* (ch: CHAR);
	VAR nWritten: CARD32; r: Kernel32.BOOL;
BEGIN r := Kernel32.WriteConsoleW (stdout, ch, 1, nWritten, NIL)
END Write;

PROCEDURE WriteLn*;
	VAR str: ARRAY 2 OF CHAR; nWritten: CARD32; r: Kernel32.BOOL;
BEGIN str[0] := CHR(13); str[1] := CHR(10);
	r := Kernel32.WriteConsoleW (stdout, str, 2, nWritten, NIL)
END WriteLn;

PROCEDURE WriteString* (str: ARRAY OF CHAR);
	VAR nWritten: CARD32; r: Kernel32.BOOL; len: INTEGER;
BEGIN len := Strings.Length(str);
	r := Kernel32.WriteConsoleW (stdout, str, len, nWritten, NIL)
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
	VAR nRead: CARD32; r: Kernel32.BOOL;
BEGIN
	r := Kernel32.ReadConsoleW (stdin, ch, 1, nRead, NIL)
END Read;

PROCEDURE Init;
	VAR res: Kernel32.BOOL;
BEGIN
	res := Kernel32.AllocConsole();
	stdin := Kernel32.GetStdHandle(Kernel32.STD_INPUT_HANDLE);
	stdout := Kernel32.GetStdHandle(Kernel32.STD_OUTPUT_HANDLE)
END Init;

BEGIN Init
END Console.