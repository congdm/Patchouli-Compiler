MODULE Test;

CONST
	STD_OUTPUT_HANDLE = -11;

TYPE
	AnsiStr = ARRAY 256 OF BYTE;
	
VAR
	AllocConsole : PROCEDURE() : INTEGER;
	GetStdHandle : PROCEDURE(nStdHandle : INTEGER) : INTEGER;
	WriteFile : PROCEDURE(hFile, lpBuffer, nNumberOfBytesToWrite,
	                      lpNumberOfBytesWritten, lpOverlapped : INTEGER) : INTEGER;
	
	stdout : INTEGER;
	
PROCEDURE MakeAnsiStr (VAR out : AnsiStr; in : ARRAY OF CHAR);
	VAR
		i, n : INTEGER;
BEGIN
	n := LEN(in);
	IF n > LEN(out) THEN n := LEN(out) END;
	i := 0;
	WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END;
	IF out[n - 1] # 0 THEN out [n - 1] := 0 END
END MakeAnsiStr;

PROCEDURE NullStringLen(str : ARRAY OF BYTE) : INTEGER;
	VAR
		len : INTEGER;
		flag : BOOLEAN;
BEGIN
	len := 0; flag := TRUE;
	WHILE (len < LEN(str)) & flag DO
		IF str[len] # 0 THEN len := len + 1 ELSE flag := FALSE END
	END
RETURN len
END NullStringLen;

PROCEDURE InitLibrary;
	VAR
		s : AnsiStr;
		kernel32 : INTEGER;
BEGIN
	LoadLibrary (kernel32, 'kernel32.dll');
	MakeAnsiStr (s, 'AllocConsole');
	GetProcAddress (AllocConsole, kernel32, ADR(s));
	MakeAnsiStr (s, 'GetStdHandle');
	GetProcAddress (GetStdHandle, kernel32, ADR(s));
	MakeAnsiStr (s, 'WriteFile');
	GetProcAddress (WriteFile, kernel32, ADR(s))
END InitLibrary;

PROCEDURE InitConsole;
	VAR
		res : INTEGER;
BEGIN
	res := AllocConsole();
	stdout := GetStdHandle(STD_OUTPUT_HANDLE)
END InitConsole;

PROCEDURE WriteAnsiString (str : ARRAY OF BYTE);
	VAR
		res, n : INTEGER;
BEGIN
	res := WriteFile(stdout, ADR(str), NullStringLen(str), ADR(n), 0)
END WriteAnsiString;

PROCEDURE Main;
	VAR
		s : AnsiStr;
BEGIN
	MakeAnsiStr (s, 'Hello, world!');
	WriteAnsiString (s)
END Main;

BEGIN
InitLibrary;
InitConsole;
Main
END Test.
