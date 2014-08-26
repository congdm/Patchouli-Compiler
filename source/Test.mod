MODULE Test;
(*$EXE*)

IMPORT
	SYSTEM;

TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	
VAR
	MessageBoxW* : PROCEDURE (hwnd, lpText, lpCaption, uType : INTEGER);
	
PROCEDURE Make_AsciiStr (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
	VAR n, i : INTEGER;
BEGIN
	n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
	i := 0; WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
END Make_AsciiStr;

PROCEDURE Init;
	VAR user32 : INTEGER;
		str : AsciiStr; 
BEGIN
	SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	Make_AsciiStr (str, 'MessageBoxW');
	SYSTEM.GetProcAddress (MessageBoxW, user32, SYSTEM.ADR(str))
END Init;

PROCEDURE Main;
	CONST mess = 'Oberon for Win64'; title = 'Hello, World!';
BEGIN
	MessageBoxW (0, SYSTEM.ADR(mess), SYSTEM.ADR(title), 0)
END Main;
	
BEGIN
	Init;
	Main
END Test.