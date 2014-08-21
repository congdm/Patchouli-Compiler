MODULE Test;
	TYPE
		AsciiStr = ARRAY 64 OF BYTE;
	VAR
		AllocConsole : PROCEDURE () : INTEGER;
		MessageBoxW : PROCEDURE (hwnd, lpText, lpCaption, uType : INTEGER);
		
	PROCEDURE Make_AsciiStr (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
		VAR n, i : INTEGER;
	BEGIN
		n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
		i := 0;
		WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
	END Make_AsciiStr;
	
	PROCEDURE Init;
		VAR user32 : INTEGER;
		    str : AsciiStr; 
	BEGIN
		LoadLibrary (user32, 'USER32.DLL');
		Make_AsciiStr (str, 'MessageBoxW');
		GetProcAddress (MessageBoxW, user32, ADR(str))
	END Init;
	
	PROCEDURE Main;
		VAR i : INTEGER;
	BEGIN
		i := 2;
		MessageBoxW (0, ADR('Oberon for Win64'), ADR('Hello, World!'), 0)
	END Main;
	
BEGIN
	Init;
	Main (0)
END Test.