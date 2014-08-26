MODULE WinApi;

IMPORT
	SYSTEM;
	
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	
VAR
	Kernel* : RECORD
	END;
	
	User* : RECORD
		MessageBoxW* : PROCEDURE (hwnd, lpText, lpCaption, uType : INTEGER)
	END;
	
PROCEDURE Make_AsciiStr* (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
	VAR n, i : INTEGER;
BEGIN
	n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
	i := 0; WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
END Make_AsciiStr;

PROCEDURE Init;
	VAR user32, kernel32 : INTEGER;
		str : AsciiStr; 
BEGIN
	SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	Make_AsciiStr (str, 'MessageBoxW');
	SYSTEM.GetProcAddress (User.MessageBoxW, user32, SYSTEM.ADR(str))
END Init;
	
BEGIN
	Init
END WinApi.