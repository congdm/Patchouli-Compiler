MODULE WinApi;

IMPORT
	SYSTEM;
	
CONST
	STD_INPUT_HANDLE* = -10;
	STD_OUTPUT_HANDLE* = -11;
	STD_ERROR_HANDLE* = -12;
	
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	
VAR
	Kernel* : RECORD
		AllocConsole* : PROCEDURE () : INTEGER;
		GetStdHandle* : PROCEDURE (nStdHandle : INTEGER) : INTEGER;
		WriteConsoleW* : PROCEDURE (hConsoleOutput, lpBuffer,
			nNumberOfCharsToWrite, lpNumberOfCharsWritten,
			lpReserved : INTEGER) : INTEGER;
		ReadConsoleW* : PROCEDURE (hConsoleInput, lpBuffer,
			nNumberOfCharsToRead, lpNumberOfCharsRead,
			pInputConsole : INTEGER) : INTEGER;
		SetConsoleCP* : PROCEDURE (wCodePageID : INTEGER) : INTEGER;
		SetConsoleOutputCP* : PROCEDURE (wCodePageID : INTEGER) : INTEGER
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
	SYSTEM.LoadLibraryW (kernel32, 'KERNEL32.DLL');
	Make_AsciiStr (str, 'AllocConsole');
	SYSTEM.GetProcAddress (Kernel.AllocConsole, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'GetStdHandle');
	SYSTEM.GetProcAddress (Kernel.GetStdHandle, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'WriteConsoleW');
	SYSTEM.GetProcAddress (Kernel.WriteConsoleW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'ReadConsoleW');
	SYSTEM.GetProcAddress (Kernel.ReadConsoleW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'SetConsoleCP');
	SYSTEM.GetProcAddress (Kernel.SetConsoleCP, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'SetConsoleOutputCP');
	SYSTEM.GetProcAddress (Kernel.SetConsoleOutputCP,kernel32,SYSTEM.ADR(str));
	
	SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	Make_AsciiStr (str, 'MessageBoxW');
	SYSTEM.GetProcAddress (User.MessageBoxW, user32, SYSTEM.ADR(str))
END Init;
	
BEGIN
	Init
END WinApi.