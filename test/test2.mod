MODULE Test2;

TYPE
	AnsiStr = ARRAY 256 OF BYTE;
	
	Dword = ARRAY 4 OF BYTE;
	Word = ARRAY 2 OF BYTE;
	
	POINTrecord = RECORD
		x, y : Dword
	END;
	
	MSGrecord = RECORD
		hwnd : INTEGER;
		message : Dword;
		wParam, lParam : INTEGER;
		time : Dword;
		pt : POINTrecord
	END;
	
	WindowProcType = PROCEDURE(hWnd, Msg, wParam, lParam : INTEGER) : INTEGER;
	
	WNDCLASSWrecord = RECORD
		style : Dword;
		lpfnWndProc : WindowProcType;
		cbClsExtra, cbWndExtra : Dword;
		hInstance, hIcon, hCursor, hbrBackground : INTEGER;
		lpszMenuName, lpszClassName : INTEGER
	END;
	
VAR
	GetModuleHandle : PROCEDURE(lpModuleName : INTEGER) : INTEGER;
	RegisterClassW : PROCEDURE(lpWndClass : INTEGER) : INTEGER;
	CreateWindowW : PROCEDURE(lpClassName, lpWindowName, dwStyle, x, y,
	                          nWidth, nHeight, hWndParent, hMenu, hInstance,
	                          lpParam : INTEGER) : INTEGER;
	ShowWindow : PROCEDURE(hWnd, nCmdShow : INTEGER) : INTEGER;
	GetMessageW : PROCEDURE(lpMsg, hWnd, uMsgFilterMin, uMsgFilterMax : INTEGER) : INTEGER;
	TranslateMessage : PROCEDURE(lpMsg : INTEGER) : INTEGER;
	DispatchMessage : PROCEDURE(lpMsg : INTEGER) : INTEGER;
	DefWindowProc : WindowProcType;
	
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

PROCEDURE ZeroClearRecord (adr, size : INTEGER);
	VAR
		i : INTEGER;
BEGIN
	i := 0;
	WHILE i < size DO
		PUT (adr + i, VAL(0, BYTE));
		i := i + 1
	END
END ZeroClearRecord;

PROCEDURE InitLibrary;
	VAR
		s : AnsiStr;
		kernel32, user32 : INTEGER;
BEGIN
	LoadLibrary (kernel32, 'kernel32.dll');
	LoadLibrary (user32, 'user32.dll');
	
	MakeAnsiStr (s, 'GetModuleHandle');
	GetProcAddress (GetModuleHandle, kernel32, ADR(s));
	MakeAnsiStr (s, 'RegisterClassW');
	GetProcAddress (RegisterClassW, user32, ADR(s));
	MakeAnsiStr (s, 'CreateWindowW');
	GetProcAddress (CreateWindowW, user32, ADR(s));
	MakeAnsiStr (s, 'ShowWindow');
	GetProcAddress (ShowWindow, user32, ADR(s));
	MakeAnsiStr (s, 'GetMessageW');
	GetProcAddress (GetMessageW, user32, ADR(s));
	MakeAnsiStr (s, 'TranslateMessage');
	GetProcAddress (TranslateMessage, user32, ADR(s));
	MakeAnsiStr (s, 'DispatchMessage');
	GetProcAddress (DispatchMessage, user32, ADR(s));
	MakeAnsiStr (s, 'DefWindowProc');
	GetProcAddress (DefWindowProc, user32, ADR(s))
END InitLibrary;

PROCEDURE Main;
	VAR
		s : AnsiStr;
		winclass : WNDCLASSWrecord;
BEGIN
	ZeroClearRecord (ADR(winclass), SIZE(WNDCLASSWrecord))
END Main;

BEGIN
InitLibrary;
Main
END Test2.
