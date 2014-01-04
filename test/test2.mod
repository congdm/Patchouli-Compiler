MODULE Test2;

CONST
	WM_DESTROY = 2;
	WM_CLOSE = 16;
	
	WS_OVERLAPPED = {};
	WS_TILED = WS_OVERLAPPED;
	WS_CAPTION = {22, 23};
	WS_SYSMENU = {19};
	WS_THICKFRAME = {18};
	WS_MINIMIZEBOX = {17};
	WS_MAXIMIZEBOX = {16};
	WS_OVERLAPPEDWINDOW = WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU
	                      + WS_THICKFRAME + WS_MINIMIZEBOX + WS_MAXIMIZEBOX;
	WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW;

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
	
	WindowProcType = PROCEDURE (hWnd, Msg, wParam, lParam : INTEGER) : INTEGER;
	
	WNDCLASSrecord = RECORD
		style : Dword;
		lpfnWndProc : WindowProcType;
		cbClsExtra, cbWndExtra : Dword;
		hInstance, hIcon, hCursor, hbrBackground : INTEGER;
		lpszMenuName, lpszClassName : INTEGER
	END;
	
VAR
	GetModuleHandleW : PROCEDURE(lpModuleName : INTEGER) : INTEGER;
	RegisterClassW : PROCEDURE (lpWndClass : INTEGER) : INTEGER;
	CreateWindowExW : PROCEDURE (dwExStyle, lpClassName, lpWindowName, dwStyle,
	                             x, y, nWidth, nHeight, hWndParent, hMenu,
	                             hInstance, lpParam : INTEGER) : INTEGER;
	ShowWindow : PROCEDURE (hWnd, nCmdShow : INTEGER) : INTEGER;
	GetMessageW : PROCEDURE (lpMsg, hWnd, uMsgFilterMin, uMsgFilterMax : INTEGER) : INTEGER;
	TranslateMessage : PROCEDURE (lpMsg : INTEGER) : INTEGER;
	DispatchMessageW : PROCEDURE (lpMsg : INTEGER) : INTEGER;
	DefWindowProcW : WindowProcType;
	DestroyWindow : PROCEDURE (hWnd : INTEGER) : INTEGER;
	PostQuitMessage : PROCEDURE (nExitCode : INTEGER);
	
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
	
	MakeAnsiStr (s, 'GetModuleHandleW');
	GetProcAddress (GetModuleHandleW, kernel32, ADR(s));
	MakeAnsiStr (s, 'RegisterClassW');
	GetProcAddress (RegisterClassW, user32, ADR(s));
	MakeAnsiStr (s, 'CreateWindowExW');
	GetProcAddress (CreateWindowExW, user32, ADR(s));
	MakeAnsiStr (s, 'ShowWindow');
	GetProcAddress (ShowWindow, user32, ADR(s));
	MakeAnsiStr (s, 'GetMessageW');
	GetProcAddress (GetMessageW, user32, ADR(s));
	MakeAnsiStr (s, 'TranslateMessage');
	GetProcAddress (TranslateMessage, user32, ADR(s));
	MakeAnsiStr (s, 'DispatchMessageW');
	GetProcAddress (DispatchMessageW, user32, ADR(s));
	MakeAnsiStr (s, 'DefWindowProcW');
	GetProcAddress (DefWindowProcW, user32, ADR(s));
	MakeAnsiStr (s, 'DestroyWindow');
	GetProcAddress (DestroyWindow, user32, ADR(s));
	MakeAnsiStr (s, 'PostQuitMessage');
	GetProcAddress (PostQuitMessage, user32, ADR(s))
END InitLibrary;

PROCEDURE WndProc (hWnd, Msg, wParam, lParam : INTEGER) : INTEGER;
	VAR
		r, res : INTEGER;
BEGIN
	r := 0;
	IF Msg = WM_CLOSE THEN
		res := DestroyWindow (hWnd)
	ELSIF Msg = WM_DESTROY THEN
		PostQuitMessage (0)
	ELSE
		r := DefWindowProcW (hWnd, Msg, wParam, lParam)
	END
RETURN r
END WndProc;

PROCEDURE Main;
	VAR
		hInstance, className, res, res2, hWnd : INTEGER;
		winclass : WNDCLASSrecord;
		msg : MSGrecord;
BEGIN
	hInstance := GetModuleHandleW(0);
	ZeroClearRecord (ADR(winclass), SIZE(WNDCLASSrecord));

	className := ADR('MyClass');
	winclass.lpfnWndProc := WndProc;
	winclass.hInstance := hInstance;
	winclass.lpszClassName := className;
	winclass.hbrBackground := 1;
	
	res := RegisterClassW (ADR(winclass));
	res := res MOD 65536;
	IF res # 0 THEN
		hWnd := CreateWindowExW (0, className, ADR('MyWindow'), ORD(WS_TILEDWINDOW),
		                         0, 0, 640, 480, 0, 0, hInstance, 0);
		res := hWnd
	END;
	
	IF res # 0 THEN
		res := ShowWindow (hWnd, 1);
		REPEAT
			res := GetMessageW (ADR(msg), 0, 0, 0);
			IF res > 0 THEN
				res2 := TranslateMessage (ADR(msg));
				res2 := DispatchMessageW (ADR(msg))
			ELSIF res < 0 THEN
			END
		UNTIL res <= 0
	END
END Main;

BEGIN
InitLibrary;
Main
END Test2.
