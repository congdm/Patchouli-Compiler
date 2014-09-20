MODULE WinApi;

IMPORT
	SYSTEM;
	
CONST
	STD_INPUT_HANDLE* = -10;
	STD_OUTPUT_HANDLE* = -11;
	STD_ERROR_HANDLE* = -12;
	
	(* Window message *)
	WM_DESTROY* = 2;
	WM_CLOSE* = 16;
	
	(* Window style *)
	WS_OVERLAPPED* = {};
	WS_TILED* = WS_OVERLAPPED;
	WS_BORDER* = {22};
	WS_CAPTION* = WS_BORDER + {23};
	WS_SYSMENU* = {19};
	WS_THICKFRAME* = {18};
	WS_MINIMIZEBOX* = {17};
	WS_MAXIMIZEBOX* = {16};
	WS_OVERLAPPEDWINDOW* = WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU
	                      + WS_THICKFRAME + WS_MINIMIZEBOX + WS_MAXIMIZEBOX;
	WS_TILEDWINDOW* = WS_OVERLAPPEDWINDOW;
	
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	
	Point* = RECORD
		x*, y* : SYSTEM.DWORD
	END;
	
	Msg* = RECORD
		hwnd* : INTEGER;
		message* : SYSTEM.DWORD;
		wParam*, lParam* : INTEGER;
		time* : SYSTEM.DWORD;
		pt* : Point
	END;
	
	WindowProcType* = PROCEDURE (
		hWnd, uMsg, wParam, lParam : INTEGER
	) : INTEGER;
	
	Wndclass* = RECORD
		style* : SYSTEM.DWORD;
		lpfnWndProc* : WindowProcType;
		cbClsExtra*, cbWndExtra* : SYSTEM.DWORD;
		hInstance*, hIcon*, hCursor*, hbrBackground* : INTEGER;
		lpszMenuName*, lpszClassName* : INTEGER
	END;
	
VAR
	(* Console functions *)
	AllocConsole* : PROCEDURE () : INTEGER;
	GetStdHandle* : PROCEDURE (nStdHandle : INTEGER) : INTEGER;
	WriteConsoleW* : PROCEDURE (
		hConsoleOutput, lpBuffer, nNumberOfCharsToWrite,
		lpNumberOfCharsWritten, lpReserved : INTEGER
	) : INTEGER;
	ReadConsoleW* : PROCEDURE (
		hConsoleInput, lpBuffer, nNumberOfCharsToRead, lpNumberOfCharsRead, 
		pInputConsole : INTEGER
	) : INTEGER;
	SetConsoleCP* : PROCEDURE (wCodePageID : INTEGER) : INTEGER;
	SetConsoleOutputCP* : PROCEDURE (wCodePageID : INTEGER) : INTEGER;
	
	(* DLL functions *)
	GetModuleHandleW* : PROCEDURE (lpModuleName : INTEGER) : INTEGER;
	
	(* Window Class functions *)
	RegisterClassW* : PROCEDURE (lpWndClass : INTEGER) : INTEGER;
	
	(* Window functions *)
	CreateWindowExW* : PROCEDURE (
		dwExStyle, lpClassName, lpWindowName, dwStyle,
		x, y, nWidth, nHeight,
		hWndParent, hMenu, hInstance, lpParam : INTEGER
	) : INTEGER;
	ShowWindow* : PROCEDURE (hWnd, nCmdShow : INTEGER) : INTEGER;
	DestroyWindow* : PROCEDURE (hWnd : INTEGER) : INTEGER;
	
	(* Window Procedure functions *)
	DefWindowProcW* : WindowProcType;
	
	(* Message functions *)
	GetMessageW* : PROCEDURE (
		lpMsg, hWnd, uMsgFilterMin, uMsgFilterMax : INTEGER
	) : INTEGER;
	TranslateMessage* : PROCEDURE (lpMsg : INTEGER) : INTEGER;
	DispatchMessageW* : PROCEDURE (lpMsg : INTEGER) : INTEGER;
	PostQuitMessage* : PROCEDURE (nExitCode : INTEGER);
	
	(* Dialog Box functions *)
	MessageBoxW* : PROCEDURE (hwnd, lpText, lpCaption, uType : INTEGER);
	
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
	SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	
	Make_AsciiStr (str, 'AllocConsole');
	SYSTEM.GetProcAddress (AllocConsole, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'GetStdHandle');
	SYSTEM.GetProcAddress (GetStdHandle, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'WriteConsoleW');
	SYSTEM.GetProcAddress (WriteConsoleW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'ReadConsoleW');
	SYSTEM.GetProcAddress (ReadConsoleW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'SetConsoleCP');
	SYSTEM.GetProcAddress (SetConsoleCP, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'SetConsoleOutputCP');
	SYSTEM.GetProcAddress (SetConsoleOutputCP, kernel32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'GetModuleHandleW');
	SYSTEM.GetProcAddress (GetModuleHandleW, kernel32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'RegisterClassW');
	SYSTEM.GetProcAddress (RegisterClassW, user32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'CreateWindowExW');
	SYSTEM.GetProcAddress (CreateWindowExW, user32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'ShowWindow');
	SYSTEM.GetProcAddress (ShowWindow, user32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'DestroyWindow');
	SYSTEM.GetProcAddress (DestroyWindow, user32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'DefWindowProcW');
	SYSTEM.GetProcAddress (DefWindowProcW, user32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'GetMessageW');
	SYSTEM.GetProcAddress (GetMessageW, user32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'TranslateMessage');
	SYSTEM.GetProcAddress (TranslateMessage, user32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'DispatchMessageW');
	SYSTEM.GetProcAddress (DispatchMessageW, user32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'PostQuitMessage');
	SYSTEM.GetProcAddress (PostQuitMessage, user32, SYSTEM.ADR(str));

	Make_AsciiStr (str, 'MessageBoxW');
	SYSTEM.GetProcAddress (MessageBoxW, user32, SYSTEM.ADR(str))
END Init;
	
BEGIN
	Init
END WinApi.