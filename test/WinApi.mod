MODULE WinApi;

IMPORT
	SYSTEM;
	
CONST
	STD_INPUT_HANDLE* = -10;
	STD_OUTPUT_HANDLE* = -11;
	STD_ERROR_HANDLE* = -12;
	
	(* Window message *)
	WM_NULL* = 0;
	WM_CREATE* = 1;
	WM_DESTROY* = 2;
	WM_CLOSE* = 16;
	WM_KEYDOWN* = 256;
	WM_KEYUP* = 257;
	
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
	
	(* Virtual-Key Codes *)
	VK_SHIFT* = 10H;
	VK_CONTROL* = 11H;
	VK_LEFT* = 25H;
	VK_UP* = 26H;
	VK_RIGHT* = 27H;
	VK_DOWN* = 28H;
	
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	
	Handle* = ARRAY 1 OF INTEGER;
	Bool* = ARRAY 1 OF SYSTEM.DWORD;
	Address* = ARRAY 1 OF INTEGER;
	
	WindowProc* = PROCEDURE (
		hWnd : Handle; uMsg, wParam, lParam : INTEGER
	) : INTEGER;
	
	Point* = RECORD
		x*, y* : SYSTEM.DWORD
	END;
	
	Msg* = RECORD
		hwnd* : Handle;
		message* : SYSTEM.DWORD;
		wParam*, lParam* : INTEGER;
		time* : SYSTEM.DWORD;
		pt* : Point
	END;
	
	Wndclassw* = RECORD
		style* : SYSTEM.DWORD;
		lpfnWndProc* : WindowProc;
		cbClsExtra*, cbWndExtra* : SYSTEM.DWORD;
		hInstance*, hIcon*, hCursor*, hbrBackground* : Handle;
		lpszMenuName*, lpszClassName* : Address
	END;
	
	Rect* = RECORD
		left*, right*, top*, bottom* : SYSTEM.DWORD
	END;
	
	Paintstruct* = RECORD
		hdc* : Handle;
		fErase* : Bool;
		rcPaint* : Rect;
		fRestore*, fIncUpdate* : Bool;
		rgbReserved* : ARRAY 32 OF BYTE
	END;
	
	Console_readconsole_control* = RECORD
		nLength*, nInitialChars* : SYSTEM.DWORD;
		dwCtrlWakeupMask*, dwControlKeyState* : SYSTEM.DWORD
	END;
	
VAR
	(* Console functions *)
	AllocConsole* : PROCEDURE () : Bool;
	GetStdHandle* : PROCEDURE (nStdHandle : SYSTEM.DWORD) : Handle;
	
	WriteConsoleW* : PROCEDURE (
		hConsoleOutput : Handle;
		lpBuffer : Address;
		nNumberOfCharsToWrite : SYSTEM.DWORD;
		lpNumberOfCharsWritten, lpReserved : Address
	) : Bool;
	
	ReadConsoleW* : PROCEDURE (
		hConsoleInput : Handle;
		lpBuffer : Address;
		nNumberOfCharsToRead : SYSTEM.DWORD;
		lpNumberOfCharsRead, pInputConsole : Address
	) : Bool;
	
	SetConsoleCP* : PROCEDURE (wCodePageID : SYSTEM.DWORD) : Bool;
	SetConsoleOutputCP* : PROCEDURE (wCodePageID : SYSTEM.DWORD) : Bool;
	
	(* DLL functions *)
	GetModuleHandleW* : PROCEDURE (lpModuleName : Address) : Handle;
	
	(* Window Class functions *)
	RegisterClassW* : PROCEDURE (lpWndClass : Address) : SYSTEM.WORD;
	
	(* Window functions *)
	CreateWindowExW* : PROCEDURE (
		dwExStyle : SYSTEM.DWORD;
		lpClassName, lpWindowName : Address;
		dwStyle, x, y, nWidth, nHeight : SYSTEM.DWORD;
		hWndParent, hMenu, hInstance : Handle;
		lpParam : Address
	) : Handle;
	
	ShowWindow* : PROCEDURE (hWnd : Handle; nCmdShow : SYSTEM.DWORD) : Bool;
	DestroyWindow* : PROCEDURE (hWnd : Handle) : Bool;
	
	(* Window Procedure functions *)
	DefWindowProcW* : WindowProc;
	
	(* Message functions *)
	GetMessageW* : PROCEDURE (
		lpMsg : Address;
		hWnd : Handle;
		uMsgFilterMin, uMsgFilterMax : SYSTEM.DWORD
	) : Bool;
	
	TranslateMessage* : PROCEDURE (lpMsg : Address) : Bool;
	DispatchMessageW* : PROCEDURE (lpMsg : Address) : Bool;
	PostQuitMessage* : PROCEDURE (nExitCode : SYSTEM.DWORD);
	
	(* Dialog Box functions *)
	MessageBoxW* : PROCEDURE (
		hwnd : Handle;
		lpText, lpCaption : Address;
		uType : SYSTEM.DWORD
	) : SYSTEM.DWORD;
	
	(* Painting and Drawing functions *)
	BeginPaint* : PROCEDURE (hwnd : Handle; lpPaint : Address) : Handle;
	
PROCEDURE Make_AsciiStr* (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
	VAR n, i : INTEGER;
BEGIN
	n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
	i := 0; WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
END Make_AsciiStr;

PROCEDURE Adr* (v : ARRAY OF BYTE) : Address;
	VAR res : Address;
BEGIN
	res[0] := SYSTEM.ADR (v)
	RETURN res
END Adr;

PROCEDURE IsTrue* (bool : Bool) : BOOLEAN;
BEGIN
	RETURN bool[0] # 0
END IsTrue;

PROCEDURE Init;
	VAR user32, kernel32 : INTEGER; str : AsciiStr; 
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