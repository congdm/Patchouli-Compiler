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
	
	(* File attributes *)
	INVALID_FILE_ATTRIBUTES* = {0..31};
	
	(* Generic access rights *)
	GENERIC_ALL* = {28};
	GENERIC_READ* = {31};
	GENERIC_WRITE* = {30};
	GENERIC_EXECUTE* = {29};
	
	(* File share mode *)
	FILE_SHARE_DELETE* = {2};
	FILE_SHARE_READ* = {0};
	FILE_SHARE_WRITE* = {1};
	
	(* File creation disposition *)
	CREATE_ALWAYS* = 2;
	CREATE_NEW* = 1;
	OPEN_ALWAYS* = 4;
	OPEN_EXISTING* = 3;
	TRUNCATE_EXISTING* = 5;
	
	(* File move mode *)
	FILE_BEGIN* = 0;
	FILE_CURRENT* = 1;
	FILE_END* = 2;
	
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	
	Handle* = INTEGER;
	Bool* = CARD32;
	Dword* = CARD32;
	Word* = CARD16;
	Lpvoid* = INTEGER;
	Long_ptr* = INTEGER;
	Ulong_ptr* = INTEGER;
	
	PChar* = ADDRESS OF CHAR;
	PDword* = ADDRESS OF Dword;
	
	WindowProc* = PROCEDURE (
		hWnd : Handle; uMsg, wParam, lParam : INTEGER
	) : INTEGER;
	
	Point* = RECORD
		x*, y* : Dword
	END;
	
	Msg* = RECORD
		hwnd* : Handle;
		message* : Dword;
		wParam*, lParam* : INTEGER;
		time* : Dword;
		pt* : Point
	END;
	PMsg* = ADDRESS OF Msg;
	
	Wndclassw* = RECORD
		style* : Dword;
		lpfnWndProc* : WindowProc;
		cbClsExtra*, cbWndExtra* : Dword;
		hInstance*, hIcon*, hCursor*, hbrBackground* : Handle;
		lpszMenuName*, lpszClassName* : PChar
	END;
	PWndclassw* = ADDRESS OF Wndclassw;
	
	Rect* = RECORD
		left*, right*, top*, bottom* : Dword
	END;
	
	Paintstruct* = RECORD
		hdc* : Handle;
		fErase* : Bool;
		rcPaint* : Rect;
		fRestore*, fIncUpdate* : Bool;
		rgbReserved* : ARRAY 32 OF BYTE
	END;
	PPaintstruct* = ADDRESS OF Paintstruct;
	
	Console_readconsole_control* = RECORD
		nLength*, nInitialChars* : Dword;
		dwCtrlWakeupMask*, dwControlKeyState* : Dword
	END;
	PConsole_readconsole_control* = ADDRESS OF Console_readconsole_control;
	
	Security_attributes* = RECORD
		nLength*: Dword;
		lpSecurityDescriptor*: Lpvoid;
		bInheritHandle*: Bool
	END;
	PSecurity_attributes* = ADDRESS OF Security_attributes;
	
	Overlapped* = RECORD
		Internal*, InternalHigh*: Ulong_ptr;
		Offset*, OffsetHigh*: Dword;
		hEvent*: Handle
	END;
	POverlapped* = ADDRESS OF Overlapped;
	
	Large_integer* = RECORD
		QuadPart*: INTEGER
	END;
	PLarge_integer* = ADDRESS OF Large_integer;
	
VAR
	(* Console functions *)
	AllocConsole* : PROCEDURE () : Bool;
	GetStdHandle* : PROCEDURE (nStdHandle : Dword) : Handle;
	
	WriteConsoleW* : PROCEDURE (
		hConsoleOutput : Handle;
		lpBuffer : Lpvoid;
		nNumberOfCharsToWrite : Dword;
		lpNumberOfCharsWritten : PDword;
		lpReserved : Lpvoid
	) : Bool;
	
	ReadConsoleW* : PROCEDURE (
		hConsoleInput : Handle;
		lpBuffer : Lpvoid;
		nNumberOfCharsToRead : Dword;
		lpNumberOfCharsRead : PDword;
		pInputConsole : PConsole_readconsole_control
	) : Bool;
	
	SetConsoleCP* : PROCEDURE (wCodePageID : Dword) : Bool;
	SetConsoleOutputCP* : PROCEDURE (wCodePageID : Dword) : Bool;
	
	(* DLL functions *)
	GetModuleHandleW* : PROCEDURE (lpModuleName : PChar) : Handle;
	
	(* Process and Thread functions *)
	GetCommandLineW*: PROCEDURE(): PChar;
	
	(* File Management functions *)
	GetFileAttributesW* : PROCEDURE (lpFilename: PChar) : Dword;
	MoveFileW*: PROCEDURE (lpExistingFileName, lpNewFileName: PChar) : Bool;
	DeleteFileW*: PROCEDURE (lpFilename: PChar) : Bool;
	
	SetFilePointerEx*: PROCEDURE (
		hFile: Handle;
		liDistanceToMove: Large_integer;
		lpNewFilePointer: PLarge_integer;
		dwMoveMethod: Dword
	) : Bool;
	
	CreateFileW* : PROCEDURE (
		lpFilename : PChar;
		dwDesiredAccess, dwShareMode : Dword;
		lpSecurityAttributes: PSecurity_attributes;
		dwCreationDisposition, dwFlagsAndAttributes: Dword;
		hTemplateFile: Handle
	) : Handle;
	
	ReadFile*: PROCEDURE (
		hFile: Handle;
		lpBuffer: Lpvoid;
		nNumberOfBytesToRead: Dword;
		lpNumberOfBytesRead: PDword;
		lpOverlapped: POverlapped
	) : Bool;
	
	WriteFile*: PROCEDURE (
		hFile: Handle;
		lpBuffer: Lpvoid;
		nNumberOfBytesToWrite: Dword;
		lpNumberOfBytesWrite: PDword;
		lpOverlapped: POverlapped
	) : Bool;
	
	(* Handle and Object functions *)
	CloseHandle*: PROCEDURE (hObject: Handle) : Bool;
	
	(* Window Class functions *)
	RegisterClassW* : PROCEDURE (lpWndClass : PWndclassw) : SYSTEM.WORD;
	
	(* Window functions *)
	CreateWindowExW* : PROCEDURE (
		dwExStyle : Dword;
		lpClassName, lpWindowName : PChar;
		dwStyle, x, y, nWidth, nHeight : Dword;
		hWndParent, hMenu, hInstance : Handle;
		lpParam : Lpvoid
	) : Handle;
	
	ShowWindow* : PROCEDURE (hWnd : Handle; nCmdShow : Dword) : Bool;
	DestroyWindow* : PROCEDURE (hWnd : Handle) : Bool;
	
	(* Window Procedure functions *)
	DefWindowProcW* : WindowProc;
	
	(* Message functions *)
	GetMessageW* : PROCEDURE (
		lpMsg : PMsg;
		hWnd : Handle;
		uMsgFilterMin, uMsgFilterMax : Dword
	) : Bool;
	
	TranslateMessage* : PROCEDURE (lpMsg : PMsg) : Bool;
	DispatchMessageW* : PROCEDURE (lpMsg : PMsg) : Bool;
	PostQuitMessage* : PROCEDURE (nExitCode : Dword);
	
	(* Dialog Box functions *)
	MessageBoxW* : PROCEDURE (
		hwnd : Handle;
		lpText, lpCaption : PChar;
		uType : Dword
	) : Dword;
	
	(* Painting and Drawing functions *)
	BeginPaint* : PROCEDURE (hwnd : Handle; lpPaint : PPaintstruct) : Handle;
	
	
	
PROCEDURE Make_AsciiStr* (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
	VAR n, i : INTEGER;
BEGIN
	n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
	i := 0; WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
END Make_AsciiStr;

PROCEDURE Init;
	VAR user32, kernel32 : Handle; str : AsciiStr; 
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
	
	Make_AsciiStr (str, 'GetCommandLineW');
	SYSTEM.GetProcAddress (GetCommandLineW, kernel32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'GetModuleHandleW');
	SYSTEM.GetProcAddress (GetModuleHandleW, kernel32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'GetFileAttributesW');
	SYSTEM.GetProcAddress (GetFileAttributesW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'CreateFileW');
	SYSTEM.GetProcAddress (CreateFileW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'MoveFileW');
	SYSTEM.GetProcAddress (MoveFileW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'DeleteFileW');
	SYSTEM.GetProcAddress (DeleteFileW, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'ReadFile');
	SYSTEM.GetProcAddress (ReadFile, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'WriteFile');
	SYSTEM.GetProcAddress (WriteFile, kernel32, SYSTEM.ADR(str));
	Make_AsciiStr (str, 'SetFilePointerEx');
	SYSTEM.GetProcAddress (SetFilePointerEx, kernel32, SYSTEM.ADR(str));
	
	Make_AsciiStr (str, 'CloseHandle');
	SYSTEM.GetProcAddress (CloseHandle, kernel32, SYSTEM.ADR(str));
	
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