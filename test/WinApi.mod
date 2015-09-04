MODULE WinApi;

IMPORT
	S := SYSTEM, T := WinBase;
	
VAR
	(* Time functions *)
	GetTickCount*: PROCEDURE (): T.DWORD;

	(* Console functions *)
	AllocConsole*: PROCEDURE (): T.BOOL;
	GetStdHandle*: PROCEDURE (nStdHandle: T.DWORD): T.HANDLE;
	
	WriteConsoleW*: PROCEDURE (
		hConsoleOutput: T.HANDLE;
		lpBuffer : T.LPVOID;
		nNumberOfCharsToWrite: T.DWORD;
		lpNumberOfCharsWritten: T.LPDWORD;
		lpReserved : T.LPVOID
	): T.BOOL;
	
	ReadConsoleW*: PROCEDURE (
		hConsoleInput: T.HANDLE;
		lpBuffer: T.LPVOID;
		nNumberOfCharsToRead: T.DWORD;
		lpNumberOfCharsRead: T.LPDWORD;
		pInputConsole: T.PCONSOLE_READCONSOLE_CONTROL
	): T.BOOL;
	
	SetConsoleCP*: PROCEDURE (wCodePageID: T.UINT): T.BOOL;
	SetConsoleOutputCP*: PROCEDURE (wCodePageID: T.UINT): T.BOOL;
	
	(* DLL functions *)
	GetModuleHandleW*: PROCEDURE (lpModuleName: T.LPCWSTR): T.HMODULE;
	
	(* Process and Thread functions *)
	GetCommandLineW*: PROCEDURE (): T.LPWSTR;
	
	(* File Management functions *)
	GetFileAttributesW*: PROCEDURE (lpFilename: T.LPCWSTR): T.DWORD;
	
	MoveFileW*: PROCEDURE (
		lpExistingFileName, lpNewFileName: T.LPCWSTR
	): T.BOOL;
	
	DeleteFileW*: PROCEDURE (
		lpFilename: T.LPCWSTR
	): T.BOOL;
	
	SetFilePointerEx*: PROCEDURE (
		hFile: T.HANDLE;
		liDistanceToMove: T.LARGE_INTEGER;
		lpNewFilePointer: T.PLARGE_INTEGER;
		dwMoveMethod: T.DWORD
	): T.BOOL;
	
	CreateFileW*: PROCEDURE (
		lpFilename: T.LPCWSTR;
		dwDesiredAccess, dwShareMode: T.DWORD;
		lpSecurityAttributes: T.LPSECURITY_ATTRIBUTES;
		dwCreationDisposition, dwFlagsAndAttributes: T.DWORD;
		hTemplateFile: T.HANDLE
	): T.HANDLE;
	
	ReadFile*: PROCEDURE (
		hFile: T.HANDLE;
		lpBuffer: T.LPVOID;
		nNumberOfBytesToRead: T.DWORD;
		lpNumberOfBytesRead: T.LPDWORD;
		lpOverlapped: T.LPOVERLAPPED
	): T.BOOL;
	
	WriteFile*: PROCEDURE (
		hFile: T.HANDLE;
		lpBuffer: T.LPVOID;
		nNumberOfBytesToWrite: T.DWORD;
		lpNumberOfBytesWrite: T.LPDWORD;
		lpOverlapped: T.LPOVERLAPPED
	): T.BOOL;
	
	(* Handle and Object functions *)
	CloseHandle*: PROCEDURE (hObject: T.HANDLE): T.BOOL;
	
	(* Window Class functions *)
	RegisterClassW*: PROCEDURE (lpWndClass: T.LPWNDCLASSW): T.ATOM;
	
	(* Window functions *)
	CreateWindowExW*: PROCEDURE (
		dwExStyle: T.DWORD;
		lpClassName, lpWindowName: T.LPCWSTR;
		dwStyle: T.DWORD;
		x, y, nWidth, nHeight: INT32;
		hWndParent: T.HWND;
		hMenu: T.HMENU;
		hInstance: T.HINSTANCE;
		lpParam: T.LPVOID
	): T.HWND;
	
	ShowWindow*: PROCEDURE (hWnd: T.HWND; nCmdShow: INT32): T.BOOL;
	DestroyWindow*: PROCEDURE (hWnd: T.HWND): T.BOOL;
	
	(* Window Procedure functions *)
	DefWindowProcW*: T.WNDPROC;
	
	(* Message functions *)
	GetMessageW*: PROCEDURE (
		lpMsg: T.LPMSG;
		hWnd: T.HWND;
		uMsgFilterMin, uMsgFilterMax: T.UINT
	): T.BOOL;
	
	TranslateMessage*: PROCEDURE (lpMsg: T.LPMSG): T.BOOL;
	DispatchMessageW*: PROCEDURE (lpMsg: T.LPMSG): T.BOOL;
	PostQuitMessage*: PROCEDURE (nExitCode: INT32);
	
	(* Dialog Box functions *)
	MessageBoxW*: PROCEDURE (
		hwnd: T.HWND;
		lpText, lpCaption: T.LPCWSTR;
		uType: T.UINT
	): INT32;
	
	(* Painting and Drawing functions *)
	BeginPaint*: PROCEDURE (hwnd: T.HWND; lpPaint: T.LPPAINTSTRUCT): T.HDC;

PROCEDURE Init;
	VAR user32, kernel32: T.HMODULE; 
BEGIN
	S.LoadLibraryW (kernel32, 'KERNEL32.DLL');
	S.LoadLibraryW (user32, 'USER32.DLL');
	
	S.GetProcAddress (GetTickCount, kernel32, 'GetTickCount'@);
	
	S.GetProcAddress (AllocConsole, kernel32, 'AllocConsole'@);
	S.GetProcAddress (GetStdHandle, kernel32, 'GetStdHandle'@);
	S.GetProcAddress (WriteConsoleW, kernel32, 'WriteConsoleW'@);
	S.GetProcAddress (ReadConsoleW, kernel32, 'ReadConsoleW'@);
	S.GetProcAddress (SetConsoleCP, kernel32, 'SetConsoleCP'@);
	S.GetProcAddress (SetConsoleOutputCP, kernel32, 'SetConsoleOutputCP'@);

	S.GetProcAddress (GetCommandLineW, kernel32, 'GetCommandLineW'@);
	
	S.GetProcAddress (GetModuleHandleW, kernel32, 'GetModuleHandleW'@);
	
	S.GetProcAddress (GetFileAttributesW, kernel32, 'GetFileAttributesW'@);
	S.GetProcAddress (CreateFileW, kernel32, 'CreateFileW'@);
	S.GetProcAddress (MoveFileW, kernel32, 'MoveFileW'@);
	S.GetProcAddress (DeleteFileW, kernel32, 'DeleteFileW'@);
	S.GetProcAddress (ReadFile, kernel32, 'ReadFile'@);
	S.GetProcAddress (WriteFile, kernel32, 'WriteFile'@);
	S.GetProcAddress (SetFilePointerEx, kernel32, 'SetFilePointerEx'@);
	
	S.GetProcAddress (CloseHandle, kernel32, 'CloseHandle'@);
	
	S.GetProcAddress (RegisterClassW, user32, 'RegisterClassW'@);
	
	S.GetProcAddress (CreateWindowExW, user32, 'CreateWindowExW'@);
	S.GetProcAddress (ShowWindow, user32, 'ShowWindow'@);
	S.GetProcAddress (DestroyWindow, user32, 'DestroyWindow'@);
	
	S.GetProcAddress (DefWindowProcW, user32, 'DefWindowProcW'@);
	
	S.GetProcAddress (GetMessageW, user32, 'GetMessageW'@);
	S.GetProcAddress (TranslateMessage, user32, 'TranslateMessage'@);
	S.GetProcAddress (DispatchMessageW, user32, 'DispatchMessageW'@);
	S.GetProcAddress (PostQuitMessage, user32, 'PostQuitMessage'@);

	S.GetProcAddress (MessageBoxW, user32, 'MessageBoxW'@)
END Init;
	
BEGIN Init
END WinApi.