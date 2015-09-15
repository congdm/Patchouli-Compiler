LIBRARY Kernel32;

TYPE
	BOOL* = CARD32;
	HANDLE* = INTEGER;
	PVOID* = INTEGER;
	
	CONSOLE_READCONSOLE_CONTROL* = RECORD
		nLength*, nInitialChars*: CARD32;
		dwCtrlWakeupMask*, dwControlKeyState*: CARD32
	END;

(* Time Functions *)
PROCEDURE GetTickCount*(): CARD32;

(* Console functions *)
PROCEDURE AllocConsole*(): BOOL;
PROCEDURE GetStdHandle*(nStdHandle: CARD32): HANDLE;
	
PROCEDURE WriteConsoleW*(
	hConsoleOutput: HANDLE;
	VAR lpBuffer: ARRAY OF BYTE;
	nNumberOfCharsToWrite: CARD32;
	VAR lpNumberOfCharsWritten: CARD32;
	VAR [NIL] lpReserved: 
): CARD32;
	
PROCEDURE ReadConsoleW*(
	hConsoleInput: HANDLE;
	VAR lpBuffer: ARRAY OF BYTE;
	nNumberOfCharsToRead: CARD32;
	VAR lpNumberOfCharsRead: CARD32;
	VAR [NIL] pInputConsole: PVOID
): CARD32;
	
PROCEDURE SetConsoleCP*(wCodePageID: CARD32): BOOL;
PROCEDURE SetConsoleOutputCP*(wCodePageID: CARD32): BOOL;

(* DLL functions *)
PROCEDURE GetModuleHandleW*(lpModuleName: ARRAY OF CHAR): HANDLE;

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

END Kernel32.