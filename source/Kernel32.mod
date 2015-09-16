DEFINITION Kernel32;

CONST
	(* Standard devices *)
	STD_INPUT_HANDLE* = -10;
	STD_OUTPUT_HANDLE* = -11;
	STD_ERROR_HANDLE* = -12;
	
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
	BOOL* = CARD32;
	HANDLE* = INTEGER;
	LPVOID* = INTEGER;
	PVOID* = INTEGER;
	
	LPWSTR* = POINTER TO ARRAY OF CHAR;
	
	CONSOLE_READCONSOLE_CONTROL* = RECORD
		nLength*, nInitialChars*: CARD32;
		dwCtrlWakeupMask*, dwControlKeyState*: CARD32
	END;
	
	LARGE_INTEGER* = RECORD
		UNION
			LowPart*: CARD32; HighPart*: INT32 |
			u*: RECORD LowPart*: CARD32; HighPart*: INT32 END |
			QuadPart*: INTEGER
		END
	END;
	
	SECURITY_ATTRIBUTES* = RECORD
		nLength*: CARD32;
		lpSecurityDescriptor*: LPVOID;
		bInheritHandle*: BOOL
	END;
	
	OVERLAPPED* = RECORD
		Internal*, InternalHigh*: INTEGER;
		UNION
			Offset*, OffsetHigh*: CARD32 |
			Pointer*: PVOID
		END;
		hEvent*: HANDLE
	END;

(* Time Functions *)
PROCEDURE GetTickCount*(): CARD32;

(* Console functions *)
PROCEDURE AllocConsole*(): BOOL;
PROCEDURE GetStdHandle*(nStdHandle: CARD32): HANDLE;
	
PROCEDURE WriteConsoleW*(
	hConsoleOutput: HANDLE;
	lpBuffer: ARRAY OF BYTE;
	nNumberOfCharsToWrite: CARD32;
	VAR lpNumberOfCharsWritten: CARD32;
	VAR [NIL] lpReserved: ARRAY OF BYTE
): CARD32;
	
PROCEDURE ReadConsoleW*(
	hConsoleInput: HANDLE;
	VAR lpBuffer: ARRAY OF BYTE;
	nNumberOfCharsToRead: CARD32;
	VAR lpNumberOfCharsRead: CARD32;
	VAR [NIL] pInputConsole: CONSOLE_READCONSOLE_CONTROL
): CARD32;
	
PROCEDURE SetConsoleCP*(wCodePageID: CARD32): BOOL;
PROCEDURE SetConsoleOutputCP*(wCodePageID: CARD32): BOOL;

(* DLL functions *)
PROCEDURE GetModuleHandleW*(lpModuleName: ARRAY OF CHAR): HANDLE;

(* Process and Thread functions *)
PROCEDURE GetCommandLineW*(): LPWSTR;

(* File Management functions *)
PROCEDURE GetFileAttributesW*(lpFilename: ARRAY OF CHAR): CARD32;
PROCEDURE MoveFileW*(lpExistingFileName, lpNewFileName: ARRAY OF CHAR): BOOL;
PROCEDURE DeleteFileW*(lpFilename: ARRAY OF CHAR): BOOL;

PROCEDURE SetFilePointerEx*(
	hFile: HANDLE;
	liDistanceToMove: LARGE_INTEGER;
	VAR [NIL] lpNewFilePointer: LARGE_INTEGER;
	dwMoveMethod: CARD32
): BOOL;

PROCEDURE CreateFileW*(
	lpFilename: ARRAY OF CHAR;
	dwDesiredAccess, dwShareMode: CARD32;
	VAR [NIL] lpSecurityAttributes: SECURITY_ATTRIBUTES;
	dwCreationDisposition, dwFlagsAndAttributes: CARD32;
	hTemplateFile: HANDLE
): HANDLE;

PROCEDURE ReadFile*(
	hFile: HANDLE;
	VAR lpBuffer: ARRAY OF BYTE;
	nNumberOfBytesToRead: CARD32;
	VAR [NIL] lpNumberOfBytesRead: CARD32;
	VAR [NIL] lpOverlapped: OVERLAPPED
): BOOL;

PROCEDURE WriteFile*(
	hFile: HANDLE;
	lpBuffer: ARRAY OF BYTE;
	nNumberOfBytesToWrite: CARD32;
	VAR [NIL] lpNumberOfBytesWrite: CARD32;
	VAR [NIL] lpOverlapped: OVERLAPPED
): BOOL;

(* Handle and Object functions *)
PROCEDURE CloseHandle*(hObject: HANDLE): BOOL;

END Kernel32.