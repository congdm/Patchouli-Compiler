MODULE Files;
IMPORT SYSTEM;

CONST
	(* Win32 Const *)
	INVALID_FILE_ATTRIBUTES = {0..31}; (* Dword value = -1 *)
	FILE_ATTRIBUTE_READONLY = 0; (* Dword value = 1 *)
	GENERIC_READ = 31; (* Dword value = 80000000H *)
	GENERIC_WRITE = 30; (* Dword value = 40000000H *)
	OPEN_EXISTING = 3;
	CREATE_ALWAYS = 2;

TYPE
	Handle = INTEGER;
	Pointer = INTEGER;
	LargeInteger = INTEGER;
	Dword = SYSTEM.CARD32;
	Bool = Dword;

	File* = POINTER TO FileDesc;
	Rider* = RECORD
		eof*: BOOLEAN; res*: INTEGER
	END;
	FileDesc = RECORD hFile: Handle END;
	
VAR
	(* Win32 Interface *)
	GetFileAttributesW: PROCEDURE(
		lpFileName: ARRAY [untagged] OF CHAR
	): Dword;
	MoveFileW: PROCEDURE(
		lpExistingFileName, lpNewFileName: ARRAY [untagged] OF CHAR
	): Bool;
	DeleteFileW: PROCEDURE(lpFilename: ARRAY [untagged] OF CHAR): Bool;
	CreateFileW: PROCEDURE(
		lpFileName: ARRAY [untagged] OF CHAR;
		dwDesiredAccess, dwShareMode: Dword;
		lpSecurityAttributes: Pointer;
		dwCreationDisposition, dwFlagsAndAttributes: Dword;
		hTemplateFile: Handle
	): Handle;
	CloseHandle: PROCEDURE(hObject: Handle): Bool;
	ReadFile: PROCEDURE(
		hFile: Handle;
		lpBuffer: Pointer;
		nNumberOfBytesToRead: Dword;
		lpNumberOfBytesRead, lpOverlapped: Pointer
	): Bool;
	WriteFile: PROCEDURE(
		hFile: Handle;
		lpBuffer: Pointer;
		nNumberOfBytesToWrite: Dword;
		lpNumberOfBytesWrite, lpOverlapped: Pointer
	): Bool;
	SetFilePointerEx: PROCEDURE(
		hFile: Handle;
		liDistanceToMove: LargeInteger;
		lpNewFilePointer: Pointer;
		dwMoveMethod: Dword
	): Bool;
	FlushFileBuffers: PROCEDURE(hFile: Handle): Bool;
	
	readOnly*: BOOLEAN;
	
PROCEDURE Old*(name: ARRAY OF CHAR): File;
	VAR file: File; hFile: Handle; attr: Dword;
BEGIN
	ASSERT(GetFileAttributesW # NIL);
	attr := GetFileAttributesW(name);
	IF attr # ORD(INVALID_FILE_ATTRIBUTES) THEN
		IF FILE_ATTRIBUTE_READONLY IN SYSTEM.VAL(SET, attr) THEN
			hFile := CreateFileW(
				name, ORD({GENERIC_READ}), 0, 0, OPEN_EXISTING, 0, 0
			)
		ELSE hFile := CreateFileW(
				name, ORD({GENERIC_READ, GENERIC_WRITE}),
				0, 0, OPEN_EXISTING, 0, 0
			)
		END;
		IF hFile # 0 THEN NEW(file); file.hFile := hFile END
	END;
	RETURN file
END Old;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Import*(VAR proc: ARRAY OF BYTE; libPath, name: ARRAY OF CHAR);
	VAR hLib, adr, i: INTEGER; byteStr: ARRAY 256 OF BYTE;
BEGIN SYSTEM.LoadLibraryW(hLib, libPath);
	IF hLib # 0 THEN i := 0;
		WHILE name[i] # 0X DO byteStr[i] := ORD(name[i]); INC(i) END;
		byteStr[i] := 0; SYSTEM.GetProcAddress(adr, hLib, SYSTEM.ADR(byteStr))
	ELSE adr := 0
	END;
	SYSTEM.PUT(SYSTEM.ADR(proc), adr)
END Import;

PROCEDURE InitWin32;
	CONST Kernel32 = 'KERNEL32.DLL';
BEGIN
	Import(GetFileAttributesW, Kernel32, 'GetFileAttributesW');
	Import(CreateFileW, Kernel32, 'CreateFileW');
	Import(CloseHandle, Kernel32, 'CloseHandle');
	Import(MoveFileW, Kernel32, 'MoveFileW');
	Import(DeleteFileW, Kernel32, 'DeleteFileW');
	Import(ReadFile, Kernel32, 'ReadFile');
	Import(WriteFile, Kernel32, 'WriteFile');
	Import(SetFilePointerEx, Kernel32, 'SetFilePointerEx');
	Import(FlushFileBuffers, Kernel32, 'FlushFileBuffers')
END InitWin32;
	
BEGIN
END Files.