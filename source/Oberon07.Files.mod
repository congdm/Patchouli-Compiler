MODULE Oberon07.Files;
IMPORT SYSTEM, Rtl;

CONST
	(* Win32 Const *)
	INVALID_FILE_ATTRIBUTES = {0..31};
	FILE_ATTRIBUTE_READONLY = {0};
	FILE_ATTRIBUTE_TEMPORARY = {8};
	FILE_FLAG_DELETE_ON_CLOSE = {26};
	
	GENERIC_READ = {31};
	GENERIC_WRITE = {30};
	
	OPEN_EXISTING = 3;
	CREATE_ALWAYS = 2;
	CREATE_NEW = 1;
	
	MAX_PATH = 260;
	
	FILE_BEGIN = 0;
	FILE_CURRENT = 1;
	FILE_END = 2;
	
	MOVEFILE_COPY_ALLOWED = {1};
	
	FILE_SHARE_DELETE = {2};
	FILE_SHARE_READ = {0};
	FILE_SHARE_WRITE = {1};

TYPE
	Handle = INTEGER;
	Pointer = INTEGER;
	LargeInteger = INTEGER;
	Dword = SYSTEM.CARD32;
	Bool = SYSTEM.CARD32;
	Int = SYSTEM.CARD32;
		
	PathStr = ARRAY MAX_PATH+1 OF CHAR;

	File* = POINTER TO FileDesc;
	Rider* = RECORD
		eof*: BOOLEAN; res*: INTEGER;
		f: File; pos: INTEGER
	END;
	FileDesc = RECORD (Rtl.Finalised)
		new, ronly: BOOLEAN; hFile: Handle;
		name, temp: PathStr; pos, len: INTEGER
	END;
	
VAR
	tempId: INTEGER;

	(* Win32 Interface *)
	GetFileAttributesW: PROCEDURE(
		lpFileName: ARRAY [untagged] OF CHAR
	): Dword;
	MoveFileExW: PROCEDURE(
		lpExistingFileName, lpNewFileName: ARRAY [untagged] OF CHAR;
		dwFlags: Dword
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
	SetEndOfFile: PROCEDURE(hFile: Handle): Bool;
	GetFileSizeEx: PROCEDURE(hFile: Handle; lpFileSize: Pointer): Bool;
	
	wsprintfW: PROCEDURE(
		VAR lpOut: ARRAY [untagged] OF CHAR;
		lpFmt: ARRAY [untagged] OF CHAR;
		par1, par2: INTEGER
	): Int;
	GetEnvironmentVariableW: PROCEDURE(
		lpName: ARRAY [untagged] OF CHAR;
		lpBuffer: Pointer;
		nSize: Dword
	): Dword;
	GetCurrentProcessId: PROCEDURE(): Dword;
	
PROCEDURE Finalise(ptr: Rtl.Finalised);
	VAR bRes: Bool; f: File;
BEGIN
	f := ptr(File); bRes := CloseHandle(f.hFile); ASSERT(bRes # 0)
END Finalise;

PROCEDURE NewFile(VAR file: File; hFile: Handle);
BEGIN
	NEW(file); file.hFile := hFile;
	Rtl.RegisterFinalised(file, Finalise)
END NewFile;
	
PROCEDURE Old*(name: ARRAY OF CHAR): File;
	VAR file: File; hFile: Handle;
		attr: Dword; ronly: BOOLEAN; bRes: Bool;
BEGIN
	attr := GetFileAttributesW(name);
	IF attr # ORD(INVALID_FILE_ATTRIBUTES) THEN
		IF FILE_ATTRIBUTE_READONLY * SYSTEM.VAL(SET, attr) # {} THEN
			hFile := CreateFileW(
				name, ORD(GENERIC_READ), ORD(FILE_SHARE_READ),
				0, OPEN_EXISTING, 0, 0
			);
			ronly := TRUE
		ELSE hFile := CreateFileW(
				name, ORD(GENERIC_READ+GENERIC_WRITE),
				ORD(FILE_SHARE_READ+FILE_SHARE_WRITE+FILE_SHARE_DELETE),
				0, OPEN_EXISTING, 0, 0
			);
			ronly := FALSE
		END;
		IF hFile # -1 THEN
			NewFile(file, hFile); file.new := FALSE;
			file.ronly := ronly; file.name := name; file.pos := 0;
			bRes := GetFileSizeEx(hFile, SYSTEM.ADR(file.len))
		ELSE ASSERT(FALSE)
		END
	END;
	RETURN file
END Old;

PROCEDURE New*(name: ARRAY OF CHAR): File;
	VAR str, temp: PathStr; pathLen, pid: Dword;
		hFile: Handle; file: File; iRes: Int; time: INTEGER;
BEGIN
	pathLen := GetEnvironmentVariableW(
		'USERPROFILE', SYSTEM.ADR(str), LEN(str)
	);
	pid := GetCurrentProcessId(); time := Rtl.Time();
	IF pathLen < MAX_PATH-60 THEN
		iRes := wsprintfW(temp, '%s\.pocTemp_%x', SYSTEM.ADR(str), pid);
		iRes := wsprintfW(str, '%s_%lx', SYSTEM.ADR(temp), time);
		iRes := wsprintfW(temp, '%s_%lx', SYSTEM.ADR(str), tempId)
	ELSE iRes := wsprintfW(temp, '.temp%lu_%lu', time, tempId) 
	END;
	INC(tempId);
	hFile := CreateFileW(
		temp, ORD(GENERIC_READ+GENERIC_WRITE),
		ORD(FILE_SHARE_READ+FILE_SHARE_WRITE+FILE_SHARE_DELETE), 0,
		CREATE_NEW, ORD(FILE_ATTRIBUTE_TEMPORARY+FILE_FLAG_DELETE_ON_CLOSE), 0
	);
	IF hFile # -1 THEN
		NewFile(file, hFile); file.new := TRUE;
		file.name := name; file.temp := temp;
		file.pos := 0; file.len := 0
	END;
	RETURN file
END New;

PROCEDURE Register*(f: File);
	VAR hFile2: Handle; buf: ARRAY 10000H OF BYTE;
		bRes: Bool; byteRead, byteWritten: Dword;
BEGIN
	IF f.new THEN
		hFile2 := CreateFileW(
			f.name, ORD(GENERIC_READ+GENERIC_WRITE),
			ORD(FILE_SHARE_READ+FILE_SHARE_WRITE+FILE_SHARE_DELETE),
			0, CREATE_ALWAYS, 0, 0
		);
		ASSERT(hFile2 # -1); f.pos := 0;
		bRes := SetFilePointerEx(f.hFile, 0, 0, FILE_BEGIN);
		REPEAT
			bRes := ReadFile(
				f.hFile, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(byteRead), 0
			);
			IF (bRes # 0) & (byteRead # 0) THEN
				bRes := WriteFile(
					hFile2, SYSTEM.ADR(buf), byteRead,
					SYSTEM.ADR(byteWritten), 0
				);
				INC(f.pos, byteWritten)
			END
		UNTIL (byteRead = 0) OR (bRes = 0);
		bRes := CloseHandle(f.hFile); ASSERT(bRes # 0);
		f.hFile := hFile2; f.new := FALSE;
		(* bRes := FlushFileBuffers(hFile2) very slow *)
	END
END Register;

PROCEDURE Close*(f: File);
	VAR bRes: Bool;
BEGIN
	bRes := FlushFileBuffers(f.hFile);
	ASSERT(bRes # 0)
END Close;

PROCEDURE Purge*(f: File);
	VAR bRes: Bool; pos: INTEGER;
BEGIN
	bRes := SetFilePointerEx(f.hFile, 0, SYSTEM.ADR(pos), FILE_BEGIN);
	ASSERT(bRes # 0); bRes := SetEndOfFile(f.hFile); ASSERT(bRes # 0)
END Purge;

PROCEDURE Delete*(name: ARRAY OF CHAR; VAR res: INTEGER);
	VAR bRes: Bool;
BEGIN
	bRes := DeleteFileW(name);
	IF bRes # 0 THEN res := 0 ELSE res := -1 END
END Delete;

PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
	VAR bRes: Bool;
BEGIN
	bRes := MoveFileExW(old, new, ORD(MOVEFILE_COPY_ALLOWED));
	IF bRes # 0 THEN res := 0 ELSE res := -1 END
END Rename;

PROCEDURE Length*(f: File): INTEGER;
	RETURN f.len
END Length;

PROCEDURE GetDate*(f: File; VAR t, d: INTEGER);
BEGIN ASSERT(FALSE)
END GetDate;

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
BEGIN
	IF f = NIL THEN r.f := NIL
	ELSIF pos < 0 THEN r.pos := 0; r.eof := FALSE; r.f := f
	ELSE r.pos := pos; r.eof := FALSE; r.f := f
	END
END Set;

PROCEDURE Pos*(VAR r: Rider): INTEGER;
	RETURN r.pos
END Pos;

PROCEDURE Base*(VAR r: Rider): File;
	RETURN r.f
END Base;

PROCEDURE CheckFilePos(VAR r: Rider);
	VAR bRes: Bool;
BEGIN
	IF r.pos # r.f.pos THEN
		bRes := SetFilePointerEx(
			r.f.hFile, r.pos, SYSTEM.ADR(r.pos), FILE_BEGIN
		);
		ASSERT(bRes # 0); r.f.pos := r.pos
	END
END CheckFilePos;

PROCEDURE Read0(VAR r: Rider; VAR x: ARRAY OF BYTE);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN
	f := r.f; CheckFilePos(r);
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), LEN(x), SYSTEM.ADR(byteRead), 0);
	r.eof := (bRes # 0) & (byteRead = 0);
	IF ~r.eof THEN INC(r.pos, byteRead); INC(f.pos, byteRead) END
END Read0;

PROCEDURE Read*(VAR r: Rider; VAR x: BYTE);
BEGIN Read0(r, x)
END Read;

PROCEDURE ReadInt*(VAR r: Rider; VAR x: INTEGER);
BEGIN Read0(r, x)
END ReadInt;

PROCEDURE ReadReal*(VAR r: Rider; VAR x: REAL);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadReal;

PROCEDURE ReadCard32*(VAR r: Rider; VAR x: SYSTEM.CARD32);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadCard32;

PROCEDURE ReadCard16*(VAR r: Rider; VAR x: SYSTEM.CARD16);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadCard16;

PROCEDURE ReadNum*(VAR r: Rider; VAR x: INTEGER);
	VAR s, b: BYTE; n: INTEGER;
BEGIN
	s := 0; n := 0; Read(r, b);
	WHILE b >= 128 DO
		INC(n, LSL(b - 128, s)); INC(s, 7); Read(r, b)
	END;
	x := n + LSL(b MOD 64 - b DIV 64 * 64, s)
END ReadNum;

PROCEDURE ReadChar*(VAR r: Rider; VAR x: CHAR);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadChar;

PROCEDURE ReadString*(VAR r: Rider; VAR x: ARRAY OF CHAR);
	VAR i: INTEGER; done: BOOLEAN;
BEGIN
	done := FALSE; i := 0;
	WHILE ~r.eof & ~done DO
		ReadChar(r, x[i]);
		IF r.eof THEN x[i] := 0X ELSE done := x[i] = 0X; INC(i) END
	END
END ReadString;

PROCEDURE ReadByteStr*(VAR r: Rider; VAR x: ARRAY OF CHAR);
	VAR i: INTEGER; done: BOOLEAN; b: BYTE;
BEGIN
	done := FALSE; i := 0;
	WHILE ~r.eof & ~done DO
		Read(r, b); x[i] := CHR(b);
		IF r.eof THEN x[i] := 0X ELSE done := x[i] = 0X; INC(i) END
	END
END ReadByteStr;

PROCEDURE ReadSet*(VAR r: Rider; VAR x: SET);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadSet;

PROCEDURE ReadBool*(VAR r: Rider; VAR x: BOOLEAN);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadBool;

PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
	VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN
	f := r.f; CheckFilePos(r);
	IF n > LEN(x) THEN byteRead := LEN(x) ELSE byteRead := n END;
	bRes := ReadFile(
		f.hFile, SYSTEM.ADR(x), byteRead, SYSTEM.ADR(byteRead), 0
	);
	r.eof := (bRes # 0) & (byteRead = 0);
	IF ~r.eof THEN INC(r.pos, byteRead); INC(f.pos, byteRead) END;
	IF byteRead # n THEN r.res := n - byteRead ELSE r.res := 0 END
END ReadBytes;

(* -------------------------------------------------------------------------- *)

PROCEDURE Write0(VAR r: Rider; x: ARRAY OF BYTE);
	VAR bRes: Bool; nWritten: Dword; f: File;
BEGIN
	f := r.f; CheckFilePos(r);
	bRes := WriteFile(f.hFile, SYSTEM.ADR(x), LEN(x), SYSTEM.ADR(nWritten), 0);
	IF bRes # 0 THEN
		INC(r.pos, nWritten); INC(f.pos, nWritten);
		IF f.pos > f.len THEN f.len := f.pos END
	END
END Write0;

PROCEDURE Write*(VAR r: Rider; x: BYTE);
BEGIN Write0(r, x)
END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN Write0(r, x)
END WriteInt;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN Write0(r, x)
END WriteReal;

PROCEDURE WriteCard32*(VAR r: Rider; x: SYSTEM.CARD32);
BEGIN Write0(r, x)
END WriteCard32;

PROCEDURE WriteCard16*(VAR r: Rider; x: SYSTEM.CARD16);
BEGIN Write0(r, x)
END WriteCard16;

PROCEDURE WriteNum*(VAR r: Rider; x: INTEGER);
BEGIN
	WHILE (x < -64) OR (x >= 64) DO
		Write(r, x MOD 128 + 128); x := x DIV 128
	END;
	Write(r, x MOD 128)
END WriteNum;

PROCEDURE WriteChar*(VAR r: Rider; x: CHAR);
BEGIN Write0(r, x)
END WriteChar;

PROCEDURE WriteString*(VAR r: Rider; x: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE x[i] # 0X DO WriteChar(r, x[i]); INC(i) END; WriteChar(r, 0X)
END WriteString;

PROCEDURE WriteByteStr*(VAR r: Rider; x: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE x[i] # 0X DO
		ASSERT(x[i] < 100X); Write(r, ORD(x[i])); INC(i)
	END;
	Write(r, 0)
END WriteByteStr;

PROCEDURE WriteSet*(VAR r: Rider; x: SET);
BEGIN Write0(r, x)
END WriteSet;

PROCEDURE WriteBool*(VAR r: Rider; x: BOOLEAN);
BEGIN Write0(r, x)
END WriteBool;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword; f: File;
BEGIN
	f := r.f; CheckFilePos(r);
	IF n > LEN(x) THEN byteWritten := LEN(x) ELSE byteWritten := n END;
	bRes := WriteFile(
		f.hFile, SYSTEM.ADR(x), byteWritten, SYSTEM.ADR(byteWritten), 0
	);
	IF bRes # 0 THEN
		INC(r.pos, byteWritten); INC(f.pos, byteWritten);
		IF f.pos > f.len THEN f.len := f.pos END
	END;
	r.res := n - byteWritten
END WriteBytes;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InitWin32;
	CONST Kernel32 = 'KERNEL32.DLL';
BEGIN
	Rtl.Import(GetFileAttributesW, Kernel32, 'GetFileAttributesW');
	Rtl.Import(CreateFileW, Kernel32, 'CreateFileW');
	Rtl.Import(CloseHandle, Kernel32, 'CloseHandle');
	Rtl.Import(MoveFileExW, Kernel32, 'MoveFileExW');
	Rtl.Import(DeleteFileW, Kernel32, 'DeleteFileW');
	Rtl.Import(ReadFile, Kernel32, 'ReadFile');
	Rtl.Import(WriteFile, Kernel32, 'WriteFile');
	Rtl.Import(SetFilePointerEx, Kernel32, 'SetFilePointerEx');
	Rtl.Import(FlushFileBuffers, Kernel32, 'FlushFileBuffers');
	Rtl.Import(SetEndOfFile, Kernel32, 'SetEndOfFile');
	Rtl.Import(GetFileSizeEx, Kernel32, 'GetFileSizeEx');
	Rtl.Import(wsprintfW, 'USER32.DLL', 'wsprintfW');
	Rtl.Import(GetEnvironmentVariableW, Kernel32, 'GetEnvironmentVariableW');
	Rtl.Import(GetCurrentProcessId, Kernel32, 'GetCurrentProcessId')
END InitWin32;
	
BEGIN InitWin32
END Files.