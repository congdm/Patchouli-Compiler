MODULE Rtl; (* multi-threaded application NOT SUPPORTED *)
(*$POINTER*)(*$RTL-*)
IMPORT SYSTEM;
	
CONST
	MinInt = 8000000000000000H; MaxInt = 7FFFFFFFFFFFFFFFH;

	Kernel32 = 'KERNEL32.DLL';
	heapErrMsg = 'Heap corruption';
	
	GENERIC_READ = {31}; GENERIC_WRITE = {30};
	MEM_RESERVE = 2000H; MEM_COMMIT = 1000H; PAGE_READWRITE = 4;
	
TYPE
	Handle = INTEGER;
	Pointer = INTEGER;
	Bool = SYSTEM.CARD32;
	Int = SYSTEM.CARD32;
	Dword = SYSTEM.CARD32;
	Ulong = SYSTEM.CARD32;
	Uint = SYSTEM.CARD32;
	
	File* = RECORD hFile: INTEGER END;

VAR
	modList*, nMod*: INTEGER;
	argv, numArgs*: INTEGER;

	(* Utility *)
	ExitProcess: PROCEDURE(uExitCode: INTEGER);
	AddVectoredExceptionHandler: PROCEDURE(
		FirstHandler: Ulong; VectoredHandler: Pointer
	);
	MessageBoxW: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): Int;
	GetSystemTimeAsFileTime: PROCEDURE(lpSystemTimeAsFileTime: Pointer);
	GetCommandLineW: PROCEDURE(): Pointer;
	CommandLineToArgvW: PROCEDURE(lpCmdLine, pNumArgs: Pointer): Pointer;

	(* Heap *)
	VirtualAlloc: PROCEDURE(
		lpAddress, dwSize, flAllocationType, flProtect: INTEGER
	): Pointer;
	heapBase, heapSize: INTEGER; allocated*: INTEGER;
	fList: ARRAY 4 OF INTEGER; fList0: INTEGER;
	
	(* File *)
	GetFileAttributesW: PROCEDURE(lpFileName: INTEGER): Dword;
	MoveFileW: PROCEDURE(lpExistingFileName, lpNewFileName: INTEGER): Bool;
	DeleteFileW: PROCEDURE(lpFilename: INTEGER): Bool;
	CreateFileW: PROCEDURE(
		lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes,
		dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile: INTEGER
	): Handle;
	CloseHandle: PROCEDURE(hObject: INTEGER): Bool;
	ReadFile: PROCEDURE(
		hFile, lpBuffer, nNumberOfBytesToRead,
		lpNumberOfBytesRead, lpOverlapped: INTEGER
	): Bool;
	WriteFile: PROCEDURE(
		hFile, lpBuffer, nNumberOfBytesToWrite,
		lpNumberOfBytesWrite, lpOverlapped: INTEGER
	): Bool;
	SetFilePointerEx: PROCEDURE(
		hFile, liDistanceToMove, lpNewFilePointer, dwMoveMethod: INTEGER
	): Bool;
	FlushFileBuffers: PROCEDURE(hFile: Handle): Bool;
	
	(* Unicode *)
	WideCharToMultiByte: PROCEDURE(
		CodePage: Uint;
		dwFlags: Dword;
		lpWideCharStr: Pointer;
		cchWideChar: Int;
		lpMultiByteStr: Pointer;
		cbMultiByte: Int;
		lpDefaultChar, lpUsedDefaultChar: Pointer
	): Int;
	MultiByteToWideChar: PROCEDURE(
		CodePage: Uint;
		dwFlags: Dword;
		lpMultiByteStr: Pointer;
		cbMultiByte: Int;
		lpWideCharStr: Pointer;
		cchWideChar: Int
	): Int;
	CharLowerBuffW: PROCEDURE(lpsz: Pointer; cchLength: Dword): Dword;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Utility procedures *)
	
PROCEDURE Import*(
	VAR proc: ARRAY OF SYSTEM.BYTE;
	libPath, procName: ARRAY OF CHAR
);
	VAR hLib, procAdr, i: INTEGER; ansiStr: ARRAY 256 OF BYTE;
BEGIN
	SYSTEM.LoadLibraryW(hLib, libPath);
	IF hLib # 0 THEN i := -1;
		REPEAT INC(i); ansiStr[i] := ORD(procName[i])
		UNTIL (procName[i] = 0X) OR (i = LEN(ansiStr)-1); ansiStr[i] := 0;
		SYSTEM.GetProcAddress(procAdr, hLib, SYSTEM.ADR(ansiStr))
	ELSE procAdr := 0
	END;
	SYSTEM.PUT(SYSTEM.ADR(proc), procAdr)
END Import;

PROCEDURE MessageBox*(title, msg: ARRAY OF CHAR);
	VAR iRes: Int;
BEGIN iRes := MessageBoxW(0, SYSTEM.ADR(msg), SYSTEM.ADR(title), 0)
END MessageBox;

PROCEDURE Halt*(msg: ARRAY OF CHAR);
BEGIN MessageBox('Halt', msg); ExitProcess(0)
END Halt;

PROCEDURE Assert*(cond: BOOLEAN; msg: ARRAY OF CHAR);
BEGIN
	IF ~cond THEN Halt(msg) END
END Assert;

PROCEDURE Format*(
	type: INTEGER; src: ARRAY OF BYTE; VAR dst: ARRAY OF CHAR
): INTEGER;
	CONST MinIntStr = '-9223372036854775808';
	VAR neg: BOOLEAN; x, i, j, dstLen: INTEGER; y: BYTE;
		s: ARRAY 32 OF CHAR;
BEGIN
	IF type = 0 (* Decimal Int *) THEN
		IF LEN(src) = 8 THEN SYSTEM.GET(SYSTEM.ADR(src), x)
		ELSIF LEN(src) = 1 THEN x := src[0] ELSE ASSERT(FALSE)
		END;
		IF x # MinInt THEN i := 0; j := 0;
			IF x < 0 THEN neg := TRUE; x := -x ELSE neg := FALSE END;
			REPEAT s[i] := CHR(x MOD 10 + ORD('0')); INC(i); x := x DIV 10
			UNTIL x = 0;		
			IF ~neg THEN
				WHILE j < i DO dst[j] := s[i-1-j]; INC(j) END;
				dst[j] := 0X; dstLen := j+1
			ELSE dst[0] := '-';
				WHILE j < i DO dst[j+1] := s[i-1-j]; INC(j) END;
				dst[j+1] := 0X; dstLen := j+2
			END
		ELSE dst := MinIntStr; dstLen := LEN(MinIntStr)
		END
	ELSIF type = 1 (* Hexadecimal Int *) THEN
		IF LEN(src) = 8 THEN SYSTEM.GET(SYSTEM.ADR(src), x)
		ELSIF LEN(src) = 1 THEN x := src[0] ELSE ASSERT(FALSE)
		END; i := 15; j := 0;
		WHILE (ASR(x, i*4) MOD 16 = 0) & (i > 0) DO DEC(i) END;
		WHILE i >= 0 DO y := ASR(x, i*4) MOD 16;
			IF y <= 9 THEN dst[j] := CHR(y + ORD('0'))
			ELSE dst[j] := CHR(y-10 + ORD('a'))
			END; INC(j); DEC(i)
		END;
		dst[j] := 0X; dstLen := j+1
	ELSE ASSERT(FALSE)
	END;
	RETURN dstLen
END Format;

PROCEDURE Parse*(type: INTEGER; src: ARRAY OF CHAR; VAR dst: ARRAY OF BYTE);
BEGIN ASSERT(FALSE)
END Parse;

PROCEDURE GetArg*(VAR out: ARRAY OF CHAR; n: INTEGER);
	VAR i, arg: INTEGER;
BEGIN (* GetArg *)
	ASSERT(argv # 0);
	IF (n < numArgs) & (n >= 0) THEN i := 0;
		SYSTEM.GET(argv+n*8, arg); ASSERT(arg # 0); SYSTEM.GET(arg, out[i]);
		WHILE out[i] # 0X DO INC(arg, 2); INC(i); SYSTEM.GET(arg, out[i]) END
	ELSE out[0] := 0X
	END
END GetArg;

PROCEDURE Time*(): INTEGER;
	VAR tick: INTEGER;
BEGIN
	GetSystemTimeAsFileTime(SYSTEM.ADR(tick));
	RETURN tick
END Time;

PROCEDURE TimeToMSecs*(time: INTEGER): INTEGER;
	RETURN time DIV 10000
END TimeToMSecs;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* File *)

PROCEDURE ExistFile*(fname: ARRAY OF CHAR): BOOLEAN;
	CONST INVALID_FILE_ATTRIBUTES = {0..31};
	VAR dwRes: Dword;
BEGIN
	dwRes := GetFileAttributesW(SYSTEM.ADR(fname));
	RETURN dwRes # ORD(INVALID_FILE_ATTRIBUTES)
END ExistFile;
	
PROCEDURE Open*(VAR f: File; fname: ARRAY OF CHAR);
	CONST OPEN_EXISTING = 3;
BEGIN
	f.hFile := CreateFileW(
		SYSTEM.ADR(fname), ORD(GENERIC_READ + GENERIC_WRITE),
		0, 0, OPEN_EXISTING, 0, 0
	)
END Open;

PROCEDURE Reset*(VAR f: File; fname: ARRAY OF CHAR);
	CONST OPEN_EXISTING = 3;
BEGIN
	f.hFile := CreateFileW(
		SYSTEM.ADR(fname), ORD(GENERIC_READ), 0, 0, OPEN_EXISTING, 0, 0
	)
END Reset;
	
PROCEDURE Rewrite*(VAR f: File; fname: ARRAY OF CHAR);
	CONST CREATE_ALWAYS = 2;
BEGIN
	f.hFile := CreateFileW(
		SYSTEM.ADR(fname), ORD(GENERIC_READ + GENERIC_WRITE),
		0, 0, CREATE_ALWAYS, 0, 0
	)
END Rewrite;

PROCEDURE Flush*(f: File);
BEGIN ASSERT(FlushFileBuffers(f.hFile) # 0)
END Flush;

PROCEDURE Close*(VAR f: File);
	VAR bRes: Bool;
BEGIN bRes := CloseHandle(f.hFile); f.hFile := 0
END Close;

PROCEDURE Rename*(old, new: ARRAY OF CHAR);
	VAR bRes: Bool;
BEGIN bRes := MoveFileW(SYSTEM.ADR(old), SYSTEM.ADR(new))
END Rename;

PROCEDURE Delete*(fname: ARRAY OF CHAR);
	VAR bRes: Bool;
BEGIN bRes := DeleteFileW(SYSTEM.ADR(fname))
END Delete;

(* -------------------------------------------------------------------------- *)
(* Read *)

PROCEDURE Read1*(f: File; VAR n: INTEGER);
	VAR bRes: Bool; byteRead: Dword; x: BYTE;
BEGIN byteRead := 0;
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 1, SYSTEM.ADR(byteRead), 0);
	IF (bRes # 0) & (byteRead = 1) THEN n := x ELSE n := -1 END
END Read1;
	
PROCEDURE Read2*(f: File; VAR n: INTEGER);
	VAR bRes: Bool; byteRead: Dword; x: SYSTEM.CARD16;
BEGIN
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 2, SYSTEM.ADR(byteRead), 0);
	IF (bRes # 0) & (byteRead = 2) THEN n := x ELSE n := -1 END
END Read2;

PROCEDURE ReadStr*(f: File; VAR str: ARRAY OF CHAR);
	VAR i: INTEGER; bRes: Bool; byteRead: Dword; ch: CHAR;
BEGIN i := 0;
	bRes := ReadFile(f.hFile, SYSTEM.ADR(ch), 2, SYSTEM.ADR(byteRead), 0);
	WHILE (i < LEN(str)) & (ch # 0X) DO
		str[i] := ch; INC(i);
		bRes := ReadFile(f.hFile, SYSTEM.ADR(ch), 2, SYSTEM.ADR(byteRead), 0);
		IF (bRes = 0) OR (byteRead # 2) THEN ch := 0X END
	END;
	IF i = LEN(str) THEN DEC(i) END; str[i] := 0X
END ReadStr;
	
PROCEDURE Read4*(f: File; VAR n: INTEGER);
	VAR bRes: Bool; byteRead: Dword; x: SYSTEM.CARD32;
BEGIN
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 4, SYSTEM.ADR(byteRead), 0);
	IF (bRes # 0) & (byteRead = 4) THEN n := x ELSE n := -1 END
END Read4;
	
PROCEDURE Read8*(f: File; VAR n: INTEGER);
	VAR bRes: Bool; byteRead: Dword;
BEGIN
	bRes := ReadFile(f.hFile, SYSTEM.ADR(n), 8, SYSTEM.ADR(byteRead), 0)
END Read8;

PROCEDURE ReadBytes*(f: File; VAR buf: ARRAY OF BYTE; VAR byteRead: INTEGER);
	VAR bRes: Bool; dwByteRead: Dword;
BEGIN dwByteRead := 0;
	bRes := ReadFile(
		f.hFile, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(dwByteRead), 0
	);
	byteRead := dwByteRead
END ReadBytes;

(* -------------------------------------------------------------------------- *)
(* Write *)
	
PROCEDURE Write1*(f: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 1, SYSTEM.ADR(byteWritten), 0)
END Write1;
	
PROCEDURE Write2*(f: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 2, SYSTEM.ADR(byteWritten), 0)
END Write2;

PROCEDURE WriteStr*(f: File; str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Write2(f, ORD(str[i])) UNTIL str[i] = 0X
END WriteStr;

PROCEDURE WriteAnsiStr*(f: File; str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Write1(f, ORD(str[i])) UNTIL str[i] = 0X
END WriteAnsiStr;
	
PROCEDURE Write4*(f: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 4, SYSTEM.ADR(byteWritten), 0)
END Write4;
	
PROCEDURE Write8*(f: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 8, SYSTEM.ADR(byteWritten), 0)
END Write8;

PROCEDURE WriteBytes*(f: File; a: ARRAY OF BYTE; VAR byteWritten: INTEGER);
	VAR bRes: Bool; dwByteWritten: Dword;
BEGIN dwByteWritten := 0;
	bRes := WriteFile(
		f.hFile, SYSTEM.ADR(a), LEN(a), SYSTEM.ADR(dwByteWritten), 0
	);
	byteWritten := dwByteWritten
END WriteBytes;

PROCEDURE WriteBuf*(f: File; ptr: INTEGER; VAR byteWritten: INTEGER);
	VAR bRes: Bool; dwByteWritten: Dword;
BEGIN dwByteWritten := 0;
	bRes := WriteFile(
		f.hFile, ptr, byteWritten, SYSTEM.ADR(dwByteWritten), 0
	);
	byteWritten := dwByteWritten
END WriteBuf;

(* -------------------------------------------------------------------------- *)

PROCEDURE Pos*(f: File): INTEGER;
	CONST FILE_CURRENT = 1;
	VAR bRes, byteToMove, newPointer: INTEGER;
BEGIN byteToMove := 0;
	bRes := SetFilePointerEx(
		f.hFile, byteToMove, SYSTEM.ADR(newPointer), FILE_CURRENT
	)
	RETURN newPointer
END Pos;

PROCEDURE Seek*(f: File; pos: INTEGER);
	CONST FILE_BEGIN = 0;
	VAR bRes, byteToMove, newPointer: INTEGER;
BEGIN byteToMove := pos;
	bRes := SetFilePointerEx(
		f.hFile, byteToMove, SYSTEM.ADR(newPointer), FILE_BEGIN
	)
END Seek;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Unicode *)

PROCEDURE Utf8ToUnicode*(src: ARRAY OF BYTE; VAR dst: ARRAY OF CHAR): INTEGER;
	CONST CP_UTF8 = 65001;
	VAR iRes: INTEGER;
BEGIN iRes := 0;
	iRes := MultiByteToWideChar(
		CP_UTF8, 0, SYSTEM.ADR(src), -1, SYSTEM.ADR(dst), LEN(dst)
	);
	ASSERT(iRes > 0);
	RETURN iRes
END Utf8ToUnicode;

PROCEDURE UnicodeToUtf8*(src: ARRAY OF CHAR; VAR dst: ARRAY OF BYTE): INTEGER;
	CONST CP_UTF8 = 65001;
	VAR iRes: INTEGER;
BEGIN iRes := 0;
	iRes := WideCharToMultiByte(
		CP_UTF8, 0, SYSTEM.ADR(src), -1, SYSTEM.ADR(dst), LEN(dst), 0, 0
	);
	ASSERT(iRes > 0);
	RETURN iRes
END UnicodeToUtf8;

PROCEDURE SizeInUtf8*(str: ARRAY OF CHAR): INTEGER;
	CONST CP_UTF8 = 65001;
	RETURN WideCharToMultiByte(CP_UTF8, 0, SYSTEM.ADR(str), -1, 0, 0, 0, 0)
END SizeInUtf8;

PROCEDURE LowerCase*(VAR str: ARRAY OF CHAR);
BEGIN
	ASSERT(CharLowerBuffW(SYSTEM.ADR(str), LEN(str)) = LEN(str))
END LowerCase;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Module management *)

PROCEDURE Register*(modAdr: INTEGER);
BEGIN
	ASSERT(nMod < 512);
	IF modList = 0 THEN
		modList := VirtualAlloc(0, 4096, MEM_COMMIT, PAGE_READWRITE);
		nMod := 0; ASSERT(modList # 0)
	END;
	INC(nMod); SYSTEM.PUT(modList+nMod*8-8, modAdr)
END Register;

PROCEDURE TrapHandler(ExceptionInfo: INTEGER): INTEGER;
	VAR ExceptionRecord: INTEGER;
BEGIN
	SYSTEM.GET(ExceptionInfo, ExceptionRecord);
	RETURN 0
END TrapHandler;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Heap management *)
(* Based on Kernel.mod in Project Oberon 2013 *)

PROCEDURE HeapLimit(): INTEGER;
	RETURN heapBase + heapSize
END HeapLimit;

PROCEDURE ExtendHeap;
	VAR p, mark, size, prev, p2: INTEGER;
BEGIN
	IF heapSize = 80000000H THEN Halt('Out of memory') END;
	p := VirtualAlloc(HeapLimit(), heapSize, MEM_COMMIT, PAGE_READWRITE);
	IF p = 0 THEN Halt('VirtualAlloc cannot commit') END;
	SYSTEM.PUT(p, heapSize); SYSTEM.PUT(p+8, -1); SYSTEM.PUT(p+16, 0);
	IF fList0 = 0 THEN fList0 := p
	ELSE prev := fList0; SYSTEM.GET(fList0+16, p2);
		WHILE p2 # 0 DO prev := p2; SYSTEM.GET(p2+16, p2) END;
		SYSTEM.PUT(prev+16, p)
	END;
	heapSize := heapSize*2
END ExtendHeap;

PROCEDURE GetBlock(need: INTEGER): INTEGER;
	(* need is multiple of 512 *)
	VAR p, q0, q1, q2, size: INTEGER; done: BOOLEAN;
BEGIN q0 := 0; q1 := fList0; done := FALSE;
	WHILE ~done & (q1 # 0) DO
		SYSTEM.GET(q1, size); SYSTEM.GET(q1+16, q2);
		IF size < need THEN (* no fit *) q0 := q1; q1 := q2
		ELSIF size = need THEN (* extract -> p *)
			done := TRUE; p := q1;
			IF q0 # 0 THEN SYSTEM.PUT(q0+16, q2) ELSE fList0 := q2 END
		ELSE (* reduce size *)
			done := TRUE; p := q1; q1 := q1 + need; SYSTEM.PUT(q1, size-need);
			SYSTEM.PUT(q1+8, -1); SYSTEM.PUT(q1+16, q2);
			IF q0 # 0 THEN SYSTEM.PUT(q0+16, q1) ELSE fList0 := q1 END
		END
	END ;
	IF ~done THEN ExtendHeap; p := GetBlock(need) END ;
	RETURN p
END GetBlock;

PROCEDURE GetBlock256(): INTEGER;
	VAR p, q: INTEGER;
BEGIN
	IF fList[3] # 0 THEN p := fList[3]; SYSTEM.GET(fList[3]+16, fList[3])
	ELSE q := GetBlock(512); SYSTEM.PUT(q+256, 256); SYSTEM.PUT(q+(256+8), -1);
		SYSTEM.PUT(q+(256+16), 0); fList[3] := q + 256; p := q
	END;
	RETURN p
END GetBlock256;

PROCEDURE GetBlock128(): INTEGER;
	VAR p, q: INTEGER;
BEGIN
	IF fList[2] # 0 THEN p := fList[2]; SYSTEM.GET(fList[2]+16, fList[2])
	ELSE q := GetBlock256(); SYSTEM.PUT(q+128, 128); SYSTEM.PUT(q+(128+8), -1);
		SYSTEM.PUT(q+(128+16), 0); fList[2] := q + 128; p := q
	END;
	RETURN p
END GetBlock128;

PROCEDURE GetBlock64(): INTEGER;
	VAR p, q: INTEGER;
BEGIN
	IF fList[1] # 0 THEN p := fList[1]; SYSTEM.GET(fList[1]+16, fList[1])
	ELSE q := GetBlock128(); SYSTEM.PUT(q+64, 64); SYSTEM.PUT(q+(64+8), -1);
		SYSTEM.PUT(q+(64+16), 0); fList[1] := q + 64; p := q
	END;
	RETURN p
END GetBlock64;

PROCEDURE Rounding(VAR size: INTEGER);
BEGIN
	IF size < 64 THEN size := 64 ELSIF size < 128 THEN size := 128
	ELSIF size < 256 THEN size := 256 ELSE size := (size+511) DIV 512 * 512
	END
END Rounding;

PROCEDURE New*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR p, size, need, lim: INTEGER;
BEGIN
	SYSTEM.GET(tdAdr, size); need := size+16; Rounding(need);
	IF need = 64 THEN p := GetBlock64()
	ELSIF need = 128 THEN p := GetBlock128()
	ELSIF need = 256 THEN p := GetBlock256()
	ELSE p := GetBlock(need)
	END;
	
	SYSTEM.PUT(p, tdAdr); SYSTEM.PUT(p+8, 0); ptr := p+16;
	INC(p, 16); INC(allocated, need); lim := (p+size+7) DIV 8 * 8;
	WHILE p < lim DO SYSTEM.PUT(p, 0); INC(p, 8) END
END New;

(* -------------------------------------------------------------------------- *)
(* Mark and Sweep *)

PROCEDURE Mark(pref, modBase: INTEGER);
	VAR pvadr, offadr, offset, tag, p, q, r: INTEGER;
BEGIN SYSTEM.GET(pref, pvadr); (* pointers < heapBase considered NIL *)
	WHILE pvadr # -1 DO
		INC(pvadr, modBase); SYSTEM.GET(pvadr, p);
		IF p >= heapBase THEN SYSTEM.GET(p-8, offadr) END ;
		IF (p >= heapBase) & (offadr = 0) THEN q := p;
			(*mark elements in data structure with root p*)
			REPEAT SYSTEM.GET(p-8, offadr); (* mark word *)
				IF offadr = 0 THEN SYSTEM.GET(p-16, tag); offadr := tag + 64
				ELSE INC(offadr, 8)
				END ;
				SYSTEM.PUT(p-8, offadr); SYSTEM.GET(offadr, offset);
				IF offset # -1 THEN (*down*)
					SYSTEM.GET(p+offset, r);
					IF r >= heapBase THEN SYSTEM.GET(r-8, offadr);
						IF offadr = 0 THEN
							SYSTEM.PUT(p+offset, q); q := p; p := r
						END
					END
				ELSE (*up*)
					SYSTEM.GET(q-8, offadr); SYSTEM.GET(offadr, offset);
					IF p # q THEN
						SYSTEM.GET(q+offset, r); SYSTEM.PUT(q+offset, p);
						p := q; q := r
					END
				END
			UNTIL (p = q) & (offset = -1)
		END ;
		INC(pref, 8); SYSTEM.GET(pref, pvadr)
	END
END Mark;

PROCEDURE Scan;
	VAR p, q, mark, tag, size: INTEGER;
BEGIN p := heapBase;
	REPEAT SYSTEM.GET(p+8, mark); q := p;
		WHILE mark = 0 DO
			SYSTEM.GET(p, tag); SYSTEM.GET(tag, size); INC(size, 16);
			Rounding(size); INC(p, size); SYSTEM.GET(p+8, mark)
		END ;
		size := p - q; DEC(allocated, size);  (* size of free block *)
		IF size > 0 THEN
			IF size MOD 128 # 0 THEN
				SYSTEM.PUT(q, 64); SYSTEM.PUT(q+8, -1);
				SYSTEM.PUT(q+16, fList[1]); fList[1] := q;
				INC(q, 64); DEC(size, 64)
			END ;
			IF size MOD 256 # 0 THEN
				SYSTEM.PUT(q, 128); SYSTEM.PUT(q+8, -1);
				SYSTEM.PUT(q+16, fList[2]); fList[2] := q;
				INC(q, 128); DEC(size, 128)
			END ;
			IF size MOD 512 # 0 THEN
				SYSTEM.PUT(q, 256); SYSTEM.PUT(q+8, -1);
				SYSTEM.PUT(q+16, fList[3]); fList[3] := q;
				INC(q, 256); DEC(size, 256)
			END ;
			IF size > 0 THEN
				SYSTEM.PUT(q, size); SYSTEM.PUT(q+8, -1);
				SYSTEM.PUT(q+16, fList0); fList0 := q; INC(q, size)
			END
		END ;
		IF mark > 0 THEN
			SYSTEM.GET(p, tag); SYSTEM.GET(tag, size);
			SYSTEM.PUT(p+8, 0); INC(size, 16); Rounding(size); INC(p, size)
		ELSE (*free*) SYSTEM.GET(p, size); INC(p, size)
		END
	UNTIL p >= HeapLimit()
END Scan;

PROCEDURE Collect*;
	VAR i, modBase, ptrTable, off: INTEGER;
BEGIN i := 0;
	WHILE i < nMod DO
		SYSTEM.GET(modList+i*8, modBase); SYSTEM.GET(modBase-8, ptrTable);
		Mark(ptrTable, modBase); INC(i)
	END;
	Scan
END Collect;

(* -------------------------------------------------------------------------- *)

PROCEDURE InitHeap;
	VAR i, p: INTEGER;
BEGIN heapSize := 80000H;
	heapBase := VirtualAlloc(0, 80000000H, MEM_RESERVE, PAGE_READWRITE);
	IF heapBase = 0 THEN Halt('Cannot init heap') END;
	heapBase := VirtualAlloc(heapBase, heapSize, MEM_COMMIT, PAGE_READWRITE);
	IF heapBase = 0 THEN Halt('Cannot init heap') END;
	
	FOR i := 1 TO LEN(fList)-1 DO fList[i] := 0 END;
	p := heapBase; fList0 := heapBase; allocated := 0;
	SYSTEM.PUT(p, heapSize); SYSTEM.PUT(p+8, -1); SYSTEM.PUT(p+16, 0)
END InitHeap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Init *)

PROCEDURE GetArgv;
	VAR pCmdLine: Pointer;
BEGIN
	pCmdLine := GetCommandLineW();
	argv := CommandLineToArgvW(pCmdLine, SYSTEM.ADR(numArgs))
END GetArgv;

BEGIN
	Import(ExitProcess, Kernel32, 'ExitProcess');
	Import(
		AddVectoredExceptionHandler, Kernel32, 'AddVectoredExceptionHandler'
	);
	Import(MessageBoxW, 'USER32.DLL', 'MessageBoxW');
	Import(GetSystemTimeAsFileTime, Kernel32, 'GetSystemTimeAsFileTime');
	Import(GetCommandLineW, Kernel32, 'GetCommandLineW');
	Import(CommandLineToArgvW, 'Shell32.dll', 'CommandLineToArgvW');

	Import(VirtualAlloc, Kernel32, 'VirtualAlloc');
	
	Import(GetFileAttributesW, Kernel32, 'GetFileAttributesW');
	Import(CreateFileW, Kernel32, 'CreateFileW');
	Import(CloseHandle, Kernel32, 'CloseHandle');
	Import(MoveFileW, Kernel32, 'MoveFileW');
	Import(DeleteFileW, Kernel32, 'DeleteFileW');
	Import(ReadFile, Kernel32, 'ReadFile');
	Import(WriteFile, Kernel32, 'WriteFile');
	Import(SetFilePointerEx, Kernel32, 'SetFilePointerEx');
	Import(FlushFileBuffers, Kernel32, 'FlushFileBuffers');
	
	Import(WideCharToMultiByte, Kernel32, 'WideCharToMultiByte');
	Import(MultiByteToWideChar, Kernel32, 'MultiByteToWideChar');
	Import(CharLowerBuffW, 'User32.dll', 'CharLowerBuffW');
	
	GetArgv; InitHeap
END Rtl.