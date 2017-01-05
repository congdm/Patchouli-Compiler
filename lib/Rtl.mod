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
	Bool = INTEGER;
	Int = INTEGER;
	Dword = INTEGER;
	Uint = INTEGER;
	
	File* = RECORD hFile: INTEGER END;

VAR
	modList*, nMod*: INTEGER;

	(* Utility *)
	ExitProcess: PROCEDURE(uExitCode: INTEGER);
	MessageBoxW: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): Int;
	GetSystemTimeAsFileTime: PROCEDURE(lpSystemTimeAsFileTime: Pointer);
	GetCommandLineW: PROCEDURE(): Pointer;

	(* Heap *)
	VirtualAlloc: PROCEDURE(
		lpAddress, dwSize, flAllocationType, flProtect: INTEGER
	): Pointer;
	heapBase, heapSize: INTEGER;
	fList: ARRAY 9 OF INTEGER; fList0: INTEGER;
	
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
	VAR iRes: INTEGER;
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

PROCEDURE GetArg*(VAR out: ARRAY OF CHAR; VAR paramLen: INTEGER; n: INTEGER);
	CONST chSize = 2;
	VAR i, k: INTEGER; buf: Pointer; ch: CHAR;
BEGIN buf := GetCommandLineW(); i := 0;
	WHILE n > 0 DO
		SYSTEM.GET(buf, ch);
		WHILE (ch # ' ') & (ch # 0X) DO
			buf := buf + chSize; SYSTEM.GET(buf, ch)
		END;
		IF ch = 0X THEN n := 0
		ELSIF ch = ' ' THEN DEC(n);
			WHILE ch = ' ' DO buf := buf + chSize; SYSTEM.GET(buf, ch) END
		END
	END;
	k := 0; paramLen := 0;
	WHILE (ch # ' ') & (ch # 0X) DO
		IF k < LEN(out) THEN out[k] := ch END;
		INC(k); INC(paramLen); buf := buf + chSize; SYSTEM.GET(buf, ch)
	END;
	IF k < LEN(out) THEN out[k] := 0X END
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
	VAR dwRes: INTEGER;
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

PROCEDURE Close*(VAR f: File);
	VAR bRes: INTEGER;
BEGIN bRes := CloseHandle(f.hFile); f.hFile := 0
END Close;

PROCEDURE Rename*(old, new: ARRAY OF CHAR);
	VAR bRes: INTEGER;
BEGIN bRes := MoveFileW(SYSTEM.ADR(old), SYSTEM.ADR(new))
END Rename;

PROCEDURE Delete*(fname: ARRAY OF CHAR);
	VAR bRes: INTEGER;
BEGIN bRes := DeleteFileW(SYSTEM.ADR(fname))
END Delete;

(* -------------------------------------------------------------------------- *)
(* Read *)

PROCEDURE Read1*(f: File; VAR n: INTEGER);
	VAR bRes, byteRead: INTEGER; x: BYTE;
BEGIN byteRead := 0;
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 1, SYSTEM.ADR(byteRead), 0);
	IF (bRes # 0) & (byteRead = 1) THEN n := x ELSE n := -1 END
END Read1;
	
PROCEDURE Read2*(f: File; VAR n: INTEGER);
	VAR bRes, x, byteRead: INTEGER;
BEGIN
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 2, SYSTEM.ADR(byteRead), 0);
	IF (bRes # 0) & (byteRead = 2) THEN n := x MOD 10000H ELSE n := -1 END
END Read2;

PROCEDURE ReadStr*(f: File; VAR str: ARRAY OF CHAR);
	VAR i: INTEGER; bRes, byteRead: INTEGER; ch: CHAR;
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
	VAR bRes, x, byteRead: INTEGER;
BEGIN
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 4, SYSTEM.ADR(byteRead), 0);
	IF (bRes # 0) & (byteRead = 2) THEN n := x MOD 100000000H ELSE n := -1 END
END Read4;
	
PROCEDURE Read8*(f: File; VAR n: INTEGER);
	VAR bRes, x, byteRead: INTEGER;
BEGIN
	bRes := ReadFile(f.hFile, SYSTEM.ADR(x), 8, SYSTEM.ADR(byteRead), 0);
	n := x
END Read8;

PROCEDURE ReadBytes*(f: File; VAR buf: ARRAY OF BYTE; VAR byteRead: INTEGER);
	VAR bRes, dwByteRead: INTEGER;
BEGIN dwByteRead := 0;
	bRes := ReadFile(
		f.hFile, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(dwByteRead), 0
	);
	byteRead := dwByteRead
END ReadBytes;

(* -------------------------------------------------------------------------- *)
(* Write *)
	
PROCEDURE Write1*(f: File; n: INTEGER);
	VAR bRes, byteWritten: INTEGER;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 1, SYSTEM.ADR(byteWritten), 0)
END Write1;
	
PROCEDURE Write2*(f: File; n: INTEGER);
	VAR bRes, byteWritten: INTEGER;
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
	VAR bRes, byteWritten: INTEGER;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 4, SYSTEM.ADR(byteWritten), 0)
END Write4;
	
PROCEDURE Write8*(f: File; n: INTEGER);
	VAR bRes, byteWritten: INTEGER;
BEGIN
	bRes := WriteFile(f.hFile, SYSTEM.ADR(n), 8, SYSTEM.ADR(byteWritten), 0)
END Write8;

PROCEDURE WriteBytes*(f: File; a: ARRAY OF BYTE; VAR byteWritten: INTEGER);
	VAR bRes, dwByteWritten: INTEGER;
BEGIN dwByteWritten := 0;
	bRes := WriteFile(
		f.hFile, SYSTEM.ADR(a), LEN(a), SYSTEM.ADR(dwByteWritten), 0
	);
	byteWritten := dwByteWritten
END WriteBytes;

PROCEDURE WriteBuf*(f: File; ptr: INTEGER; VAR byteWritten: INTEGER);
	VAR bRes, dwByteWritten: INTEGER;
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

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Heap management *)
(* The algorithm in here is based on Quick Fit described in *)
(* http://www.flounder.com/memory_allocation.htm *)

PROCEDURE ValidMark(mark: INTEGER): BOOLEAN;
	RETURN (mark = 0) OR (mark = -1) OR (mark = -2)
END ValidMark;

PROCEDURE HeapLimit(): INTEGER;
	RETURN heapBase + heapSize
END HeapLimit;

PROCEDURE MergeSort(VAR f0: INTEGER);
	TYPE FileArray = ARRAY 4 OF INTEGER; 
	VAR f: FileArray; run, len: INTEGER; flag: BOOLEAN;
	
	PROCEDURE Merge(VAR f0, f1, f2, f3: INTEGER; run: INTEGER);
		VAR l2, l3, i0, i1, x, y: INTEGER; flag2: BOOLEAN;
	BEGIN
		l2 := SYSTEM.ADR(f2); l3 := SYSTEM.ADR(f3); flag2 := TRUE;
		REPEAT x := f0; y := f1;
			IF x = 0 THEN i0 := run ELSE i0 := 0 END;
			IF y = 0 THEN i1 := run ELSE i1 := 0 END;
			WHILE (i0 < run) & (i1 < run) DO
				IF x < y THEN SYSTEM.GET(f0, f0);
					IF flag2 THEN SYSTEM.PUT(l2, x); l2 := x
					ELSE SYSTEM.PUT(l3, x); l3 := x
					END;
					x := f0; INC(i0); IF x = 0 THEN i0 := run END
				ELSE SYSTEM.GET(f1, f1);
					IF flag2 THEN SYSTEM.PUT(l2, y); l2 := y
					ELSE SYSTEM.PUT(l3, y); l3 := y
					END;
					y := f1; INC(i1); IF y = 0 THEN i1 := run END
				END	
			END;
			WHILE i0 < run DO SYSTEM.GET(f0, f0);
				IF flag2 THEN SYSTEM.PUT(l2, x); l2 := x
				ELSE SYSTEM.PUT(l3, x); l3 := x
				END;
				x := f0; INC(i0); IF x = 0 THEN i0 := run END
			END;
			WHILE i1 < run DO SYSTEM.GET(f1, f1);
				IF flag2 THEN SYSTEM.PUT(l2, y); l2 := y
				ELSE SYSTEM.PUT(l3, y); l3 := y
				END;
				 y := f1; INC(i1); IF y = 0 THEN i1 := run END
			END;
			flag2 := ~flag2
		UNTIL (f0 = 0) & (f1 = 0);
		IF l2 # 0 THEN SYSTEM.PUT(l2, 0) END;
		IF l3 # 0 THEN SYSTEM.PUT(l3, 0) END
	END Merge;
	
	PROCEDURE Distribute(f0: INTEGER; VAR f: FileArray): INTEGER;
		VAR l: ARRAY 2 OF INTEGER; x, y, i, len: INTEGER;
	BEGIN
		l[0] := SYSTEM.ADR(f[0]); l[1] := SYSTEM.ADR(f[1]);
		i := -1; len := 0;
		WHILE f0 # 0 DO i := (i + 1) MOD 2; INC(len);
			x := f0; SYSTEM.GET(f0, f0); y := f0;
			IF f0 # 0 THEN SYSTEM.GET(f0, f0); INC(len);
				IF x < y THEN SYSTEM.PUT(l[i], x); SYSTEM.PUT(x, y); l[i] := y
				ELSE SYSTEM.PUT(l[i], y); SYSTEM.PUT(y, x); l[i] := x
				END
			ELSE SYSTEM.PUT(l[i], x); l[i] := x
			END
		END;
		IF l[0] # 0 THEN SYSTEM.PUT(l[0], 0) END;
		IF l[1] # 0 THEN SYSTEM.PUT(l[1], 0) END;
		RETURN len
	END Distribute;
	
BEGIN (* MergeSort *)
	ASSERT(f0 # 0); len := Distribute(f0, f); run := 2; flag := TRUE;
	WHILE run < len DO
		IF flag THEN Merge(f[0], f[1], f[2], f[3], run)
		ELSE Merge(f[2], f[3], f[0], f[1], run)
		END; run := run*2; flag := ~flag
	END;
	IF f[0] # 0 THEN f0 := f[0]
	ELSIF f[1] # 0 THEN f0 := f[1]
	ELSIF f[2] # 0 THEN f0 := f[2]
	ELSIF f[3] # 0 THEN f0 := f[3]
	ELSE ASSERT(FALSE)
	END
END MergeSort;

PROCEDURE ExtendHeap;
	VAR p, mark, size, prev, p2: INTEGER;
BEGIN
	IF heapSize = 80000000H THEN Halt('Out of memory') END;
	p := VirtualAlloc(HeapLimit(), heapSize, MEM_COMMIT, PAGE_READWRITE);
	IF p = 0 THEN Halt('VirtualAlloc cannot commit') END;
	SYSTEM.PUT(p+8, heapSize);
	IF fList0 = 0 THEN fList0 := p
	ELSE prev := fList0; SYSTEM.GET(fList0, p2);
		WHILE p2 # 0 DO prev := p2; SYSTEM.GET(p2, p2) END;
		SYSTEM.PUT(prev, p)
	END;
	heapSize := heapSize*2
END ExtendHeap;

PROCEDURE Split(p, need: INTEGER);
	VAR size, i, p2, next: INTEGER;
BEGIN
	SYSTEM.GET(p+8, size); SYSTEM.GET(p, next);
	IF need < size THEN i := (size-need) DIV 64; p2 := p+need;
		SYSTEM.PUT(p+8, need); SYSTEM.PUT(p2+8, size-need);
		IF i < LEN(fList) THEN SYSTEM.PUT(p2, fList[i]); fList[i] := p2
		ELSE SYSTEM.PUT(p2, next); SYSTEM.PUT(p, p2)
		END
	END
END Split;

PROCEDURE Split2(need: INTEGER): INTEGER;
	VAR p, size, p2, next, k: INTEGER;
BEGIN
	IF fList0 = 0 THEN ExtendHeap END; p := fList0;
	SYSTEM.GET(p+8, size); SYSTEM.GET(p, next); p2 := p+need;
	SYSTEM.PUT(p+8, need); SYSTEM.PUT(p2+8, size-need);
	k := (size-need) DIV 64;
	IF k >= LEN(fList) THEN fList0 := p2; SYSTEM.PUT(p2, next)
	ELSE fList0 := next; SYSTEM.PUT(p2, fList[k]); fList[k] := p2
	END;
	RETURN p
END Split2;

PROCEDURE Alloc0(need: INTEGER): INTEGER;
	VAR p, prev, next, i, k, size: INTEGER;
BEGIN i := need DIV 64;
	IF i < LEN(fList) THEN p := fList[i];
		IF p = 0 THEN p := Split2(need)
		ELSE SYSTEM.GET(p, next); fList[i] := next
		END
	ELSE p := fList0; prev := 0;
		IF p # 0 THEN SYSTEM.GET(p+8, size) END;
		WHILE (p # 0) & (size < need) DO
			prev := p; SYSTEM.GET(p, p);
			IF p # 0 THEN SYSTEM.GET(p+8, size) END
		END;
		IF p # 0 THEN Split(p, need); SYSTEM.GET(p, next);
			IF prev = 0 THEN fList0 := next ELSE SYSTEM.PUT(prev, next) END
		ELSE ExtendHeap; p := Alloc0(need)
		END
	END;
	RETURN p
END Alloc0;

PROCEDURE Free0(p: INTEGER);
	VAR size, i, p2, prev, size0, size2: INTEGER;
BEGIN
	SYSTEM.GET(p+8, size); i := size DIV 64;
	IF i < LEN(fList) THEN SYSTEM.PUT(p, fList[i]); fList[i] := p
	ELSE prev := 0; p2 := fList0;
		IF (p2 = 0) OR (p2 > p) THEN SYSTEM.PUT(p, p2); fList0 := p
		ELSE prev := fList0; SYSTEM.GET(p2, p2);
			WHILE (p2 # 0) & (p2 < p) DO prev := p2; SYSTEM.GET(p2, p2) END;
			SYSTEM.PUT(prev, p); SYSTEM.PUT(p, p2)
		END;
		IF (prev # 0) & (prev < p) THEN SYSTEM.GET(prev+8, size0);
			IF prev+size0 = p THEN
				INC(size, size0); p := prev;
				SYSTEM.PUT(p, p2); SYSTEM.PUT(p+8, size)
			END
		END;
		IF (p+size = p2) THEN
			SYSTEM.GET(p2+8, size2); SYSTEM.GET(p2, p2);
			SYSTEM.PUT(p, p2); SYSTEM.PUT(p+8, size+size2)
		END
	END
END Free0;

PROCEDURE New*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR p, size, i, off: INTEGER;
BEGIN
	SYSTEM.GET(tdAdr, size); size := (size+32+63) DIV 64 * 64;
	p := Alloc0(size); SYSTEM.PUT(p+16, tdAdr); ptr := p+32; INC(p, 32);
	
	i := tdAdr+64; SYSTEM.GET(i, off);
	WHILE off # -1 DO SYSTEM.PUT(p+off, 0); INC(i, 8); SYSTEM.GET(i, off) END
END New;

PROCEDURE Alloc*(VAR ptr: INTEGER; size: INTEGER);
BEGIN size := (size+32+63) DIV 64 * 64; ptr := Alloc0(size) + 32
END Alloc;

PROCEDURE Free*(ptr: INTEGER);
BEGIN Free0(ptr-32)
END Free;

PROCEDURE ReAlloc*(VAR ptr: INTEGER; nSize: INTEGER);
	VAR p, p2, size, size2, tSize, prev, k, next: INTEGER; reloc: BOOLEAN;
BEGIN
	nSize := (nSize+32+63) DIV 64 * 64; p := ptr-32; SYSTEM.GET(p+8, size);
	p2 := Alloc0(nSize); SYSTEM.COPY(p+32, p2+32, size-32);
	Free0(p); ptr := p2+32
END ReAlloc;

(* -------------------------------------------------------------------------- *)
(* Mark and Sweep *)


PROCEDURE InitHeap;
	VAR i: INTEGER;
BEGIN heapSize := 80000H;
	heapBase := VirtualAlloc(0, 80000000H, MEM_RESERVE, PAGE_READWRITE);
	IF heapBase = 0 THEN Halt('Cannot init heap') END;
	heapBase := VirtualAlloc(heapBase, heapSize, MEM_COMMIT, PAGE_READWRITE);
	IF heapBase = 0 THEN Halt('Cannot init heap') END;
	FOR i := 1 TO LEN(fList)-1 DO fList[i] := 0 END; fList0 := heapBase;
	SYSTEM.PUT(heapBase, 0); SYSTEM.PUT(heapBase+8, heapSize)
END InitHeap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Init *)

BEGIN
	Import(ExitProcess, Kernel32, 'ExitProcess');
	Import(MessageBoxW, 'USER32.DLL', 'MessageBoxW');
	Import(GetSystemTimeAsFileTime, Kernel32, 'GetSystemTimeAsFileTime');
	Import(GetCommandLineW, Kernel32, 'GetCommandLineW');

	Import(VirtualAlloc, Kernel32, 'VirtualAlloc');
	
	Import(GetFileAttributesW, Kernel32, 'GetFileAttributesW');
	Import(CreateFileW, Kernel32, 'CreateFileW');
	Import(CloseHandle, Kernel32, 'CloseHandle');
	Import(MoveFileW, Kernel32, 'MoveFileW');
	Import(DeleteFileW, Kernel32, 'DeleteFileW');
	Import(ReadFile, Kernel32, 'ReadFile');
	Import(WriteFile, Kernel32, 'WriteFile');
	Import(SetFilePointerEx, Kernel32, 'SetFilePointerEx');
	
	Import(WideCharToMultiByte, Kernel32, 'WideCharToMultiByte');
	Import(MultiByteToWideChar, Kernel32, 'MultiByteToWideChar');
	
	InitHeap
END Rtl.