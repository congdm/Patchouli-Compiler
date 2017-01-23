MODULE Oberon07.Rtl; (* multi-threaded application NOT SUPPORTED *)
(*$RTL-*)
IMPORT SYSTEM;
	
CONST
	MinInt = 8000000000000000H; MaxInt = 7FFFFFFFFFFFFFFFH;

	Kernel32 = 'KERNEL32.DLL';
	HeapMax = 80000000H; blkMeta = 16; markedSentinel = 2;
	
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
	
	Finalised* = POINTER [untraced] TO FinalisedDesc;
	FinaliseProc* = PROCEDURE(ptr: Finalised);
	FinalisedDesc* = RECORD
		Finalise: FinaliseProc; next: Finalised
	END;

VAR
	modList*, nMod*: INTEGER;
	argv, numArgs*: INTEGER;
	finalisedList: Finalised;

	(* Utility *)
	ExitProcess: PROCEDURE(uExitCode: Uint);
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
	heapBase, heapSize, markedList: INTEGER; allocated*: INTEGER;
	fList: ARRAY 4 OF INTEGER; fList0: INTEGER;
	
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
	p := VirtualAlloc(HeapLimit(), heapSize, MEM_COMMIT, PAGE_READWRITE);
	ASSERT(heapSize < HeapMax); ASSERT(p # 0);
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

PROCEDURE GetBlock32(): INTEGER;
	VAR p, q: INTEGER;
BEGIN
	IF fList[0] # 0 THEN p := fList[0]; SYSTEM.GET(fList[0]+16, fList[0])
	ELSE q := GetBlock64(); SYSTEM.PUT(q+32, 32); SYSTEM.PUT(q+(32+8), -1);
		SYSTEM.PUT(q+(32+16), 0); fList[0] := q + 32; p := q
	END;
	RETURN p
END GetBlock32;

PROCEDURE Rounding(VAR size: INTEGER);
BEGIN
	IF size < 32 THEN size := 32 ELSIF size < 64 THEN size := 64
	ELSIF size < 128 THEN size := 128 ELSIF size < 256 THEN size := 256
	ELSE size := (size+511) DIV 512 * 512
	END
END Rounding;

PROCEDURE New*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR p, size, need, lim: INTEGER;
BEGIN
	SYSTEM.GET(tdAdr, size); need := size+16; Rounding(need);
	IF need = 32 THEN p := GetBlock32()
	ELSIF need = 64 THEN p := GetBlock64()
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
(* Combined ideas from both N. Wirth's and F. Negele's Garbage Collectors *)

(* Block metadata is 16 bytes for allocated block *)
(* First word: Type desc address *)
(* Second word: Mark word - 0 is not marked - 1 is marked - otherwise *)
(*              pointer to the next element in marked list            *)

(* For free block is 32 bytes *)
(* First word: Size *)
(* Second word: Mark = -1 *)
(* Third word: Next element in free list *)
(* Fourth word: Unused *)

(*
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
*)

PROCEDURE Mark(blk: INTEGER);
	VAR mark: INTEGER;
BEGIN SYSTEM.GET(blk+8, mark);
	IF mark # 0 THEN (* already marked *)
	ELSE SYSTEM.PUT(blk+8, markedList); markedList := blk
	END
END Mark;

PROCEDURE TraceMarked;
	VAR list, tdAdr, off, ptr, next: INTEGER;
BEGIN list := markedList; markedList := markedSentinel;
	WHILE list # markedSentinel DO
		SYSTEM.GET(list, tdAdr); INC(tdAdr, 64); SYSTEM.GET(tdAdr, off);
		WHILE off # -1 DO
			SYSTEM.GET(list+blkMeta+off, ptr); DEC(ptr, blkMeta);
			IF ptr >= heapBase THEN Mark(ptr) END;
			INC(tdAdr, 8); SYSTEM.GET(tdAdr, off)
		END;
		SYSTEM.GET(list+8, next); SYSTEM.PUT(list+8, 1); list := next
	END
END TraceMarked;

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
			IF size MOD 64 # 0 THEN
				SYSTEM.PUT(q, 32); SYSTEM.PUT(q+8, -1);
				SYSTEM.PUT(q+16, fList[0]); fList[0] := q;
				INC(q, 32); DEC(size, 32)
			END ;
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

PROCEDURE Finalise;
	VAR prev, ptr, next: Finalised; p, mark: INTEGER;
BEGIN ptr := finalisedList;
	WHILE ptr # NIL DO
		p := SYSTEM.VAL(INTEGER, ptr) - 16; SYSTEM.GET(p+8, mark);
		IF mark = 0 (* released *) THEN
			next := ptr.next;
			IF prev # NIL THEN prev.next := next
			ELSE finalisedList := SYSTEM.VAL(Finalised, next)
			END;
			ptr.Finalise(ptr); ptr := SYSTEM.VAL(Finalised, next)
		ELSE prev := ptr; ptr := SYSTEM.VAL(Finalised, ptr.next)
		END
	END
END Finalise;

PROCEDURE Collect*;
	VAR i, modBase, ptrTable, off, ptr: INTEGER;
BEGIN i := 0;
	WHILE i < nMod DO SYSTEM.GET(modList+i*8, modBase);
		SYSTEM.GET(modBase+112, ptrTable); SYSTEM.GET(ptrTable, off);
		WHILE off # -1 DO
			SYSTEM.GET(modBase+off, ptr); DEC(ptr, blkMeta);
			IF ptr >= heapBase THEN Mark(ptr) END;
			INC(ptrTable, 8); SYSTEM.GET(ptrTable, off)
		END;
		INC(i)
	END;
	WHILE markedList # markedSentinel DO TraceMarked END; Finalise; Scan
END Collect;

PROCEDURE RegisterFinalised*(ptr: Finalised; finalise: FinaliseProc);
BEGIN
	ASSERT(finalise # NIL); ptr.Finalise := finalise;
	ptr.next := SYSTEM.VAL(Finalised, finalisedList); finalisedList := ptr
END RegisterFinalised;

(* -------------------------------------------------------------------------- *)

PROCEDURE InitHeap;
	VAR i, p: INTEGER;
BEGIN
	heapBase := VirtualAlloc(0, 80000000H, MEM_RESERVE, PAGE_READWRITE);
	heapSize := 80000H; ASSERT(heapBase # 0);
	heapBase := VirtualAlloc(heapBase, heapSize, MEM_COMMIT, PAGE_READWRITE);
	ASSERT(heapBase # 0);
	
	FOR i := 0 TO LEN(fList)-1 DO fList[i] := 0 END;
	p := heapBase; fList0 := heapBase; allocated := 0;
	SYSTEM.PUT(p, heapSize); SYSTEM.PUT(p+8, -1); SYSTEM.PUT(p+16, 0);
	markedList := markedSentinel
END InitHeap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Halt*(exitCode: INTEGER);
BEGIN
	nMod := 0; Collect;
	ExitProcess(exitCode)
END Halt;

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
	
	Import(WideCharToMultiByte, Kernel32, 'WideCharToMultiByte');
	Import(MultiByteToWideChar, Kernel32, 'MultiByteToWideChar');
	Import(CharLowerBuffW, 'User32.dll', 'CharLowerBuffW');
	
	GetArgv; InitHeap
END Rtl.