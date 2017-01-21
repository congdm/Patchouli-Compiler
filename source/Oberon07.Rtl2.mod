(*
   Based on F. Negele's lock-free concurrent Garbage Collector
   in Active Oberon: https://e-collection.library.ethz.ch/view/eth:47094
*)
MODULE Oberon07.Rtl;
(*$RTL-*)
(* Only support single-threaded application *)
(* Only one mutator and one collector running concurrently *)

IMPORT
	SYSTEM;

CONST
	blkOverhead = 48;

TYPE
	CARD32 = SYSTEM.CARD32;
	CARD16 = SYSTEM.CARD16;

	Finalised* = POINTER [untraced] TO FinalisedDesc;
	FinaliseProc* = PROCEDURE(ptr: Finalised);
	FinalisedDesc* = RECORD END;

	Block = RECORD
		tdesc, cycle, ref, nextWatched, nextMarked: INTEGER;
		Finaliser: FinaliseProc
	END;
	
	Module = POINTER [untraced] TO RECORD
		base, ptrTable: INTEGER; next: Module
	END;
	
VAR
	modList: Module;
	cycleCount, firstMarked, firstWatched: INTEGER;
	markedSentinel, watchedSentinel: INTEGER;
	assignmentOngoing: INTEGER;
	
	hHeap: INTEGER;
	
	(* Win32 Interfaces *)
	GetProcessHeap: PROCEDURE(): INTEGER;
	HeapAlloc: PROCEDURE(
		hHeap: INTEGER; dwFlags: CARD32; dwBytes: INTEGER
	): INTEGER;
	HeapFree: PROCEDURE(
		hHeap: INTEGER; dwFlags: CARD32; lpMem: INTEGER
	): CARD32;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Increment(VAR x: INTEGER): INTEGER;
	VAR prev, value: INTEGER;
BEGIN
	REPEAT
		prev := SYSTEM.CAS(x, 0, 0);
		value := SYSTEM.CAS(x, prev, prev+1);
		IF value # prev THEN SYSTEM.PAUSE END
	UNTIL value = prev;
	RETURN prev
END Increment;

PROCEDURE Decrement(VAR x: INTEGER): INTEGER;
	VAR prev, value: INTEGER;
BEGIN
	REPEAT
		prev := SYSTEM.CAS(x, 0, 0);
		value := SYSTEM.CAS(x, prev, prev-1);
		IF value # prev THEN SYSTEM.PAUSE END
	UNTIL value = prev;
	RETURN prev
END Decrement;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Mark*(blk: INTEGER);
	VAR cycle, current, value, first, unused: INTEGER; exit: BOOLEAN;
BEGIN
	exit := FALSE;
	REPEAT
		cycle := SYSTEM.CAS(blk{Block}.cycle, 0, 0);
		current := SYSTEM.CAS(cycleCount, 0, 0);
		IF cycle - current >= 0 THEN exit := TRUE
		ELSE value := SYSTEM.CAS(blk{Block}.cycle, cycle, current)
		END;
		IF ~exit & (value # cycle) THEN SYSTEM.PAUSE END
	UNTIL exit OR (value = cycle);
	IF ~exit THEN
		REPEAT
			first := SYSTEM.CAS(firstMarked, 0, 0);
			IF SYSTEM.CAS(blk{Block}.nextMarked, 0, first) # 0 THEN
				exit := TRUE	
			ELSIF SYSTEM.CAS(firstMarked, first, blk) = first THEN
				exit := TRUE
			ELSE unused := SYSTEM.CAS(blk{Block}.nextMarked, first, 0)
			END;
			IF ~exit THEN SYSTEM.PAUSE END
		UNTIL exit
	END
END Mark;

PROCEDURE Trace(blk: INTEGER);
	VAR tdesc, off, ptr: INTEGER;
BEGIN
	tdesc := blk{Block}.tdesc; INC(tdesc, 64); SYSTEM.GET(tdesc, off);
	WHILE off # -1 DO
		INC(tdesc, 8); SYSTEM.GET(blk + blkOverhead + off, ptr);
		SYSTEM.GET(tdesc, off); IF ptr # 0 THEN Mark(ptr - blkOverhead) END
	END
END Trace;

PROCEDURE TraceMarked;
	VAR first, current, next: INTEGER; exit: BOOLEAN;
BEGIN
	exit := FALSE;
	REPEAT
		REPEAT
			first := SYSTEM.CAS(firstMarked, 0, 0);
			IF first = markedSentinel THEN exit := TRUE
			ELSE current := SYSTEM.CAS(firstMarked, first, markedSentinel)
			END;
			IF ~exit & (current # first) THEN SYSTEM.PAUSE END
		UNTIL exit OR (current = first);
		IF ~exit THEN
			REPEAT
				Trace(current); next := current{Block}.nextMarked;
				current{Block}.nextMarked := 0; current := next
			UNTIL current = markedSentinel
		END
	UNTIL exit
END TraceMarked;

PROCEDURE MarkGlobalReferences;
	VAR mod: Module; base, table, off, ptr: INTEGER;
BEGIN mod := modList;
	WHILE mod # NIL DO
		base := mod.base; table := mod.ptrTable; SYSTEM.GET(table, off);
		WHILE off # -1 DO SYSTEM.GET(base+off, ptr);
			IF ptr # 0 THEN Mark(ptr - blkOverhead) END;
			INC(table, 8); SYSTEM.GET(table, off)
		END
	END
END MarkGlobalReferences;

PROCEDURE MarkLocalReferences(first: INTEGER);
BEGIN
	WHILE first # watchedSentinel DO
		IF first{Block}.ref > 0 THEN Mark(first) END;
		first := first{Block}.nextWatched
	END
END MarkLocalReferences;

PROCEDURE InsertWatched(first, last: INTEGER);
	VAR value: INTEGER; exit: BOOLEAN;
BEGIN
	IF last # 0 THEN exit := FALSE;
		REPEAT
			value := SYSTEM.CAS(firstWatched, 0, 0);
			last{Block}.nextWatched := value;
			IF SYSTEM.CAS(firstWatched, value, first) = value THEN exit := TRUE
			ELSE SYSTEM.PAUSE
			END
		UNTIL exit
	END
END InsertWatched;

PROCEDURE Dispose(blk: INTEGER);
BEGIN
	IF blk{Block}.Finaliser # NIL THEN
		blk{Block}.Finaliser(SYSTEM.VAL(Finalised, blk+blkOverhead))
	END;
	ASSERT(HeapFree(hHeap, 0, blk) # 0)
END Dispose;

PROCEDURE Sweep(first, cycle: INTEGER);
	VAR current, previous, next: INTEGER;
BEGIN
	current := first; previous := 0;
	WHILE current # watchedSentinel DO
		next := current{Block}.nextWatched; ASSERT(next # 0);
		IF current{Block}.cycle - cycle <= 0 THEN
			IF previous # 0 THEN previous{Block}.nextWatched := next
			ELSE first := next
			END;
			Dispose(current)
		ELSE previous := current
		END;
		current := next
	END;
	InsertWatched(first, previous)
END Sweep;

PROCEDURE Collect;
	VAR cycle, first, current: INTEGER;
BEGIN
	cycle := Increment(cycleCount);
	REPEAT
		first := SYSTEM.CAS(firstWatched, 0, 0);
		current := SYSTEM.CAS(firstWatched, first, watchedSentinel)
	UNTIL current = first;
	MarkGlobalReferences; MarkLocalReferences(first); TraceMarked;
	WHILE assignmentOngoing > 0 DO TraceMarked END;
	Sweep(first, cycle)
END Collect;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Assign*(VAR ptr: INTEGER; val: INTEGER);
	VAR blk: INTEGER;
BEGIN
	INC(assignmentOngoing);
	IF val # 0 THEN blk := val - blkOverhead; Mark(blk) END; ptr := val;
	DEC(assignmentOngoing)
END Assign;

PROCEDURE AssignLocal*(VAR ptr: INTEGER; val: INTEGER);
	VAR blk: INTEGER;
BEGIN
	INC(assignmentOngoing);
	IF val # 0 THEN blk := val-blkOverhead; Mark(blk); INC(blk{Block}.ref) END;
	IF ptr # 0 THEN blk := ptr-blkOverhead; DEC(blk{Block}.ref) END;
	ptr := val;
	DEC(assignmentOngoing)
END AssignLocal;

PROCEDURE ReleaseLocal*(ptr: INTEGER);
	VAR blk: INTEGER;
BEGIN
	IF ptr # 0 THEN blk := ptr - blkOverhead; DEC(blk{Block}.ref) END
END ReleaseLocal;

PROCEDURE Watch(blk: INTEGER);
	VAR value, value2: INTEGER;
BEGIN
	blk{Block}.cycle := SYSTEM.CAS(cycleCount, 0, 0);
	REPEAT
		value := SYSTEM.CAS(firstWatched, 0, 0);
		blk{Block}.nextWatched := value;
		value2 := SYSTEM.CAS(firstWatched, value, blk);
		IF value2 # value THEN SYSTEM.PAUSE END
	UNTIL value2 = value
END Watch;

PROCEDURE New*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR newPtr, size: INTEGER;
BEGIN
	SYSTEM.GET(tdAdr, size); newPtr := HeapAlloc(hHeap, 8, blkOverhead + size);
	ASSERT(newPtr # 0); ASSERT(newPtr{Block}.nextWatched = 0);
	Watch(newPtr); INC(newPtr, blkOverhead); Assign(ptr, newPtr)
END New;

PROCEDURE NewLocal*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR newPtr, size: INTEGER;
BEGIN
	SYSTEM.GET(tdAdr, size); newPtr := HeapAlloc(hHeap, 8, blkOverhead + size);
	ASSERT(newPtr # 0); ASSERT(newPtr{Block}.nextWatched = 0);
	Watch(newPtr); INC(newPtr, blkOverhead); AssignLocal(ptr, newPtr)
END NewLocal;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Win32 API *)

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

PROCEDURE InitWin32;
	CONST Kernel32 = 'KERNEL32.DLL';
BEGIN
	Import(GetProcessHeap, Kernel32, 'GetProcessHeap');
	Import(HeapAlloc, Kernel32, 'HeapAlloc');
	Import(HeapFree, Kernel32, 'HeapFree');
	hHeap := GetProcessHeap();
END InitWin32;

BEGIN
	InitWin32;
	markedSentinel := HeapAlloc(hHeap, 8, 64); ASSERT(markedSentinel # 0);
	watchedSentinel := HeapAlloc(hHeap, 8, 64); ASSERT(watchedSentinel # 0);
	firstMarked := markedSentinel; firstWatched := watchedSentinel
END Rtl.