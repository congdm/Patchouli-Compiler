MODULE Test5;
(*$CONSOLE*)
IMPORT SYSTEM, Rtl := [Oberon07.Rtl], Out := [Oberon07.Out];

TYPE
	Handle = INTEGER;
	Dword = SYSTEM.CARD32;
	SizeT = INTEGER;
	Pointer = INTEGER;

VAR
	list, p: Pointer; i: INTEGER;
	HeapAlloc: PROCEDURE(hHeap: Handle; dwFlags: Dword; dwBytes: SizeT): Pointer;
	GetProcessHeap: PROCEDURE(): Handle;
	
PROCEDURE PrintList(list: Pointer);
	VAR p: Pointer;
BEGIN p := list; 
	WHILE p # 0 DO Out.Hex(p, 0); Out.Char(' '); SYSTEM.GET(p, p) END; Out.Ln
END PrintList;

PROCEDURE MergeSort(VAR f0: INTEGER);
	TYPE FileArray = ARRAY 4 OF INTEGER; 
	VAR f: FileArray; run, len: INTEGER; flag: BOOLEAN;
	
	PROCEDURE Merge(VAR f0, f1, f2, f3: INTEGER; run: INTEGER);
		VAR l2, l3, i0, i1, x, y: INTEGER; flag2: BOOLEAN;
	BEGIN
		l2 := SYSTEM.ADR(f2); l3 := SYSTEM.ADR(f3); flag2 := TRUE;
		Out.String('Phase with run size '); Out.Int(run, 0); Out.Char(':'); Out.Ln;
		REPEAT x := f0; y := f1;
			IF x = 0 THEN i0 := run ELSE i0 := 0 END;
			IF y = 0 THEN i1 := run ELSE i1 := 0 END;
			WHILE (i0 < run) & (i1 < run) DO
				IF x < y THEN SYSTEM.GET(f0, f0);
					Out.Hex(x, 0); Out.Char(' ');
					IF flag2 THEN SYSTEM.PUT(l2, x); l2 := x
					ELSE SYSTEM.PUT(l3, x); l3 := x
					END;
					x := f0; INC(i0); IF x = 0 THEN i0 := run END
				ELSE SYSTEM.GET(f1, f1);
					Out.Hex(y, 0); Out.Char(' ');
					IF flag2 THEN SYSTEM.PUT(l2, y); l2 := y
					ELSE SYSTEM.PUT(l3, y); l3 := y
					END;
					y := f1; INC(i1); IF y = 0 THEN i1 := run END
				END	
			END;
			WHILE i0 < run DO SYSTEM.GET(f0, f0);
				Out.Hex(x, 0); Out.Char(' ');
				IF flag2 THEN SYSTEM.PUT(l2, x); l2 := x
				ELSE SYSTEM.PUT(l3, x); l3 := x
				END;
				x := f0; INC(i0); IF x = 0 THEN i0 := run END
			END;
			WHILE i1 < run DO SYSTEM.GET(f1, f1);
				Out.Hex(y, 0); Out.Char(' ');
				IF flag2 THEN SYSTEM.PUT(l2, y); l2 := y
				ELSE SYSTEM.PUT(l3, y); l3 := y
				END;
				 y := f1; INC(i1); IF y = 0 THEN i1 := run END
			END;
			Out.Ln; flag2 := ~flag2
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

BEGIN list := 0;
	Rtl.Import(HeapAlloc, 'kernel32.dll', 'HeapAlloc');
	Rtl.Import(GetProcessHeap, 'kernel32.dll', 'GetProcessHeap');
	FOR i := 1 TO 397 DO
		p := HeapAlloc(GetProcessHeap(), 0, 32);
		SYSTEM.PUT(p, list); list := p
	END;
	MergeSort(list)
END Test5.