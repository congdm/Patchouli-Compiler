MODULE MemFiles;
IMPORT SYSTEM, Rtl;
	
CONST
	memFileBlk = 64;
	
TYPE
	Pointer = INTEGER;
	File* = POINTER TO RECORD ptr: Pointer; len, maxlen: INTEGER END;
	Rider* = RECORD f: File; pos: INTEGER; eof*: BOOLEAN END;
		
PROCEDURE New*(VAR f: File);
BEGIN
	NEW(f); Rtl.Alloc(f.ptr, memFileBlk);
	f.maxlen := memFileBlk; f.len := 0
END New;

PROCEDURE Delete*(f: File);
BEGIN 
	Rtl.Free(f.ptr); f.ptr := 0; f.maxlen := 0; f.len := 0
END Delete;

PROCEDURE Extend(f: File; amount: INTEGER);
BEGIN
	amount := amount + (-amount) MOD memFileBlk;
	INC(f.maxlen, amount); Rtl.ReAlloc(f.ptr, f.maxlen)
END Extend;

PROCEDURE Merge*(f1, f2: File);
	VAR newLen: INTEGER;
BEGIN
	IF f1.maxlen < f1.len + f2.len THEN
		newLen := f1.len + f2.len;
		newLen := newLen + (-newLen) MOD memFileBlk;
		Extend(f1, newLen - f1.maxlen)
	END;
	SYSTEM.COPY(f2.ptr, f1.ptr + f1.len, f2.len);
	INC(f1.len, f2.len); Delete(f2)
END Merge;

PROCEDURE Length*(f: File): INTEGER;
	RETURN f.len
END Length;

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
BEGIN
	r.f := f; r.eof := FALSE;
	IF pos >= 0 THEN
		IF pos <= f.len THEN r.pos := pos ELSE r.pos := f.len END
	ELSE r.pos := 0
	END
END Set;

PROCEDURE Write*(VAR r: Rider; x: BYTE);
BEGIN
	IF r.pos <= r.f.len THEN
		IF r.pos+1 > r.f.maxlen THEN Extend(r.f, memFileBlk) END;
		SYSTEM.PUT(r.f.ptr + r.pos, x); INC(r.pos);
		IF r.pos > r.f.len THEN r.f.len := r.pos END
	ELSE r.eof := TRUE
	END
END Write;

PROCEDURE Write8*(VAR r: Rider; x: INTEGER);
BEGIN
	IF r.pos <= r.f.len THEN
		IF r.pos+8 > r.f.maxlen THEN Extend(r.f, memFileBlk) END;
		SYSTEM.PUT(r.f.ptr + r.pos, x); INC(r.pos, 8);
		IF r.pos > r.f.len THEN r.f.len := r.pos END
	ELSE r.eof := TRUE
	END
END Write8;

PROCEDURE Read*(VAR r: Rider; VAR x: BYTE);
BEGIN
	IF r.pos < r.f.len THEN
		SYSTEM.GET(r.f.ptr + r.pos, x); INC(r.pos)
	ELSE r.eof := TRUE
	END
END Read;

PROCEDURE ToDisk*(mf: File; f: Rtl.File);
	VAR byteWritten: INTEGER;
BEGIN
	byteWritten := mf.len;
	Rtl.WriteBuf(f, mf.ptr, byteWritten) 
END ToDisk;

END MemFiles.