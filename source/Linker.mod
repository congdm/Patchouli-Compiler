MODULE Linker;
IMPORT
	SYSTEM, Rtl, Strings,
	S := Scanner, B := Base;
	
CONST
	HeaderSize = 400H;
	
VAR
	out: Rtl.File; fname*: B.String;
	imagebase, entry, pc: INTEGER;
	
	code_rva, code_size, code_rawsize, code_fadr: INTEGER;
	data_rva, data_size, data_rawsize, data_fadr: INTEGER;
	bss_size, bss_rva: INTEGER;
	edata_rva, edata_size, edata_rawsize, edata_fadr: INTEGER;
	idata_rva, idata_fadr: INTEGER;
	reloc_rva, reloc_fadr: INTEGER;
	pdata_rva, pdata_size, pdata_rawsize, pdata_fadr: INTEGER;
			
	debug_info_rva, debug_info_size: INTEGER;
	debug_info_rawsize, debug_info_fadr: INTEGER;	
	debug_abbrev_rva, debug_abbrev_size: INTEGER;
	debug_abbrev_rawsize, debug_abbrev_fadr: INTEGER;
	
	Kernel32Table: ARRAY 6 OF INTEGER;
	modPtrTable: INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Align(VAR a: INTEGER; align: INTEGER);
BEGIN
	IF a > 0 THEN a := (a + align - 1) DIV align * align
	ELSIF a < 0 THEN a := a DIV align * align
	END
END Align;
	
PROCEDURE Write_idata_section;
	CONST table_len = LEN(Kernel32Table);
		table_size = table_len * 8; table_rva = 40;
		name_rva = table_rva + table_size; hint_rva = name_rva + 16;
	VAR i: INTEGER;
	
	PROCEDURE WriteHint(func: ARRAY OF CHAR; VAR off: INTEGER);
	BEGIN
		Rtl.Seek(out, idata_fadr + hint_rva + 2 + off); INC(off, 32);
		Rtl.WriteAnsiStr(out, func); ASSERT(LEN(func) <= 30)
	END WriteHint;
	
BEGIN (* Write_idata_section *)
	Rtl.Seek(out, idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Rtl.Write4(out, idata_rva + table_rva);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, idata_rva + name_rva);
	Rtl.Write4(out, data_rva);

	Rtl.Seek(out, idata_fadr + table_rva); i := 0;
	WHILE i <= table_len - 2 DO
		Kernel32Table[i] := idata_rva + hint_rva + 32 * i;
		Rtl.Write8(out, Kernel32Table[i]); INC(i)
	END;
	Kernel32Table[table_len - 1] := 0;
	
	Rtl.Seek(out, idata_fadr + name_rva);
	Rtl.WriteAnsiStr(out, 'KERNEL32.DLL'); i := 0;
	
	WriteHint('GetProcAddress', i);
	WriteHint('LoadLibraryW', i);
	WriteHint('ExitProcess', i);
	WriteHint('GetModuleHandleExW', i);
	WriteHint('AddVectoredExceptionHandler', i)
END Write_idata_section;

PROCEDURE Write_pointer_offset(offset: INTEGER; type: B.Type);
	VAR ident: B.Ident; field: B.Field; size, k, ptrcnt, n: INTEGER;
BEGIN ptrcnt := type.nptr;
	IF type.form = B.tRec THEN ident := type.fields;
		IF (type.base # NIL) & (type.base.nptr # 0) THEN
			Write_pointer_offset(offset, type.base)
		END;
		WHILE ident # NIL DO field := ident.obj(B.Field);
			IF field.type.nptr > 0 THEN n := offset + field.off;
				IF field.type.form = B.tPtr THEN Rtl.Write8(out, n)
				ELSE Write_pointer_offset(n, field.type)
				END
			END;
			ident := ident.next
		END
	ELSIF type.form = B.tArray THEN type := type.base;
		IF type.form = B.tPtr THEN n := offset;
			REPEAT Rtl.Write8(out, n); DEC(ptrcnt); INC(n, 8)
			UNTIL ptrcnt <= 0; ASSERT(ptrcnt = 0)
		ELSE k := type.nptr; size := type.size;
			REPEAT
				Write_pointer_offset(offset, type);
				DEC(ptrcnt, k); INC(offset, size)
			UNTIL ptrcnt <= 0; ASSERT(ptrcnt = 0)
		END
	ELSE ASSERT(FALSE)
	END
END Write_pointer_offset;

PROCEDURE Write_data_section;
	VAR basefadr, i, j, adr: INTEGER; b: BYTE;
		imod: B.Module; ident: B.Ident;
		x: B.Str; t: B.TypeList; obj: B.Object;
BEGIN
	basefadr := data_fadr;
	Rtl.Seek(out, basefadr); i := 0;
	WHILE i < LEN(Kernel32Table) DO
		Rtl.Write8(out, Kernel32Table[i]); INC(i)
	END; i := 0;
	WHILE i < B.modno DO imod := B.modList[i];
		Rtl.Seek(out, basefadr + imod.adr); j := 0;
		WHILE imod.name[j] # 0X DO
			Rtl.Write2(out, ORD(imod.name[j])); INC(j)
		END;
		Rtl.WriteStr(out, '.dll'); INC(i)	
	END;
	ident := B.strList;
	WHILE ident # NIL DO x := ident.obj(B.Str);
		Rtl.Seek(out, basefadr + x.adr); i := 0;
		WHILE i < x.len DO
			Rtl.Write2(out, ORD(B.strbuf[x.bufpos+i])); INC(i)
		END;
		ident := ident.next
	END;
	t := B.recList;
	WHILE t # NIL DO
		Rtl.Seek(out, basefadr + t.type.adr); Rtl.Write8(out, t.type.size);
		Rtl.Seek(out, basefadr + t.type.adr + 8 + B.MaxExt*8);
		IF t.type.nptr > 0 THEN Write_pointer_offset(0, t.type) END;
		Rtl.Write8(out, -1); t := t.next
	END;
	Rtl.Seek(out, basefadr + modPtrTable); ident := B.universe.first;
	WHILE ident # NIL DO obj := ident.obj;
		IF (obj IS B.Var) & ~(obj IS B.Str) THEN
			IF obj.type.nptr > 0 THEN adr := obj(B.Var).adr;
				IF obj.type.form = B.tPtr THEN Rtl.Write8(out, adr)
				ELSE Write_pointer_offset(adr, obj.type)
				END
			END
		END;
		ident := ident.next
	END;
	Rtl.Write8(out, -1)
END Write_data_section;

PROCEDURE Write_code_section(code: ARRAY OF BYTE);
	VAR cnt: INTEGER;
BEGIN
	Rtl.Seek(out, code_fadr); cnt := pc;
	Rtl.WriteBuf(out, SYSTEM.ADR(code), cnt)
END Write_code_section;

PROCEDURE Write_reloc_section;
BEGIN
	Rtl.Seek(out, reloc_fadr);
	Rtl.Write4(out, 4);
	Rtl.Write4(out, 12);
	Rtl.Write2(out, 0);
	Rtl.Write2(out, 0)
END Write_reloc_section;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* .pdata *)

PROCEDURE Write_pdata_section(VAR debug: Rtl.File);
	VAR data: ARRAY 200H OF BYTE; len, cnt: INTEGER;
BEGIN
	Rtl.Seek(out, pdata_fadr+32); len := Rtl.Pos(debug);
	Rtl.Seek(debug, 0); Rtl.ReadBytes(debug, data, cnt);
	WHILE len > 0 DO
		IF cnt > len THEN cnt := len END; DEC(len, cnt);
		Rtl.WriteBuf(out, SYSTEM.ADR(data), cnt);
		Rtl.ReadBytes(debug, data, cnt)
	END;
	Rtl.Write8(out, -1); Rtl.Close(debug); Rtl.Delete('.pocDebug');
	pdata_size := Rtl.Pos(out) - pdata_fadr;
	pdata_rawsize := (pdata_size + 511) DIV 512 * 512
END Write_pdata_section;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* .edata *)

PROCEDURE Write_edata_section;
	CONST dirSz = 40;	
	TYPE
		Export = POINTER TO RECORD
			no: INTEGER; ident: B.Ident; next: Export
		END;	
	VAR
		namedList, p: Export;
		ident: B.Ident; x: B.Object; name: B.String;
		i, rva, expno, nameRva, nameSz, namecnt: INTEGER;
		adrTblSz, namePtrTblSz, ordTblSz: INTEGER;	

	PROCEDURE Insert(VAR list: Export; x: Export);
		VAR p: Export;
	BEGIN
		IF list = NIL THEN list := x
		ELSIF x.ident.name < list.ident.name THEN x.next := list; list := x
		ELSE p := list;
			WHILE (p.next # NIL) & (x.ident.name > p.next.ident.name) DO
				p := p.next
			END;
			x.next := p.next; p.next := x
		END
	END Insert;
	
	PROCEDURE NewExport(ident: B.Ident; no: INTEGER): Export;
		VAR p: Export;
	BEGIN
		NEW(p); p.ident := ident; p.no := no;
		RETURN p
	END NewExport;
		
BEGIN (* Write_edata_section *)
	edata_rva := pdata_rva + pdata_size; Align(edata_rva, 4096);
	edata_fadr := pdata_fadr + pdata_rawsize;

	name[0] := 0X; Strings.Append(B.modid, name);
	IF B.Flag.main THEN Strings.Append('.exe', name)
	ELSE Strings.Append('.dll', name)
	END; nameSz := Strings.Length(name)+1;
	expno := B.expno; adrTblSz := expno*4;
	
	ident := B.expList; namecnt := 0; i := 1;
	WHILE ident # NIL DO
		IF ident.name[0] # 0X THEN
			INC(namecnt); Insert(namedList, NewExport(ident, i))
		END;
		ident := ident.next; INC(i)
	END;
	namePtrTblSz := namecnt*4; ordTblSz := namecnt*2;
	nameRva := edata_rva + dirSz + adrTblSz + namePtrTblSz + ordTblSz;

	(* Export directory *)
	Rtl.Seek(out, edata_fadr + 12);
	Rtl.Write4(out, nameRva);
	Rtl.Write4(out, 1);
	Rtl.Write4(out, expno);
	Rtl.Write4(out, namecnt);
	Rtl.Write4(out, edata_rva + dirSz);
	Rtl.Write4(out, edata_rva + dirSz + adrTblSz);
	Rtl.Write4(out, edata_rva + dirSz + adrTblSz + namePtrTblSz);
	
	(* Export address table *)
	Rtl.Seek(out, edata_fadr + dirSz); ident := B.expList;
	WHILE ident # NIL DO x := ident.obj;
		IF x.class = B.cType THEN rva := data_rva + x.type.adr
		ELSIF x IS B.Var THEN rva := data_rva + x(B.Var).adr
		ELSIF x IS B.Proc THEN rva := code_rva + x(B.Proc).adr
		END;
		Rtl.Write4(out, rva); ident := ident.next
	END;
	
	IF namecnt > 0 THEN
		(* Export Name Pointer Table *)
		p := namedList; i := nameRva + nameSz;
		WHILE p # NIL DO Rtl.Write4(out, i);
			INC(i, Strings.Length(p.ident.name)+1); p := p.next
		END;
		(* Export Ordinal Table *)
		p := namedList;
		WHILE p # NIL DO Rtl.Write2(out, p.no-1); p := p.next END
	END;
		
	(* Name string *)
	Rtl.WriteAnsiStr(out, name);
	IF namecnt > 0 THEN p := namedList;
		WHILE p # NIL DO Rtl.WriteAnsiStr(out, p.ident.name); p := p.next END
	END;
	
	edata_size := Rtl.Pos(out) - edata_fadr;
	edata_rawsize := edata_size;
	IF edata_rawsize MOD 512 # 0 THEN
		Align(edata_rawsize, 512);
		Rtl.Seek(out, edata_fadr + edata_rawsize - 1);
		Rtl.Write1(out, 1)
	END
END Write_edata_section;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* PE Header *)

PROCEDURE Write_SectionHeader (
	name: ARRAY OF CHAR; chr, rva, rawsize, size, fileadr: INTEGER
);	
	VAR b: BYTE; i: INTEGER;	
BEGIN i := 0;
	WHILE i < 8 DO b := 0;
		IF i < LEN(name) THEN b := ORD(name[i]) END;
		Rtl.Write1(out, b); INC(i)
	END;
	Rtl.Write4(out, size);
	Rtl.Write4(out, rva);
	Rtl.Write4(out, rawsize);
	Rtl.Write4(out, fileadr);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, chr)
END Write_SectionHeader;

PROCEDURE Write_PEHeader;
	VAR k, nSection: INTEGER;
BEGIN
	Rtl.Seek(out, 0);
	Rtl.Write2(out, 5A4DH);
	Rtl.Seek(out, 60);
	Rtl.Write4(out, 128);
	Rtl.Seek (out, 128);
	Rtl.Write4(out, 4550H);
	
	Rtl.Write2(out, 8664H); (* Machine = AMD64/Intel 64 *)
	nSection := 6; IF bss_size > 0 THEN INC(nSection) END;
	Rtl.Write2(out, nSection); (* NumberOfSections *)
	Rtl.Write4(out, 0); (* TimeDateStamp *)
	Rtl.Write4(out, 0); (* PointerToSymbolTable *)
	Rtl.Write4(out, 0); (* NumberOfSymbols *)
	Rtl.Write2(out, 240);
	
	(* Characteristics *)
	IF B.Flag.main THEN Rtl.Write2(out, 20H + 2 + 1)
	ELSE Rtl.Write2(out, 2000H + 20H + 2)
	END;
	
	Rtl.Write2(out, 20BH); (* Magic number for PE32+ *)
	Rtl.Write2(out, 0);
	Rtl.Write4(out, code_rawsize); (* SizeOfCode *)
	k := data_rawsize + 200H * 2 + edata_rawsize;
	k := k + pdata_rawsize;
	Rtl.Write4(out, k); (* SizeOfInitializedData *)
	Rtl.Write4(out, bss_size); (* SizeOfUninitializedData *)
	Rtl.Write4(out, code_rva + entry);
	Rtl.Write4(out, code_rva);
	
	Rtl.Write8(out, imagebase);
	Rtl.Write4(out, 4096);
	Rtl.Write4(out, 512);
	Rtl.Write2(out, 5); (* MajorOSVer *)
	Rtl.Write2(out, 0);
	Rtl.Write2(out, 0);
	Rtl.Write2(out, 0);
	Rtl.Write2(out, 5);
	Rtl.Write2(out, 0);
	Rtl.Write4(out, 0);
	k := 4096 + (code_size + 4095) DIV 4096 * 4096;
	k := k + (pdata_size + 4095) DIV 4096 * 4096;
	k := k + bss_size + data_size + 4096 + 4096;
	k := k + (edata_size + 4095) DIV 4096 * 4096;
	Rtl.Write4(out, k); (* SizeOfImage *)
	Rtl.Write4(out, HeaderSize); (* SizeOfHeaders *)
	Rtl.Write4(out, 0);
	IF B.Flag.console THEN Rtl.Write2(out, 3) (* Subsys = Console *)
	ELSE Rtl.Write2(out, 2) (* Subsys = GUI *)
	END;
	
	(* DLL Characteristics *)
	IF B.Flag.main THEN Rtl.Write2(out, 0)
	ELSE Rtl.Write2(out, 100H + 40H)
	END;
	
	Rtl.Write8(out, 1000H); (* Size of stack reserve *)
	Rtl.Write8(out, 1000H); (* Size of stack commit *)
	Rtl.Write8(out, 10000H); (* Size of heap reserve *)
	Rtl.Write8(out, 0);
	Rtl.Write4(out, 4);
	Rtl.Write4(out, 16);
	
	Rtl.Write4(out, edata_rva);
	Rtl.Write4(out, edata_size);
	Rtl.Write4(out, idata_rva);
	Rtl.Write4(out, 130H);
	Rtl.Write8(out, 0); (* Resource Table *)
	Rtl.Write8(out, 0); (* Exception Table *)
	Rtl.Write8(out, 0); (* Certificate Table *)
	Rtl.Write4(out, reloc_rva);
	Rtl.Write4(out, 12);
	Rtl.Seek(out, Rtl.Pos(out) + 8 * 10);
	
	IF bss_size > 0 THEN
		Write_SectionHeader(
			'.bss', -1073741696, bss_rva, 0, bss_size, 0
		)
	END;
	Write_SectionHeader (
		'.data', -1073741760, data_rva, data_rawsize, data_size, data_fadr
	);
	Write_SectionHeader (
		'.text', 60000020H, code_rva, code_rawsize, code_size, code_fadr
	);
	Write_SectionHeader (
		'.idata', -1073741760, idata_rva, 200H, 130H, idata_fadr
	);
	Write_SectionHeader (
		'.reloc', 42000040H, reloc_rva, 200H, 12, reloc_fadr
	);
	Write_SectionHeader (
		'.pdata', 40000040H, pdata_rva, pdata_rawsize, pdata_size, pdata_fadr
	);
	Write_SectionHeader (
		'.edata', 40000040H, edata_rva, edata_rawsize, edata_size, edata_fadr
	);
	
	(* Compiler-specifics data *)
	Rtl.Seek(out, 400H - 32);
	Rtl.Write8(out, code_rva); Rtl.Write8(out, pdata_rva+32);
	Rtl.Write8(out, B.modkey[0]); Rtl.Write8(out, B.modkey[1])
END Write_PEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Link*(
	out0: Rtl.File; VAR debug: Rtl.File; code: ARRAY OF BYTE;
	pc0, entry0, staticSize, varSize, modPtrTable0: INTEGER
);
	VAR n: INTEGER;
BEGIN
	out := out0; pc := pc0; entry := entry0; modPtrTable := modPtrTable0;
	IF B.Flag.main THEN imagebase := 400000H ELSE imagebase := 10000000H END;

	code_size := pc;
	code_rawsize := (code_size + 511) DIV 512 * 512;
	
	data_size := staticSize;
	data_rawsize := (data_size+511) DIV 512 * 512;
	
	bss_size := varSize; Align(bss_size, 4096);
	
	bss_rva := 1000H;
	data_rva := bss_rva + bss_size;
	n := data_size; Align(n, 4096);
	code_rva := data_rva + n;
	n := code_size; Align(n, 4096);
	idata_rva := code_rva + n;
	reloc_rva := idata_rva + 4096;
	pdata_rva := reloc_rva + 4096;
	
	code_fadr := HeaderSize;
	idata_fadr := code_fadr + code_rawsize;
	data_fadr := idata_fadr + 200H;
	reloc_fadr := data_fadr + data_rawsize;
	pdata_fadr := reloc_fadr + 200H;
	
	Write_code_section(code); Write_idata_section; Write_data_section;
	Write_reloc_section; Write_pdata_section(debug);
	Write_edata_section; Write_PEHeader; Rtl.Close(out);
	
	(* Rename files *)
	fname[0] := 0X; Strings.Append(B.modid, fname);
	IF B.Flag.main THEN Strings.Append('.exe', fname)
	ELSE Strings.Append('.dll', fname)
	END;
	Rtl.Delete(fname); Rtl.Rename('.tempOut', fname)
END Link;

END Linker.