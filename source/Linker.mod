MODULE Linker;
IMPORT
	SYSTEM, Files,
	B := Base;
	
CONST
	HeaderSize = 400H;
	
VAR
	out: Files.File; rider: Files.Rider;
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
	VAR i, n: INTEGER;
	
	PROCEDURE WriteHint(func: ARRAY OF CHAR; VAR off: INTEGER);
	BEGIN
		Files.Set(rider, out, idata_fadr + hint_rva + 2 + off);
		INC(off, 32); Files.WriteByteStr(rider, func); ASSERT(LEN(func) <= 30)
	END WriteHint;
	
BEGIN (* Write_idata_section *)
	Files.Set(rider, out, idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Files.WriteCard32(rider, idata_rva + table_rva);
	Files.WriteCard32(rider, 0);
	Files.WriteCard32(rider, 0);
	Files.WriteCard32(rider, idata_rva + name_rva);
	Files.WriteCard32(rider, data_rva);

	Files.Set(rider, out, idata_fadr + table_rva); i := 0;
	WHILE i <= table_len - 2 DO
		Kernel32Table[i] := idata_rva + hint_rva + 32 * i;
		Files.WriteInt(rider, Kernel32Table[i]); INC(i)
	END;
	Kernel32Table[table_len - 1] := 0;
	
	Files.Set(rider, out, idata_fadr + name_rva);
	Files.WriteByteStr(rider, 'KERNEL32.DLL'); i := 0;
	
	WriteHint('GetProcAddress', i);
	WriteHint('LoadLibraryW', i);
	WriteHint('ExitProcess', i);
	WriteHint('GetModuleHandleExW', i);
	WriteHint('AddVectoredExceptionHandler', i)
END Write_idata_section;

PROCEDURE Write_pointer_offset(offset: INTEGER; type: B.Type);
	VAR ident: B.Ident; field: B.Field;
		size, k, n: INTEGER; base: B.Type;
BEGIN
	IF type.form = B.tRec THEN ident := type.fields;
		IF (type.base # NIL) & (type.base.nTraced > 0) THEN
			Write_pointer_offset(offset, type.base)
		END;
		WHILE ident # NIL DO field := ident.obj(B.Field);
			IF field.type.nTraced > 0 THEN n := offset + field.off;
				IF field.type.form = B.tPtr THEN Files.WriteInt(rider, n)
				ELSE Write_pointer_offset(n, field.type)
				END
			END;
			ident := ident.next
		END
	ELSIF type.form = B.tArray THEN
		base := type.base; k := 0;
		IF base.form = B.tPtr THEN n := offset;
			WHILE k < type.len DO
				Files.WriteInt(rider, n); INC(k); INC(n, 8)
			END
		ELSE size := base.size;
			WHILE k < type.len DO
				Write_pointer_offset(offset, base);
				INC(k); INC(offset, size)
			END
		END
	ELSE ASSERT(FALSE)
	END
END Write_pointer_offset;

PROCEDURE Write_data_section;
	VAR basefadr, i, j, adr: INTEGER; b: BYTE;
		imod: B.Module; slist: B.StrList; ident: B.Ident;
		x: B.Str; t: B.TypeList; obj: B.Object;
BEGIN
	basefadr := data_fadr;
	Files.Set(rider, out, basefadr); i := 0;
	WHILE i < LEN(Kernel32Table) DO
		Files.WriteInt(rider, Kernel32Table[i]); INC(i)
	END;
	imod := B.modList;
	WHILE imod # NIL DO
		Files.Set(rider, out, basefadr + imod.adr); j := 0;
		WHILE imod.name[j] # 0X DO
			Files.WriteChar(rider, imod.name[j]); INC(j)
		END;
		Files.WriteString(rider, '.dll'); imod := imod.next
	END;
	slist := B.strList;
	WHILE slist # NIL DO x := slist.obj;
		Files.Set(rider, out, basefadr + x.adr); i := 0;
		WHILE i < x.len DO
			Files.WriteChar(rider, B.strbuf[x.bufpos+i]); INC(i)
		END;
		slist := slist.next
	END;
	t := B.recList;
	WHILE t # NIL DO
		Files.Set(rider, out, basefadr + t.type.adr);
		Files.WriteInt(rider, t.type.size);
		Files.Set(rider, out, basefadr + t.type.adr + 8 + B.MaxExt*8);
		IF t.type.nTraced > 0 THEN Write_pointer_offset(0, t.type) END;
		Files.WriteInt(rider, -1); t := t.next
	END;
	Files.Set(rider, out, basefadr + modPtrTable); ident := B.universe.first;
	WHILE ident # NIL DO obj := ident.obj;
		IF (obj IS B.Var) & ~(obj IS B.Str) THEN
			IF obj.type.nTraced > 0 THEN adr := obj(B.Var).adr;
				IF obj.type.form = B.tPtr THEN Files.WriteInt(rider, adr)
				ELSE Write_pointer_offset(adr, obj.type)
				END
			END
		END;
		ident := ident.next
	END;
	Files.WriteInt(rider, -1)
END Write_data_section;

PROCEDURE Write_code_section(code: ARRAY OF BYTE);
	VAR cnt: INTEGER;
BEGIN
	Files.Set(rider, out, code_fadr); cnt := pc;
	Files.WriteBytes(rider, code, cnt)
END Write_code_section;

PROCEDURE Write_reloc_section;
BEGIN
	Files.Set(rider, out, reloc_fadr);
	Files.WriteCard32(rider, 4);
	Files.WriteCard32(rider, 12);
	Files.WriteCard16(rider, 0);
	Files.WriteCard16(rider, 0)
END Write_reloc_section;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* .pdata *)

PROCEDURE Write_pdata_section(debug: Files.File);
	VAR data: ARRAY 200H OF BYTE;
		len, cnt: INTEGER; r: Files.Rider;
BEGIN
	Files.Set(rider, out, pdata_fadr+32);
	Files.Set(r, debug, 0); len := Files.Length(debug);
	Files.ReadBytes(r, data, LEN(data)); cnt := LEN(data) - r.res;
	WHILE len > 0 DO
		IF cnt > len THEN cnt := len END; DEC(len, cnt);
		Files.WriteBytes(rider, data, cnt);
		Files.ReadBytes(r, data, LEN(data)); cnt := LEN(data) - r.res
	END;
	Files.WriteInt(rider, -1);
	pdata_size := Files.Pos(rider) - pdata_fadr;
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
		exp: B.ObjList; x: B.Object; name: ARRAY LEN(B.modid)+4 OF CHAR;
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

	name := B.modid;
	IF B.Flag.main THEN B.Append('.exe', name)
	ELSE B.Append('.dll', name)
	END; nameSz := B.StrLen(name)+1;
	expno := B.expno; adrTblSz := expno*4;
	
	exp := B.expList; namecnt := 0; i := 1;
	WHILE exp # NIL DO
		IF exp.obj IS B.Proc THEN
			INC(namecnt); Insert(namedList, NewExport(exp.obj.ident, i))
		END;
		exp := exp.next; INC(i)
	END;
	namePtrTblSz := namecnt*4; ordTblSz := namecnt*2;
	nameRva := edata_rva + dirSz + adrTblSz + namePtrTblSz + ordTblSz;

	(* Export directory *)
	Files.Set(rider, out, edata_fadr + 12);
	Files.WriteCard32(rider, nameRva);
	Files.WriteCard32(rider, 1);
	Files.WriteCard32(rider, expno);
	Files.WriteCard32(rider, namecnt);
	Files.WriteCard32(rider, edata_rva + dirSz);
	Files.WriteCard32(rider, edata_rva + dirSz + adrTblSz);
	Files.WriteCard32(rider, edata_rva + dirSz + adrTblSz + namePtrTblSz);
	
	(* Export address table *)
	Files.Set(rider, out, edata_fadr + dirSz); exp := B.expList;
	WHILE exp # NIL DO x := exp.obj;
		IF x.class = B.cType THEN rva := data_rva + x.type.adr
		ELSIF x IS B.Var THEN rva := data_rva + x(B.Var).adr
		ELSIF x IS B.Proc THEN rva := code_rva + x(B.Proc).adr
		END;
		Files.WriteCard32(rider, rva); exp := exp.next
	END;
	
	IF namecnt > 0 THEN
		(* Export Name Pointer Table *)
		p := namedList; i := nameRva + nameSz;
		WHILE p # NIL DO Files.WriteCard32(rider, i);
			INC(i, B.StrLen(p.ident.name)+1); p := p.next
		END;
		(* Export Ordinal Table *)
		p := namedList;
		WHILE p # NIL DO Files.WriteCard16(rider, p.no-1); p := p.next END
	END;
		
	(* Name string *)
	Files.WriteByteStr(rider, name);
	IF namecnt > 0 THEN p := namedList;
		WHILE p # NIL DO
			Files.WriteByteStr(rider, p.ident.name); p := p.next
		END
	END;
	
	edata_size := Files.Pos(rider) - edata_fadr;
	edata_rawsize := edata_size;
	IF edata_rawsize MOD 512 # 0 THEN
		Align(edata_rawsize, 512);
		Files.Set(rider, out, edata_fadr + edata_rawsize - 1);
		Files.Write(rider, 1)
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
		Files.Write(rider, b); INC(i)
	END;
	Files.WriteCard32(rider, size);
	Files.WriteCard32(rider, rva);
	Files.WriteCard32(rider, rawsize);
	Files.WriteCard32(rider, fileadr);
	Files.WriteCard32(rider, 0);
	Files.WriteCard32(rider, 0);
	Files.WriteCard32(rider, 0);
	Files.WriteCard32(rider, chr)
END Write_SectionHeader;

PROCEDURE Write_PEHeader;
	VAR k, nSection: INTEGER;
BEGIN
	Files.Set(rider, out, 0);
	Files.WriteCard16(rider, 5A4DH);
	Files.Set(rider, out, 60);
	Files.WriteCard32(rider, 128);
	Files.Set(rider, out, 128);
	Files.WriteCard32(rider, 4550H);
	
	Files.WriteCard16(rider, 8664H); (* Machine = AMD64/Intel 64 *)
	nSection := 6; IF bss_size > 0 THEN INC(nSection) END;
	Files.WriteCard16(rider, nSection); (* NumberOfSections *)
	Files.WriteCard32(rider, 0); (* TimeDateStamp *)
	Files.WriteCard32(rider, 0); (* PointerToSymbolTable *)
	Files.WriteCard32(rider, 0); (* NumberOfSymbols *)
	Files.WriteCard16(rider, 240);
	
	(* Characteristics *)
	IF B.Flag.main THEN Files.WriteCard16(rider, 20H + 2 + 1)
	ELSE Files.WriteCard16(rider, 2000H + 20H + 2)
	END;
	
	Files.WriteCard16(rider, 20BH); (* Magic number for PE32+ *)
	Files.WriteCard16(rider, 0);
	Files.WriteCard32(rider, code_rawsize); (* SizeOfCode *)
	k := data_rawsize + 200H * 2 + edata_rawsize;
	k := k + pdata_rawsize;
	Files.WriteCard32(rider, k); (* SizeOfInitializedData *)
	Files.WriteCard32(rider, bss_size); (* SizeOfUninitializedData *)
	Files.WriteCard32(rider, code_rva + entry);
	Files.WriteCard32(rider, code_rva);
	
	Files.WriteInt(rider, imagebase);
	Files.WriteCard32(rider, 4096);
	Files.WriteCard32(rider, 512);
	Files.WriteCard16(rider, 5); (* MajorOSVer *)
	Files.WriteCard16(rider, 0);
	Files.WriteCard16(rider, 0);
	Files.WriteCard16(rider, 0);
	Files.WriteCard16(rider, 5);
	Files.WriteCard16(rider, 0);
	Files.WriteCard32(rider, 0);
	k := 4096 + (code_size + 4095) DIV 4096 * 4096;
	k := k + (pdata_size + 4095) DIV 4096 * 4096;
	k := k + bss_size + data_size + 4096 + 4096;
	k := k + (edata_size + 4095) DIV 4096 * 4096;
	Files.WriteCard32(rider, k); (* SizeOfImage *)
	Files.WriteCard32(rider, HeaderSize); (* SizeOfHeaders *)
	Files.WriteCard32(rider, 0);
	IF B.Flag.console THEN Files.WriteCard16(rider, 3) (* Subsys = Console *)
	ELSE Files.WriteCard16(rider, 2) (* Subsys = GUI *)
	END;
	
	(* DLL Characteristics *)
	IF B.Flag.main THEN Files.WriteCard16(rider, 0)
	ELSE Files.WriteCard16(rider, 100H + 40H)
	END;
	
	Files.WriteInt(rider, 1000H); (* Size of stack reserve *)
	Files.WriteInt(rider, 1000H); (* Size of stack commit *)
	Files.WriteInt(rider, 10000H); (* Size of heap reserve *)
	Files.WriteInt(rider, 0);
	Files.WriteCard32(rider, 4);
	Files.WriteCard32(rider, 16);
	
	Files.WriteCard32(rider, edata_rva);
	Files.WriteCard32(rider, edata_size);
	Files.WriteCard32(rider, idata_rva);
	Files.WriteCard32(rider, 130H);
	Files.WriteInt(rider, 0); (* Resource Table *)
	Files.WriteInt(rider, 0); (* Exception Table *)
	Files.WriteInt(rider, 0); (* Certificate Table *)
	Files.WriteCard32(rider, reloc_rva);
	Files.WriteCard32(rider, 12);
	Files.Set(rider, out, Files.Pos(rider) + 8 * 10);
	
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
	Files.Set(rider, out, 400H - 32);
	Files.WriteInt(rider, code_rva); Files.WriteInt(rider, pdata_rva+32);
	Files.WriteInt(rider, B.modkey[0]); Files.WriteInt(rider, B.modkey[1])
END Write_PEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Link*(
	debug: Files.File; code: ARRAY OF BYTE;
	pc0, entry0, staticSize, varSize, modPtrTable0: INTEGER
);
	VAR n: INTEGER; fname: ARRAY 512 OF CHAR;
BEGIN
	fname := B.modid;
	IF B.Flag.main THEN B.Append('.exe', fname)
	ELSE B.Append('.dll', fname)
	END;
	out := Files.New(fname); Files.Set(rider, out, 0);
	
	pc := pc0; entry := entry0; modPtrTable := modPtrTable0;
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
	Write_edata_section; Write_PEHeader;
	
	Files.Register(out)
END Link;

END Linker.