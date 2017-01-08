MODULE Linker;
IMPORT
	SYSTEM, Rtl, Strings,
	S := Scanner, B := Base;
	
VAR
	out: Rtl.File;
	imagebase, entry, pc: INTEGER;
	
	code_rva, code_size, code_rawsize, code_fadr: INTEGER;
	data_rva, data_size, data_rawsize, data_fadr: INTEGER;
	bss_size, bss_rva: INTEGER;
	edata_rva, edata_size, edata_rawsize, edata_fadr: INTEGER;
	idata_rva, idata_fadr: INTEGER;
	reloc_rva, reloc_fadr: INTEGER;
	debug_rva, debug_size, debug_rawsize, debug_fadr: INTEGER;
			
	debug_info_rva, debug_info_size: INTEGER;
	debug_info_rawsize, debug_info_fadr: INTEGER;	
	debug_abbrev_rva, debug_abbrev_size: INTEGER;
	debug_abbrev_rawsize, debug_abbrev_fadr: INTEGER;
	
	Kernel32Table: ARRAY 5 OF INTEGER;
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
BEGIN
	Rtl.Seek(out, idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Rtl.Write4(out, idata_rva + table_rva);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, idata_rva + name_rva);
	i := data_rva + data_size - table_size;
	Rtl.Write4(out, i);

	Rtl.Seek(out, idata_fadr + table_rva); i := 0;
	WHILE i <= table_len - 2 DO
		Kernel32Table[i] := idata_rva + hint_rva + 32 * i;
		Rtl.Write8(out, Kernel32Table[i]); INC(i)
	END;
	Kernel32Table[table_len - 1] := 0;
	
	Rtl.Seek(out, idata_fadr + name_rva);
	Rtl.WriteAnsiStr(out, 'KERNEL32.DLL');
	
	Rtl.Seek(out, idata_fadr + hint_rva + 2);
	Rtl.WriteAnsiStr (out, 'GetModuleHandleExW');
	Rtl.Seek(out, idata_fadr + hint_rva + (32 + 2));
	Rtl.WriteAnsiStr (out, 'ExitProcess');
	Rtl.Seek(out, idata_fadr + hint_rva + (64 + 2));
	Rtl.WriteAnsiStr(out, 'LoadLibraryW');
	Rtl.Seek(out, idata_fadr + hint_rva + (96 + 2));
	Rtl.WriteAnsiStr(out, 'GetProcAddress')
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
	basefadr := data_fadr + data_size;
	Rtl.Seek(out, basefadr - LEN(Kernel32Table)*8); i := 0;
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
		IF x.class = B.cType THEN rva := x.type.adr
		ELSIF x IS B.Var THEN rva := x(B.Var).adr
		ELSIF x IS B.Proc THEN rva := x(B.Proc).adr
		END; INC(rva, code_rva);
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

PROCEDURE Write_reloc_section;
BEGIN
	Rtl.Seek(out, reloc_fadr);
	Rtl.Write4(out, 4);
	Rtl.Write4(out, 12);
	Rtl.Write2(out, 0);
	Rtl.Write2(out, 0)
END Write_reloc_section;

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
	nSection := 5; IF bss_size > 0 THEN INC(nSection) END;
	IF debug_size > 0 THEN INC(nSection) END;
	Rtl.Write2(out, nSection); (* NumberOfSections *)
	Rtl.Write4(out, 0);
	Rtl.Write4(out, 0);
	Rtl.Write4(out, 0);
	Rtl.Write2(out, 240);
	
	(* Characteristics *)
	IF B.Flag.main THEN Rtl.Write2(out, 20H + 2 + 1)
	ELSE Rtl.Write2(out, 2000H + 20H + 2)
	END;
	
	Rtl.Write2(out, 20BH); (* Magic number for PE32+ *)
	Rtl.Write2(out, 0);
	Rtl.Write4(out, code_rawsize); (* SizeOfCode *)
	k := data_rawsize + 200H * 2 + edata_rawsize;
	k := k + debug_rawsize;
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
	k := k + (debug_size + 4095) DIV 4096 * 4096;
	k := k + bss_size + data_size + 4096 + 4096;
	k := k + (edata_size + 4095) DIV 4096 * 4096;
	Rtl.Write4(out, k); (* SizeOfImage *)
	Rtl.Write4(out, 400H);
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
	Rtl.Write8(out, 0);
	Rtl.Write8(out, 0);
	Rtl.Write8(out, 0);
	Rtl.Write4(out, reloc_rva);
	Rtl.Write4(out, 12);
	Rtl.Seek(out, Rtl.Pos(out) + 8 * 10);
	
	IF bss_size > 0 THEN
		Write_SectionHeader(
			'.bss', -1073741696, bss_rva, 0, bss_size, 0
		)
	END;
	Write_SectionHeader (
		'.data', -1073741760, data_rva, data_rawsize,
		data_size, data_fadr
	);
	Write_SectionHeader (
		'.text', 60000020H, code_rva, code_rawsize,
		code_size, code_fadr
	);
	Write_SectionHeader (
		'.idata', -1073741760, idata_rva, 200H,
		130H, idata_fadr
	);
	Write_SectionHeader (
		'.reloc', 42000040H, reloc_rva, 200H,
		12, reloc_fadr
	);
	IF debug_size > 0  THEN
		Write_SectionHeader (
			'debug', 40000040H, debug_rva, debug_rawsize,
			debug_size, debug_fadr
		)
	END;
	Write_SectionHeader (
		'.edata', 40000040H, edata_rva, edata_rawsize,
		edata_size, edata_fadr
	);
	(*Write_SectionHeader (
		'.debug_info', 40000040H, debug_info_rva,
		debug_info_rawsize, debug_info_size, debug_info_fadr
	);
	Write_SectionHeader (
		'.debug_abbrev', 40000040H, debug_abbrev_rva,
		debug_abbrev_rawsize, debug_abbrev_size, debug_abbrev_fadr
	)*)
END Write_PEHeader;

PROCEDURE Write_code_section(code: ARRAY OF BYTE);
	VAR cnt: INTEGER;
BEGIN
	Rtl.Seek(out, 400H); cnt := pc;
	Rtl.WriteBuf(out, SYSTEM.ADR(code), cnt)
END Write_code_section;

PROCEDURE Write_debug_section(VAR debug: Rtl.File);
	VAR data: ARRAY 200H OF BYTE;
		len, cnt: INTEGER;
BEGIN
	len := Rtl.Pos(debug); Rtl.Seek(debug, 0);
	Rtl.Seek(out, debug_fadr); Rtl.ReadBytes(debug, data, cnt);
	WHILE len > 0 DO
		IF cnt > len THEN cnt := len END; DEC(len, cnt);
		Rtl.WriteBuf(out, SYSTEM.ADR(data), cnt);
		Rtl.ReadBytes(debug, data, cnt)
	END;
	Rtl.Close(debug); Rtl.Delete('.pocDebug')
END Write_debug_section;

PROCEDURE Link*(
	out0: Rtl.File; VAR debug: Rtl.File; code: ARRAY OF BYTE;
	pc0, entry0, staticSize, varSize, modPtrTable0: INTEGER;
	VAR str: B.String
);
	VAR n: INTEGER;
BEGIN
	out := out0; pc := pc0; entry := entry0; modPtrTable := modPtrTable0;
	IF B.Flag.main THEN imagebase := 400000H ELSE imagebase := 10000000H END;

	code_size := pc;
	code_rawsize := (code_size + 511) DIV 512 * 512;
	
	data_size := staticSize; Align(data_size, 4096);
	data_rawsize := data_size;
	
	bss_size := varSize; Align(bss_size, 4096);
	
	debug_size := Rtl.Pos(debug);
	debug_rawsize := (debug_size + 511) DIV 512 * 512;
	
	bss_rva := 1000H;
	data_rva := bss_rva + bss_size;
	code_rva := data_rva + data_size;
	n := code_size; Align(n, 4096);
	idata_rva := code_rva + n;
	reloc_rva := idata_rva + 4096;
	debug_rva := reloc_rva + 4096;
	n := debug_size; Align(n, 4096);
	edata_rva := debug_rva + n;
	
	code_fadr := 400H;
	idata_fadr := code_fadr + code_rawsize;
	data_fadr := idata_fadr + 200H;
	reloc_fadr := data_fadr + data_rawsize;
	debug_fadr := reloc_fadr + 200H;
	edata_fadr := debug_fadr + debug_rawsize;
	
	Rtl.Seek(out, 400H - 32);
	Rtl.Write8(out, code_rva); Rtl.Write8(out, debug_rva);
	Rtl.Write8(out, B.modkey[0]); Rtl.Write8(out, B.modkey[1]);
	
	Write_code_section(code); Write_idata_section; Write_data_section;
	Write_reloc_section; Write_debug_section(debug); Write_edata_section;
	
	Write_PEHeader; Rtl.Close(out);
	
	(* Rename files *)
	str[0] := 0X; Strings.Append(B.modid, str);
	IF B.Flag.main THEN Strings.Append('.exe', str)
	ELSE Strings.Append('.dll', str)
	END;
	Rtl.Delete(str); Rtl.Rename('.tempOut', str)
END Link;

END Linker.