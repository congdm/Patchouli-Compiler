MODULE SymTable;

IMPORT
	Strings, Sys, Base, Scanner;

TYPE
	ModuleKey* = ARRAY 16 OF BYTE;

	UndefPtrList* = POINTER TO RECORD
		exported: BOOLEAN; typ: Base.Type; basename: Base.IdentStr;
		next : UndefPtrList
	END;
	
	Module* = RECORD
		imported: BOOLEAN;
		name*: Base.IdentStr; lev: INTEGER;
		objects*: Base.Object;
		usedTypes*: Base.Type;
		key*: ModuleKey
	END;
	
VAR
	topScope*, universe*: Base.Object;
	undefPtr*: UndefPtrList;
	curLev*: INTEGER;
	
	modlev, visiblemodno*, hiddenmodno*: INTEGER;
	refno, expno*, impMod: INTEGER;
	importSystem*: BOOLEAN;
	
	importModules*: ARRAY 256 OF Module;
	reimportModules: ARRAY 256 OF INTEGER;
	reimportModuleNames: ARRAY 256 OF Base.IdentStr;
	reimportModuleKeys: ARRAY 256 OF ModuleKey;
	importTypes: ARRAY 1024 OF Base.Type;
	
	modkey*: ModuleKey;
	exportAdrList*: ARRAY 4096 OF RECORD
		class*, adr*: INTEGER
	END;
	
	symfile : Sys.FileHandle;
	
	(* Forward decl procedure *)
	_Export_type : PROCEDURE (typ: Base.Type; haveRef: BOOLEAN);
	_Import_type : PROCEDURE (
		VAR typ: Base.Type; haveRef: BOOLEAN; mod: INTEGER
	);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope* (name: Base.IdentStr);
	VAR scope: Base.Object;
BEGIN
	NEW (scope); scope.class := Base.cHead; scope.name := name;
	scope.dsc := topScope; scope.next := Base.guard; topScope := scope
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;
	
PROCEDURE IncLevel* (x: INTEGER);
BEGIN INC (curLev, x)
END IncLevel;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE FindInScope (
	VAR obj: Base.Object; name: Base.IdentStr; scope: Base.Object
);
BEGIN Base.StrCopy(name, Base.guard.name); obj := scope.next;
	WHILE obj.name # name DO obj := obj.next END
END FindInScope;

PROCEDURE Find* (VAR obj: Base.Object; name: Base.IdentStr);
BEGIN
	IF (topScope # universe) & (topScope.name = name) THEN
		FindInScope (obj, name, topScope.dsc)
	ELSE FindInScope (obj, name, topScope);
		IF (obj = Base.guard) & (topScope # universe) THEN
			FindInScope (obj, name, universe)
		END
	END
END Find;
	
PROCEDURE FindField* (
	VAR obj: Base.Object; name: Base.IdentStr; record: Base.Type
);
BEGIN Base.StrCopy(name, Base.guard.name); obj := record.fields;
	WHILE obj.name # name DO obj := obj.next END
END FindField;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(*
	PROCEDURE Module_ID_of (name : Base.String) : INTEGER;
		VAR modid : INTEGER;
	BEGIN
		modid := 0;
		WHILE (modid < hiddenmodno) & (importModules [modid].name # name) DO
			modid := modid + 1
		END;
		IF modid < hiddenmodno THEN (* ok *) ELSE modid := -1 END;
		RETURN modid
	END Module_ID_of;

	PROCEDURE Find_in_module (
		VAR obj : Base.Object; name : Base.String; modid : INTEGER
	);
	BEGIN
		Base.guard.name := name; obj := importModules [modid].objects;
		WHILE obj.name # name DO obj := obj.next END
	END Find_in_module;

	PROCEDURE Find_import_object* (
		VAR obj : Base.Object; name : Base.String; modul : Base.Object
	);
	BEGIN
		IF modul.val >= 0 THEN Find_in_module (obj, name, SHORT (modul.val))
		ELSIF modul.val = -1 THEN
			Base.guard.name := name; obj := modul.dsc;
			WHILE obj.name # name DO obj := obj.next END
		ELSE ASSERT(FALSE)
		END
	END Find_import_object;
*)
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE New* (VAR obj: Base.Object; name: Base.IdentStr; class: INTEGER);
BEGIN
	Base.StrCopy(name, Base.guard.name); obj := topScope;
	WHILE obj.next.name # name DO obj := obj.next END;
	
	IF obj.next = Base.guard THEN
		NEW (obj.next); obj := obj.next;
		obj.export := FALSE; obj.readonly := FALSE; obj.param := FALSE;
		Base.StrCopy(name, obj.name); obj.class := class; obj.lev := curLev;
		obj.next := Base.guard; obj.val := 0; obj.val2 := 0
	ELSE Scanner.Mark ('Duplicated identifer definition'); obj := Base.guard
	END
END New;
	
PROCEDURE Enter (cl, n: INTEGER; name: Base.IdentStr; typ: Base.Type);
	VAR obj: Base.Object;
BEGIN
	NEW (obj); obj.readonly := FALSE; obj.param := FALSE;
	obj.class := cl; obj.val := n; Base.StrCopy(name, obj.name);
	obj.type := typ; obj.next := topScope.next; topScope.next := obj
END Enter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE RegisterUndefType* (
	typ: Base.Type; basename: Base.IdentStr; export: BOOLEAN
);
	VAR undef: UndefPtrList;
BEGIN
	NEW (undef); undef.typ := typ; Base.StrCopy(basename, undef.basename);
	undef.next := undefPtr; undefPtr := undef
END RegisterUndefType;
	
PROCEDURE CheckUndefList* (obj: Base.Object);
	VAR p, prev: UndefPtrList;
BEGIN
	p := undefPtr;
	REPEAT
		IF p.basename # obj.name THEN prev := p
		ELSE p.typ.base := obj.type;
			IF p = undefPtr THEN undefPtr := p.next ELSE prev.next := p.next
			END;
			p := p.next
		END
	UNTIL p = NIL
END CheckUndefList;
	
PROCEDURE CleanupUndefList*;
	VAR msg: Base.String;
BEGIN
	REPEAT msg := 'Record type '; Strings.Append (undefPtr.basename, msg);
		Strings.Append (' is not defined', msg); Scanner.Mark (msg);
		undefPtr := undefPtr.next
	UNTIL undefPtr = NIL
END CleanupUndefList;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import/Export *)
(*
PROCEDURE Detect_typeI (VAR typ : Base.Type);
	VAR mod, m, ref, i : INTEGER; typename : Base.IdentStr;
		obj : Base.Object;
BEGIN
	ref := 0; mod := 0;
	Base.ReadInt (symfile, mod); Base.ReadInt (symfile, ref);
	IF mod = -2 THEN typ := Base.predefinedTypes [ref]
	ELSIF ref >= 0 THEN typ := importTypes [ref]
	ELSIF mod = -1 THEN _Import_type (typ, TRUE, impMod)
	ELSE
		m := reimportModules [mod];
		IF m = -1 THEN
			m := hiddenmodno; reimportModules [mod] := hiddenmodno;
			hiddenmodno := hiddenmodno + 1;
			importModules [m].name := reimportModuleNames [mod];
			importModules [m].key := reimportModuleKeys [mod]
		END;
		ref := refno; _Import_type (typ, TRUE, m);
		Sys.Read_string (symfile, typename);
		IF (typename # '') & (m < visiblemodno) THEN
			Find_in_module (obj, typename, m);
			IF (obj # Base.guard) & (obj.class = Base.class_type) THEN
				importTypes [ref] := obj.type; typ := obj.type
			END
		END
	END
END Detect_typeI;

PROCEDURE Import_type (VAR typ : Base.Type; haveRef : BOOLEAN; mod : INTEGER);
	VAR field : Base.Object;
		n, form, class : INTEGER;
		name : Base.IdentStr;
BEGIN
	form := 0; class := 0; n := 0;
	Base.ReadInt (symfile, form); Base.New_typ (typ, form);
	IF haveRef THEN importTypes [refno] := typ; refno := refno + 1 END;
	typ.mod := mod;
	
	IF form = Base.type_record THEN
		Detect_typeI (typ.base);
		Base.ReadInt (symfile, typ.len);
		Base.ReadInt (symfile, typ.size);
		Base.ReadInt (symfile, typ.num_ptr);
		Base.ReadInt (symfile, typ.alignment);
		
		IF mod = impMod THEN expno := expno + 1; typ.expno := expno
		ELSE Base.ReadInt (symfile, typ.expno)
		END;
		
		Open_scope ('');
		Base.ReadInt (symfile, class);
		WHILE class = Base.class_field DO
			Sys.Read_string (symfile, name);
			New_obj (field, name, Base.class_field);
			Detect_typeI (field.type);
			Base.ReadInt64 (symfile, field.val);
			Base.ReadInt (symfile, class)
		END;
		ASSERT (class = Base.class_type);
		typ.fields := top_scope.next;
		Close_scope
	ELSIF form = Base.type_array THEN
		Detect_typeI (typ.base);
		Base.ReadInt (symfile, typ.len);
		typ.size := typ.len * typ.base.size;
		typ.num_ptr := typ.len * typ.base.num_ptr;
		typ.alignment := typ.base.alignment
	ELSIF form = Base.type_pointer THEN
		Detect_typeI (typ.base);
		typ.size := Base.Word_size;
		typ.num_ptr := 1;
		typ.alignment := Base.Word_size
	ELSIF form = Base.type_procedure THEN
		Detect_typeI (typ.base);
		Base.ReadInt (symfile, typ.len);
		typ.size := Base.Word_size;
		typ.alignment := Base.Word_size;
		
		Open_scope ('');
		Base.ReadInt (symfile, class);
		WHILE class # Base.class_type DO
			Sys.Read_string (symfile, name);
			New_obj (field, name, class);
			Detect_typeI (field.type);
			field.param := TRUE;
			Base.ReadInt (symfile, n);
			field.readonly := n = 1;
			Base.ReadInt (symfile, class)
		END;
		ASSERT (class = Base.class_type);
		typ.fields := top_scope.next;
		Close_scope
	ELSIF form = Base.type_string THEN
		Base.ReadInt (symfile, typ.len);
		Base.ReadInt (symfile, typ.charVal);
		typ.base := Base.char_type;
		typ.size := typ.len * Base.Char_size;
		typ.alignment := Base.char_type.alignment
	ELSIF typ.form = Base.type_address THEN
		Detect_typeI (typ.base);
		typ.size := Base.Word_size;
		typ.alignment := Base.Word_size
	END
END Import_type;

PROCEDURE Read_module_key (VAR key : ModuleKey);
	VAR i, n : INTEGER;
BEGIN
	n := 0;
	FOR i := 0 TO 15 DO Sys.Read_byte (symfile, n); key[i] := USHORT (n) END
END Read_module_key;

PROCEDURE Different_key (key1, key2 : ModuleKey) : BOOLEAN;
	VAR i : INTEGER; result : BOOLEAN;
BEGIN
	i := 0; WHILE (i < 16) & (key1[i] = key2[i]) DO i := i + 1 END;
	RETURN i < 16
END Different_key;
	
PROCEDURE Import_symbols_file* (filename : ARRAY OF CHAR);
	VAR class, n, reimpmodno : INTEGER; key : ModuleKey;
		obj : Base.Object; name : Base.IdentStr;
		errormsg : ARRAY 1024 OF CHAR;
BEGIN
	refno := 0;	expno := 0; class := 0; reimpmodno := 0;
	Sys.Open (symfile, filename);
	Sys.Seek (symfile, 16); (* Skip module key *)
	Base.ReadInt (symfile, class); (* Skip module level *)
	
	Base.ReadInt (symfile, class);
	WHILE class # Base.class_head DO
		IF class = Base.class_module THEN
			Sys.Read_string (symfile, name);
			Read_module_key (key);
			n := Module_ID_of (name); reimportModules [reimpmodno] := n;
			IF n = -1 THEN
				reimportModuleNames [reimpmodno] := name;
				reimportModuleKeys [reimpmodno] := key
			ELSIF Different_key (key, importModules [n].key) THEN
				errormsg := 'Module '; Base.Append_str (errormsg, name);
				Base.Append_str (errormsg, ' is imported from module ');
				Base.Append_str (errormsg, importModules [impMod].name);
				Base.Append_str (errormsg, ' with a different key');
				Scanner.Mark (errormsg)
			END;
			reimpmodno := reimpmodno + 1
		ELSIF class = Base.class_const THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, Base.class_const);
			Base.ReadInt64 (symfile, obj.val);
			Detect_typeI (obj.type)
		ELSIF class = Base.class_type THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, Base.class_type);
			Detect_typeI (obj.type);
			obj.type.obj := obj
		ELSIF class = Base.class_var THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class);
			Detect_typeI (obj.type);
			expno := expno + 1; obj.val2 := expno
		ELSIF class = Base.class_proc THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class);
			Import_type (obj.type, FALSE, impMod);
			expno := expno + 1; obj.val2 := expno
		ELSE ASSERT(FALSE)
		END;
		Base.ReadInt (symfile, class)
	END;
	Sys.Close (symfile)
END Import_symbols_file;

PROCEDURE Import_SYSTEM (modul : Base.Object);
BEGIN
	importSystem := TRUE;
	cur_lev := -2; Open_scope ('');
	
	Enter (Base.class_type, 0, 'WORD', Base.word_type);
	Enter (Base.class_type, 0, 'DWORD', Base.dword_type);
	
	Enter (Base.cSproc, 100, 'GET', NIL);
	Enter (Base.cSproc, 101, 'PUT', NIL);
	Enter (Base.cSproc, 102, 'COPY', NIL);
	Enter2 (Base.cSproc, 103, 'LoadLibraryW', NIL, Base.LoadLibraryW);
	Enter2 (Base.cSproc, 104, 'GetProcAddress', NIL, Base.GetProcAddress);
	
	Enter (Base.cSproc, 300, 'ADR', Base.int_type);
	Enter (Base.cSproc, 301, 'SIZE', Base.int_type);
	Enter (Base.cSproc, 302, 'BIT', Base.bool_type);
	Enter (Base.cSproc, 303, 'VAL', Base.int_type);
	Enter (Base.cSproc, 310, 'ADR2', Base.int_type);
	Enter (Base.cSproc, 311, 'STRADR', Base.int_type);
	
	modul.dsc := top_scope.next; modul.val := -1;
	Close_scope; cur_lev := 0
END Import_SYSTEM;

PROCEDURE Import_modules*;
	VAR filename : Base.LongString; 
		i, min, minlev : INTEGER; finish : BOOLEAN;
BEGIN
	(* Find the lowest level module to import first *)
	finish := FALSE; hiddenmodno := visiblemodno; minlev := 0;
	REPEAT i := 0; min := -1;
		WHILE i < visiblemodno DO
			IF importModules [i].not_imported
				& ((min = -1) OR (importModules [i].lev <= minlev)) THEN
				min := i; minlev := importModules [i].lev
			END;
			INC (i)
		END;
		IF (min # -1) & (min < visiblemodno)
			& importModules [min].not_imported THEN
			impMod := min; filename := '';
			Base.Append_str (filename, importModules [min].name);
			Base.Append_str (filename, '.sym');
			
			cur_lev := -2; Open_scope ('');
			Import_symbols_file (filename);
			importModules [min].objects := top_scope.next;
			importModules [min].not_imported := FALSE;
			Close_scope; cur_lev := 0
		ELSE finish := TRUE
		END
	UNTIL finish
END Import_modules;

PROCEDURE Find_module_symfile* (modul : Base.Object; sym_name : Base.String);
	VAR filename : Base.LongString; obj : Base.Object;
BEGIN
	IF sym_name = 'SYSTEM' THEN Import_SYSTEM (modul)
	ELSIF visiblemodno < LEN(importModules) THEN
		importModules [visiblemodno].name := sym_name;
		importModules [visiblemodno].not_imported := TRUE;
		
		filename := ''; Base.Append_str (filename, sym_name);
		Base.Append_str (filename, '.sym');
		IF Sys.File_existed (filename) THEN
			Sys.Open (symfile, filename);
			Read_module_key (importModules [visiblemodno].key);
			Base.ReadInt (symfile, importModules [visiblemodno].lev);
			IF importModules [visiblemodno].lev >= modlev THEN
				modlev := importModules [visiblemodno].lev + 1
			END;
			Sys.Close (symfile);
			modul.val := visiblemodno; INC (visiblemodno)
		ELSE Scanner.Mark ('Symbol file not found'); modul.name := '#'
		END
	ELSE Scanner.Mark ('Compiler limit: Too many imported modules');
		modul.name := '#'
	END;
END Find_module_symfile;

(* -------------------------------------------------------------------------- *)

PROCEDURE Detect_type (typ : Base.Type);
BEGIN
	IF typ = NIL THEN
		Base.WriteInt (symfile, -2);
		Base.WriteInt (symfile, 0)
	ELSE
		Base.WriteInt (symfile, typ.mod);
		Base.WriteInt (symfile, typ.ref);
		IF typ.ref = -1 THEN
			_Export_type (typ, TRUE);
			IF typ.mod >= 0 THEN
				IF typ.obj # NIL THEN Sys.Write_string (symfile, typ.obj.name)
				ELSE Sys.Write_string (symfile, '')
				END
			END
		END
	END
END Detect_type;
	
PROCEDURE Export_type (typ : Base.Type; haveRef : BOOLEAN);
	VAR field : Base.Object; i : INTEGER; s : Base.String;
BEGIN
	IF haveRef THEN typ.ref := refno; refno := refno + 1 END;
	Base.WriteInt (symfile, typ.form);
	
	IF typ.form = Base.type_record THEN
		Detect_type (typ.base);
		Base.WriteInt (symfile, typ.len);
		Base.WriteInt (symfile, typ.size);
		Base.WriteInt (symfile, typ.num_ptr);
		Base.WriteInt (symfile, typ.alignment);
		
		IF typ.mod = -1 THEN
			expno := expno + 1;
			exportAdrList [expno].class := Base.class_type;
			exportAdrList [expno].adr := SHORT (typ.tdAdr)
		ELSE Base.WriteInt (symfile, typ.expno)
		END;
		
		i := 0; field := typ.fields;
		WHILE field # Base.guard DO
			IF field.export OR (field.type.num_ptr > 0) THEN
				Base.WriteInt (symfile, Base.class_field);
				IF ~ field.export THEN
					Sys.Int_to_string (i, s); Sys.Write_string (symfile, s)
				ELSE Sys.Write_string (symfile, field.name)
				END;
				Detect_type (field.type);
				Base.WriteInt (symfile, field.val)
			END;
			field := field.next
		END;
		Base.WriteInt (symfile, Base.class_type)
	ELSIF typ.form = Base.type_array THEN
		Detect_type (typ.base);
		Base.WriteInt (symfile, typ.len)
	ELSIF typ.form = Base.type_pointer THEN
		Detect_type (typ.base)
	ELSIF typ.form = Base.type_procedure THEN
		Detect_type (typ.base);
		Base.WriteInt (symfile, typ.len);
		
		field := typ.fields;
		WHILE field # Base.guard DO
			Base.WriteInt (symfile, field.class);
			Sys.Write_string (symfile, field.name);
			Detect_type (field.type);
			IF ~ field.readonly THEN Base.WriteInt (symfile, 0)
			ELSE Base.WriteInt (symfile, 1)
			END;
			field := field.next
		END;
		Base.WriteInt (symfile, Base.class_type)
	ELSIF typ.form = Base.type_string THEN
		Base.WriteInt (symfile, typ.len);
		Base.WriteInt (symfile, typ.charVal)
	ELSIF typ.form = Base.type_address THEN
		Detect_type (typ.base)
	END
END Export_type;

PROCEDURE Write_module_key (key : ModuleKey);
	VAR i : INTEGER;
BEGIN
	FOR i := 0 TO 15 DO Sys.Write_byte (symfile, key[i]) END
END Write_module_key;

PROCEDURE Write_symbols_file*;
	VAR obj : Base.Object; i, k : INTEGER;
BEGIN
	refno := 0;	expno := 0;
	Sys.Rewrite (symfile, 'sym.temp_');
	Sys.Seek (symfile, 16);
	Base.WriteInt (symfile, modlev);
	
	FOR i := 0 TO hiddenmodno - 1 DO
		Base.WriteInt (symfile, Base.class_module);
		Sys.Write_string (symfile, importModules [i].name);
		Write_module_key (importModules [i].key)
	END;
	
	obj := universe.next;
	WHILE obj # Base.guard DO
		IF obj.export THEN
			IF obj.class = Base.class_const THEN
				Base.WriteInt (symfile, Base.class_const);
				Sys.Write_string (symfile, obj.name);
				Base.WriteInt (symfile, obj.val);
				Detect_type (obj.type)
			ELSIF obj.class = Base.class_type THEN
				Base.WriteInt (symfile, Base.class_type);
				Sys.Write_string (symfile, obj.name);
				Detect_type (obj.type)
			ELSIF obj.class = Base.class_var THEN
				Base.WriteInt (symfile, obj.class);
				Sys.Write_string (symfile, obj.name);
				Detect_type (obj.type);
				
				expno := expno + 1;
				exportAdrList [expno].class := Base.class_var;
				exportAdrList [expno].adr := SHORT (obj.val)
			ELSIF obj.class = Base.class_proc THEN
				Base.WriteInt (symfile, Base.class_proc);
				Sys.Write_string (symfile, obj.name);
				Export_type (obj.type, FALSE);
				
				expno := expno + 1;
				exportAdrList [expno].class := Base.class_proc;
				exportAdrList [expno].adr := SHORT (obj.val)
			ELSE ASSERT(FALSE)
			END
		END;
		obj := obj.next
	END;
	Base.WriteInt (symfile, Base.class_head);
	Sys.Seek (symfile, 0);
	Sys.Calculate_MD5_hash (symfile, modkey);
	Sys.Seek (symfile, 0);
	Write_module_key (modkey);
	Sys.Close (symfile)
END Write_symbols_file;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Add_usedType* (typ : Base.Type; modid : INTEGER);
BEGIN
	typ.next := importModules [modid].usedTypes;
	importModules [modid].usedTypes := typ
END Add_usedType;
*)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Init* (modname : Base.IdentStr);
BEGIN
	modlev := 0; visiblemodno := 0; importSystem := FALSE; NEW (universe);
	universe.class := Base.cHead; universe.name := modname;
	universe.next := Base.guard; topScope := universe;
	
	Enter (Base.cType, 0, 'INTEGER', Base.intType);
	Enter (Base.cType, 0, 'BOOLEAN', Base.boolType);
	Enter (Base.cType, 0, 'SET', Base.setType);
	Enter (Base.cType, 0, 'BYTE', Base.byteType);
	Enter (Base.cType, 0, 'CHAR', Base.charType);
	Enter (Base.cType, 0, 'REAL', Base.realType);
	Enter (Base.cType, 0, 'LONGREAL', Base.realType);
	
	Enter (Base.cSProc, 0, 'INC', NIL);
	Enter (Base.cSProc, 1, 'DEC', NIL);
	Enter (Base.cSProc, 2, 'INCL', NIL);
	Enter (Base.cSProc, 3, 'EXCL', NIL);
	Enter (Base.cSProc, 4, 'NEW', NIL);
	Enter (Base.cSProc, 5, 'ASSERT', NIL);
(*	Enter (Base.cSproc, 6, 'PACK', NIL); *)
(*	Enter (Base.cSproc, 7, 'UNPK', NIL); *)
	
	Enter (Base.cSFunc, 200, 'ABS', Base.intType);
	Enter (Base.cSFunc, 201, 'ODD', Base.boolType);
	Enter (Base.cSFunc, 202, 'LEN', Base.intType);
	Enter (Base.cSFunc, 203, 'LSL', Base.intType);
	Enter (Base.cSFunc, 204, 'ASR', Base.intType);
	Enter (Base.cSFunc, 205, 'ROR', Base.intType);
	Enter (Base.cSFunc, 206, 'FLOOR', Base.intType);
	Enter (Base.cSFunc, 207, 'FLT', Base.realType);
	Enter (Base.cSFunc, 208, 'ORD', Base.intType);
	Enter (Base.cSFunc, 209, 'CHR', Base.charType)
END Init;
	
BEGIN
(*	_Export_type := Export_type;
	_Import_type := Import_type *)
END SymTable.