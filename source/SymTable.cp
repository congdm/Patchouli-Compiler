MODULE SymTable;

IMPORT
	Sys, Base, Scanner;
	
CONST
	(* Object class/Item mode *)
	class_head* = 0; class_module* = 1; class_var* = 2; class_ref* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; mode_reg* = 9; mode_regI* = 10; mode_cond* = 11;
	mode_xreg* = 12;
	class_string = 255;
	
	classes_Variable* = {class_var, class_ref, mode_regI};
	classes_Value* = classes_Variable
		+ {class_const, mode_reg, mode_cond, class_proc};
	cls_Variable* = classes_Variable; cls_HasValue* = classes_Value;

	(* Type form *)
	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_char* = 3;
	type_real* = 4; type_pointer* = 5; type_procedure* = 6;
	type_array* = 7; type_record* = 8; type_string* = 9; type_nil* = 10;
	
	types_Simple* = {type_integer, type_boolean, type_set, type_real, type_char};
	types_Address* = {type_pointer, type_procedure};
	types_Scalar* = types_Simple + types_Address;
	types_Numberic* = {type_integer, type_real};
	types_Character* = {type_char, type_string};
	types_HasExt* = {type_record, type_pointer};

TYPE
	Type* = POINTER TO RECORD
		ref*, iref*, mod*, tdAdr* : INTEGER;
		
		form* : INTEGER;
		base* : Type;
		fields* : Object;
		size*, len*, num_ptr*, alignment* : INTEGER;
		
		obj* : Object;
		next* : Type
	END;

	Object* = POINTER TO RECORD
		param*, readonly*, export* : BOOLEAN;
		name* : String;
		class*, lev* : INTEGER;
		type* : Type;
		next*, dsc* : Object;
		val2* : INTEGER; val* : LONGINT
	END;
	
	Item* = RECORD
		readonly*, param* : BOOLEAN;
		mode*, lev* : INTEGER;
		obj* : Object; type* : Type;
		r* : INTEGER; b*, c* : INTEGER;
		a* : LONGINT
	END;
		
	UndefPtrList* = POINTER TO RECORD
		exported : BOOLEAN; typ : Type; basename : String;
		next : UndefPtrList
	END;
	
	Module = RECORD
		imported : BOOLEAN; name : String;
		lev : INTEGER; objects : Object;
		types : ARRAY 1024 OF Type
	END;

VAR
	top_scope*, universe*, guard* : Object;
	undef_ptr_list* : UndefPtrList;
	cur_lev* : INTEGER;
	
	(* predefined type *)
	int_type*, bool_type*, set_type*, char_type*, byte_type*, real_type* : Type;
	longreal_type*, nil_type*, word_type*, dword_type* : Type;
	guardRecord_type*, guardPointer_type*, guardArray_type* : Type;
	
	refno, expno*, modno, modlev, rexmodno : INTEGER;
	impMods, rexMods : ARRAY 256 OF Module;
	symfile : Sys.FileHandle;
	
	(* Forward decl procedure *)
	_Export_type : PROCEDURE (typ : Type);
	_Import_type : PROCEDURE (VAR typ : Type);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Open_scope* (scope_name : Base.String);
	VAR new_scope : Object;
BEGIN
	NEW (new_scope);
	new_scope.class := class_head;
	new_scope.name := scope_name;
	new_scope.dsc := top_scope;
	new_scope.next := guard;
	top_scope := new_scope
END Open_scope;

PROCEDURE Close_scope*;
BEGIN
	top_scope := top_scope.dsc
END Close_scope;
	
PROCEDURE Inc_level* (x : INTEGER);
BEGIN
	INC (cur_lev, x)
END Inc_level;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Find_obj_in_scope (
	VAR obj : Object; name : Base.String; scope : Object
);
BEGIN
	guard.name := name; obj := scope.next;
	WHILE obj.name # name DO obj := obj.next END
END Find_obj_in_scope;

PROCEDURE Find_obj* (VAR obj : Object; name : Base.String);
	VAR	loop_exit : BOOLEAN; scope : Object;
BEGIN
	loop_exit := FALSE; scope := top_scope;
	REPEAT
		IF scope = universe THEN
			Find_obj_in_scope (obj, name, scope); loop_exit := TRUE
		ELSIF scope = top_scope THEN
			IF scope.name = name THEN
				Find_obj_in_scope (obj, name, scope.dsc); loop_exit := TRUE
			ELSE
				Find_obj_in_scope (obj, name, scope);
				IF obj # guard THEN loop_exit := TRUE END
			END
		END;
		scope := scope.dsc
	UNTIL loop_exit
END Find_obj;
	
PROCEDURE Find_field* (VAR obj : Object; name : Base.String; rectyp : Type);
BEGIN
	guard.name := name; obj := rectyp.fields;
	WHILE obj.name # name DO obj := obj.next END
END Find_field;

PROCEDURE Find_in_module* (
	VAR obj : Object; name : Base.String; modid : INTEGER
);
BEGIN
	guard.name := name; obj := importModules [modid].objects;
	WHILE obj.name # name DO obj := obj.next END
END Find_in_module;

PROCEDURE New_obj* (VAR obj : Object; name : Base.String; class : INTEGER);
BEGIN
	IF class = class_field THEN	Find_obj_in_scope (obj, name, top_scope)
	ELSE Find_obj (obj, name)
	END;
	IF obj = guard THEN
		obj := top_scope; WHILE obj.next # guard DO obj := obj.next END;
		NEW (obj.next); obj := obj.next;
		
		obj.export := FALSE;
		obj.readonly := FALSE;
		obj.param := FALSE;
		obj.name := name;
		obj.class := class;
		obj.lev := cur_lev;
		obj.next := guard;
		obj.val := 0;
		obj.val2 := 0
	ELSE Scanner.Mark ('Duplicated identifer definition'); obj := guard
	END
END New_obj;

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
BEGIN
	NEW (typ);
	typ.form := form;
	typ.mod := -1;
	typ.ref := -1;
	typ.iref := -1;
	typ.num_ptr := 0
END New_typ;

PROCEDURE New_predefined_typ (VAR typ : Type; form, size, ref : INTEGER);
BEGIN
	New_typ (typ, form);
	typ.mod := -2;
	typ.size := size;
	typ.alignment := size;
	typ.ref := ref
END New_predefined_typ;
	
PROCEDURE Enter (cl, n : INTEGER; name : Base.String; typ : Type);
	VAR obj : Object;
BEGIN
	NEW (obj);
	obj.readonly := FALSE;
	obj.param := FALSE;
	obj.class := cl;
	obj.val := n;
	obj.name := name;
	obj.type := typ;
	obj.next := top_scope.next;
	top_scope.next := obj
END Enter;

PROCEDURE Enter2 (
	cl, n : INTEGER; name : Base.String; typ : Type; n2 : INTEGER
);
BEGIN
	Enter (cl, n, name, typ);
	top_scope.next.val2 := n2
END Enter2;
	
PROCEDURE Register_undef_type* (
	typ : Type; basename : Base.String; export : BOOLEAN
);
	VAR undef : UndefPtrList;
BEGIN
	NEW (undef); undef.typ := typ; undef.basename := basename;
	undef.exported := export; typ.base := int_type;
	undef.next := undef_ptr_list; undef_ptr_list := undef
END Register_undef_type;
	
PROCEDURE Check_undef_list* (obj : Object);
	VAR p, prev : UndefPtrList;
BEGIN
	p := undef_ptr_list;
	REPEAT
		IF p.basename # obj.name THEN prev := p; p := p.next
		ELSE
			p.typ.base := obj.type;
			IF ~ p.exported OR obj.export THEN error := 0 (* Good *)
			ELSE Scanner.Mark ('This record type is not exported')
			END;
			IF p = undef_ptr_list THEN undef_ptr_list := p.next
			ELSE prev.next := p.next
			END;
			p := NIL (* exit *)
		END
	UNTIL p = NIL
END Check_undef_list;
	
PROCEDURE Cleanup_undef_list*;
	VAR errormsg : Base.LongString;
BEGIN
	REPEAT
		errormsg := 'Record type ';
		Base.Append_str (errormsg, undef_ptr_list.basename);
		Base.Append_str (errormsg, ' is not defined');
		Scanner.Mark (errormsg);
		undef_ptr_list.typ.base := guardRecord_type;
		undef_ptr_list := undef_ptr_list.next
	UNTIL undef_ptr_list = NIL
END Cleanup_undef_list;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import/Export *)

PROCEDURE Import_reExport_type (VAR typ : Type; m : INTEGER);
	VAR ref : INTEGER; tp : Type;
BEGIN
	Sys.Read_2bytes (symfile, ref);
	IF ref > 8000H THEN ref := ref + 0FFFF0000H END;
	
	i := 0;
	WHILE (i < modno)
		& (importModules[i].name # reExportModules[m].name)
	DO i := i + 1
	END;
	IF i < modno THEN (* found *)
		tp := importModules[i].types;
		WHILE (tp # NIL) & (tp.iref # ref) THEN tp := tp.next END;
		typ := tp
	END
END Import_reExport_type;

PROCEDURE Detect_typeI (VAR typ : Type);
	VAR m, ref : INTEGER;
BEGIN
	ref := 0; m := 0;
	Sys.Read_2bytes (symfile, m); Sys.Read_2bytes (symfile, ref);
	IF m > 8000H THEN m := m + 0FFFF0000H END;
	IF ref > 8000H THEN ref := ref + 0FFFF0000H END;
	
	IF m = -2 THEN
		IF ref = 0 THEN typ := NIL
		ELSIF ref = 1 THEN typ := int_type
		ELSIF ref = 2 THEN typ := bool_type
		ELSIF ref = 3 THEN typ := set_type
		ELSIF ref = 4 THEN typ := byte_type
		ELSIF ref = 5 THEN typ := char_type
		ELSIF ref = 6 THEN typ := nil_type
		ELSIF ref = 7 THEN typ := real_type
		ELSIF ref = 8 THEN typ := longreal_type
		ELSIF ref = 9 THEN typ := guardRecord_type
		ELSIF ref = 10 THEN typ := guardPointer_type
		ELSIF ref = 11 THEN typ := guardArray_type
		ELSIF ref = 12 THEN typ := word_type
		ELSIF ref = 13 THEN typ := dword_type
		ELSE ASSERT(FALSE)
		END
	ELSE
		IF ref >= 0 THEN typ := importTypes [ref]
		ELSE
			IF m >= 0 THEN Import_reExport_type (typ, m) END;
			_Import_type (typ)
		END
	END
END Detect_typeI;

PROCEDURE Import_type (VAR typ : Type);
	VAR field : Object; i, n : INTEGER; name : Base.String;
		basetp, fieldtp : Type; not_reExportType : BOOLEAN;
BEGIN
	n := 0; Sys.Read_byte (symfile, n);
	not_reExportType := typ = NIL;
	IF not_reExportType THEN New_typ (typ, n) END;
	importTypes [refno] := typ; refno := refno + 1;
	
	IF n = type_record THEN
		Detect_typeI (basetp);
		IF not_reExportType THEN
			typ.base := basetp;
			Sys.Read_byte (symfile, typ.len);
			Sys.Read_4bytes (symfile, typ.size);
			Sys.Read_4bytes (symfile, typ.num_ptr);
			Sys.Read_4bytes (symfile, typ.alignment);
			Open_scope ('')
		ELSE Sys.SeekRel (symfile, 13)
		END;
		
		Sys.Read_byte (symfile, n);
		WHILE n = class_field DO
			Sys.Read_string (symfile, name);
			Detect_typeI (fieldtp);
			IF not_reExportType THEN
				New_obj (field, name, class_field);
				field.type := fieldtp;
				Sys.Read_4bytes (symfile, n); field.val := n;
			ELSE Sys.SeekRel (symfile, 4)
			END;
			Sys.Read_byte (symfile, n)
		END;
		
		IF not_reExportType THEN
			typ.fields := top_scope.next;
			Close_scope
		END
	ELSIF n = type_array THEN
		Detect_typeI (basetp);
		IF not_reExportType THEN
			typ.base := basetp;
			Sys.Read_4bytes (symfile, typ.len);
			typ.size := typ.len * basetp.size;
			typ.num_ptr := typ.len * basetp.num_ptr;
			typ.alignment := basetp.alignment
		ELSE Sys.SeekRel (symfile, 4)
		END
	ELSIF n = type_pointer THEN
		Detect_typeI (basetp);
		IF not_reExportType THEN
			typ.base := basetp;
			typ.size := Base.Word_size;
			typ.num_ptr := 1;
			typ.alignment := Base.Word_size;
			IF basetp = int_type THEN
				Sys.Read_string (symfile, name);
				Register_undef_type (typ, name, FALSE)
			END
		END
	ELSIF n = type_procedure THEN
		Detect_typeI (basetp);
		IF not_reExportType THEN
			typ.base := basetp;
			Sys.Read_4bytes (symfile, typ.len);
			typ.size := Base.Word_size;
			typ.alignment := Base.Word_size;
			Open_scope ('')
		ELSE Sys.SeekRel (symfile, 4)
		END;		
		
		Sys.Read_byte (symfile, n);
		WHILE n # class_type DO
			Sys.Read_string (symfile, name);
			Detect_typeI (fieldtp);
			IF not_reExportType THEN
				New_obj (field, name, n);
				field.type := fieldtp;
				field.param := TRUE;
				Sys.Read_byte (n); field.readonly := n = 1
			ELSE Sys.SeekRel (symfile, 1)
			END;
			Sys.Read_byte (symfile, n)
		END;
		
		IF not_reExportType THEN
			typ.fields := top_scope.next;
			Close_scope
		END
	END
END Import_type;
	
PROCEDURE Import_symbols_file* (filename : ARRAY OF CHAR);
	VAR n : INTEGER; obj : Object; name : Base.String;
BEGIN
	refno := 0;	expno := 0; n := 0; rexmodno := 0;
	Sys.Open (symfile, filename);
	Sys.Read_4bytes (symfile, n); (* Skip module level *)
	
	Sys.Read_byte (symfile, n);
	WHILE n # class_head DO
		IF n IN {class_proc, class_var} THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, n);
			Detect_typeI (obj.type);
			expno := expno + 1; obj.val2 := expno
		ELSIF n = class_type THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class_type);
			Detect_typeI (obj.type);
			IF obj.type.form = type_record THEN
				expno := expno + 1; obj.val2 := expno;
				IF undef_ptr_list # NIL THEN Check_undef_list (obj) END
			END
		ELSIF n = class_const THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class_const);
			Sys.Read_8bytes (symfile, obj.val);
			Detect_typeI (obj.type)
		ELSIF n = class_string THEN
			(* Implement later *)
			ASSERT(FALSE)
		ELSIF n = class_module THEN
			Sys.Read_string (symfile, reExportModules [rexmodno].name);
			Sys.Read_4bytes (symfile, reExportModules [rexmodno].lev);
			INC (rexmodno)
		END;
		Sys.Read_byte (symfile, n)
	END;
	Sys.Close (symfile); ASSERT (undef_ptr_list = NIL)
END Import_symbols_file;

PROCEDURE Import_SYSTEM (modid : INTEGER);
BEGIN
	cur_lev := -2; Open_scope ('SYSTEM');
	
	Enter (class_type, 0, 'WORD', word_type);
	Enter (class_type, 0, 'DWORD', dword_type);
	
	Enter (class_sproc, 100, 'GET', NIL);
	Enter (class_sproc, 101, 'PUT', NIL);
	Enter (class_sproc, 102, 'COPY', NIL);
	Enter2 (class_sproc, 103, 'LoadLibraryW', NIL, -48);
	Enter2 (class_sproc, 104, 'GetProcAddress', NIL, -40);
	
	Enter (class_sproc, 300, 'ADR', int_type);
	Enter (class_sproc, 301, 'SIZE', int_type);
	Enter (class_sproc, 302, 'BIT', bool_type);
	Enter (class_sproc, 303, 'VAL', int_type);
	
	importModules [modid].objects := top_scope.next;
	Close_scope; cur_lev := 0
END Import_SYSTEM;

PROCEDURE Import_modules* (modul : Object);
	VAR filename : LongString; i, min : INTEGER; finish : BOOLEAN;
BEGIN
	(* Find the lowest level module to import first *)
	finish := FALSE;
	REPEAT i := 0; min := -1;
		WHILE i < modno DO
			IF (importModules [i].not_imported) & ((min = -1)
				OR (importModules [i].lev < importModules [min].lev))
			THEN min := i
			END;
			INC (i)
		END;
		IF min # -1 THEN
			IF importModules [min].name # 'SYSTEM' THEN
				filename := ''; Append_str (filename, modul.realname);
				Append_str (filename, '.sym');		
				cur_lev := -2; Open_scope (modul.name);
				Import_symbols_file (filename);
				importModules [min].objects := top_scope.next;
				Close_scope; cur_lev := 0
			ELSE Import_SYSTEM (i)
			END
		ELSE finish := TRUE
		END
	UNTIL finish
END Import_modules;

PROCEDURE Find_module_symfile* (
	modul : Object; sym_name : String; VAR error : INTEGER
);
	VAR filename : LongString; obj : Object; file : Sys.FileHandle;
BEGIN
	error := 0;
	IF modno < LEN(importModules) THEN
		importModules [modno].name := sym_name;
		importModules [modno].not_imported := TRUE;
		IF sym_name # 'SYSTEM' THEN
			filename := ''; Append_str (filename, sym_name);
			Append_str (filename, '.sym');
			IF Sys.File_existed (filename) THEN
				Sys.Open (file, filename);
				Sys.Read_4bytes (file, importModules [modno].lev);
				Sys.Close (file);
				modul.val := modno; INC (modno)
			ELSE error := 1
			END
		ELSE importModules [modno].lev := -1; modul.val := modno; INC (modno)
		END
	ELSE error := 2
	END
END Find_module_symfile;

(* -------------------------------------------------------------------------- *)

PROCEDURE Detect_type (typ : Type);
BEGIN
	IF typ = NIL THEN
		Sys.Write_2bytes (symfile, -2);
		Sys.Write_2bytes (symfile, 0)
	ELSE
		Sys.Write_2bytes (symfile, typ.mod);
		Sys.Write_2bytes (symfile, typ.ref);
		IF typ.ref = -1 THEN _Export_type (typ) END
	END
END Detect_type;
	
PROCEDURE Export_type (typ : Type);
	VAR field : Object;
BEGIN
	typ.ref := refno; refno := refno + 1;
	IF typ.mod >= 0 THEN Sys.Write_byte (typ.iref) END;
	Sys.Write_byte (symfile, typ.form);
	
	IF typ.form = type_record THEN
		Detect_type (typ.base);
		Sys.Write_byte (symfile, typ.len);
		Sys.Write_4bytes (symfile, typ.size);
		Sys.Write_4bytes (symfile, typ.num_ptr);
		field := typ.fields;
		WHILE field # guard DO
			IF field.export OR (field.type.num_ptr > 0) THEN
				Sys.Write_byte (symfile, class_field);
				IF ~ field.export THEN Sys.Write_string (symfile, '')
				ELSE Sys.Write_string (symfile, field.name)
				END;
				Sys.Write_4bytes (symfile, SHORT(field.val));
				Detect_type (field.type)
			END;
			field := field.next
		END;
		Sys.Write_byte (symfile, class_type)
	ELSIF typ.form = type_array THEN
		Sys.Write_4bytes (symfile, typ.len);
		Detect_type (typ.base)
	ELSIF typ.form = type_pointer THEN
		IF typ.base.obj # NIL THEN Detect_type (int_type);
			Sys.Write_string (symfile, typ.base.obj.name)
		ELSE Detect_type (typ.base)
		END
	ELSIF typ.form = type_procedure THEN
		Sys.Write_4bytes (symfile, typ.len);
		Detect_type (typ.base); field := typ.fields;
		WHILE field # guard DO
			Sys.Write_byte (symfile, field.class);
			Sys.Write_string (symfile, field.name);
			IF ~ field.readonly THEN Sys.Write_byte (symfile, 0)
			ELSE Sys.Write_byte (symfile, 1)
			END;
			Detect_type (field.type);
			field := field.next
		END;
		Sys.Write_byte (symfile, class_type)
	END
END Export_type;
	
PROCEDURE Write_symbols_file*;
	VAR obj : Object;
BEGIN
	refno := 0;	expno := 0;
	Sys.Rewrite (symfile, 'sym.temp_');
	Sys.Write_4bytes (symfile, modlev);
	
	obj := universe.next;
	WHILE obj # guard DO
		IF obj.export THEN
			IF obj.class IN {class_var, class_proc} THEN
				IF obj.type.form # type_string THEN
					expno := expno + 1;
					Sys.Write_byte (symfile, obj.class);
					Sys.Write_string (symfile, obj.name);
					Detect_type (obj.type)
				ELSE
					(* Implement later *)
					ASSERT(FALSE);
					Sys.Write_byte (symfile, class_string);
					Sys.Write_string (symfile, obj.name)
				END
			ELSIF obj.class = class_type THEN
				Sys.Write_byte (symfile, class_type);
				Sys.Write_string (symfile, obj.name);
				IF obj.type.form = type_record THEN expno := expno + 1 END;
				Detect_type (obj.type)
			ELSIF obj.class = class_const THEN
				Sys.Write_byte (symfile, class_const);
				Sys.Write_string (symfile, obj.name);
				Sys.Write_8bytes (symfile, obj.val);
				Detect_type (obj.type)
			END
		ELSIF obj.class = class_module THEN
			Sys.Write_byte (symfile, class_module);
			Sys.Write_string (symfile, importModules [obj.val].name);
			Sys.Write_4bytes (symfile, importModules [obj.val].lev)
		END;
		obj := obj.next
	END;
	Sys.Write_byte (symfile, class_head);
	Sys.Close (symfile)
END Write_symbols_file;
	
PROCEDURE Init* (modid : String);
BEGIN
	modlev := 0; modno := 0;
	NEW (universe);
	universe.class := class_head;
	universe.name := modid;
	universe.next := guard;
	top_scope := universe;
	
	Enter (class_const, 0, 'NIL', nil_type);
	
	Enter (class_type, 0, 'INTEGER', int_type);
	Enter (class_type, 0, 'BOOLEAN', bool_type);
	Enter (class_type, 0, 'SET', set_type);
	Enter (class_type, 0, 'BYTE', byte_type);
	Enter (class_type, 0, 'CHAR', char_type);
(*	Enter (class_type, 0, 'REAL', real_type); *)
(*	Enter (class_type, 0, 'LONGREAL', longreal_type); *)
	
	Enter (class_sproc, 0, 'INC', NIL);
	Enter (class_sproc, 1, 'DEC', NIL);
	Enter (class_sproc, 2, 'INCL', NIL);
	Enter (class_sproc, 3, 'EXCL', NIL);
	Enter (class_sproc, 4, 'NEW', NIL);
	Enter (class_sproc, 5, 'ASSERT', NIL);
(*	Enter (class_sproc, 6, 'PACK', NIL); *)
(*	Enter (class_sproc, 7, 'UNPK', NIL); *)

	Enter (class_sproc, 8, 'DISPOSE', NIL);
	
	Enter (class_sproc, 200, 'ABS', int_type);
	Enter (class_sproc, 201, 'ODD', bool_type);
	Enter (class_sproc, 202, 'LEN', int_type);
	Enter (class_sproc, 203, 'LSL', int_type);
	Enter (class_sproc, 204, 'ASR', int_type);
	Enter (class_sproc, 205, 'ROR', int_type);
(*	Enter (class_sproc, 206, 'FLOOR', int_type); *)
(*	Enter (class_sproc, 207, 'FLT', int_type); *)
	Enter (class_sproc, 208, 'ORD', int_type);
	Enter (class_sproc, 209, 'CHR', char_type);
END Init;
	
BEGIN
	_Export_type := Export_type; _Import_type := Import_type;
	NEW (guard); guard.class := class_head;

	New_predefined_typ (int_type, type_integer, Base.Word_size, 1);
	New_predefined_typ (bool_type, type_boolean, 1, 2);
	New_predefined_typ (set_type, type_set, Base.Word_size, 3);
	New_predefined_typ (byte_type, type_integer, 1, 4);
	New_predefined_typ (char_type, type_char, Base.Char_size, 5);
	New_predefined_typ (nil_type, type_nil, Base.Word_size, 6);
	New_predefined_typ (real_type, type_real, 4, 7);
	New_predefined_typ (longreal_type, type_real, 8, 8);
	
	New_predefined_typ (guardRecord_type, type_record, Base.Word_size, 9);
	guardRecord_type.fields := guard;
	
	New_predefined_typ (guardPointer_type, type_pointer, Base.Word_size, 10);
	guardPointer_type.base := guardRecord_type;
	guardPointer_type.num_ptr := 1;
	
	New_predefined_typ (guardArray_type, type_array, Base.Word_size, 11);
	guardArray_type.base := int_type;
	guardPointer_type.len := 1;
	
	New_predefined_typ (word_type, type_integer, 2, 12);
	New_predefined_typ (dword_type, type_integer, 4, 13)
END SymTable.