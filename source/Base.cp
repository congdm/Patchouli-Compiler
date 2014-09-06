MODULE Base;

IMPORT
	Sys;

CONST
	success* = TRUE; failed* = FALSE;

	Word_size* = 8;
	Char_size* = 2;
	MAX_CHAR* = 65535;
	MAX_INT* = 9223372036854775807;
	MIN_INT* = -MAX_INT - 1;
	max_ident_len* = 63;
	max_str_len* = 255;
	max_extension* = 8;
	max_record_types* = 512;
	set_size_limit* = 64;

	(* Object class/Item mode *)
	class_head* = 0; class_module* = 1; class_var* = 2; class_ref* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; mode_reg* = 9; mode_regI* = 10; mode_cond* = 11;
	mode_xreg* = 12;
	class_string = 255;
	
	classes_Variable* = {class_var, class_ref, mode_regI};
	classes_Value* = classes_Variable + {class_const, mode_reg, mode_cond};
	cls_Variable* = {class_var, class_ref, mode_regI};
	cls_HasValue* = cls_Variable + {class_const, mode_reg, mode_cond};
	modes_UseReg* = {mode_reg, mode_regI};

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
	String* = ARRAY max_ident_len + 1 OF CHAR;
	LongString* = ARRAY max_str_len + 1 OF CHAR;

	Type* = POINTER TO RECORD
		predefined*, unsafe*, import* : BOOLEAN;
		form* : INTEGER;
		fields*, obj* : Object;
		base*, next* : Type;
		size*, len*, alignment* : INTEGER;
		num_ptr*, ref*, tdAdr* : INTEGER
	END;

	Object* = POINTER TO RECORD
		param*, readonly*, export* : BOOLEAN;
		name*, realname* : String;
		class*, lev*, parblksize* : INTEGER;
		type* : Type;
		next*, dsc* : Object;
		val2* : INTEGER; val* : LONGINT
	END;
	
	Item* = RECORD
		r* : UBYTE; readonly*, param* : BOOLEAN;
		mode*, lev* : INTEGER;
		obj* : Object; type* : Type;
		b*, c* : INTEGER;
		a* : LONGINT
	END;
		
	UndefPtrList* = POINTER TO RECORD
		exported : BOOLEAN; typ : Type; basename : String;
		next : UndefPtrList
	END;

VAR
	top_scope*, universe*, guard* : Object;
	undef_ptr_list* : UndefPtrList;
	cur_lev* : INTEGER;
	
	(* predefined type *)
	int_type*, bool_type*, set_type*, char_type*, byte_type*, real_type* : Type;
	longreal_type*, nil_type* : Type;
	guardRecord_type*, guardPointer_type*, guardArray_type* : Type;
	
	refno, expno* : INTEGER;
	symfile : Sys.FileHandle;
	importTypes : ARRAY 1024 OF Type;
	
	stringdata : ARRAY 65536 OF CHAR;
	strpos : INTEGER;
	
	CompilerFlag* : RECORD
		array_check*, overflow_check*, type_check* : BOOLEAN;
		alignment*, main*, console* : BOOLEAN
	END;
	
	(* Forward decl procedure *)
	_Export_type : PROCEDURE (typ : Type);
	_Import_type : PROCEDURE (VAR typ : Type);
	
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Str_len* (IN s : ARRAY OF CHAR) : INTEGER;
	VAR i : INTEGER;
BEGIN
	i := 0; WHILE (s[i] # 0X) & (i < LEN(s)) DO INC (i) END;
	RETURN i
END Str_len;

PROCEDURE Append_str* (VAR dst : ARRAY OF CHAR; src : ARRAY OF CHAR);
	VAR i, k : INTEGER;
BEGIN
	k := Str_len (dst); i := 0;
	WHILE (k + i < LEN(dst)) & (i < LEN(src)) & (src[i] # 0X) DO
		dst[k + i] := src[i];
		i := i + 1
	END;
	IF k + i < LEN(dst) THEN dst[k + i] := 0X END
END Append_str;

PROCEDURE log2* (a : LONGINT) : INTEGER;
	VAR e : INTEGER;
BEGIN
	IF a > 0 THEN
		e := 0;
		WHILE a > 1 DO
			IF ~ ODD(a) THEN INC (e); a := a DIV 2
			ELSE e := -1; a := 1
			END
		END
	ELSE e := -1
	END;
	RETURN e
END log2;
	
PROCEDURE Size_of_const* (n : LONGINT) : INTEGER;
	VAR res : INTEGER;
BEGIN
	IF (n >= 0) & (n < 256) THEN res := 1
	ELSIF (n >= 0) & (n < 65536) THEN res := 2
	ELSIF (n >= 0) & (n < 4294967296) THEN res := 4
	ELSE res := 8
	END;
	RETURN res
END Size_of_const;

PROCEDURE Safe_to_add* (x, y : LONGINT) : BOOLEAN;
	VAR result : BOOLEAN;
BEGIN
	result := TRUE;
	IF (x >= 0) & (y >= 0) THEN
		IF y > MAX_INT - x THEN result := FALSE	END;
	ELSIF (x < 0) & (y < 0) THEN
		IF y < MIN_INT - x THEN result := FALSE END
	END;
	RETURN result
END Safe_to_add;
	
PROCEDURE Safe_to_subtract* (x, y : LONGINT) : BOOLEAN;
	VAR result : BOOLEAN;
BEGIN
	result := TRUE;
	IF (x >= 0) & (y < 0) THEN
		IF x > MAX_INT + y THEN result := FALSE END
	ELSIF (x < 0) & (y >= 0) THEN
		IF x < MIN_INT + y THEN result := FALSE END
	END;
	RETURN result
END Safe_to_subtract;
	
PROCEDURE Safe_to_multiply* (x, y : LONGINT) : BOOLEAN;
	VAR result : BOOLEAN; q, r : LONGINT;
BEGIN
	result := TRUE;
	IF (x < 0) & (y >= 0) THEN
		q := x;	x := y; y := q (* swap *)
	ELSIF (x < 0) & (y < 0) THEN
		IF (x = MIN_INT) OR (y = MIN_INT) THEN result := FALSE
		ELSE x := -x; y := -y
		END
	END;
	IF x > 0 THEN
		IF y > 0 THEN
			IF x > MAX_INT / y THEN result := FALSE END
		ELSIF y < 0 THEN
			q := MIN_INT DIV y; r := MIN_INT MOD y;
			IF r = 0 THEN
				IF x > q THEN result := FALSE END
			ELSE
				IF x >= q THEN result := FALSE END
			END
		END
	END;
	RETURN result
END Safe_to_multiply;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Open_scope* (scope_name : String);
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
	
PROCEDURE Find_obj_in_scope (VAR obj : Object; IN name : String; scope : Object);
BEGIN
	guard.name := name; obj := scope.next;
	WHILE obj.name # name DO obj := obj.next END
END Find_obj_in_scope;

PROCEDURE Find_obj* (VAR obj : Object; IN name : String);
	VAR	loop_exit : BOOLEAN;
		scope : Object;
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
	
PROCEDURE Find_field* (VAR obj : Object; IN name : String; rec_typ : Type);
BEGIN
	guard.name := name; obj := rec_typ.fields;
	WHILE obj.name # name DO obj := obj.next END
END Find_field;

PROCEDURE Find_in_module* (VAR obj : Object; IN name : String; modul : Object);
BEGIN
	guard.name := name; obj := modul.dsc;
	WHILE obj.name # name DO obj := obj.next END
END Find_in_module;

PROCEDURE New_obj* (VAR obj : Object; IN name : String; class : INTEGER);
BEGIN
	IF class = class_field THEN	Find_obj_in_scope (obj, name, top_scope)
	ELSE Find_obj (obj, name)
	END;
	
	IF obj = guard THEN
		obj := top_scope;
		WHILE obj.next # guard DO obj := obj.next END;
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
	ELSE
		obj := guard
	END
END New_obj;

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
BEGIN
	NEW (typ);
	typ.predefined := FALSE;
	typ.unsafe := FALSE;
	typ.import := FALSE;
	typ.form := form;
	typ.num_ptr := 0;
	typ.ref := 0
END New_typ;

PROCEDURE New_predefined_typ (VAR typ : Type; form, size, ref : INTEGER);
BEGIN
	New_typ (typ, form);
	typ.predefined := TRUE;
	typ.size := size;
	typ.alignment := size;
	typ.ref := ref
END New_predefined_typ;
	
PROCEDURE Enter (cl, n : INTEGER; name : String; typ : Type);
	VAR obj : Object; str : String;
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

PROCEDURE Enter2 (cl, n : INTEGER; name : String; typ : Type; n2 : INTEGER);
BEGIN
	Enter (cl, n, name, typ);
	top_scope.next.val2 := n2
END Enter2;
	
PROCEDURE Register_undef_type* (typ : Type; basename : String; export : BOOLEAN);
	VAR undef : UndefPtrList;
BEGIN
	NEW (undef); undef.typ := typ; undef.basename := basename;
	undef.exported := export; typ.base := int_type;
	undef.next := undef_ptr_list; undef_ptr_list := undef
END Register_undef_type;
	
PROCEDURE Check_undef_list* (obj : Object; VAR error : INTEGER);
	VAR p, prev : UndefPtrList;
BEGIN
	p := undef_ptr_list;
	REPEAT
		IF p.basename # obj.name THEN prev := p; p := p.next
		ELSE
			p.typ.base := obj.type;
			IF ~ p.exported OR obj.export THEN error := 0 (* Good *)
			ELSE error := 1
			END;
			IF p = undef_ptr_list THEN undef_ptr_list := p.next
			ELSE prev.next := p.next
			END;
			p := NIL
		END
	UNTIL p = NIL
END Check_undef_list;
	
PROCEDURE Cleanup_undef_list*;
BEGIN
	REPEAT
		undef_ptr_list.typ.base := guardRecord_type;
		undef_ptr_list := undef_ptr_list.next
	UNTIL undef_ptr_list = NIL
END Cleanup_undef_list;
	
PROCEDURE Adjust_alignment* (VAR offset : INTEGER; alignment : INTEGER);
	VAR i : INTEGER;
BEGIN
	IF CompilerFlag.alignment & (alignment # 0) THEN
		i := offset MOD alignment;
		IF i # 0 THEN
			IF offset < 0 THEN DEC (offset, i)
			ELSE INC (offset, alignment - i)
			END
		END
	END
END Adjust_alignment;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Is_string* (type : Type) : BOOLEAN;
BEGIN
	RETURN (type.form = type_string)
	OR (type.form = type_array) & (type.base = char_type)
END Is_string;

PROCEDURE Is_extension* (ext, bas : Type) : BOOLEAN;
BEGIN
	RETURN (ext = bas) OR (ext.base # NIL) & Is_extension_type (ext.base, bas)
END Is_extension;
	
PROCEDURE Compatible_open_array* (typ1, typ2 : Type) : BOOLEAN;
BEGIN
	RETURN (typ1.base = typ2.base)
	OR
		(typ1.base.form = type_array) & (typ2.base.form = type_array)
		& (typ1.base.len < 0) & (typ2.base.len < 0)
		& Compatible_open_array (typ1, typ2)
END Compatible_open_array;
	
PROCEDURE Same_parlist* (parlist1, parlist2 : Object) : BOOLEAN;
BEGIN
	RETURN (parlist1 = guard) & (parlist2 = guard)
	OR
		(parlist1 # guard) & (parlist2 # guard)
		& (parlist1.class = parlist2.class)
		& (parlist1.readonly = parlist2.readonly)
		& ((parlist1.type = parlist2.type)
			OR (parlist1.type.form = type_array) & (parlist1.type.len < 0)
			& (parlist2.type.form = type_array) & (parlist2.type.len < 0)
			& Compatible_open_array (parlist1.type, parlist2.type))
END Same_parlist;

PROCEDURE Equality_applicable* (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN (x.mode = class_proc) & (x.lev = 0)
	OR (x.mode IN classes_Value)
		& ((x.type.form IN types_Scalar + {type_string, type_nil})
			OR (x.type.form = type_array) & (x.type.base = char_type))
END Equality_applicable;

PROCEDURE Comparison_applicable* (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN (x.mode IN classes_Value)
		& ((x.type.form IN {type_integer, type_real, type_char, type_string})
			OR (x.type.form = type_array) & (x.type.base = char_type))
END Comparison_applicable;

PROCEDURE Type_test_applicable* (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN (x.mode IN classes_Value)
		& ((x.type.form = type_pointer)
			OR (x.type.form = type_record) & x.param & ~ x.readonly)
END Type_test_applicable;
	
PROCEDURE Compatible_array* (typ1, typ2 : Type) : BOOLEAN;
BEGIN
	RETURN (typ1.base = typ2.base)
	OR (typ1.base.form = type_array) & (typ2.base.form = type_array)
		& Compatible_array (typ1.base, typ2.base)
END Compatible_array;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Alloc_string* (s : LongString; len : INTEGER) : INTEGER;
	VAR i, res : INTEGER;
BEGIN
	IF strpos + len <= LEN (stringdata) THEN
		res := strpos; strpos := strpos + len; i := 0;
		WHILE i < len DO
			stringdata [res + i] := s[i];
			i := i + 1
		END
	ELSE res := -1
	END;
	RETURN res
END Alloc_string;

PROCEDURE Get_string* (VAR s : LongString; ref : INTEGER);
	VAR i : INTEGER;
BEGIN
	i := -1;
	REPEAT i := i + 1; s[i] := stringdata[ref + i] UNTIL s[i] = 0X
END Get_string;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import/Export *)

PROCEDURE Detect_typeI (VAR typ : Type);
	VAR ref : INTEGER;
BEGIN
	ref := 0; Sys.Read_2bytes (symfile, ref);
	IF ref > 8000H THEN ref := ref + 0FFFF0000H END; 
	IF ref < -1 THEN
		IF ref = -2 THEN typ := int_type
		ELSIF ref = -3 THEN typ := bool_type
		ELSIF ref = -4 THEN typ := set_type
		ELSIF ref = -5 THEN typ := byte_type
		ELSIF ref = -6 THEN typ := char_type
		ELSIF ref = -7 THEN typ := nil_type
		ELSIF ref = -8 THEN typ := real_type
		ELSIF ref = -9 THEN typ := longreal_type
		ELSIF ref = -10 THEN typ := guardRecord_type
		ELSIF ref = -11 THEN typ := guardPointer_type
		ELSIF ref = -12 THEN typ := guardArray_type
		ELSE ASSERT(FALSE)
		END
	ELSIF ref = -1 THEN
		_Import_type (typ)
	ELSIF ref = 0 THEN typ := NIL
	ELSIF ref > 0 THEN typ := importTypes [ref]
	END
END Detect_typeI;

PROCEDURE Import_param (cls : INTEGER);
	VAR name : String; par : Object; n : INTEGER;
BEGIN
	n := 0;
	Sys.Read_string (symfile, name); New_obj (par, name, cls);
	Sys.Read_byte (symfile, n);
	IF n # 0 THEN par.readonly := TRUE END;
	Detect_typeI (par.type)
END Import_param;
	
PROCEDURE Import_type (VAR typ : Type);
	VAR field : Object; n : INTEGER;
		name : String;
BEGIN
	n := 0;
	refno := refno + 1; Sys.Read_byte (symfile, n);
	New_typ (importTypes [refno], n);
	typ := importTypes [refno];
	
	IF n = type_record THEN
		Detect_typeI (typ.base);
		Sys.Read_byte (symfile, typ.len);
		Sys.Read_4bytes (symfile, typ.size);
		Sys.Read_4bytes (symfile, typ.num_ptr);
		
		Open_scope ('');
		Sys.Read_byte (symfile, n);
		WHILE n = class_field DO
			Sys.Read_string (symfile, name);
			New_obj (field, name, class_field);
			Sys.Read_4bytes (symfile, n); field.val := n;
			Detect_typeI (field.type);
			Sys.Read_byte (symfile, n)
		END;
		typ.fields := top_scope.next;
		Close_scope
	ELSIF n = type_array THEN
		Sys.Read_4bytes (symfile, typ.len);
		Detect_typeI (typ.base);
		typ.size := typ.len * typ.base.size;
		typ.alignment := typ.base.alignment
	ELSIF n = type_pointer THEN
		typ.size := Word_size;
		typ.alignment := Word_size;
		Detect_typeI (typ.base);
		IF typ.base = int_type THEN Sys.Read_string (symfile, name);
			Register_undef_type (typ, name, FALSE)
		END
	ELSIF n = type_procedure THEN
		typ.size := Word_size;
		typ.alignment := Word_size;
		Sys.Read_4bytes (symfile, typ.len);
		Detect_typeI (typ.base);
		
		Open_scope ('');
		Sys.Read_byte (symfile, n);
		WHILE n # class_type DO
			Import_param (n); Sys.Read_byte (symfile, n)
		END;
		typ.fields := top_scope.next;
		Close_scope
	END
END Import_type;
	
PROCEDURE Import_proc;
	VAR proc : Object; name : String; n : INTEGER;
BEGIN
	n := 0;
	Sys.Read_string (symfile, name);
	New_obj (proc, name, class_proc);
	Sys.Read_4bytes (symfile, proc.parblksize);
	Detect_typeI (proc.type);
	
	expno := expno + 1; proc.val2 := expno; 
	
	Open_scope (name);
	Sys.Read_byte (symfile, n);
	WHILE n # class_proc DO
		Import_param (n); Sys.Read_byte (symfile, n)
	END;
	proc.dsc := top_scope.next;
	Close_scope
END Import_proc;
	
PROCEDURE Import_symbols_file* (filename : ARRAY OF CHAR);
	VAR n, error : INTEGER; obj : Object; name : String;
BEGIN
	refno := 0;	expno := 0; n := 0;
	Sys.Open (symfile, filename);
	
	Sys.Read_byte (symfile, n);
	WHILE n # class_head DO
		IF n = class_proc THEN Import_proc
		ELSIF n = class_type THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class_type);
			Detect_typeI (obj.type);
			IF obj.type.form = type_record THEN
				expno := expno + 1; obj.val2 := expno; error := 0;
				Check_undef_list (obj, error); ASSERT (error = 0)
			END
		ELSIF n = class_const THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class_const);
			Sys.Read_8bytes (symfile, obj.val);
			Detect_typeI (obj.type)
		ELSIF n = class_var THEN
			Sys.Read_string (symfile, name);
			New_obj (obj, name, class_var);
			Detect_typeI (obj.type);
			expno := expno + 1; obj.val2 := expno
		ELSIF n = class_string THEN
			(* Implement later *)
			ASSERT(FALSE)
		END;
		Sys.Read_byte (symfile, n)
	END;
	Sys.Close (symfile); ASSERT (undef_ptr_list = NIL)
END Import_symbols_file;

PROCEDURE Import_SYSTEM (modul : Object);
BEGIN
	cur_lev := -2; Open_scope (modul.name);
	
	Enter (class_sproc, 100, 'GET', NIL);
	Enter (class_sproc, 101, 'PUT', NIL);
	Enter (class_sproc, 102, 'COPY', NIL);
	Enter2 (class_sproc, 103, 'LoadLibraryW', NIL, -48);
	Enter2 (class_sproc, 104, 'GetProcAddress', NIL, -40);
	
	Enter (class_sproc, 300, 'ADR', int_type);
	Enter (class_sproc, 301, 'SIZE', int_type);
	Enter (class_sproc, 302, 'BIT', bool_type);
	Enter (class_sproc, 303, 'VAL', int_type);
	
	modul.dsc := top_scope.next; Close_scope; cur_lev := 0
END Import_SYSTEM;

PROCEDURE Import_module* (modul : Object);
	VAR filename : LongString;
BEGIN
	IF modul.realname # 'SYSTEM' THEN
		filename := ''; Append_str (filename, modul.realname);
		Append_str (filename, '.sym');
		
		cur_lev := -2;
		Open_scope (modul.name);
		Import_symbols_file (filename);
		modul.dsc := top_scope.next;
		Close_scope;
		cur_lev := 0
	ELSE Import_SYSTEM (modul)
	END
END Import_module;

(* -------------------------------------------------------------------------- *)

PROCEDURE Detect_type (typ : Type);
BEGIN
	IF typ = NIL THEN Sys.Write_2bytes (symfile, 0)
	ELSIF (typ.ref > 0) OR (typ.ref < 0) THEN
		Sys.Write_2bytes (symfile, typ.ref)
	ELSE Sys.Write_2bytes (symfile, -1); _Export_type (typ)
	END
END Detect_type;

PROCEDURE Export_param (par : Object);
BEGIN
	Sys.Write_byte (symfile, par.class);
	Sys.Write_string (symfile, par.name);
	IF ~ par.readonly THEN Sys.Write_byte (symfile, 0)
	ELSE Sys.Write_byte (symfile, 1)
	END;
	Detect_type (par.type)
END Export_param;
	
PROCEDURE Export_type (typ : Type);
	VAR field : Object;
BEGIN
	refno := refno + 1; typ.ref := refno; 
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
		WHILE field # guard DO Export_param (field); field := field.next END;
		Sys.Write_byte (symfile, class_type)
	END
END Export_type;
	
PROCEDURE Export_proc (proc : Object);
	VAR par : Object;
BEGIN
	expno := expno + 1;
	Sys.Write_byte (symfile, class_proc);
	Sys.Write_string (symfile, proc.name);
	Sys.Write_4bytes (symfile, proc.parblksize);
	Detect_type (proc.type);
	par := proc.dsc;
	WHILE par # guard DO Export_param (par); par := par.next END;
	Sys.Write_byte (symfile, class_proc)
END Export_proc;
	
PROCEDURE Write_symbols_file*;
	VAR obj : Object;
BEGIN
	refno := 0;	expno := 0;
	Sys.Rewrite (symfile, 'sym.temp_');
	obj := universe.next;
	WHILE obj # guard DO
		IF obj.export THEN
			IF obj.class = class_proc THEN Export_proc (obj)
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
			ELSIF obj.class = class_var THEN
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
			END
		END;
		obj := obj.next
	END;
	Sys.Write_byte (symfile, class_head);
	Sys.Close (symfile)
END Write_symbols_file;

PROCEDURE Set_compiler_flag* (pragma : ARRAY OF CHAR);
BEGIN
	IF pragma = 'MAIN' THEN CompilerFlag.main := TRUE
	ELSIF pragma = 'CONSOLE' THEN
		CompilerFlag.main := TRUE; CompilerFlag.console := TRUE
	END
END Set_compiler_flag;

PROCEDURE Reset_compiler_flag*;
BEGIN
	CompilerFlag.array_check := TRUE;
	CompilerFlag.overflow_check := TRUE;
	CompilerFlag.type_check := TRUE;
	CompilerFlag.alignment := TRUE;
	CompilerFlag.main := FALSE;
	CompilerFlag.console := FALSE
END Reset_compiler_flag;
	
PROCEDURE Init* (modid : String);
BEGIN
	NEW (universe);
	universe.class := class_head;
	universe.name := modid;
	universe.next := guard;
	
	top_scope := universe;
	
	Enter (class_type, 0, 'INTEGER', int_type);
	Enter (class_type, 0, 'BOOLEAN', bool_type);
	Enter (class_type, 0, 'SET', set_type);
	Enter (class_type, 0, 'BYTE', byte_type);
	Enter (class_type, 0, 'CHAR', char_type);
(*	Enter (class_type, 0, 'REAL', real_type);
	Enter (class_type, 0, 'LONGREAL', longreal_type);
*)

	Enter (class_const, 0, 'NIL', nil_type);
	
	Enter (class_sproc, 0, 'INC', NIL);
	Enter (class_sproc, 1, 'DEC', NIL);
	Enter (class_sproc, 2, 'INCL', NIL);
	Enter (class_sproc, 3, 'EXCL', NIL);
	Enter (class_sproc, 4, 'NEW', NIL);
	Enter (class_sproc, 5, 'ASSERT', NIL);
(*
	Enter (class_sproc, 6, 'PACK', NIL);
	Enter (class_sproc, 7, 'UNPK', NIL);
*)
	Enter (class_sproc, 8, 'DISPOSE', NIL);
	
	Enter (class_sproc, 200, 'ABS', int_type);
	Enter (class_sproc, 201, 'ODD', bool_type);
	Enter (class_sproc, 202, 'LEN', int_type);
	Enter (class_sproc, 203, 'LSL', int_type);
	Enter (class_sproc, 204, 'ASR', int_type);
	Enter (class_sproc, 205, 'ROR', int_type);
	
(*	Enter (class_sproc, 206, 'FLOOR', int_type);
	Enter (class_sproc, 207, 'FLT', int_type);
*)
	Enter (class_sproc, 208, 'ORD', int_type);
	Enter (class_sproc, 209, 'CHR', char_type);
	
	strpos := 0
END Init;
	
BEGIN
	_Export_type := Export_type; _Import_type := Import_type;
	NEW (guard); guard.class := class_head;

	New_predefined_typ (int_type, type_integer, Word_size, -2);
	New_predefined_typ (bool_type, type_boolean, 1, -3);
	New_predefined_typ (set_type, type_set, Word_size, -4);
	New_predefined_typ (byte_type, type_integer, 1, -5);
	New_predefined_typ (char_type, type_char, Char_size, -6);
	New_predefined_typ (nil_type, type_nil, Word_size, -7);
	New_predefined_typ (real_type, type_real, 4, -8);
	New_predefined_typ (longreal_type, type_real, 8, -9);
	
	New_predefined_typ (guardRecord_type, type_record, Word_size, -10);
	guardRecord_type.fields := guard;
	New_predefined_typ (guardPointer_type, type_pointer, Word_size, -11);
	guardPointer_type.base := guardRecord_type; guardPointer_type.num_ptr := 1;
	New_predefined_typ (guardArray_type, type_array, Word_size, -12);
	guardArray_type.base := int_type; guardPointer_type.len := 1
END Base.
