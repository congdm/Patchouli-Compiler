MODULE Base;

IMPORT
	Sys;

CONST
	success* = TRUE; failed* = FALSE;

	Word_size* = 8;
	MAX_INT* = 9223372036854775807;
	MIN_INT* = -MAX_INT - 1;
	MAX_CHAR* = 65535;
	max_ident_len* = 63;
	max_str_len* = 255;
	type_extension_limit* = 7;
	max_number_record_types* = 1024;
	set_size_limit* = 64;

	(* Object class/Item mode *)
	class_head* = 0; class_module* = 1; class_var* = 2; class_ref* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; mode_reg* = 9; mode_regI* = 10; mode_cond* = 11;
	mode_xreg* = 12;
	
	classes_Variable* = {class_var, class_ref, mode_regI};
	classes_Value* = classes_Variable + {class_const, mode_reg, mode_cond};
	cls_Variable* = {class_var, class_ref, mode_regI};
	cls_HasValue* = cls_Variable + {class_const, mode_reg, mode_cond};
	modes_UseReg* = {mode_reg, mode_regI};

	(* Type form *)
	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_char* = 3;
	type_real* = 4; type_pointer* = 5; type_procedure* = 6;
	type_array* = 7; type_record* = 8; type_string* = 9;
	
	types_Simple* = {type_integer, type_boolean, type_set, type_real, type_char};
	types_Address* = {type_pointer, type_procedure};
	types_Scalar* = types_Simple + types_Address;
	types_Numberic* = {type_integer, type_real};
	types_Character* = {type_char, type_string};
	
	(* Compiler flag *)
	array_bound_check* = 0;
	integer_overflow_check* = 1;
	alignment_flag* = 2;

TYPE
	String* = ARRAY max_ident_len + 1 OF CHAR;
	LongString* = ARRAY max_str_len + 1 OF CHAR;

	Type* = POINTER TO RECORD
		hasExtension*, predefined* : BOOLEAN;
		form* : INTEGER;
		fields*, obj* : Object;
		base* : Type;
		size*, len*, alignment* : INTEGER;
		num_ptr*, ref* : INTEGER
	END;

	Object* = POINTER TO RECORD
		param*, readonly*, export* : BOOLEAN;
		name* : String;
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
		ptr_typ : Type;
		base_typ_name : String;
		next : UndefPtrList
	END;

VAR
	top_scope*, universe*, guard* : Object;
	undef_ptr_list* : UndefPtrList;
	cur_lev* : INTEGER;
	
	(* predefined type *)
	int_type*, bool_type*, set_type*, char_type*, byte_type*, real_type* : Type;
	longreal_type*, nilrecord_type*, nil_type* : Type;
	
	exportno : INTEGER;
	symfile : Sys.FileHandle;
	
	compiler_flag* : SET;
	
	(* Forward decl procedure *)
	_Export_type : PROCEDURE (typ : Type);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Str_len* (IN s : ARRAY OF CHAR) : INTEGER;
	VAR i : INTEGER;
BEGIN
	i := 0; WHILE (s[i] # 0X) & (i < LEN(s)) DO INC (i) END;
	RETURN i
END Str_len;

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
	
PROCEDURE Find_obj_in_scope(VAR obj : Object; IN name : String; scope : Object);
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

PROCEDURE New_obj* (VAR obj : Object; IN name : String; class : INTEGER);
BEGIN
	IF class = class_field THEN	Find_obj_in_scope (obj, name, top_scope)
	ELSE Find_obj (obj, name)
	END;
	
	IF obj = guard THEN
		obj := top_scope;
		WHILE obj.next # guard DO obj := obj.next END;
		NEW (obj.next); obj := obj.next;
		
		obj.readonly := FALSE;
		obj.param := FALSE;
		obj.name := name;
		obj.class := class;
		obj.lev := cur_lev;
		obj.next := guard;
		obj.val2 := 0
	ELSE
		obj := guard
	END
END New_obj;

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
BEGIN
	NEW (typ);
	typ.hasExtension := FALSE;
	typ.predefined := FALSE;
	typ.form := form;
	typ.num_ptr := 0
END New_typ;

PROCEDURE New_predefined_typ (VAR typ : Type; form, size : INTEGER);
BEGIN
	New_typ (typ, form);
	typ.predefined := TRUE;
	typ.size := size;
	typ.alignment := size
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
	obj.next := universe.next;
	universe.next := obj
END Enter;

PROCEDURE Enter2 (cl, n : INTEGER; name : String; typ : Type; n2 : INTEGER);
BEGIN
	Enter (cl, n, name, typ);
	universe.next.val2 := n2
END Enter2;
	
PROCEDURE Register_undefined_pointer_type* (typ : Type; base_typ_name : String);
	VAR undef : UndefPtrList;
BEGIN
	NEW (undef);
	undef.ptr_typ := typ;
	typ.base := int_type;
	undef.base_typ_name := base_typ_name;
	undef.next := undef_ptr_list;
	undef_ptr_list := undef
END Register_undefined_pointer_type;
	
PROCEDURE Check_undefined_pointer_list* (obj : Object);
	VAR p, prev : UndefPtrList;
BEGIN
	p := undef_ptr_list;
	REPEAT
		IF p.base_typ_name = obj.name THEN
			p.ptr_typ.base := obj.type;
			IF p = undef_ptr_list THEN undef_ptr_list := p.next
			ELSE prev.next := p.next
			END;
			p := NIL
		ELSE
			prev := p;
			p := p.next
		END
	UNTIL p = NIL
END Check_undefined_pointer_list;
	
PROCEDURE Cleanup_undefined_pointer_list*;
BEGIN
	REPEAT
		undef_ptr_list.ptr_typ.base := nilrecord_type;
		undef_ptr_list := undef_ptr_list.next
	UNTIL undef_ptr_list = NIL
END Cleanup_undefined_pointer_list;
	
PROCEDURE Adjust_alignment* (VAR offset : INTEGER; alignment : INTEGER);
	VAR i : INTEGER;
BEGIN
	IF alignment_flag IN compiler_flag THEN
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
	
PROCEDURE Is_extension_type* (ext, bas : Type) : BOOLEAN;
BEGIN
	IF ext.form = type_pointer THEN ext := ext.base END;
	IF bas.form = type_pointer THEN bas := bas.base END;
	RETURN (ext = bas) OR (ext.base # NIL) & Is_extension_type (ext.base, bas)
END Is_extension_type;
	
PROCEDURE Compatible_open_array* (typ1, typ2 : Type) : BOOLEAN;
BEGIN
	RETURN (typ1.base = typ2.base)
	OR
		(typ1.base.form = type_array) & (typ2.base.form = type_array)
		& (typ1.base.len < 0) & (typ2.base.len < 0)
		& Compatible_open_array (typ1, typ2)
END Compatible_open_array;
	
PROCEDURE Same_parlist (parlist1, parlist2 : Object) : BOOLEAN;
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
	
PROCEDURE Compatible_proc1 (proc1, proc2 : Object) : BOOLEAN;
BEGIN
	RETURN
		(proc1.type = proc2.type) & (proc1.parblksize = proc2.parblksize)
		& Same_parlist (proc1.dsc, proc2.dsc)
END Compatible_proc1;
	
PROCEDURE Compatible_proc2 (proc : Object; proctyp : Type) : BOOLEAN;
BEGIN
	RETURN
		(proc.type = proctyp.base) & (proc.parblksize = proctyp.len)
		& Same_parlist (proc.dsc, proctyp.fields)
END Compatible_proc2;
	
PROCEDURE Compatible_proc3 (proctyp1, proctyp2 : Type) : BOOLEAN;
BEGIN
	RETURN
		(proctyp1.base = proctyp2.base) & (proctyp1.len = proctyp2.len)
		& Same_parlist (proctyp1.fields, proctyp2.fields)
END Compatible_proc3;
	
(* Result codes for procedure Assignable: *)
(* 0: Good *)
(* 1: Invalid assignment source *)
(* 2: Source and destination are incompatible *)
(* 3: Source is not an extension of destination *)
(* 4: Assignment with non-global procedure *)
(* 5: Assignment with incompatible procedure *)
(* 6: Source string is oversized *)
PROCEDURE Assignable* (dst_type : Type; VAR src : Item) : INTEGER;
	VAR result : INTEGER;
BEGIN
	result := 0;
	IF src.mode = class_proc THEN
		IF dst_type.form # type_procedure THEN
			result := 2
		ELSIF src.obj.lev > 0 THEN
			result := 4
		ELSIF ~ Compatible_proc2 (src.obj, dst_type) THEN
			result := 5
		END
	ELSIF ~ (src.mode IN cls_HasValue) THEN
		result := 1
	ELSIF dst_type = src.type THEN
		(* Ok *)
	ELSIF dst_type = char_type THEN
		IF (src.type.form # type_string) OR (src.type.len # 2) THEN
			result := 2
		END
	ELSIF dst_type.form = type_integer THEN
		IF src.type.form # type_integer THEN
			result := 2
		END
	ELSIF dst_type.form IN types_Simple - {type_char, type_integer} THEN
		result := 2
	ELSIF dst_type.form IN {type_pointer, type_record} THEN
		IF (dst_type.form = type_pointer) & (src.type = nil_type) THEN
			(* Ok *)
		ELSIF src.type.form # dst_type.form THEN
			result := 2
		ELSIF ~ Is_extension_type (src.type, dst_type) THEN
			result := 3
		END
	ELSIF dst_type.form = type_array THEN
		IF (dst_type.base = char_type) & (src.type.form = type_string) THEN
			IF src.type.len > dst_type.len + 1 THEN
				result := 6
			END
		ELSE
			result := 2
		END
	ELSIF dst_type.form = type_procedure THEN
		IF src.type = nil_type THEN
			(* Ok *)
		ELSIF src.type.form # type_procedure THEN
			result := 2
		ELSIF ~ Compatible_proc3 (src.type, dst_type) THEN
			result := 5
		END
	END;
	RETURN result
END Assignable;

PROCEDURE Equality_applied* (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN
		(x.mode = class_proc) & (x.lev = 0)
	OR
		(x.mode IN cls_HasValue)
		&
			((x.type.form IN types_Scalar + {type_string})
			OR (x.type.form = type_array) & (x.type.base = char_type))
END Equality_applied;

PROCEDURE Comparison_applied* (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN 
		(x.mode IN cls_HasValue)
		& 
			((x.type.form IN {type_integer, type_real, type_char, type_string})
			OR (x.type.form = type_array) & (x.type.base = char_type))
END Comparison_applied;

PROCEDURE Type_test_applied* (VAR x : Item) : BOOLEAN;
BEGIN
	RETURN 
		(x.mode IN cls_HasValue)
		& 
			((x.type.form = type_pointer)
			OR (x.type.form = type_record) & x.param & ~ x.readonly)
END Type_test_applied;

PROCEDURE Comparable* (VAR x, y : Item) : BOOLEAN;
	VAR
		xform, yform : INTEGER;
BEGIN
	xform := x.type.form; yform := y.type.form;
	RETURN
		(xform = yform)
	OR
		(xform = type_char) & (yform = type_string) & (y.type.len = 2)
	OR
		(xform = type_string)
		& ((yform = type_array) OR (yform = type_char) & (x.type.len = 2))
	OR
		(xform = type_array) & (yform = type_string)
END Comparable;

PROCEDURE Equalable* (VAR x, y : Item) : BOOLEAN;
	VAR
		xform, yform : INTEGER;
BEGIN
	IF x.mode = class_proc THEN xform := -1 ELSE xform := x.type.form END;
	IF y.mode = class_proc THEN yform := -1 ELSE yform := y.type.form END;
	RETURN
		(xform = -1)
		&
			((yform = -1) & Compatible_proc1 (x.obj, y.obj)
			OR (y.type = nil_type)
			OR (yform = type_procedure) & Compatible_proc2 (x.obj, y.type))
	OR
		(yform = -1)
		&
			((x.type = nil_type)
			OR (xform = type_procedure) & Compatible_proc2 (y.obj, x.type))
	OR
		(x.type = y.type)
	OR
		(xform IN types_Numberic + types_Character + {type_array})
		& (yform IN types_Numberic + types_Character + {type_array})
		& Comparable (x, y)
	OR
		(x.type = nil_type) & (yform IN types_Address)
	OR
		(y.type = nil_type) & (xform IN types_Address)
	OR
		(xform = type_pointer) & (yform = type_pointer)
		& (Is_extension_type (x.type, y.type) OR Is_extension_type (y.type, x.type))
	OR
		(xform = type_procedure) & (yform = type_procedure)
		& Compatible_proc3 (x.type, y.type)
END Equalable;
	
PROCEDURE Is_matching_array* (typ1, typ2 : Type) : BOOLEAN;
BEGIN
	RETURN (typ1.base = typ2.base)
	OR
		(typ1.base.form = type_array) & (typ2.base.form = type_array)
		& Is_matching_array (typ1.base, typ2.base)
END Is_matching_array;
	
(* Result codes for procedure Check_parameter: *)
(* 0: Normal parameter *)
(* 1: Open array *)
(* 2: Reference parameter *)
(* 3: Record variable parameter *)
(* 9: String parameter *)
(* 4: Formal parameter is variable but actual is read-only *)
(* 5: Formal parameter is variable but actual is not *)
(* 6: Formal type and actual type are incompatible *)
(* 7: Invalid parameter *)
(* 8: Actual type is not an extension of formal type *)
PROCEDURE Check_parameter* (formal : Object; VAR actual : Item) : INTEGER;
	VAR
		result : INTEGER;
BEGIN
	IF actual.readonly & (formal.class = class_ref) & ~ formal.readonly THEN
		result := 4
	ELSIF (formal.type.form = type_array) & (formal.type.len < 0) THEN
		(* Open array formal parameter *)
		IF ~ (actual.mode IN cls_HasValue) THEN
			result := 7
		ELSIF (actual.type.form = type_array)
		& Is_matching_array (formal.type, actual.type) THEN
			result := 1
		ELSIF actual.type.form = type_string THEN
			IF formal.type.base = char_type THEN result := 1
			ELSE result := 6
			END
		ELSE
			result := 6
		END
	ELSIF (formal.class = class_ref) & ~ formal.readonly THEN
		(* Variable parameter *)
		IF ~ (actual.mode IN cls_Variable) THEN
			result := 5
		ELSIF formal.type = actual.type THEN
			result := 2
		ELSIF formal.type.form = type_record THEN
			IF actual.type.form # type_record THEN
				result := 6
			ELSIF Is_extension_type (actual.type, formal.type) THEN
				result := 2
			ELSE
				result := 8
			END
		ELSE
			result := 6
		END
	ELSE
		(* Value parameter *)
		CASE Assignable (formal.type, actual) OF
			1: result := 7 |
			3: result := 8 |
			0:
			IF formal.class = class_var THEN result := 0
			ELSIF actual.type.form = type_string THEN result := 9
			ELSE result := 2
			END
		ELSE
			result := 6
		END
	END;
	RETURN result
END Check_parameter;

(*
	
PROCEDURE Detect_type (typ : Type);
	BEGIN
	IF typ = NIL THEN Sys.Write_2bytes (symfile, 0)
	ELSIF typ.exportno > 0 THEN Sys.Write_2bytes (symfile, typ.exportno)
	ELSE _Export_type (typ) END
	END Detect_type;
	
PROCEDURE Export_type (typ : Type);
	VAR
		field : Object;
	BEGIN
	INC (exportno); typ.exportno := exportno;
	Sys.Write_byte (symfile, typ.form);
	
	IF typ.form = type_record THEN
		Detect_type (typ.base);
		Sys.Write_byte (symfile, typ.len);
		Sys.Write_8bytes (symfile, typ.size);
		Sys.Write_8bytes (symfile, typ.num_ptr);
		
		field := typ.fields;
		WHILE field # guard DO
			IF (flag_export IN field.flag) OR (field.type.num_ptr > 0) THEN
				Sys.Write_byte (symfile, class_field);
				IF ~(flag_export IN field.flag) THEN Sys.Write_string (symfile, '1')
				ELSE Sys.Write_string (symfile, field.name.content) END;
				Sys.Write_8bytes (symfile, field.val);
				Detect_type (field.type)
				END;
			field := field.next
			END;
	ELSIF typ.form = type_array THEN
		Sys.Write_8bytes (symfile, typ.len);
		Detect_type (typ.base)
	ELSIF typ.form = type_pointer THEN
		Detect_type (typ.base)
		END
	END Export_type;
	
PROCEDURE Export_var (var : Object);
	BEGIN
	Sys.Write_byte (symfile, var.class);
	Sys.Write_string (symfile, var.name.content);
	Detect_type (var.type)
	END Export_var;
	
PROCEDURE Export_proc (proc : Object);
	VAR
		par : Object;
	BEGIN
	Sys.Write_byte (symfile, class_proc);
	Sys.Write_string (symfile, proc.name.content);
	Detect_type (proc.type);
	Sys.Write_8bytes (symfile, proc.parblksize);
	par := proc.dsc;
	WHILE flag_param IN par.flag DO Export_var (par); par := par.next END
	END Export_proc;
	
PROCEDURE Export_const (const : Object);
	BEGIN
	Sys.Write_byte (symfile, class_const);
	Sys.Write_string (symfile, const.name.content);
	Sys.Write_8bytes (symfile, const.val);
	Detect_type (const.type)
	END Export_const;
	
PROCEDURE Write_symbols_file;
	VAR
		obj : Object;
	BEGIN
	obj := universe.next;
	WHILE obj # guard DO
		IF flag_export IN obj.flag THEN
			IF obj.class = class_proc THEN
				Export_proc (obj)
			ELSIF obj.class = class_type THEN
				Sys.Write_byte (symfile, class_type);
				Sys.Write_string (symfile, obj.name.content);
				Detect_type (obj.type)
			ELSIF obj.class = class_const THEN
				Export_const (obj)
			ELSIF obj.class = class_var THEN
				Export_var (obj)
				END
			END;
		obj := obj.next
		END
	END Write_symbols_file;
*)
	
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
	Enter (class_type, 0, 'REAL', real_type);
	Enter (class_type, 0, 'LONGREAL', longreal_type);
	
	(*
	Enter (class_sproc, 0, 'INC', NIL);
	Enter (class_sproc, 1, 'DEC', NIL);
	Enter (class_sproc, 2, 'INCL', NIL);
	Enter (class_sproc, 3, 'EXCL', NIL);
	Enter (class_sproc, 4, 'NEW', NIL);
	Enter (class_sproc, 5, 'ASSERT', NIL);
	Enter (class_sproc, 6, 'PACK', NIL);
	Enter (class_sproc, 7, 'UNPK', NIL);
	Enter (class_sproc, 8, 'DISPOSE', NIL);
	*)
	
	Enter (class_sproc, 100, 'GET', NIL);
	Enter (class_sproc, 101, 'PUT', NIL);
	Enter (class_sproc, 102, 'COPY', NIL);
	Enter2 (class_sproc, 103, 'LoadLibrary', NIL, -48);
	Enter2 (class_sproc, 104, 'GetProcAddress', NIL, -40);
	
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
	
	Enter (class_sproc, 300, 'ADR', int_type);
	Enter (class_sproc, 301, 'SIZE', int_type);
	Enter (class_sproc, 302, 'BIT', bool_type);
	Enter (class_sproc, 303, 'VAL', int_type);
	
	exportno := 0;	
	compiler_flag := {integer_overflow_check, array_bound_check, alignment_flag}
END Init;
	
BEGIN
	(* _Export_type := Export_type; *)

	NEW (guard); guard.class := class_head;

	New_predefined_typ (int_type, type_integer, Word_size);
	New_predefined_typ (bool_type, type_boolean, 1);
	New_predefined_typ (set_type, type_set, Word_size);
	New_predefined_typ (byte_type, type_integer, 1);
	New_predefined_typ (char_type, type_char, 2);
	New_predefined_typ (nil_type, type_pointer, Word_size);
	New_predefined_typ (real_type, type_real, 4);
	New_predefined_typ (longreal_type, type_real, 8)
END Base.
