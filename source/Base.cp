MODULE Base;

IMPORT
	Sys;

CONST
	success* = TRUE; failed* = FALSE;

	Word_size* = 8;
	MAX_INT* = 9223372036854775807;
	MIN_INT* = -MAX_INT - 1;
	MAX_CHAR* = 65535;
	max_str_len* = 255;
	type_extension_limit* = 7;
	max_number_record_types* = 1024;
	set_size_limit* = 64;

	sym_null* = 0;
	sym_times* = 1; sym_slash* = 2; sym_div* = 3; sym_mod* = 4;
	sym_and* = 5; sym_plus* = 6; sym_minus* = 7; sym_or* = 8;
	sym_equal* = 9; sym_not_equal* = 10; sym_less* = 11;
	sym_greater_equal* = 12; sym_greater* = 13; sym_less_equal* = 14;
	sym_in* = 15; sym_is* = 16; sym_arrow* = 17;
	sym_period* = 18; sym_comma* = 19; sym_colon* = 20; sym_upto* = 21;
	sym_rparen* = 22; sym_rbrak* = 23; sym_rbrace* = 24;
	sym_of* = 25; sym_then* = 26; sym_do* = 27; sym_to* = 28;
	sym_lparen* = 29; sym_lbrak* = 30; sym_lbrace* = 31;
	sym_not* = 32; sym_becomes* = 33;
	sym_number* = 34; sym_nil* = 35; sym_true* = 36; sym_false* = 37;
	sym_string* = 38; sym_ident* = 39; sym_by* = 40;
	sym_semicolon* = 50; sym_end* = 51; sym_else* = 52;
	sym_elsif* = 53; sym_until* = 54;
	sym_if* = 55; sym_while* = 56; sym_repeat* = 57; sym_for* = 58;
	sym_array* = 60; sym_record* = 61; sym_pointer* = 62;
	sym_const* = 63; sym_type* = 64; sym_var* = 65; sym_procedure* = 66;
	sym_begin* = 67; sym_return* = 68; 
	sym_module* = 69;
	sym_eof* = 70;

	(* Object class/Item mode *)
	class_head* = 0; class_module* = 1; class_var* = 2; class_ref* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; mode_reg* = 9; mode_regI* = 10; mode_cond* = 11;
	
	cls_Variable* = {class_var, class_ref, mode_regI};
	cls_HasValue* = cls_Variable + {class_const, mode_reg, mode_cond};
	modes_UseReg* = {mode_reg, mode_regI};

	(* Type form *)
	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_char* = 3;
	type_real* = 4; type_pointer* = 5; type_procedure* = 6;
	type_array* = 7; type_record* = 8; type_string* = 9;
	
	types_Scalar* = {type_integer, type_boolean, type_set, type_real,
					type_char, type_pointer, type_procedure};

	(* Object/Item/Type flag *)
	flag_param* = 0; flag_export* = 1; flag_import* = 2; flag_used* = 3;
	flag_readOnly* = 4; flag_predefined* = 5; flag_varParam* = 6;
	flag_hasExtension* = 7; flag_hasName* = 8; flag_typeDesc* = 9;
	
	(* Compiler flag *)
	array_bound_check* = 0;
	integer_overflow_check* = 1;
	alignment_flag* = 2;
	
	(* Classify *)
	unclassified* = 0; csf_Integer* = 1; csf_Set* = 2; csf_Boolean* = 3;
	csf_Char* = 4; csf_Array* = 5; csf_CharArray* = 6; csf_String* = 7;
	csf_Record* = 8; csf_Pointer* = 9; csf_Nil* = 10; csf_Procedure* = 11;
	csf_Type* = 12; csf_Real* = 13;
	
	csf_HasOrderRelation* = {csf_Integer, csf_Real, csf_Char,
	                         csf_CharArray, csf_String};
	csf_HasEqualRelation* = csf_HasOrderRelation
		+ {csf_Set, csf_Boolean, csf_Pointer, csf_Procedure, csf_Nil};

TYPE
	String* = POINTER TO RECORD
		content* : ARRAY max_str_len + 1 OF CHAR;
		len* : INTEGER;
		END;

	Type* = POINTER TO RECORD
		flag* : SET;
		form* : INTEGER;
		fields*, obj* : Object;
		base* : Type;
		size*, len*, alignment* : INTEGER;
		num_ptr*, exportno* : INTEGER
		END;

	Object* = POINTER TO RECORD
		flag* : SET;
		class*, lev*, parblksize* : INTEGER;
		name* : String;
		type* : Type;
		next*, dsc*, scope* : Object;
		val* : LONGINT
		END;

	Item* = RECORD
		flag* : SET;
		mode*, lev* : INTEGER;
		type* : Type;
		b*, c*, d*, e*, r* : INTEGER;
		proc* : Object;
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
	
PROCEDURE Str_len* (s : ARRAY OF CHAR) : INTEGER;
	VAR
		i : INTEGER;
	BEGIN
	i := 0;
	WHILE (s [i] # 0X) & (i < LEN (s)) DO
		INC (i)
		END;
	RETURN i
	END Str_len;

PROCEDURE Str_equal* (s1 : String; s2 : ARRAY OF CHAR) : BOOLEAN;
	VAR
		i : INTEGER;
		result : BOOLEAN;
	BEGIN
	i := 0;
	result := TRUE;
	WHILE (s1.content [i] # 0X) & (s2 [i] # 0X) & result DO
		IF s1.content [i] # s2 [i] THEN result := FALSE; END;
		INC (i);
		END;
	IF result & ((s1.content [i] # 0X) OR (s2 [i] # 0X)) THEN
		result := FALSE;
		END;
	RETURN result
	END Str_equal;

PROCEDURE Str_equal2* (s1, s2 : String) : BOOLEAN;
	VAR
		i : INTEGER;
		result : BOOLEAN;
	BEGIN
	IF (s1 = NIL) OR (s2 = NIL) OR (s1.len # s2.len) THEN
		result := FALSE
	ELSE
		i := 0;
		result := TRUE;
		WHILE (i < s1.len) & result DO
			IF s1.content [i] # s2.content [i] THEN result := FALSE; END;
			INC (i);
			END;
		END;
	RETURN result
	END Str_equal2;

PROCEDURE Make_string* (const_str : ARRAY OF CHAR) : String;
	VAR
		i : INTEGER;
		s : String;
	BEGIN
	NEW (s);
	s.len := LEN (const_str) - 1;
	FOR i := 0 TO s.len DO s.content [i] := const_str [i] END;
	RETURN s
	END Make_string;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Integer_binary_logarithm* (a : LONGINT) : INTEGER;
	VAR
		e : INTEGER;
	BEGIN
	IF a <= 0 THEN
		e := -1
	ELSE
		e := 0;
		WHILE a > 1 DO
			IF a MOD 2 = 0 THEN INC (e); a := a DIV 2
			ELSE e := -1; a := 1 END
			END
		END;
	RETURN e
	END Integer_binary_logarithm;
	
PROCEDURE Size_of_const* (n : LONGINT) : INTEGER;
	VAR
		res : INTEGER;
	BEGIN
	IF (n >= 0) & (n < 256) THEN res := 1
	ELSIF (n >= 0) & (n < 65536) THEN res := 2
	ELSIF (n >= 0) & (n < 4294967296) THEN res := 4
	ELSE res := 8 END;
	RETURN res
	END Size_of_const;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Open_scope* (scope_name : String);
	VAR
		new_scope : Object;
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
	
PROCEDURE Find_obj_in_scope (VAR obj : Object; name : String; scope : Object);
	BEGIN
	guard.name := name;
	obj := scope.next;
	WHILE ~ Str_equal2 (obj.name, name) DO
		obj := obj.next
		END
	END Find_obj_in_scope;

PROCEDURE Find_obj* (VAR obj : Object; name : String);
	BEGIN
	IF top_scope = universe THEN
		Find_obj_in_scope (obj, name, universe)
	ELSIF Str_equal2 (top_scope.name, name) THEN
		Find_obj_in_scope (obj, name, top_scope.dsc)
	ELSE
		Find_obj_in_scope (obj, name, top_scope);
		IF obj = guard THEN
			Find_obj_in_scope (obj, name, universe)
			END
		END	
	END Find_obj;
	
PROCEDURE Find_field* (VAR obj : Object; name : String; rec_typ : Type);
	BEGIN
	guard.name := name;
	obj := rec_typ.fields;
	WHILE ~ Str_equal2 (obj.name, name) DO
		obj := obj.next
		END
	END Find_field;

PROCEDURE New_obj* (VAR obj : Object; name : String; class : INTEGER);
	VAR
		result : BOOLEAN;
	BEGIN
	IF class = class_field THEN	Find_obj_in_scope (obj, name, top_scope)
	ELSE Find_obj (obj, name) END;
	IF obj # guard THEN
		obj := guard
	ELSE
		obj := top_scope;
		WHILE obj.next # guard DO obj := obj.next END;
		NEW (obj.next);
		obj := obj.next;
		obj.name := name;
		obj.class := class;
		obj.next := guard;
		obj.flag := {};
		obj.lev := cur_lev;
		obj.scope := top_scope
		END
	END New_obj;

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
	BEGIN
	NEW (typ);
	typ.flag := {};
	typ.form := form;
	typ.num_ptr := 0
	END New_typ;

PROCEDURE New_predefined_typ (VAR typ : Type; form, size : INTEGER);
	BEGIN
	New_typ (typ, form);
	typ.flag := {flag_predefined};
	typ.size := size;
	typ.alignment := size
	END New_predefined_typ;
	
PROCEDURE Enter (cl, n : INTEGER; name : String; typ : Type);
	VAR
		obj : Object;
		str : String;
	BEGIN
	NEW (obj);
	obj.class := cl;
	obj.val := n;
	obj.name := name;
	obj.type := typ;
	obj.flag := {flag_predefined};
	obj.next := top_scope.next;
	top_scope.next := obj
	END Enter;
	
PROCEDURE Register_undefined_pointer_type* (typ : Type; base_typ_name : String);
	VAR
		undef : UndefPtrList;
	BEGIN
	NEW (undef);
	undef.ptr_typ := typ;
	typ.base := int_type;
	undef.base_typ_name := base_typ_name;
	undef.next := undef_ptr_list;
	undef_ptr_list := undef
	END Register_undefined_pointer_type;
	
PROCEDURE Check_undefined_pointer_list* (obj : Object);
	VAR
		p, q : UndefPtrList;
	BEGIN
	p := undef_ptr_list;
	REPEAT
		IF Str_equal2 (p.base_typ_name, obj.name) THEN
			p.ptr_typ.base := obj.type;
			IF p = undef_ptr_list THEN
				undef_ptr_list := p.next
			ELSE
				q.next := p.next
				END;
			p := NIL
		ELSE
			q := p;
			p := p.next
			END
		UNTIL p = NIL;
	END Check_undefined_pointer_list;
	
PROCEDURE Cleanup_undefined_pointer_list*;
	BEGIN
	REPEAT
		undef_ptr_list.ptr_typ.base := nilrecord_type;
		undef_ptr_list := undef_ptr_list.next
		UNTIL undef_ptr_list = NIL
	END Cleanup_undefined_pointer_list;
	
PROCEDURE Adjust_alignment* (VAR offset : INTEGER; alignment : INTEGER);
	BEGIN
	IF alignment_flag IN compiler_flag THEN
		IF offset MOD alignment # 0 THEN
			INC (offset, alignment - offset MOD alignment)
			END
		END
	END Adjust_alignment;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Is_extension_type* (ext, bas : Type) : BOOLEAN;
	BEGIN
	IF ext.form = type_pointer THEN ext := ext.base END;
	IF bas.form = type_pointer THEN bas := bas.base END;
	RETURN (ext = bas) OR (ext.base # NIL) & Is_extension_type (ext.base, bas)
	END Is_extension_type;
	
PROCEDURE Compatible_open_array* (typ1, typ2 : Type) : BOOLEAN;
	BEGIN
	RETURN (typ1.base = typ2.base) OR (typ1.base.form = type_array)
	& (typ2.base.form = type_array) & (typ1.base.len < 0) & (typ2.base.len < 0)
	& Compatible_open_array (typ1, typ2)
	END Compatible_open_array;
	
PROCEDURE Same_parlist (parlist1, parlist2 : Object) : BOOLEAN;
	VAR
		result : BOOLEAN;
		typ1, typ2 : Type;
	BEGIN
	result := TRUE;
	WHILE (flag_param IN parlist1.flag) & result DO
		result := FALSE;
		IF (flag_param IN parlist2.flag) & (parlist1.class = parlist2.class) 
		& ~ (flag_readOnly IN parlist1.flag / parlist2.flag) THEN
			typ1 := parlist1.type;
			typ2 := parlist2.type;
			IF (typ1 = typ2) OR (typ1.form = type_array) & (typ1.len < 0)
			& (typ2.form = type_array) & (typ1.len < 0)
			& Compatible_open_array (typ1, typ2) THEN
				result := TRUE;
				parlist1 := parlist1.next;
				parlist2 := parlist2.next
				END
			END
		END;
	RETURN result & ~ (flag_param IN parlist2.flag) 
	END Same_parlist;
	
PROCEDURE Compatible_proc1 (proc1, proc2 : Object) : BOOLEAN;
	BEGIN
	RETURN (proc1.type = proc2.type) & (proc1.parblksize = proc2.parblksize)
	& Same_parlist (proc1.dsc, proc2.dsc)
	END Compatible_proc1;
	
PROCEDURE Compatible_proc2 (proc : Object; proctyp : Type) : BOOLEAN;
	BEGIN
	RETURN (proc.type = proctyp.base) & (proc.parblksize = proctyp.len)
	& Same_parlist (proc.dsc, proctyp.fields)
	END Compatible_proc2;
	
PROCEDURE Compatible_proc3 (proctyp1, proctyp2 : Type) : BOOLEAN;
	BEGIN
	RETURN (proctyp1.base = proctyp2.base) & (proctyp1.len = proctyp2.len)
	& Same_parlist (proctyp1.fields, proctyp2.fields)
	END Compatible_proc3;
	
PROCEDURE Classify_type* (tp : Type) : INTEGER;
	VAR
		r : INTEGER;
	BEGIN
	IF tp = nil_type THEN
		r := csf_Nil
	ELSE
		CASE tp.form OF
			type_integer: r := csf_Integer |
			type_set: r := csf_Set |
			type_boolean: r := csf_Boolean |
			type_char: r := csf_Char |
			type_pointer: r := csf_Pointer |
			type_procedure: r := csf_Procedure |
			type_string: r := csf_String |
			type_record: r := csf_Record |
			type_array:
				IF tp.base = char_type THEN r := csf_CharArray
				ELSE r := csf_Array END
			END
		END;
	RETURN r
	END Classify_type;

PROCEDURE Classify_item* (VAR x : Item) : INTEGER;
	VAR
		r : INTEGER;
	BEGIN
	IF x.mode IN cls_HasValue THEN
		r := Classify_type (x.type)
	ELSIF x.mode = class_type THEN
		r := csf_Type
	ELSIF x.mode = class_proc THEN
		r := csf_Procedure
	ELSE
		r := unclassified
		END;
	RETURN r
	END Classify_item;

(* Result codes for procedure Assignable: *)
(* 0: Good *)
(* 1: Invalid assignment source *)
(* 2: Source and destination are incompatible *)
(* 3: Source is not an extension of destination *)
(* 4: Assignment with non-global procedure *)
(* 5: Assignment with incompatible procedure *)
(* 6: Source string is oversized *)
PROCEDURE Assignable* (dst_type : Type; VAR src : Item) : INTEGER;
	VAR
		dst_csf, src_csf, result : INTEGER;
	BEGIN
	result := 0;
	dst_csf := Classify_type (dst_type);
	src_csf := Classify_item (src);
	
	IF src_csf IN {csf_Integer, csf_Set, csf_Boolean, csf_Char} THEN
		IF src_csf # dst_csf THEN
			result := 2
			END
	ELSIF src_csf = csf_String THEN
		IF dst_csf = csf_Char THEN
			IF src.type.len # 2 THEN
				result := 2
				END
		ELSIF dst_csf = csf_CharArray THEN
			IF dst_type.len + 1 < src.type.len + 1 THEN
				result := 6
				END
		ELSE
			result := 2
			END
	ELSIF src_csf = csf_Nil THEN
		IF ~ (dst_csf IN {csf_Pointer, csf_Procedure}) THEN
			result := 2
			END	
	ELSIF src_csf IN {csf_Pointer, csf_Record} THEN
		IF src.type # dst_type THEN
			IF src_csf # dst_csf THEN
				result := 2
			ELSIF ~ Is_extension_type (src.type, dst_type) THEN
				result := 3
				END
			END
	ELSIF src_csf IN {csf_Array, csf_CharArray} THEN
		IF src.type # dst_type THEN
			result := 2
			END
	ELSIF src_csf = csf_Procedure THEN
		IF dst_csf # csf_Procedure THEN
			result := 2
		ELSIF src.mode = class_proc THEN
			IF src.lev > 0 THEN
				result := 4
			ELSIF ~ Compatible_proc2 (src.proc, dst_type) THEN
				result := 5
				END
		ELSE
			IF ~ Compatible_proc3 (src.type, dst_type) THEN
				result := 5
				END
			END
	ELSE
		result := 1
		END;
	RETURN result
	END Assignable;

(* Result codes for procedure Comparable: *)
(* 0: Good *)
(* 1: Not compatible *)	
PROCEDURE Comparable* (VAR x, y : Item) : INTEGER;
	VAR
		result, csfx, csfy : INTEGER;
	BEGIN
	result := 0;
	csfx := Classify_item (x);
	csfy := Classify_item (y);
	
	IF csfx = csf_Integer THEN
		IF csfy # csf_Integer THEN result := 1 END
	ELSIF csfx IN {csf_Char, csf_CharArray} THEN
		IF csfx = csfy THEN (* Do nothing *)
		ELSIF csfy = csf_String THEN
			IF (y.type.len # 2) & (csfx = csf_Char) THEN result := 1 END
		ELSE result := 1 END
	ELSIF csfx = csf_String THEN
		IF csfy IN {csf_String, csf_CharArray} THEN (* Do nothing *)
		ELSIF csfy = csf_Char THEN
			IF x.type.len # 2 THEN result := 1 END
		ELSE result := 1 END
	ELSE
		ASSERT (FALSE)
		END;
	RETURN result
	END Comparable;

(* Result codes for procedure Equalable: *)
(* 0: Good *)
(* 1: Not compatible *)
(* 2: Comparison with non-global procedure *)
(* 3: Comparison with incompatible procedure *)
(* 4: Comparison with unrelated pointer type *)
PROCEDURE Equalable* (VAR x, y : Item) : INTEGER;
	VAR
		result, csf1, csf2 : INTEGER;
		parlist1, parlist2 : Object;
		restp1, restp2 : Type;
	BEGIN
	result := 0;
	csf1 := Classify_item (x);
	csf2 := Classify_item (y);
	
	IF csf1 IN {csf_Integer, csf_Set, csf_Boolean} THEN
		IF csf2 # csf1 THEN result := 1 END
	ELSIF csf1 IN {csf_Char, csf_CharArray} THEN
		IF csf1 = csf2 THEN (* Do nothing *)
		ELSIF csf2 = csf_String THEN
			IF (y.type.len # 2) & (csf1 = csf_Char) THEN result := 1 END
		ELSE result := 1 END
	ELSIF csf1 = csf_Pointer THEN
		IF x.type # y.type THEN
			IF csf2 = csf_Nil THEN (* Do nothing *)
			ELSIF csf2 # csf_Pointer THEN result := 1
			ELSIF ~ Is_extension_type (x.type, y.type)
			& ~ Is_extension_type (y.type, x.type) THEN result := 4 END
			END
	ELSIF csf1 = csf_Procedure THEN
		IF csf2 = csf_Nil THEN
			IF (x.mode = class_proc) & (x.lev > 0) THEN result := 2 END
		ELSIF csf2 = csf_Procedure THEN
			IF (x.mode = class_proc) & (x.lev > 0)
			OR (y.mode = class_proc) & (y.lev > 0) THEN
				result := 2
			ELSE
				IF (x.mode = class_proc)
				& ((y.mode = class_proc) & ~ Compatible_proc1 (x.proc, y.proc)
				OR (y.mode # class_proc) & ~ Compatible_proc2 (x.proc, y.type))
				OR (x.mode # class_proc)
				& ((y.mode = class_proc) & ~ Compatible_proc2 (y.proc, x.type)
				OR (y.mode # class_proc) & ~ Compatible_proc3 (x.type, y.type))
				THEN result := 3 END
				END
		ELSE result := 1 END
	ELSIF csf1 = csf_String THEN
		IF csf2 IN {csf_String, csf_CharArray} THEN (* Do nothing *)
		ELSIF csf2 = csf_Char THEN
			IF x.type.len # 2 THEN result := 1 END
		ELSE result := 1 END
	ELSIF csf1 = csf_Nil THEN
		IF csf2 IN {csf_Pointer, csf_Nil} THEN (* Do nothing *)
		ELSIF csf2 = csf_Procedure THEN
			IF (y.mode = class_proc) & (y.lev > 0) THEN result := 2 END
		ELSE result := 1 END
	ELSE
		ASSERT (FALSE)
		END;
	RETURN result
	END Equalable;
	
PROCEDURE Is_matching_array* (typ1, typ2 : Type) : BOOLEAN;
	BEGIN
	RETURN (typ1.base = typ2.base) OR (typ1.base.form = type_array)
	& (typ2.base.form = type_array) & Is_matching_array (typ1.base, typ2.base)
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
	IF (flag_readOnly IN actual.flag) & (flag_varParam IN formal.flag) THEN
		result := 4
	ELSIF (formal.type.form = type_array) & (formal.type.len < 0) THEN
		(* Open array formal parameter *)
		result := 1;
		IF ~ (actual.mode IN cls_HasValue) THEN
			result := 7
		ELSIF actual.type.form = type_string THEN
			IF formal.type.base # char_type THEN
				result := 6
				END
		ELSIF (actual.type.form # type_array)
		OR ~ Is_matching_array (formal.type, actual.type) THEN
			result := 6
			END
	ELSIF flag_varParam IN formal.flag THEN
		(* Variable parameter *)
		IF ~ (actual.mode IN cls_Variable) THEN
			result := 5
		ELSIF formal.type.form = type_record THEN
			IF actual.type.form # type_record THEN
				result := 6
			ELSIF ~ Is_extension_type (actual.type, formal.type) THEN
				result := 8
			ELSE
				result := 2
				END
		ELSIF formal.type # actual.type THEN
			result := 6
		ELSE
			result := 2
			END
	ELSE (* Value parameter *)
		CASE Assignable (formal.type, actual) OF
			1: result := 7 |
			3: result := 8 |
			0:
				IF formal.class = class_var THEN result := 0
				ELSIF actual.type.form = type_string THEN result := 9
				ELSE result := 2 END
			ELSE result := 6
			END
		END;
	RETURN result
	END Check_parameter;
	
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
	
PROCEDURE Init* (modid : String);
	BEGIN
	NEW (universe);
	universe.class := class_head;
	universe.name := modid;
	universe.next := guard;
	
	top_scope := universe;
	
	Enter (class_type, 0, Make_string ('INTEGER'), int_type);
	Enter (class_type, 0, Make_string ('BOOLEAN'), bool_type);
	Enter (class_type, 0, Make_string ('SET'), set_type);
	Enter (class_type, 0, Make_string ('BYTE'), byte_type);
	Enter (class_type, 0, Make_string ('CHAR'), char_type);
	Enter (class_type, 0, Make_string ('REAL'), real_type);
	Enter (class_type, 0, Make_string ('LONGREAL'), longreal_type);
	
	Enter (class_sproc, 0, Make_string ('INC'), NIL);
	Enter (class_sproc, 1, Make_string ('DEC'), NIL);
	Enter (class_sproc, 2, Make_string ('INCL'), NIL);
	Enter (class_sproc, 3, Make_string ('EXCL'), NIL);
	Enter (class_sproc, 4, Make_string ('NEW'), NIL);
	Enter (class_sproc, 5, Make_string ('ASSERT'), NIL);
	Enter (class_sproc, 6, Make_string ('PACK'), NIL);
	Enter (class_sproc, 7, Make_string ('UNPK'), NIL);
	Enter (class_sproc, 8, Make_string ('DISPOSE'), NIL);
	
	Enter (class_sproc, 10, Make_string ('GET'), NIL);
	Enter (class_sproc, 11, Make_string ('PUT'), NIL);
	Enter (class_sproc, 12, Make_string ('COPY'), NIL);
	Enter (class_sproc, 13, Make_string ('LoadLibrary'), NIL);
	Enter (class_sproc, 14, Make_string ('GetProcAddress'), NIL);
	
	Enter (class_sproc, 20, Make_string ('ABS'), int_type);
	Enter (class_sproc, 21, Make_string ('ODD'), bool_type);
	Enter (class_sproc, 22, Make_string ('LEN'), int_type);
	Enter (class_sproc, 25, Make_string ('ORD'), int_type);
	Enter (class_sproc, 26, Make_string ('CHR'), char_type);
	
	Enter (class_sproc, 30, Make_string ('ADR'), int_type);
	Enter (class_sproc, 31, Make_string ('SIZE'), int_type);
	Enter (class_sproc, 33, Make_string ('VAL'), int_type);
	
	exportno := 0;
	
	compiler_flag := {integer_overflow_check, array_bound_check,
	                  alignment_flag};
	END Init;
	
BEGIN
_Export_type := Export_type;

NEW (guard);
guard.class := class_head;

New_predefined_typ (int_type, type_integer, Word_size);
New_predefined_typ (bool_type, type_boolean, 1);
New_predefined_typ (set_type, type_set, Word_size);
New_predefined_typ (byte_type, type_integer, 1);
New_predefined_typ (char_type, type_char, 2);
New_predefined_typ (nil_type, type_pointer, Word_size);
New_predefined_typ (real_type, type_real, 4);
New_predefined_typ (longreal_type, type_real, 8)
END Base.
