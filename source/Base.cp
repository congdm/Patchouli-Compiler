MODULE Base;

IMPORT
	System := "[mscorlib]System",
	IO := "[mscorlib]System.IO",
	Console;

CONST
	success* = TRUE; failed* = FALSE;

	Word_size* = 8;
	MAX_INT* = 2147483647;
	MIN_INT* = -MAX_INT - 1;
	max_str_len* = 255;

	sym_null* = 0;
	sym_times* = 1; sym_slash* = 2; sym_div* = 3; sym_mod* = 4;
	sym_and* = 5; sym_plus* = 6; sym_minus* = 7; sym_or* = 8;
	sym_equal* = 9; sym_not_equal* = 10; sym_less* = 11;
	sym_greater_equal* = 12; sym_less_equal* = 13; sym_greater* = 14;
	sym_in* = 15; sym_is* = 16; sym_arrow* = 17;
	sym_period* = 18; sym_comma* = 19; sym_colon* = 20; sym_upto* = 21;
	sym_rparen* = 22; sym_rbrak* = 23; sym_rbrace* = 24;
	sym_of* = 25; sym_then* = 26; sym_do* = 27; sym_to* = 28;
	sym_lparen* = 29; sym_lbrak* = 30; sym_lbrace* = 31;
	sym_not* = 32; sym_becomes* = 33;
	sym_number* = 34; sym_nil* = 35; sym_string* = 36; sym_ident* = 37;
	sym_semicolon* = 38; sym_end* = 40; sym_else* = 41;
	sym_elsif* = 42; sym_until* = 43;
	sym_if* = 44; sym_while* = 46; sym_repeat* = 47;
	sym_array* = 54; sym_record* = 55; sym_pointer* = 56;
	sym_const* = 57; sym_type* = 58; sym_var* = 59; sym_procedure* = 60;
	sym_begin* = 61; sym_return* = 63; 
	sym_module* = 64;
	sym_eof* = 65;

	class_head* = 0; class_module* = 1; class_var* = 2; class_ref* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; mode_reg* = 9; mode_regI* = 10; mode_cond* = 11;

	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_char* = 3;
	type_pointer* = 4; type_procedure* = 5;
	type_array* = 6; type_record* = 7; type_string* = 8;

	flag_param* = 0; flag_export* = 1; flag_import* = 2; flag_used* = 3;
	flag_readonly* = 4; flag_instack* = 5; flag_predefined* = 6;
	
	array_bound_check* = 0;
	integer_overflow_check* = 1;

TYPE
	String* = POINTER TO StringDesc;
	StringDesc* = RECORD
		content* : ARRAY max_str_len + 1 OF CHAR;
		len* : INTEGER;
		END;
		
	FileHandle* = RECORD
		f : IO.FileStream;
		r : IO.StreamReader;
		w : IO.StreamWriter;
		END;

	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjectDesc;

	TypeDesc* = RECORD
		flag* : SET;
		form* : INTEGER;
		fields*, obj* : Object;
		base* : Type;
		size*, len*, tag* : INTEGER
		END;

	ObjectDesc* = RECORD
		flag* : SET;
		class*, lev* : INTEGER;
		name*, actual_name* : String;
		type* : Type;
		next*, dsc*, scope* : Object;
		val*, parblksize* : INTEGER
		END;

	Item* = RECORD
		flag* : SET;
		mode*, lev* : INTEGER;
		type* : Type;
		a*, b*, c*, d*, e*, r* : INTEGER;
		proc* : Object
		END;

VAR
	top_scope*, universe*, guard* : Object;
	cur_lev* : INTEGER;
	
	(* predefined type *)
	int_type*, bool_type*, set_type*, char_type*, byte_type* : Type;
	nilrecord_type*, nil_type* : Type;
	
	compiler_flag* : SET;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Show_error* (msg : ARRAY OF CHAR);
	BEGIN
	Console.WriteString ('COMPILER ERROR: ');
	Console.WriteString (msg);
	Console.WriteLn;
	END Show_error;

PROCEDURE Open* (VAR file : FileHandle; filename : ARRAY OF CHAR);
	BEGIN
	IF IO.File.Exists (MKSTR (filename)) THEN
		file.f := IO.File.OpenRead (MKSTR (filename));
		file.r := IO.StreamReader.init (file.f);
	ELSE
		Show_error ('File not existed!');
		END;
	END Open;
	
PROCEDURE Rewrite* (VAR file : FileHandle; filename : ARRAY OF CHAR);
	BEGIN
	file.f := IO.File.Create (MKSTR (filename));
	file.w := IO.StreamWriter.init (file.f)
	END Rewrite;

PROCEDURE Close* (VAR file : FileHandle);
	BEGIN
	IF file.f # NIL THEN
		IF file.w # NIL THEN file.w.Flush; END;
		file.f.Close;
		file.f := NIL;
		file.w := NIL;
		file.r := NIL
		END
	END Close;

PROCEDURE Read_char* (VAR file : FileHandle; VAR c : CHAR) : BOOLEAN;
	VAR
		i : INTEGER;
	BEGIN
	i := file.r.Read ();
	IF i = -1 THEN RETURN failed; END;
	c := System.Convert.ToChar (i);
	RETURN success;
	END Read_char;
	
PROCEDURE Write_string* (VAR file : FileHandle; str : ARRAY OF CHAR);
	BEGIN
	file.w.Write (MKSTR (str))
	END Write_string;
	
PROCEDURE Write_newline* (VAR file : FileHandle);
	BEGIN
	file.w.WriteLine
	END Write_newline;
	
PROCEDURE Int_to_string* (x : INTEGER; VAR str : ARRAY OF CHAR);
	VAR
		negative : BOOLEAN;
		s : ARRAY 20 OF CHAR;
		i, j : INTEGER;
	BEGIN
	IF x = MIN_INT THEN
		str := '-2147483648'
	ELSE
		IF x < 0 THEN
			negative := TRUE;
			x := -x
		ELSE
			negative := FALSE
			END;
		i := 0;
		REPEAT
			s [i] := CHR (x MOD 10 + ORD ('0'));
			INC (i);
			x := x DIV 10
			UNTIL x = 0;
		IF negative THEN
			str [0] := '-';
			FOR j := 0 TO i - 1 DO
				str [j + 1] := s [i - 1 - j]
				END;
			str [i + 1] := 0X
		ELSE
			FOR j := 0 TO i - 1 DO
				str [j] := s [i - 1 - j]
				END;
			str [i] := 0X
			END
		END
	END Int_to_string;
	
PROCEDURE Write_number* (VAR file : FileHandle; x : INTEGER);
	VAR
		s : ARRAY 22 OF CHAR;
	BEGIN
	Int_to_string (x, s);
	file.w.Write (MKSTR (s))
	END Write_number;
	
PROCEDURE Write_char* (VAR file : FileHandle; ch : CHAR);
	BEGIN
	file.w.Write (ch)
	END Write_char;
	
PROCEDURE Write_number_to_console* (x : INTEGER);
	VAR
		a : ARRAY 21 OF CHAR;
	BEGIN
	Int_to_string (x, a);
	Console.WriteString (a)
	END Write_number_to_console;
	
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
	FOR i := 0 TO s.len DO
		s.content [i] := const_str [i]
		END;
	RETURN s
	END Make_string;
	
PROCEDURE Is_safe_addition* (x, y : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	result := TRUE;
	IF (x >= 0) & (y >= 0) THEN
		IF y > MAX_INT - x THEN
			result := FALSE
			END;
	ELSIF (x < 0) & (y < 0) THEN
		IF y < MIN_INT - x THEN
			result := FALSE
			END
		END;
	RETURN result
	END Is_safe_addition;
	
PROCEDURE Is_safe_subtraction* (x, y : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	result := TRUE;
	IF (x >= 0) & (y < 0) THEN
		IF x > MAX_INT + y THEN
			result := FALSE
			END;
	ELSIF (x < 0) & (y >= 0) THEN
		IF x < MIN_INT + y THEN
			result := FALSE
			END
		END;
	RETURN result
	END Is_safe_subtraction;
	
PROCEDURE Is_safe_multiplication* (x, y : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
		q, r : INTEGER;
	BEGIN
	result := TRUE;
	IF (x < 0) & (y >= 0) THEN
		q := x;
		x := y;
		y := q
	ELSIF (x < 0) & (y < 0) THEN
		IF (x = MIN_INT) OR (y = MIN_INT) THEN
			result := FALSE
		ELSE
			x := -x;
			y := -y
			END
		END;
	IF x > 0 THEN
		IF y > 0 THEN
			IF x > MAX_INT / y THEN
				result := FALSE
				END
		ELSIF y < 0 THEN
			q := MIN_INT DIV y;
			r := MIN_INT MOD y;
			IF r = 0 THEN
				IF x > q THEN
					result := FALSE
					END
			ELSE
				IF x >= q THEN
					result := FALSE
					END
				END
			END
		END;
	RETURN result
	END Is_safe_multiplication;
	
PROCEDURE Integer_binary_logarithm* (a : INTEGER) : INTEGER;
	VAR
		e : INTEGER;
	BEGIN
	IF a <= 0 THEN
		e := -1
	ELSE
		e := 0;
		WHILE a > 1 DO
			IF a MOD 2 = 0 THEN
				INC (e);
				a := a DIV 2
			ELSE
				e := -1;
				a := 1
				END
			END
		END;
	RETURN e
	END Integer_binary_logarithm;
	
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

PROCEDURE New_obj* (VAR obj : Object; name : String; class : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF class = class_field THEN	Find_obj_in_scope (obj, name, top_scope)
	ELSE Find_obj (obj, name) END;
	IF obj # guard THEN
		result := failed
	ELSE
		obj := top_scope;
		WHILE obj.next # guard DO
			obj := obj.next
			END;
		NEW (obj.next);
		obj := obj.next;
		obj.name := name;
		obj.class := class;
		obj.next := guard;
		obj.flag := {};
		obj.lev := cur_lev;
		obj.scope := top_scope;
		result := success
		END;
	RETURN result
	END New_obj;

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
	BEGIN
	NEW (typ);
	typ.flag := {};
	typ.form := form
	END New_typ;
	
PROCEDURE New_predefined_typ (VAR typ : Type; form, size : INTEGER);
	BEGIN
	NEW (typ);
	typ.flag := {flag_predefined};
	typ.form := form;
	typ.size := size
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

PROCEDURE Is_scalar_type* (typ : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF typ.form IN {type_integer, type_boolean, type_set, type_char,
	type_pointer, type_procedure} THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Is_scalar_type;
	
PROCEDURE Is_extension_type* (ext, bas : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF (ext.form = type_pointer) & (bas.form = type_pointer) THEN
		ext := ext.base;
		bas := bas.base
		END;
	WHILE (ext # bas) & (ext # NIL) DO
		ext := ext.base
		END;
	IF ext = NIL THEN
		result := FALSE
	ELSE
		result := TRUE
		END;
	RETURN result
	END Is_extension_type;
	
PROCEDURE Is_variable* (VAR x : Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF x.mode IN {mode_regI, class_ref, class_var} THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Is_variable;
	
PROCEDURE Has_value* (VAR x : Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF Is_variable (x)
	OR (x.mode IN {class_const, mode_reg, mode_cond, mode_regI}) THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Has_value;
	
PROCEDURE Is_compatible_open_array* (typ1, typ2 : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF typ1.base = typ2.base THEN
		result := TRUE
	ELSIF (typ1.base.form = type_array) & (typ2.base.form = type_array)
	& (typ1.base.len = -1) & (typ2.base.len = -1) THEN
		result := Is_compatible_open_array (typ1, typ2)
	ELSE
		result := FALSE
		END;
	RETURN result
	END Is_compatible_open_array;
	
PROCEDURE Is_compatible_proc*
(par_list1, par_list2 : Object; result_typ1, result_typ2 : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
		typ1, typ2 : Type;
	BEGIN
	IF result_typ1 # result_typ2 THEN
		result := FALSE
	ELSE
		result := TRUE;
		WHILE (flag_param IN par_list1.flag) & result DO
			typ1 := par_list1.type;
			typ2 := par_list2.type;
			IF ~ (flag_param IN par_list2.flag) 
			OR (par_list1.class # par_list2.class) 
			OR (flag_readonly IN par_list1.flag / par_list2.flag) THEN
				result := FALSE
			ELSIF (typ1.form = type_array) & (typ2.form = type_array)
			& (typ1.len = -1) & (typ1.len = -1)
			& ~ Is_compatible_open_array (typ1, typ2) THEN
				result := FALSE
			ELSIF typ1 # typ2 THEN
				result := FALSE
			ELSE
				par_list1 := par_list1.next;
				par_list2 := par_list2.next
				END
			END;
		IF flag_param IN par_list2.flag THEN
			result := FALSE
			END
		END;
	RETURN result
	END Is_compatible_proc;

(* Result codes for procedure Assignable: *)
(* 0: Good *)
(* 1: Assignment with incompatible procedure *)
(* 2: Invalid assignment source *)
(* 3: Source and destination are incompatible *)
(* 4: Source is not an extension of destination *)
(* 5: Assignment with non-global procedure *)
(* 6: Source string is oversized *)
PROCEDURE Assignable* (dst_type : Type; VAR src : Item) : INTEGER;
	VAR
		dst_form, result : INTEGER;
	BEGIN
	result := 0;
	dst_form := dst_type.form;
	IF src.mode = class_proc THEN
		IF dst_form # type_procedure THEN
			result := 3
		ELSIF src.lev > 0 THEN
			result := 5
		ELSIF ~ Is_compatible_proc
		(dst_type.fields, src.proc.dsc, dst_type.base, src.proc.type) THEN
			result := 1
			END
	ELSIF ~ Has_value (src) THEN
		result := 2
	ELSIF src.type = dst_type THEN
		(* Ok, no problem *)
	ELSIF src.type.form = type_string THEN
		IF dst_type = char_type THEN
			IF src.type.len > 2 THEN
				result := 6
				END
		ELSIF (dst_type.form = type_array) & (dst_type.base = char_type) THEN
			IF src.type.len > dst_type.len + 1 THEN
				result := 6
				END
		ELSE
			result := 3
			END
	ELSIF src.type = nil_type THEN
		IF ~ (dst_form IN {type_pointer, type_procedure}) THEN
			result := 3
			END
	ELSIF dst_form # src.type.form THEN
		result := 3
	ELSIF dst_form IN {type_pointer, type_record} THEN
		IF ~ Is_extension_type (src.type, dst_type) THEN
			result := 4
			END
	ELSIF dst_form = type_procedure THEN
		IF ~ Is_compatible_proc
		(dst_type.fields, src.type.fields, dst_type.base, src.type.base) THEN
			result := 1
			END
	ELSIF (dst_type = int_type) & (src.type = byte_type)
	OR (dst_type = byte_type) & (src.type = int_type) THEN
		(* Ok, no problem *)
	ELSE
		result := 3
		END;
	RETURN result
	END Assignable;

PROCEDURE Have_ordering_relations* (VAR x, y : Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	result := TRUE;
	IF ~ Has_value (x) OR ~ Has_value (y) THEN
		result := FALSE
	ELSIF x.type.form # y.type.form THEN
		IF x.type.form = type_string THEN
			IF (y.type.form # type_array) OR (y.type.base # char_type) THEN
				result := FALSE
				END
		ELSIF (x.type.form = type_array) & (x.type.base = char_type) THEN
			IF y.type.form # type_string THEN
				result := FALSE
				END
		ELSE
			result := FALSE
			END
	ELSIF (x.type.form = type_array) & (x.type.base = char_type) THEN
		IF y.type.base # char_type THEN
			result := FALSE
			END
	ELSIF ~ (x.type.form IN {type_integer, type_char}) THEN
		result := FALSE
		END;
	RETURN result
	END Have_ordering_relations;

(* Result codes for procedure Comparable: *)
(* 0: Good *)
(* 1: Invalid comparison *)
(* 2: Comparison with non-global procedure *)
(* 3: Comparison with incompatible procedure *)
(* 4: Comparison with unrelated pointer type *)
PROCEDURE Comparable* (VAR x, y : Item) : INTEGER;
	VAR
		result : INTEGER;
	BEGIN
	result := 0;
	IF Have_ordering_relations (x, y) THEN
		(* Ok, no problem *)
	ELSIF x.mode = class_proc THEN
		IF x.lev > 0 THEN
			result := 2
		ELSIF y.mode = class_proc THEN
			IF y.lev > 0 THEN
				result := 2
			ELSIF ~ Is_compatible_proc
			(x.proc.dsc, y.proc.dsc, x.proc.type, y.proc.type) THEN
				result := 3
				END
		ELSIF ~ Has_value (y) OR (y.type.form # type_procedure) THEN
			result := 1
		ELSIF ~ Is_compatible_proc
		(x.proc.dsc, y.type.fields, x.proc.type, y.type.base) THEN
			result := 3
			END
	ELSIF y.mode = class_proc THEN
		IF y.lev > 0 THEN
			result := 2
		ELSIF ~ Has_value (x) OR (x.type.form # type_procedure) THEN
			result := 1
		ELSIF ~ Is_compatible_proc
		(x.type.fields, y.proc.dsc, x.type.base, y.proc.type) THEN
			result := 3
			END
	ELSIF ~ Has_value (x) OR ~ Has_value (y) THEN
		result := 1
	ELSIF x.type = y.type THEN
		IF ~ (x.type.form IN
		{type_set, type_boolean, type_pointer, type_procedure}) THEN
			result := 1
			END
	ELSIF x.type = nil_type THEN
		IF ~ (y.type.form IN {type_pointer, type_procedure}) THEN
			result := 1
			END
	ELSIF y.type = nil_type THEN
		IF ~ (x.type.form IN {type_pointer, type_procedure}) THEN
			result := 1
			END
	ELSIF x.type.form # y.type.form THEN
		result := 1
	ELSIF x.type.form = type_pointer THEN
		IF ~ Is_extension_type (x.type, y.type)
		& ~ Is_extension_type (y.type, x.type) THEN
			result := 4
			END
	ELSIF x.type.form = type_procedure THEN
		IF ~ Is_compatible_proc
		(x.type.fields, y.type.fields, x.type.base, x.type.base) THEN
			result := 3
			END
	ELSE
		result := 1
		END;
	RETURN result
	END Comparable;
	
PROCEDURE Is_matching_array* (typ1, typ2 : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF typ1.base = typ2.base THEN
		result := TRUE
	ELSIF (typ1.base.form # type_array)
	OR (typ2.base.form # type_array) THEN
		result := FALSE
	ELSE
		result := Is_matching_array (typ1.base, typ2.base)
		END;
	RETURN result
	END Is_matching_array;
	
PROCEDURE Is_open_array* (typ : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF (typ.form = type_array) & (typ.len < 0) THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Is_open_array;
	
PROCEDURE Is_variable_parameter* (par : Object) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF (par.class = class_ref) & ~ (flag_readonly IN par.flag) THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Is_variable_parameter;
	
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
		is_var_param : BOOLEAN;
	BEGIN
	is_var_param := Is_variable_parameter (formal);	
	IF (flag_readonly IN actual.flag) & is_var_param THEN
		result := 4
	ELSIF Is_open_array (formal.type) THEN
		result := 1;
		IF ~ Has_value (actual) THEN
			result := 7
		ELSIF actual.type.form = type_string THEN
			IF formal.type.base # char_type THEN
				result := 6
				END
		ELSIF (actual.type.form # type_array)
		OR ~ Is_matching_array (formal.type, actual.type) THEN
			result := 6
			END
	ELSIF is_var_param THEN
		IF ~ Is_variable (actual) THEN
			result := 5
		ELSIF formal.type.form = type_record THEN
			IF actual.type.form # type_record THEN
				result := 6
			ELSIF ~ Is_extension_type (actual.type, formal.type) THEN
				result := 8
			ELSIF formal.type.base # NIL THEN
				result := 3
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
			2: result := 7 |
			4: result := 8 |
			0:
				IF formal.class = class_var THEN
					result := 0
				ELSIF actual.type.form = type_string THEN
					result := 9
				ELSE
					result := 2
					END
			ELSE result := 6
			END
		END;
	RETURN result
	END Check_parameter;
	
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
	
	Enter (class_sproc, 0, Make_string ('LoadLibrary'), NIL);
	Enter (class_sproc, 1, Make_string ('GetProcAddress'), NIL);
	
	compiler_flag := {integer_overflow_check, array_bound_check};
	END Init;
	
BEGIN
NEW (guard);
guard.class := class_head;

New_predefined_typ (int_type, type_integer, Word_size);
New_predefined_typ (bool_type, type_boolean, 1);
New_predefined_typ (set_type, type_set, Word_size);
New_predefined_typ (byte_type, type_integer, 1);
New_predefined_typ (char_type, type_char, 1)
END Base.
