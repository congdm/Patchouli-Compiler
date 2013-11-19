MODULE Base;

IMPORT
	System := "[mscorlib]System",
	IO := "[mscorlib]System.IO",
	Console;

CONST
	success* = TRUE; failed* = FALSE;

	Word_size* = 8;
	MIN_INT = -2147483648;

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
	sym_number* = 34; sym_nil* = 35; sym_ident* = 37;
	sym_semicolon* = 38; sym_end* = 40; sym_else* = 41;
	sym_elsif* = 42; sym_until* = 43;
	sym_if* = 44; sym_while* = 46; sym_repeat* = 47;
	sym_array* = 54; sym_record* = 55; sym_pointer* = 56;
	sym_const* = 57; sym_type* = 58; sym_var* = 59; sym_procedure* = 60;
	sym_begin* = 61; sym_return* = 63; 
	sym_module* = 64;
	sym_eof* = 65;

	class_head* = 0; class_module* = 1; class_var* = 2; class_par* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; mode_reg* = 9; mode_regI* = 10; mode_cond* = 11;

	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_pointer* = 3;
	type_procedure* = 4; type_array* = 5; type_record* = 6;

	flag_param* = 0; flag_export* = 1; flag_import* = 2; flag_used* = 3;
	flag_readonly* = 4; flag_instack* = 5; flag_predefined* = 6;

TYPE
	String* = POINTER TO StringDesc;
	StringDesc* = RECORD
		content* : ARRAY 256 OF CHAR;
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
		size*, len* : INTEGER;
		END;

	ObjectDesc* = RECORD
		flag* : SET;
		class*, lev* : INTEGER;
		name*, actual_name* : String;
		type* : Type;
		next*, dsc*, scope* : Object;
		val*, parblksize* : INTEGER;
		END;

	Item* = RECORD
		flag* : SET;
		mode*, lev* : INTEGER;
		type* : Type;
		a*, b*, c*, r* : INTEGER;
		END;

VAR
	top_scope*, universe*, guard* : Object;
	cur_lev* : INTEGER;
	(* predefined type *)
	int_type*, bool_type*, set_type*, char_type* : Type;
	nilrecord_type*, nil_type* : Type;
	
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
		result := TRUE
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
		END;
	END Find_obj_in_scope;

PROCEDURE Find_obj* (VAR obj : Object; name : String; must_be_global : BOOLEAN);
	BEGIN
	IF must_be_global OR (top_scope = universe) THEN
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

PROCEDURE New_obj* (VAR obj : Object; name : String; class : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	Find_obj (obj, name, FALSE);
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

PROCEDURE Is_scalar_type* (typ : Type) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF (typ.form = type_integer) OR (typ.form = type_boolean)
	OR (typ.form = type_set) OR (typ.form = type_pointer)
	OR (typ.form = type_procedure) THEN
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
	
PROCEDURE Is_variable* (x : Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	IF x.mode IN {mode_regI, class_par, class_var} THEN
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END Is_variable;
	
PROCEDURE Has_value* (x : Item) : BOOLEAN;
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
	
PROCEDURE 

END Base.
