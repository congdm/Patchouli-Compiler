MODULE Base;

IMPORT
	System := "[mscorlib]System",
	IO := "[mscorlib]System.IO",
	Console;

CONST
	success* = TRUE; failed* = FALSE;

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
		next*, dsc* : Object;
		val* : INTEGER;
		END;

	Item* = RECORD
		flag* : SET;
		mode*, lev* : INTEGER;
		type* : Type;
		a*, b*, c*, r* : INTEGER;
		END;

VAR
	top_scope*, universe*, guard* : Object;
	cur_lev : INTEGER;
	(* predefined type *)
	int_type*, bool_type*, set_type*, nilrecord_type*, char_type* : Type;

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
		file.w := NIL;
	ELSE
		Show_error ('File not existed!');
		END;
	END Open;

PROCEDURE Close* (VAR file : FileHandle);
	BEGIN
	IF file.w # NIL THEN file.w.Flush; END;
	file.f.Close;
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

PROCEDURE Find_obj* (VAR obj : Object; name : String; must_be_global : BOOLEAN);
	BEGIN
	guard.name := name;
	IF must_be_global THEN
		obj := universe.next;
		WHILE ~ Str_equal2 (obj.name, name) DO
			obj := obj.next
			END
	ELSE
		obj := top_scope.next;
		WHILE ~ Str_equal2 (obj.name, name) DO
			obj := obj.next
			END;
		IF (obj = Base.guard) & (top_scope # universe) THEN
			obj := universe.next;
			WHILE ~ Str_equal2 (obj.name, name) DO
				obj := obj.next
				END
			END
		END	
	END Find_obj;

PROCEDURE New_obj* (VAR obj : Object; name : String; class : INTEGER) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	guard.name := name;
	obj := top_scope.next;
	WHILE ~ Str_equal2 (name, obj.name) DO
		obj := obj.next
		END;
	IF obj = guard THEN
		NEW (obj);
		obj.name := name;
		obj.class := class;
		obj.next := guard;
		obj.flag := {};
		obj.lev := cur_lev;
		result := TRUE
	ELSE
		result := FALSE
		END;
	RETURN result
	END New_obj;

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
	BEGIN
	NEW (typ);
	typ.flag := {};
	typ.form := form
	END New_typ;

END Base.
