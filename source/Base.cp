MODULE Base;

IMPORT
	System := "[mscorlib]System",
	IO := "[mscorlib]System.IO",
	Console;

CONST
	success* = TRUE; failed* = FALSE;

	class_head* = 0; class_module* = 1; class_var* = 2; class_par* = 3;
	class_const* = 4; class_field* = 5; class_type* = 6; class_proc* = 7;
	class_sproc* = 8; class_MAX* = 9;

	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_pointer* = 3;
	type_procedure* = 4; type_array* = 5; type_record* = 6;

	flag_param* = 0; flag_export* = 1; flag_import* = 2; flag_used* = 3;
	flag_readonly* = 4; flag_instack* = 5; flag_predefined* = 6;

TYPE
	String* = ARRAY 256 OF CHAR;
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

END Base.
