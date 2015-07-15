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
	class_string* = 255;
	
	classes_Variable* = {class_var, class_ref, mode_regI};
	classes_Value* = classes_Variable
		+ {class_const, mode_reg, mode_cond, class_proc, mode_xreg};
	cls_Variable* = classes_Variable; cls_HasValue* = classes_Value;

	(* Type form *)
	type_integer* = 0; type_boolean* = 1; type_set* = 2; type_char* = 3;
	type_real* = 4; type_pointer* = 5; type_procedure* = 6;
	type_array* = 7; type_record* = 8; type_string* = 9; type_nil* = 10;
	type_address* = 11;
	
	types_Simple* = {type_integer, type_boolean, type_set, type_real, type_char};
	types_Address* = {type_pointer, type_procedure, type_address};
	types_Pointer* = {type_pointer, type_address};
	types_Scalar* = types_Simple + types_Address;
	types_Numberic* = {type_integer, type_real};
	types_Character* = {type_char, type_string};
	types_HasExt* = {type_record, type_pointer};
	
	(* System procedures *)
	ExitProcess* = -56;
	LoadLibraryW* = -48;
	GetProcAddress* = -40;
	GetProcessHeap* = -32;
	HeapAlloc* = -24;
	HeapFree* = -16;

TYPE
	String* = ARRAY max_ident_len + 1 OF CHAR;
	LongString* = ARRAY max_str_len + 1 OF CHAR;
	
	Type* = POINTER TO RECORD
		ref*, mod*, tdAdr*, expno* : INTEGER;
		charVal*, strPos* : INTEGER;
		
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

VAR
	guard* : Object;
	
	(* Predefined Types *)
	int_type*, byte_type*, word_type*, dword_type* : Type;
	bool_type*, set_type*, char_type*, nil_type* : Type;
	real_type*, longreal_type* : Type;
	guardRecord_type*, guardPointer_type*, guardArray_type* : Type;
	byteArray_type* : Type; anyScalar_type* : Type;
	
	predefinedTypes* : ARRAY 32 OF Type;
	preTypeNo* : INTEGER;

	stringdata : ARRAY 65536 OF CHAR;
	strpos : INTEGER;
	
	CompilerFlag* : RECORD
		array_check*, overflow_check*, type_check*, nil_check* : BOOLEAN;
		alignment*, main*, console* : BOOLEAN
	END;
	
	
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

PROCEDURE New_typ* (VAR typ : Type; form : INTEGER);
BEGIN
	NEW (typ);
	typ.form := form;
	typ.mod := -1;
	typ.ref := -1;
	typ.num_ptr := 0
END New_typ;

PROCEDURE New_predefined_typ (VAR typ : Type; form, size : INTEGER);
BEGIN
	New_typ (typ, form);
	typ.mod := -2;
	typ.size := size;
	typ.alignment := size;
	preTypeNo := preTypeNo + 1;
	predefinedTypes [preTypeNo] := typ;
	typ.ref := preTypeNo
END New_predefined_typ;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Is_string* (type : Type) : BOOLEAN;
BEGIN
	RETURN (type.form = type_string)
	OR (type.form = type_array) & (type.base = char_type)
END Is_string;

PROCEDURE Is_extension* (ext, bas : Type) : BOOLEAN;
BEGIN
	RETURN (ext = bas) OR (ext.base # NIL) & Is_extension (ext.base, bas)
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

PROCEDURE Compatible_proc* (xtype, ytype : Type) : BOOLEAN;
BEGIN
	RETURN (xtype.base = ytype.base) & Same_parlist (xtype.fields, ytype.fields)
END Compatible_proc;

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

PROCEDURE WriteInt* (f : Sys.FileHandle; n : LONGINT);
	VAR finish : BOOLEAN; b : INTEGER;
BEGIN
	REPEAT
		b := SHORT (n MOD 128); finish := (n >= -64) & (n < 64);
		IF finish THEN b := b + 128 ELSE n := n DIV 128 END;
		Sys.Write_byte (f, b)
	UNTIL finish
END WriteInt;

PROCEDURE ReadInt64* (f : Sys.FileHandle; VAR n : LONGINT);
	VAR finish : BOOLEAN; i, b: INTEGER; k : LONGINT;
BEGIN
	n := 0; i := 1; k := 1; b := 0; 
	REPEAT
		Sys.Read_byte (f, b);
		IF i < 10 THEN
			finish := b >= 128; b := b MOD 128; n := n + b * k;
			k := k * 128; i := i + 1;
			IF finish & (b >= 64) THEN n := n + (-1 * k) END
		ELSIF i = 10 THEN
			finish := TRUE; IF b = 127 THEN n := n + MIN_INT END
		ELSE ASSERT(FALSE); finish := TRUE
		END
	UNTIL finish
END ReadInt64;

PROCEDURE ReadInt* (f : Sys.FileHandle; VAR n : INTEGER);
	VAR long : LONGINT;
BEGIN
	long := 0; ReadInt64 (f, long); n := SHORT (long)
END ReadInt;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

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
	CompilerFlag.nil_check := TRUE;
	CompilerFlag.alignment := TRUE;
	CompilerFlag.main := FALSE;
	CompilerFlag.console := FALSE
END Reset_compiler_flag;
	
PROCEDURE Init*;
BEGIN
	strpos := 0
END Init;

BEGIN
	NEW (guard); guard.class := class_head;
	preTypeNo := 0; predefinedTypes[0] := NIL;
	
	New_predefined_typ (int_type, type_integer, Word_size);
	New_predefined_typ (bool_type, type_boolean, 1);
	New_predefined_typ (set_type, type_set, Word_size);
	New_predefined_typ (byte_type, type_integer, 1);
	New_predefined_typ (char_type, type_char, Char_size);
	New_predefined_typ (nil_type, type_nil, Word_size);
	New_predefined_typ (real_type, type_real, 4);
	New_predefined_typ (longreal_type, type_real, 8);
	New_predefined_typ (anyScalar_type, type_integer, 8);
	
	New_predefined_typ (guardRecord_type, type_record, Word_size);
	guardRecord_type.fields := guard;
		
	New_predefined_typ (guardPointer_type, type_pointer, Word_size);
	guardPointer_type.base := guardRecord_type;
	guardPointer_type.num_ptr := 1;
	
	New_predefined_typ (guardArray_type, type_array, Word_size);
	guardArray_type.base := int_type;
	guardArray_type.len := 1;
	
	New_predefined_typ (word_type, type_integer, 2);
	New_predefined_typ (dword_type, type_integer, 4);

	New_predefined_typ (byteArray_type, type_array, 1);
	byteArray_type.base := byte_type;
	byteArray_type.len := 1
END Base.
