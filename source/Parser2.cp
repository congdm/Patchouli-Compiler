MODULE Parser2;

IMPORT
	Sys, Base, Scanner, Generator := GeneratorWin64v2, Console;
	
CONST
	classes_Variable = Base.classes_Variable;
	classes_Value = Base.classes_Value;

VAR
	sym : INTEGER;
	defobj : Base.Object;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Assignable (VAR dst, src : Base.Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
BEGIN
	result := FALSE;
	IF ~ (dst.mode IN classes_Variable) THEN
		Scanner.Mark ('Assignment destination is not a variable')
	ELSIF dst.readonly THEN
		Scanner.Mark ('Assignment destination is read-only')
	ELSIF (dst.type.form = Base.type_array) & (dst.type.len < 0) THEN
		Scanner.Mark ('Assignment destination is open array')
	ELSE
		CASE Base.Assignable (dst.type, src) OF
			0: result := TRUE |
			1: Scanner.Mark ('Invalid assignment source') |
			2: Scanner.Mark ('Incompatible assignment') |
			3: Scanner.Mark ('Assignment source type is not an extension of destination') |
			4: Scanner.Mark ('Assignment source is non-global procedure') |
			5: Scanner.Mark ('Assignment with incompatible procedure') |
			6: Scanner.Mark ('Assignment source string is oversized')
		END
	END;
	RETURN result
END Assignable;
	
PROCEDURE Check_operator (op : INTEGER; VAR x : Base.Item);
BEGIN
	IF ~ (x.mode IN Base.cls_HasValue) THEN
		Scanner.Mark ('Operator and object are not compatible');
		Generator.Free_item (x);
		Generator.Make_const (x, Base.int_type, 0)
	END;
	IF (op = Base.sym_plus) OR (op = Base.sym_minus)
	OR (op = Base.sym_times) THEN
		IF ~ (x.type.form IN {Base.type_integer, Base.type_set, Base.type_real}) THEN
			Scanner.Mark ('+, - and * only compatible with numberic types and SET');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF op = Base.sym_slash THEN
		IF ~ (x.type.form IN {Base.type_set, Base.type_real}) THEN
			Scanner.Mark ('/ only compatible with REAL and SET');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.set_type, 0)
		END
	ELSIF (op = Base.sym_div) OR (op = Base.sym_mod) THEN
		IF ~ (x.type.form = Base.type_integer) THEN
			Scanner.Mark ('DIV and MOD only compatible with INTEGER');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF (op = Base.sym_not) OR (op = Base.sym_or)
	OR (op = Base.sym_and) THEN
		IF ~ (x.type.form = Base.type_boolean) THEN
			Scanner.Mark ('~, & and OR only compatible with BOOLEAN');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.bool_type, 0)
		END
	END
END Check_operator;
	
PROCEDURE Check_int (VAR x : Base.Item);
BEGIN
	IF ~ (x.mode IN Base.cls_HasValue) OR (x.type.form # Base.type_integer) THEN
		Scanner.Mark ('Expect a integer expression');
		Generator.Free_item (x);
		Generator.Make_const (x, Base.int_type, 0)
	END
END Check_int;

PROCEDURE Check_bool (VAR x : Base.Item);
BEGIN
	IF ~ (x.mode IN Base.cls_HasValue) OR (x.type # Base.bool_type) THEN
		Scanner.Mark ('Expect a BOOLEAN expression');
		Generator.Free_item (x);
		Generator.Make_const (x, Base.bool_type, 0)
	END
END Check_bool;

PROCEDURE Check_set (VAR x : Base.Item);
BEGIN
	IF ~ (x.mode IN Base.cls_HasValue) OR (x.type # Base.set_type) THEN
		Scanner.Mark ('Expect a SET expression');
		Generator.Free_item (x);
		Generator.Make_const (x, Base.set_type, 0)
	END
END Check_set;

PROCEDURE Check_set_element (VAR x : Base.Item);
BEGIN
	Check_int (x);
	IF x.mode = Base.class_const THEN
		IF (x.a < 0) OR (x.a >= Base.set_size_limit) THEN
			Scanner.Mark ('This number is not in SET type range');
			x.a := 0
		END
	END
END Check_set_element;
	
PROCEDURE Check_relation (rel : INTEGER; VAR x : Base.Item);
	VAR
		is_pointer : BOOLEAN;
BEGIN
	IF (rel = Base.sym_equal) & (rel = Base.sym_not_equal) THEN
		IF ~ Base.Equality_applied (x) THEN
			Scanner.Mark ('Invalid operand');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF (rel >= Base.sym_less) & (rel <= Base.sym_less_equal) THEN
		IF Base.Comparison_applied (x) THEN
			(* Ok *)
		ELSIF ((rel = Base.sym_less_equal) OR (rel = Base.sym_greater_equal))
		& (x.mode IN Base.cls_HasValue) & (x.type = Base.set_type) THEN
			(* Ok *)
		ELSE
			Scanner.Mark ('Invalid operand');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF rel = Base.sym_is THEN
		IF ~ Base.Type_test_applied (x) THEN
			Scanner.Mark ('Expected a pointer or record var-param in type test');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.nil_type, 0)
		END
	ELSIF rel = Base.sym_in THEN
		Check_set_element (x)
	END
END Check_relation;

PROCEDURE Check_type_test (VAR x, y : Base.Item);
	VAR
		fail : BOOLEAN;
BEGIN
	IF y.mode # Base.class_type THEN
		Scanner.Mark ('Not a type'); fail := TRUE
	ELSIF (y.type.form # x.type.form)
	OR ~ Base.Is_extension_type (y.type, x.type) THEN
		Scanner.Mark ('Incompatible type'); fail := TRUE
	ELSE
		fail := FALSE
	END;
	IF fail THEN
		Generator.Free_item (x);
		Generator.Free_item (y);
		Generator.Make_const (x, Base.nil_type, 0);
		y.mode := Base.class_type;
		y.type := Base.nil_type
	END
END Check_type_test;

PROCEDURE Check_equal_comparison (VAR x, y : Base.Item);
	VAR
		fail : BOOLEAN;
BEGIN
	IF ~ Base.Equality_applied (y) THEN
		Scanner.Mark ('Invalid operand');
		fail := TRUE
	ELSIF ~ Base.Equalable (x, y) THEN
		Scanner.Mark ('Invalid comparison');
		fail := TRUE
	ELSE
		fail := FALSE
	END;
	IF fail THEN
		Generator.Free_item (x);
		Generator.Free_item (y);
		Generator.Make_const (x, Base.int_type, 0);
		Generator.Make_const (y, Base.int_type, 0)
	END
END Check_equal_comparison;

PROCEDURE Check_order_comparison (VAR x, y : Base.Item);
	VAR
		fail : BOOLEAN;
BEGIN
	IF ~ Base.Comparison_applied (y) THEN
		Scanner.Mark ('Invalid operand');
		fail := TRUE
	ELSIF ~ Base.Comparable (x, y) THEN
		Scanner.Mark ('Invalid comparison');
		fail := TRUE
	ELSE
		fail := FALSE
	END;
	IF fail THEN
		Generator.Free_item (x);
		Generator.Free_item (y);
		Generator.Make_const (x, Base.int_type, 0);
		Generator.Make_const (y, Base.int_type, 0)
	END
END Check_order_comparison;
	
PROCEDURE Check (expected : INTEGER; err : ARRAY OF CHAR);
BEGIN
	IF sym = expected THEN Scanner.Get (sym) ELSE Scanner.Mark (err) END
END Check;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE ^ expression (VAR x : Base.Item);
PROCEDURE ^ type (VAR typ : Base.Type);
PROCEDURE ^ StatementSequence;

PROCEDURE qualident (VAR obj : Base.Object);
BEGIN
	IF sym = Base.sym_ident THEN
		Base.Find_obj (obj, Scanner.id); Scanner.Get (sym)
	ELSE
		Scanner.Mark ('Identifier expected'); obj := Base.guard
	END
END qualident;

PROCEDURE ident (VAR obj : Base.Object; class : INTEGER);
	BEGIN
	IF sym = Base.sym_ident THEN
		Base.New_obj (obj, Scanner.id, class);
		IF obj = Base.guard THEN
			Scanner.Mark ('Duplicate identifer definition')
		END;
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('Identifier expected'); obj := Base.guard
	END
END ident;

PROCEDURE identdef (VAR obj : Base.Object; class : INTEGER);
BEGIN
	ident (obj, class);
	IF sym = Base.sym_times THEN
		IF obj.lev = 0 THEN obj.export := TRUE
		ELSE Scanner.Mark ('Can not export non-global identifiers')
		END;
		Scanner.Get (sym)
	END
END identdef;

PROCEDURE IdentList (VAR first : Base.Object; class : INTEGER);
	VAR obj : Base.Object;
BEGIN
	identdef (first, class);
	WHILE sym = Base.sym_comma DO
		Scanner.Get (sym); identdef (obj, class);
		IF first = Base.guard THEN first := obj END
	END
END IdentList;
	
PROCEDURE FormalType (VAR typ : Base.Type);
	VAR obj : Base.Object;
BEGIN
	IF sym = Base.sym_array THEN
		Base.New_typ (typ, Base.type_array);
		typ.len := -1;
		
		Scanner.Get (sym);
		Check (Base.sym_of, 'No OF after ARRAY');
		FormalType (typ.base);
		
		IF (typ.base.form = Base.type_array) & (typ.base.len = -1) THEN
			typ.size := typ.base.size + Base.Word_size;
			typ.base.size := -1
		ELSE
			typ.size := Base.Word_size * 2
		END
	ELSE
		qualident (obj);
		IF (obj # Base.guard) & (obj.class = Base.class_type) THEN
			typ := obj.type
		ELSE Scanner.Mark ('Type not found')
		END
	END
END FormalType;

PROCEDURE FormalParameters (VAR parblksize : INTEGER; VAR rtype : Base.Type);
	VAR obj : Base.Object;

	PROCEDURE FPSection (VAR parblksize : INTEGER);
		VAR
			first, obj : Base.Object; tp : Base.Type;
			cls, par_size : INTEGER; read_only: BOOLEAN;
	BEGIN
		IF sym # Base.sym_var THEN cls := Base.class_var
		ELSE Scanner.Get (sym); cls := Base.class_ref
		END;
		
		ident (first, cls);			
		WHILE sym = Base.sym_comma DO
			Scanner.Get (sym); ident (obj, cls);
			IF first = Base.guard THEN first := obj END
		END;
		
		Check (Base.sym_colon, 'No colon after identifier list');
			
		FormalType (tp); par_size := Base.Word_size; read_only := FALSE;
		IF tp.form IN Base.types_Scalar THEN (* Do nothing *)
		ELSE
			IF cls = Base.class_var THEN
				cls := Base.class_ref; read_only := TRUE
			ELSIF tp.form = Base.type_record THEN
				par_size := Base.Word_size * 2
			END;
			IF (tp.form = Base.type_array) & (tp.len < 0) THEN
				par_size := tp.size
			END;
		END;
		
		WHILE first # Base.guard DO
			first.class := cls;
			first.val := parblksize + 16;
			first.type := tp;
			first.lev := Base.cur_lev;
			first.param := TRUE;
			first.readonly := read_only;
			
			first := first.next;
			INC (parblksize, par_size)
		END
	END FPSection;
		
BEGIN (* FormalParameters *)
	parblksize := 0;

	Scanner.Get (sym);
	IF (sym = Base.sym_ident) OR (sym = Base.sym_var) THEN
		FPSection (parblksize);
		WHILE sym = Base.sym_semicolon DO
			Scanner.Get (sym);
			IF (sym = Base.sym_ident) OR (sym = Base.sym_var) THEN
				FPSection (parblksize)
			ELSE Scanner.Mark ('Superfluous semicolon')
			END
		END
	END;
	
	Check (Base.sym_rparen, 'No closing )');
	
	IF sym = Base.sym_colon THEN
		Scanner.Get (sym); qualident (obj);
		IF (obj # Base.guard) & (obj.class = Base.class_type)
		& (obj.type.form IN Base.types_Scalar) THEN
			rtype := obj.type
		ELSE
			Scanner.Mark ('Function result type must be scalar type');
			rtype := Base.int_type
		END
	END
END FormalParameters;

PROCEDURE ArrayType (VAR typ : Base.Type);

	PROCEDURE length (typ : Base.Type);
		VAR len : Base.Item;
	BEGIN
		expression (len);
		IF len.mode = Base.class_const THEN
			IF len.a > 0 THEN typ.len := SHORT (len.a)
			ELSE
				Scanner.Mark ('Non-positive array length is not allowed');
				typ.len := 1
			END
		ELSE Scanner.Mark ('Array length must be const'); typ.len := 1
		END
	END length;
		
BEGIN (* ArrayType *)
	Base.New_typ (typ, Base.type_array);	
	Scanner.Get (sym); length (typ);
	
	IF sym = Base.sym_comma THEN ArrayType (typ.base)
	ELSE Check (Base.sym_of, 'OF expected'); type (typ.base)
	END;
	
	typ.size := typ.len * typ.base.size;
	typ.num_ptr := typ.len * typ.base.num_ptr;
	typ.alignment := typ.base.alignment
END ArrayType;

PROCEDURE RecordType (VAR typ : Base.Type);
	VAR obj : Base.Object;

	PROCEDURE FieldListSequence (typ : Base.Type);

		PROCEDURE FieldList (typ : Base.Type);
			VAR
				first : Base.Object; tp : Base.Type;
				offset : INTEGER;
		BEGIN
			IdentList (first, Base.class_field);
			Check (Base.sym_colon, 'No colon after identifier list');
			type (tp);
			
			IF tp.alignment > typ.alignment THEN typ.alignment := tp.alignment
			END;
			offset := typ.size; Base.Adjust_alignment (offset, tp.alignment);
			
			WHILE first # Base.guard DO
				first.val := offset;
				first.type := tp;
				first.lev := Base.cur_lev;
				first := first.next;
				
				INC (offset, tp.size);
				IF tp.num_ptr > 0 THEN INC (typ.num_ptr, tp.num_ptr) END
			END;
			typ.size := offset
		END FieldList;
	
	BEGIN (* FieldListSequence *)
		FieldList (typ);
		WHILE sym = Base.sym_semicolon DO
			Scanner.Get (sym);
			IF sym = Base.sym_ident THEN FieldList (typ)
			ELSE Scanner.Mark ('Superflous semicolon')
			END
		END
	END FieldListSequence;
		
	PROCEDURE BaseType (typ : Base.Type);
		VAR
			obj : Base.Object;
			tp : Base.Type;
	BEGIN
		qualident (obj);
		
		IF (obj # Base.guard) & (obj.class = Base.class_type) THEN
			IF obj = defobj THEN Scanner.Mark ('Circular definition')
			ELSIF obj.type.form = Base.type_record THEN tp := obj.type
			ELSIF obj.type.form = Base.type_pointer THEN
				IF obj.type.base # Base.int_type THEN tp := obj.type.base
				ELSE Scanner.Mark ('Can not extend a undef pointer type')
				END
			ELSE Scanner.Mark ('Invalid record base type')
			END
		ELSE Scanner.Mark ('Not a type')
		END;
		
		IF tp # NIL THEN
			IF tp.len < Base.type_extension_limit THEN
				typ.hasExtension := TRUE;
				typ.base := tp;
				typ.size := tp.size;
				typ.num_ptr := tp.num_ptr;
				typ.len := tp.len + 1;
				typ.alignment := tp.alignment
			ELSE
				Scanner.Mark ('Type extension level too deep (compiler limit)')
			END
		END
	END BaseType;
		
BEGIN (* RecordType *)
	Base.New_typ (typ, Base.type_record);
	typ.size := 0; typ.len := 0; typ.alignment := 0; typ.num_ptr := 0;

	Scanner.Get (sym);
	IF sym = Base.sym_lparen THEN
		Scanner.Get (sym); BaseType (typ);
		Check (Base.sym_rparen, 'No closing )')
	END;
		
	Base.Open_scope (NIL);
	IF sym = Base.sym_ident THEN
		FieldListSequence (typ);
		Base.Adjust_alignment (typ.size, typ.alignment)
	END;
	Check (Base.sym_end, 'No END for record definition');
	typ.fields := Base.top_scope.next;
	Base.Close_scope
END RecordType;

PROCEDURE PointerType (VAR typ : Base.Type);
	VAR obj : Base.Object;
BEGIN
	Base.New_typ (typ, Base.type_pointer);
	typ.size := Base.Word_size; typ.num_ptr := 1;
	typ.alignment := Base.Word_size;
	
	Scanner.Get (sym); Check (Base.sym_to, 'No TO in pointer definition');
		
	IF sym = Base.sym_record THEN RecordType (typ.base)
	ELSE
		qualident (obj);
		IF obj = Base.guard THEN
			Base.Register_undefined_pointer_type (typ, Base.guard.name)
		ELSIF (obj.class = Base.class_type)
		& (obj.type.form = Base.type_record) THEN
			typ.base := obj.type
		ELSE
			Scanner.Mark ('Record type expected'); typ.base := Base.int_type
		END
	END
END PointerType;

PROCEDURE ProcedureType (VAR typ : Base.Type);
BEGIN
	Base.New_typ (typ, Base.type_procedure);
	typ.size := Base.Word_size; typ.alignment := Base.Word_size;
	
	Base.Open_scope (NIL);
	Scanner.Get (sym);
	IF sym = Base.sym_lparen THEN FormalParameters (typ.len, typ.base) END;
	typ.fields := Base.top_scope.next;
	Base.Close_scope
END ProcedureType;

PROCEDURE StrucType (VAR typ : Base.Type);
BEGIN
	IF sym = Base.sym_array THEN ArrayType (typ)
	ELSIF sym = Base.sym_record THEN RecordType (typ)
	ELSIF sym = Base.sym_pointer THEN PointerType (typ)
	ELSIF sym = Base.sym_procedure THEN ProcedureType (typ)
	ELSE Scanner.Mark ('Expect a type definition'); typ := Base.int_type
	END
END StrucType;
	
PROCEDURE type (VAR typ : Base.Type);
	VAR obj : Base.Object;
BEGIN
	typ := Base.int_type;
	IF sym = Base.sym_ident THEN
		qualident (obj);
		IF obj = Base.guard THEN
			Scanner.Mark ('Undefined type')
		ELSIF obj.class # Base.class_type THEN
			Scanner.Mark ('This identifier is not a type')
		ELSIF (obj = defobj) & (obj.type.form # Base.type_pointer) THEN
			Scanner.Mark ('Circular definition')
		ELSE
			typ := obj.type
		END
	ELSIF (sym = Base.sym_array) OR (sym = Base.sym_record)
	OR (sym = Base.sym_pointer) OR (sym = Base.sym_procedure) THEN
		StrucType (typ)
	ELSE Scanner.Mark ('Expect a type or type definition')
	END
END type;

PROCEDURE DeclarationSequence (VAR varsize : INTEGER);

	PROCEDURE ConstDeclaration;
		VAR obj : Base.Object; x : Base.Item;
	BEGIN
		identdef (obj, Base.class_head); (* Defense again circular definition *)
		Check (Base.sym_equal, 'No = in const declaration');
		expression (x);
		obj.class := Base.class_const;
		IF x.mode = Base.class_const THEN	
			obj.val := x.a;
			obj.type := x.type
		ELSIF (x.mode = Base.class_var) & (x.type.form = Base.type_string) THEN
			obj.lev := x.lev;
			obj.val := x.a;
			obj.val2 := x.b;
			obj.type := x.type
		ELSE
			Scanner.Mark ('Expect a const expression');
			obj.type := Base.int_type;
			obj.val := 0
		END
	END ConstDeclaration;
		
	PROCEDURE TypeDeclaration;
		VAR obj : Base.Object;
	BEGIN
		identdef (obj, Base.class_type);
		Check (Base.sym_equal, 'No = in type declaration');
		defobj := obj;
		StrucType (obj.type);
		(* obj.type.obj := obj; *)
		
		IF obj.type.form = Base.type_record THEN
			(* Generator.Alloc_type_desc (obj.type); *)
			IF Base.undef_ptr_list # NIL THEN
				Base.Check_undefined_pointer_list (obj)
			END
		ELSIF obj.type.form = Base.type_pointer THEN
			(*
			IF obj.type.base.form = Base.type_record THEN
				IF obj.type.base.obj = NIL THEN
					obj.type.base.obj := obj;
					Generator.Alloc_type_desc (obj.type.base)
				ELSE
					obj.val := obj.type.base.obj.val
				END
			END
			*)
		END
	END TypeDeclaration;
	
	PROCEDURE VariableDeclaration (VAR varsize : INTEGER);
		VAR first : Base.Object; tp : Base.Type;
	BEGIN
		IdentList (first, Base.class_var);
		Check (Base.sym_colon, 'No colon after identifier list');
		defobj := NIL;
		type (tp);
		
		Base.Adjust_alignment (varsize, tp.alignment);
		WHILE first # Base.guard DO
			INC (varsize, tp.size);
			first.val := -varsize;
			first.type := tp;
			first.lev := Base.cur_lev;
			first := first.next
		END
	END VariableDeclaration;
		
	PROCEDURE ProcedureDeclaration;
		VAR proc : Base.Object;
			
		PROCEDURE ProcedureHeading (VAR proc : Base.Object);
				
			PROCEDURE Copy_param_list (proc : Base.Object);
				VAR src, first, dst : Base.Object;
			BEGIN
				first := NIL; src := Base.top_scope.next;
				WHILE src # Base.guard DO
					IF first = NIL THEN NEW (first); dst := first
					ELSE NEW (dst.next); dst := dst.next
					END;
					dst^ := src^; src := src.next
				END;
				IF first = NIL THEN first := Base.guard
				ELSE dst.next := Base.guard
				END;
				proc.dsc := first
			END Copy_param_list;
				
		BEGIN (* ProcedureHeading *)
			Scanner.Get (sym); identdef (proc, Base.class_proc);
			Base.Open_scope (proc.name);
			Base.Inc_level (1);
			IF sym = Base.sym_lparen THEN
				FormalParameters (proc.parblksize, proc.type)
			ELSE proc.parblksize := 0
			END;
			Copy_param_list (proc)
		END ProcedureHeading;
			
		PROCEDURE ProcedureBody (proc : Base.Object);
			VAR locblksize : INTEGER; x : Base.Item;
		BEGIN
			locblksize := 0;
			DeclarationSequence (locblksize);
			Generator.Enter (proc, locblksize);
			IF sym = Base.sym_begin THEN
				Scanner.Get (sym);
				StatementSequence
			END;
				
			IF sym = Base.sym_return THEN
				Scanner.Get (sym); expression (x); Generator.load (x);
				IF proc.type # NIL THEN
					IF Base.Assignable (proc.type, x) # 0 THEN
						Scanner.Mark ('Return value type is incompatible')
					END
				ELSE Scanner.Mark ('Proper procedure do not need RETURN')
				END
			ELSIF proc.type # NIL THEN
				Scanner.Mark ('No return value for function procedure')
			END;
			
			Check (Base.sym_end, 'No END for procedure body');
			Generator.Return;
			Base.Inc_level (-1);
			Base.Close_scope
		END ProcedureBody;
			
	BEGIN (* ProcedureDeclaration *)
		ProcedureHeading (proc);
		Check (Base.sym_semicolon, 'No ; after procedure heading');
		ProcedureBody (proc);
		IF sym = Base.sym_ident THEN
			IF Base.Str_equal2 (proc.name, Scanner.id) THEN (* Ok *)
			ELSE Scanner.Mark ('Wrong name of procedure')
			END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('Procedure must end with its name')
		END
	END ProcedureDeclaration;

BEGIN (* DeclarationSequence *)
	IF sym = Base.sym_const THEN
		Scanner.Get (sym);
		WHILE sym = Base.sym_ident DO
			ConstDeclaration;
			Check (Base.sym_semicolon, 'No ; after const declaration')
		END
	END;
	IF sym = Base.sym_type THEN
		Scanner.Get (sym);
		WHILE sym = Base.sym_ident DO
			TypeDeclaration;
			Check (Base.sym_semicolon, 'No ; after type declaration')
		END;
		IF Base.undef_ptr_list # NIL THEN
			Base.Cleanup_undefined_pointer_list;
			Scanner.Mark ('There are pointer types with undefined base type')
		END
	END;
	IF sym = Base.sym_var THEN
		Scanner.Get (sym);
		WHILE sym = Base.sym_ident DO
			VariableDeclaration (varsize);
			Check (Base.sym_semicolon, 'No ; after variable declaration')
		END;
		IF Base.cur_lev = 0 THEN Generator.Set_varsize (varsize) END
	END;
	WHILE sym = Base.sym_procedure DO
		ProcedureDeclaration;
		Check (Base.sym_semicolon, 'No ; after procedure declaration')
	END
END DeclarationSequence;
	
PROCEDURE ActualParameters
(VAR x : Base.Item; VAR procinfo : Generator.ProcInfo);
	VAR param : Base.Object;
		
	PROCEDURE Parameter
	(VAR procinfo : Generator.ProcInfo; VAR param : Base.Object);
		VAR y : Base.Item;
	BEGIN
		expression (y);
		IF param # Base.guard THEN
			CASE Base.Check_parameter (param, y) OF
				0:	IF y.type.form = Base.type_string THEN
						Generator.Make_const (y, Base.char_type, y.b)
					END;
					Generator.Normal_parameter (y, procinfo)
				|
				1: Generator.Open_array_parameter (y, procinfo, param.type) |
				2: Generator.Reference_parameter (y, procinfo) |
				3: Generator.Record_var_parameter (y, procinfo) |
				(*
				9: Generator.String_parameter (y, procinfo, param.type) |
				*)
				4: Scanner.Mark ('Formal parameter is variable but actual is read-only') |
				5: Scanner.Mark ('Formal parameter is variable but actual is not') |
				6: Scanner.Mark ('Formal type and actual type are incompatible') |
				7: Scanner.Mark ('Invalid parameter') |
				8: Scanner.Mark ('Actual type is not an extension of formal type')
			END;
			param := param.next
		END
	END Parameter;
		
BEGIN (* ActualParameters *)
	IF x.mode = Base.class_proc THEN param := x.obj.dsc
	ELSE param := x.type.fields
	END;
	
	Scanner.Get (sym);
	IF sym # Base.sym_rparen THEN
		Parameter (procinfo, param);
		WHILE (sym = Base.sym_comma) & (param # Base.guard) DO
			Scanner.Get (sym); Parameter (procinfo, param) 
		END;
		IF sym = Base.sym_comma THEN
			Scanner.Mark ('Superflous , or too much actual parameters')
		END
	END;
	IF param # Base.guard THEN Scanner.Mark ('Not enough actual parameters')
	END;
	
	Check (Base.sym_rparen, 'No closing )')
END ActualParameters;

PROCEDURE ProcedureCall (VAR x : Base.Item; proper : BOOLEAN);
	CONST
		err1 = 'Function procedure must be called in expression';
		err2 = 'Proper procedure cannot be called in expression';
	VAR
		pinfo : Generator.ProcInfo;
		has_error : BOOLEAN;
		rtype : Base.Type;
BEGIN
	pinfo.proper := proper;
	IF x.mode = Base.class_proc THEN
		pinfo.procvar := FALSE;
		pinfo.parblksize := x.obj.parblksize;
		rtype := x.type
	ELSE
		pinfo.procvar := TRUE;
		pinfo.parblksize := x.type.len;
		rtype := x.type.base
	END;
	
	has_error := TRUE;
	IF proper THEN
		IF rtype = NIL THEN has_error := FALSE ELSE Scanner.Mark (err1) END
	ELSIF rtype # NIL THEN has_error := FALSE
	ELSE Scanner.Mark (err2)
	END;
	
	Generator.Prepare_to_call (x, pinfo);
	IF sym = Base.sym_lparen THEN ActualParameters (x, pinfo)
	ELSIF x.obj.parblksize > 0 THEN Scanner.Mark ('Not enough parameters')
	END;
	Generator.Call (x, pinfo);
	
	IF has_error THEN
		IF proper THEN Generator.Free_item (x)
		ELSE Generator.Make_const (x, Base.int_type, 0)
		END
	END
END ProcedureCall;

(* PROCEDURE StandProc (VAR x : Base.Item);
	VAR y : Base.Item;
	
	(*
	PROCEDURE SProc_NEW;
		VAR
			x : Base.Item;
	BEGIN
		expression (x);
		IF (x.mode IN Base.cls_Variable)
		& (x.type.form = Base.type_pointer) THEN
			IF Base.flag_readOnly IN x.flag THEN
				Scanner.Mark ('Not able to NEW read-only variable')
			ELSE
				Generator.SProc_NEW (x)
			END
		ELSE
			Scanner.Mark ('Expect a pointer variable')
		END
	END SProc_NEW;
	
	PROCEDURE SProc_DISPOSE;
		VAR
			x : Base.Item;
	BEGIN
		expression (x);
		IF (x.mode IN Base.cls_HasValue)
		& (x.type.form = Base.type_pointer) THEN
			Generator.SProc_DISPOSE (x)
		ELSE
			Scanner.Mark ('Expect a pointer')
		END
	END SProc_DISPOSE;
	*)
		
	PROCEDURE SProc_GET;
		VAR x, y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		Check (Base.sym_comma, 'Not enough parameters');
		expression (y);
		
		IF (y.mode IN classes_Variable)
		& (y.type.form IN Base.types_Scalar) THEN
			IF ~ y.readonly THEN Generator.SProc_GET (x, y)
			ELSE Scanner.Mark ('Cannot modify read only variable')
			END
		ELSE Scanner.Mark ('Expect a scalar variable')
		END
	END SProc_GET;
	
	PROCEDURE SProc_PUT;
		VAR x, y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		Check (Base.sym_comma, 'Not enough parameters');
		expression (y);
		
		IF (y.mode IN classes_Value) & (y.type.form IN Base.types_Scalar) THEN
			Generator.SProc_PUT (x, y)
		ELSE Scanner.Mark ('Expect a scalar value')
		END
	END SProc_PUT;
	
	PROCEDURE SProc_COPY;
		VAR x, y, z : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		Check (Base.sym_comma, 'Not enough parameters');
		expression (y); Check_int (y);
		Check (Base.sym_comma, 'Not enough parameters');
		expression (z); Check_int (z);
		
		Generator.SProc_COPY (x, y, z)
	END SProc_COPY;
		
	PROCEDURE SProc_LoadLibrary (adr : INTEGER);
		VAR
			x, y, proc : Base.Item;
			no_error : BOOLEAN;
	BEGIN
		expression (x);
		IF (x.mode IN classes_Variable) & (x.type = Base.int_type) THEN
			no_error := TRUE;
			IF ~ x.readonly THEN
			ELSE Scanner.Mark ('Cannot modify read only variable')
			END;
			proc.mode := Base.class_var;
			proc.lev := -1;
			proc.a := adr;
		ELSE
			Scanner.Mark ('Expect an INTEGER variable');
			Generator.Free_item (x); no_error := FALSE
		END;
		
		Check (Base.sym_comma, 'Not enough parameters');
		expression (y);
		
		IF no_error THEN
			IF (y.mode IN classes_Value) & Base.Is_string (y.type) THEN
				Generator.Prepare_to_call (proc, pinfo);
				Generator.SProc_LoadLibrary (x, y)
			ELSE Scanner.Mark ('Expect a string or character array');
			END
		ELSE Generator.Free_item (y)
		END
	END SProc_LoadLibrary;
		
	PROCEDURE SProc_GetProcAddress;
		VAR
			x, y, z : Base.Item;
			no_error : BOOLEAN;
	BEGIN
		expression (x);
		IF (x.mode IN classes_Variable)
		& (x.type.form = Base.type_procedure) THEN
			no_error := TRUE;
			IF ~ x.readonly THEN
			ELSE Scanner.Mark ('Cannot modify read only variable')
			END
		ELSE
			Scanner.Mark ('Expect a procedure variable');
			Generator.Free_item (x); no_error := FALSE
		END;
		
		expression (y); Check_int (y); Generator.load (y);
		Check (Base.sym_comma, 'Not enough parameters');
		expression (y); Check_int (y); Generator.load (y);
		
		Generator.SProc_GetProcAddress (x, y);
		
		Check (Base.sym_comma, 'Not enough parameters');
		expression (z);
		
		IF (z.mode IN classes_Variable) & (z.type.form IN Base.int_type) THEN
			Generator.Store (y, x)
		ELSE Scanner.Mark ('Expect an integer variable');
			Generator.Free_item (y); Generator.Free_item (x)
		END
		
		IF ~ (z.mode IN Base.cls_Variable)
		OR (z.type.form # Base.type_procedure) THEN
			Scanner.Mark ('Expect a procedure variable');
			no_error := FALSE
		ELSIF Base.flag_readOnly IN z.flag THEN
			Scanner.Mark ('Can not modify read-only variable');
			no_error := FALSE
		END;
		
		IF no_error THEN
			x.type := z.type;
			Generator.Store (z, x)
		END
	END SProc_GetProcAddress;

BEGIN (* StandProc *)
	Check (Base.sym_lparen, 'No parameter list or missing (');
	CASE SHORT (x.a) OF
		4: SProc_NEW |
		8: SProc_DISPOSE |
		10: SProc_GET |
		11: SProc_PUT |
		12: SProc_COPY |
		13: SProc_LoadLibrary |
		14: SProc_GetProcAddress 
	END;
	WHILE sym = Base.sym_comma DO
		Scanner.Mark ('Too much parameters');
		expression (y)
	END;
	Check (Base.sym_rparen, 'No closing )')
END StandProc; *)

(*
PROCEDURE StandFunc (VAR x : Base.Item);
	VAR
		result : Base.Item;

	PROCEDURE SFunc_ABS (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF ~ (x.mode IN Base.cls_HasValue)
		OR ~ (x.type.form IN Base.types_Numberic) THEN
			Scanner.Mark ('Expect a numberic value');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END;
		Generator.SFunc_ABS (x)
	END SFunc_ABS;
		
	PROCEDURE SFunc_ODD (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x);
		Generator.SFunc_ODD (x)
	END SFunc_ODD;
		
	PROCEDURE SFunc_LEN (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF ~ (x.mode IN Base.cls_HasValue)
		OR (x.type.form # Base.type_array) THEN
			Scanner.Mark ('Expect an array');
			Generator.Make_const (x, Base.int_type, 0)
		ELSE
			Generator.SFunc_LEN (x)
		END
	END SFunc_LEN;
		
	PROCEDURE SFunc_ORD (VAR x : Base.Item);
		CONST
			valid_types = {Base.type_char, Base.type_set, Base.type_boolean};
		VAR
			no_error : BOOLEAN;
	BEGIN
		expression (x);
		no_error := TRUE;
		IF ~ (x.mode IN Base.cls_HasValue) THEN
			no_error := FALSE
		ELSIF ~ (x.type.form IN valid_types) THEN
			IF (x.type.form # Base.type_string) OR (x.type.len # 2) THEN
				no_error := FALSE
			END
		END;
		
		IF no_error THEN
			Generator.SFunc_ORD (x)
		ELSE
			Scanner.Mark ('Expect a character, set or boolean value');
			Generator.Make_const (x, Base.int_type, 0)
		END
	END SFunc_ORD;
		
	PROCEDURE SFunc_CHR (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x);
		Generator.SFunc_CHR (x)
	END SFunc_CHR;
		
	PROCEDURE SFunc_ADR (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF x.mode IN Base.cls_Variable THEN
			Generator.SFunc_ADR (x)
		ELSE
			Scanner.Mark ('Expect a variable');
			Generator.Make_const (x, Base.int_type, 0)
		END
	END SFunc_ADR;
		
	PROCEDURE SFunc_SIZE (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF x.mode = Base.class_type THEN
			Generator.Make_const (x, Base.int_type, x.type.size)
		ELSE
			Scanner.Mark ('Expect a type identifier');
			Generator.Make_const (x, Base.int_type, 0)
		END
	END SFunc_SIZE;
		
	PROCEDURE SFunc_VAL (VAR x : Base.Item);
		VAR
			y : Base.Item;
	BEGIN
		expression (x);
		IF (x.mode # Base.class_type)
		OR ~ (x.type.form IN Base.types_Scalar) THEN
			Scanner.Mark ('Expect a scalar type identifier');
			x.type := Base.int_type
		END;
		
		Check (Base.sym_comma, 'Not enough parameters');
		expression (y);
		
		IF ~ (y.mode IN Base.cls_HasValue) THEN
			Scanner.Mark ('Expect a value');
			Generator.Free_item (y);
			Generator.Make_const (y, Base.int_type, 0)
		END;
		
		Generator.SFunc_VAL (x, y)
	END SFunc_VAL;
		
BEGIN (* StandFunc *)
	Check (Base.sym_lparen, 'No parameter list or missing (');
	CASE SHORT(x.a) OF
		20: SFunc_ABS (result) |
		21: SFunc_ODD (result) |
		22: SFunc_LEN (result) |
		(*23: SFunc_FLOOR (result) |
		24: SFunc_FLT (result) |*)
		25: SFunc_ORD (result) |
		26: SFunc_CHR (result) |
		(*27: SFunc_LSL (result) |
		28: SFunc_ASR (result) |
		29: SFunc_ROR (result) |*)
		30: SFunc_ADR (result) |
		31: SFunc_SIZE (result) |
		33: SFunc_VAL (result)
	END;
	
	IF (x.a # 33) & (x.a # 20) THEN result.type := x.type
	END;
	x := result;
	
	WHILE sym = Base.sym_comma DO
		Scanner.Mark ('Too much parameters');
		expression (result)
	END;
	Check (Base.sym_rparen, 'No closing )')
END StandFunc;
*)
	
PROCEDURE selector (VAR x : Base.Item);
		
	PROCEDURE Field_selector (VAR x : Base.Item);
		VAR obj : Base.Object;
	BEGIN
		IF x.type.form = Base.type_pointer THEN Generator.Deref (x) END;
		
		Scanner.Get (sym);
		IF x.type.form = Base.type_record THEN
			IF sym = Base.sym_ident THEN
				Base.Find_field (obj, Scanner.id, x.type);
				IF obj # Base.guard THEN Generator.Field (x, obj)
				ELSE Scanner.Mark ('Record field not found')
				END;
				Scanner.Get (sym)
			ELSE Scanner.Mark ('Expect a record field identifier')
			END
		ELSE
			Scanner.Mark ('Not a record or pointer but found . selector');
			IF sym = Base.sym_ident THEN Scanner.Get (sym) END
		END
	END Field_selector;
	
	PROCEDURE Element_selector (VAR x : Base.Item);
		VAR y : Base.Item;
	BEGIN
		IF x.type.form = Base.type_array THEN
			REPEAT
				Scanner.Get (sym); expression (y); Check_int (y);
				IF x.type.form = Base.type_array THEN Generator.Index (x, y)
				ELSE Scanner.Mark ('Wrong dimension')
				END
			UNTIL sym # Base.sym_comma
		ELSE
			Scanner.Mark ('Not an array but found [ selector');
			REPEAT
				Scanner.Get (sym);
				expression (y);
				Generator.Free_item (y)
			UNTIL sym # Base.sym_comma
		END;
		Check (Base.sym_rbrak, 'No closing ]')
	END Element_selector;
	
	PROCEDURE Deref_selector (VAR x : Base.Item);
	BEGIN
		Scanner.Get (sym);
		IF x.type.form = Base.type_pointer THEN Generator.Deref (x)
		ELSE Scanner.Mark ('Not a pointer but found ^ selector')
		END
	END Deref_selector;
	
	(*
	PROCEDURE Type_guard_selector (VAR x : Base.Item);
		VAR
			obj : Base.Object;
			is_pointer : BOOLEAN;
	BEGIN
		Scanner.Get (sym);
		qualident (obj);
		
		is_pointer := (x.type.form = Base.type_pointer)
			OR (x.type.form = Base.type_record)
				& (Base.flag_varParam IN x.flag);
		
		IF is_pointer THEN	
			IF (obj.class = Base.class_type)
				& (obj.type.form = x.type.form)
				& Base.Is_extension_type (obj.type, x.type)
			THEN
				Generator.Type_guard (x, obj.type)
			ELSE
				Scanner.Mark ('Invalid or incompatible type in type guard')
			END	
		ELSE
			Scanner.Mark ('Type guard is not applicable')
		END;
		
		Check (Base.sym_rparen, 'No closing )')
	END Type_guard_selector;
	*)
		
BEGIN
	IF ~ (x.mode IN Base.cls_HasValue) THEN
		Scanner.Mark ('Only array, pointer and record can have selector')
	ELSIF sym = Base.sym_period THEN
		Field_selector (x)
	ELSIF sym = Base.sym_lbrak THEN
		Element_selector (x)
	ELSIF sym = Base.sym_arrow THEN
		Deref_selector (x)
	ELSIF sym = Base.sym_lparen THEN
		(* Type_guard_selector (x) *)
	END
END selector;
	
PROCEDURE designator (VAR x : Base.Item);
	VAR obj : Base.Object; exit, is_procedure : BOOLEAN;
BEGIN
	qualident (obj);
	IF obj # Base.guard THEN
		Generator.Make_item (x, obj); exit := FALSE;
		REPEAT
			is_procedure := (x.mode IN {Base.class_proc, Base.class_sproc})
				OR (x.mode IN Base.cls_HasValue)
					& (x.type.form = Base.type_procedure);
					
			IF is_procedure & (sym = Base.sym_lparen) THEN exit := TRUE
			ELSE
				IF (sym = Base.sym_period) OR (sym = Base.sym_lparen)
				OR (sym = Base.sym_lbrak) OR (sym = Base.sym_arrow) THEN
					selector (x)
				ELSE exit := TRUE
				END
			END
		UNTIL exit
	ELSE
		Scanner.Mark ('Identifier not defined or invisible to this scope');
		Generator.Make_const (x, Base.int_type, 0)
	END
END designator;
	
PROCEDURE set (VAR x : Base.Item);

	PROCEDURE element (VAR x : Base.Item);
		VAR y, z : Base.Item;
	BEGIN
		expression (y); Check_set_element (y);
		IF sym = Base.sym_upto THEN
			IF y.mode # Base.class_const THEN Generator.load (y) END;
			Scanner.Get (sym); expression (z); Check_set_element (z);
			Generator.Set3 (x, y, z)
		ELSE Generator.Set2 (x, y)
		END
	END element;

BEGIN (* set *)
	Scanner.Get (sym);
	Generator.Make_const (x, Base.set_type, 0);
	IF sym # Base.sym_rbrace THEN
		element (x);
		WHILE sym = Base.sym_comma DO Scanner.Get (sym); element (x) END;
		Generator.Set1 (x); Check (Base.sym_rbrace, 'No closing }')
	ELSE Scanner.Get (sym)
	END
END set;
	
PROCEDURE factor (VAR x : Base.Item);
BEGIN
	IF sym = Base.sym_number THEN
		Generator.Make_const (x, Scanner.type_of_val, Scanner.val);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_ident THEN
		designator (x);
		IF sym = Base.sym_lparen THEN
			IF x.mode = Base.class_sproc THEN
				(*
				IF x.type # NIL THEN StandFunc (x)
				ELSE
					Scanner.Mark ('Found proper procedure in expression');
					Generator.Make_const (x, Base.int_type, 0)
				END
				*)
			ELSIF (x.mode = Base.class_proc) OR (x.mode IN Base.cls_HasValue)
			& (x.type.form = Base.type_procedure) THEN
				ProcedureCall (x, FALSE)
			ELSE Scanner.Mark ('Found ( but designator is not a procedure')
			END
		END
	ELSIF sym = Base.sym_string THEN
		Generator.Make_string (x, Scanner.id);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_nil THEN
		Generator.Make_const (x, Base.nil_type, 0);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_true THEN
		Generator.Make_const (x, Base.bool_type, 1);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_false THEN
		Generator.Make_const (x, Base.bool_type, 0);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_lbrace THEN
		set (x)
	ELSIF sym = Base.sym_lparen THEN
		Scanner.Get (sym); expression (x);
		Check (Base.sym_rparen, 'No closing )')
	ELSIF sym = Base.sym_not THEN
		Scanner.Get (sym); factor (x); Check_operator (Base.sym_not, x);
		Generator.Op1 (Base.sym_not, x)
	END
END factor;
	
PROCEDURE term (VAR x : Base.Item);
	VAR op : INTEGER; y : Base.Item;
BEGIN
	factor (x);
	WHILE (sym >= Base.sym_times) & (sym <= Base.sym_and) DO
		op := sym; Check_operator (op, x);
		
		IF x.mode IN Base.cls_Variable THEN Generator.load (x) END;
		IF op = Base.sym_and THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym); factor (y); Check_operator (op, y);
		
		IF x.type.form = y.type.form THEN Generator.Op2 (op, x, y)
		ELSE Scanner.Mark ('Incompatible types')
		END
	END
END term;
	
PROCEDURE SimpleExpression (VAR x : Base.Item);
	VAR op : INTEGER; y : Base.Item;
BEGIN
	IF sym = Base.sym_plus THEN
		Scanner.Get (sym); term (x); Check_operator (Base.sym_plus, x)
	ELSIF sym = Base.sym_minus THEN
		Scanner.Get (sym); term (x); Check_operator (Base.sym_minus, x);
		Generator.Op1 (Base.sym_minus, x)
	ELSE term (x)
	END;
		
	WHILE (sym >= Base.sym_plus) & (sym <= Base.sym_or) DO
		op := sym; Check_operator (op, x);
		
		IF x.mode IN Base.cls_Variable THEN Generator.load (x) END;
		IF op = Base.sym_or THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym); term (y); Check_operator (op, y);
		
		IF x.type.form = y.type.form THEN Generator.Op2 (op, x, y)
		ELSE Scanner.Mark ('Incompatible types')
		END
	END
END SimpleExpression;
	
PROCEDURE expression (VAR x : Base.Item);
	VAR op, x_form : INTEGER;
		y : Base.Item;
BEGIN
	SimpleExpression (x);
	(*
	IF (sym >= Base.sym_equal) & (sym <= Base.sym_is) THEN
		op := sym;
		Check_relation (op, x);
		
		IF x.mode = Base.mode_cond THEN
			Generator.load (x)
		ELSIF x.mode IN Base.cls_Variable THEN
			IF (op # Base.sym_is) & (x.type.form # Base.type_string) THEN
				Generator.load (x)
			END
		END;
		
		Scanner.Get (sym);
		SimpleExpression (y);

		IF op = Base.sym_in THEN
			Check_set (y);
			Generator.Membership (x, y)
		ELSIF op = Base.sym_is THEN
			Check_type_test (x, y);
			Generator.Type_test (x, y.type)
		ELSIF (op = Base.sym_equal) OR (op = Base.sym_not_equal) THEN
			Check_equal_comparison (x, y);
			Generator.Comparison (op, x, y)
		ELSIF ((op = Base.sym_less_equal) OR (op = Base.sym_greater_equal))
		& (x.type = Base.set_type) THEN
			Check_set (y);
			Generator.Inclusion (op, x, y)
		ELSE
			Check_order_comparison (x, y);
			Generator.Comparison (op, x, y)
		END
	END
	*)
END expression;

(*
PROCEDURE IfStatement;
	VAR
		x : Base.Item;
		L : INTEGER;
BEGIN
	L := 0;
	Scanner.Get (sym);
	expression (x); Check_bool (x); Generator.CFJump (x);
	Check (Base.sym_then, 'No THEN after IF condition');
	StatementSequence;
	
	WHILE sym = Base.sym_elsif DO
		Generator.FJump (L); Generator.Fix_link (SHORT (x.a));
		Scanner.Get (sym);
		expression (x); Check_bool (x); Generator.CFJump (x);
		Check (Base.sym_then, 'No THEN after ELSIF condition');
		StatementSequence
	END;
		
	IF sym = Base.sym_else THEN
		Generator.FJump (L); Generator.Fix_link (SHORT (x.a));
		Scanner.Get (sym);
		StatementSequence
	ELSE
		Generator.Fix_link (SHORT (x.a))
	END;
		
	Generator.Fix_link (L);
	Check (Base.sym_end, 'No END for IF statement')
END IfStatement;
	
PROCEDURE WhileStatement;
	VAR
		x : Base.Item;
		L : INTEGER;
BEGIN
	L := Generator.pc;
	Scanner.Get (sym);
	expression (x); Check_bool (x); Generator.CFJump (x);
	Check (Base.sym_do, 'No DO after WHILE condition');
	StatementSequence;
	
	WHILE sym = Base.sym_elsif DO
		Generator.BJump (L); Generator.Fix_link (SHORT (x.a));
		Scanner.Get (sym);
		expression (x); Check_bool (x); Generator.CFJump (x);
		Check (Base.sym_do, 'No DO after ELSIF condition');
		StatementSequence
	END;
		
	Generator.BJump (L); Generator.Fix_link (SHORT (x.a));
	Check (Base.sym_end, 'No END for WHILE statement')
END WhileStatement;
	
PROCEDURE RepeatStatement;
	VAR
		L : INTEGER;
		x : Base.Item;
BEGIN
	L := Generator.pc;
	Scanner.Get (sym);
	StatementSequence;
	Check (Base.sym_until, 'No UNTIL for REPEAT statement');
	expression (x); Check_bool (x); Generator.CBJump (x, L)
END RepeatStatement;
	
PROCEDURE ForStatement;
	VAR
		x, beg, end, rel, inc : Base.Item;
		flag : BOOLEAN;
		L : INTEGER;
BEGIN
	Scanner.Get (sym); expression (x);
	IF x.mode # Base.class_var THEN
		Scanner.Mark ('Invalid for loop control variable');
		x.mode := Base.class_var; x.lev := 0; x.a := 0
	END;
	IF x.type # Base.int_type THEN
		Scanner.Mark ('Control variable type must be INTEGER');
		x.type := Base.int_type
	END;
		
	Check (Base.sym_becomes, 'Expect :=');
	expression (beg); Check_int (beg);
	
	Check (Base.sym_to, 'No TO in FOR statement');
	expression (end); Check_int (end);
	
	IF sym = Base.sym_by THEN
		Scanner.Get (sym); expression (inc);
		IF inc.mode # Base.class_const THEN
			Scanner.Mark ('Expect a const expression'); flag := TRUE
		ELSIF inc.type # Base.int_type THEN
			Scanner.Mark ('Expect a integer value'); flag := TRUE
		ELSE
			flag := FALSE
		END
	ELSE
		flag := TRUE
	END;
	IF flag THEN Generator.Make_const (inc, Base.int_type, 1)
	END;
	
	Check (Base.sym_do, 'No DO in FOR statement');
	L := 0; Generator.Begin_FOR (x, beg, end, rel, L, SHORT (inc.a));
	StatementSequence;
	Check (Base.sym_end, 'No END for FOR statement');
	Generator.End_FOR (x, rel, inc, L)
END ForStatement;
*)
	
PROCEDURE statement;
	VAR x, y : Base.Item;
BEGIN
	IF sym = Base.sym_ident THEN
		designator (x);
		IF sym = Base.sym_becomes THEN
			Scanner.Get (sym); expression (y);
			IF Assignable (x, y) THEN
				IF (x.type = Base.char_type) & (y.type # Base.char_type) THEN
					Generator.Make_const (y, Base.char_type, y.b)
				END;
				Generator.Store (x, y)
			END
		ELSIF x.mode = Base.class_proc THEN
			ProcedureCall (x, TRUE)
		ELSIF x.mode = Base.class_sproc THEN
		(*
			IF x.type = NIL THEN StandProc (x)
			ELSE
				Scanner.Mark ('Function procedure must be called in expression')
			END
		*)
		ELSIF (sym = Base.sym_lparen) & (x.mode IN Base.cls_HasValue)
		& (x.type.form = Base.type_procedure) THEN
			ProcedureCall (x, TRUE)
		ELSE Scanner.Mark ('Invalid statement')
		END
	(*
	ELSIF sym = Base.sym_if THEN
		IfStatement
	ELSIF sym = Base.sym_while THEN
		WhileStatement
	ELSIF sym = Base.sym_repeat THEN
		RepeatStatement
	ELSIF sym = Base.sym_for THEN
		ForStatement
	ELSIF sym = Base.sym_case THEN
		CaseStatement
	*)
	END
END statement;
	
PROCEDURE StatementSequence;
BEGIN
	statement;
	WHILE sym = Base.sym_semicolon DO
		Scanner.Get (sym);
		statement
	END
END StatementSequence;

PROCEDURE Module*;
	VAR modid : Base.String;
		varsize : INTEGER;
BEGIN
	Scanner.Get (sym); Check (Base.sym_module, 'No MODULE keyword');
	
	IF sym = Base.sym_ident THEN modid := Scanner.id; Scanner.Get (sym)
	ELSE
		modid := Base.Make_string ('ERROR_MODULE_NAME');
		Scanner.Mark ('No module name')
	END;
	Check (Base.sym_semicolon, 'No ; after module name');
	
	Base.Init (modid);
	Generator.Init (modid);

	varsize := 0;
	DeclarationSequence (varsize);

	Generator.Enter (NIL, 0);
	IF sym = Base.sym_begin THEN
		Scanner.Get (sym);
		StatementSequence
	END;
	Generator.Module_exit;
	Generator.Return;

	Check (Base.sym_end, 'No END for module');
	IF sym = Base.sym_ident THEN
		IF ~ Base.Str_equal2 (modid, Scanner.id) THEN
			Scanner.Mark ('Wrong module name')
		END;
		Scanner.Get (sym)
	ELSE Scanner.Mark ('No module identifier after END')
	END;
	Check (Base.sym_period, 'No ending .');
	
	Generator.Finish;
END Module;

END Parser2.