MODULE Parser;

IMPORT
	Base, Scanner, Generator, Console;

CONST
	success = Base.success; failed = Base.failed;

VAR
	sym : INTEGER;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Assignable (VAR dst, src : Base.Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	result := FALSE;
	IF ~ Base.Is_variable (dst) THEN
		Scanner.Mark ('Assignment destination is not a variable')
	ELSIF Base.flag_readonly IN dst.flag THEN
		Scanner.Mark ('Assignment destination is read-only')
	ELSIF (dst.type.form = Base.type_array) & (dst.type.len < 0) THEN
		Scanner.Mark ('Assignment destination is open array')
	ELSE
		CASE Base.Assignable (dst.type, src) OF
			0: result := TRUE |
			1: Scanner.Mark ('Assignment with incompatible procedure') |
			2: Scanner.Mark ('Invalid assignment source') |
			3: Scanner.Mark ('Assignment source and destination are not compatible') |
			4: Scanner.Mark ('Assignment source must be a extension of destination') |
			5: Scanner.Mark ('Assignment source is non-global procedure') |
			6: Scanner.Mark ('Assignment source string has longer length than destination')
			END
		END;
	RETURN result
	END Assignable;
	
PROCEDURE Comparable (VAR x, y : Base.Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	result := FALSE;
	CASE Base.Comparable (x, y) OF
		0: result := TRUE |
		1: Scanner.Mark ('Invalid comparison') |
		2: Scanner.Mark ('Comparison with non-global procedure') |
		3: Scanner.Mark ('Comparison with incompatible procedure') |
		4: Scanner.Mark ('Comparison with unrelated pointer type')
		END;
	RETURN result
	END Comparable;
	
PROCEDURE Check_operator (op : INTEGER; VAR x : Base.Item);
	BEGIN
	IF ~ Base.Has_value (x) THEN
		Scanner.Mark ('Operator and object are not compatible');
		Generator.Make_const (x, Base.int_type, 0)
		END;
	IF (op = Base.sym_plus) OR (op = Base.sym_minus)
	OR (op = Base.sym_times)THEN
		IF ~ (x.type.form IN {Base.type_integer, Base.type_set}) THEN
			Scanner.Mark ('+ and - only compatible with INTEGER and SET');
			Generator.Make_const (x, Base.int_type, 0)
			END
	ELSIF op = Base.sym_slash THEN
		IF ~ (x.type.form = Base.type_set) THEN
			Scanner.Mark ('/ only compatible with SET');
			Generator.Make_const (x, Base.set_type, 0)
			END
	ELSIF (op = Base.sym_div) OR (op = Base.sym_mod) THEN
		IF ~ (x.type.form = Base.type_integer) THEN
			Scanner.Mark ('DIV and MOD only compatible with INTEGER');
			Generator.Make_const (x, Base.int_type, 0)
			END
	ELSIF (op = Base.sym_not) OR (op = Base.sym_or)
	OR (op = Base.sym_and) THEN
		IF ~ (x.type.form = Base.type_boolean) THEN
			Scanner.Mark ('~, & and OR only compatible with BOOLEAN');
			Generator.Make_const (x, Base.bool_type, 0)
			END
		END
	END Check_operator;

PROCEDURE ^ expression (VAR x : Base.Item);
PROCEDURE ^ type (VAR typ : Base.Type; defobj : Base.Object);
PROCEDURE ^ StatementSequence;

PROCEDURE qualident (VAR obj : Base.Object);
	BEGIN
	IF sym = Base.sym_ident THEN
		Base.Find_obj (obj, Scanner.id);
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('Identifier expected');
		obj := Base.guard
		END
	END qualident;

PROCEDURE ident (VAR obj : Base.Object; class : INTEGER);
	BEGIN
	IF sym = Base.sym_ident THEN
		IF Base.New_obj (obj, Scanner.id, class) = failed THEN
			Scanner.Mark ('Duplicate identifer definition');
			obj.class := class
			END;
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('Identifier expected');
		obj := Base.guard
		END
	END ident;

PROCEDURE identdef (VAR obj : Base.Object; class : INTEGER);
	BEGIN
	ident (obj, class)
	END identdef;

PROCEDURE IdentList (VAR first : Base.Object; class : INTEGER);
	VAR
		obj : Base.Object;
	BEGIN
	identdef (first, class);
	WHILE sym = Base.sym_comma DO
		Scanner.Get (sym);
		identdef (obj, class);
		IF first = Base.guard THEN
			first := obj
			END
		END
	END IdentList;

PROCEDURE FormalParameters (VAR parblksize : INTEGER; VAR result_typ : Base.Type);
	VAR
		obj : Base.Object;
		
	PROCEDURE FormalType (VAR typ : Base.Type);
		VAR
			tp, tp2 : Base.Type;
			obj : Base.Object;
		BEGIN
		IF sym = Base.sym_array THEN
			Base.New_typ (typ, Base.type_array);
			typ.len := -1;
			typ.size := Base.Word_size * 2;
			tp := typ;
			Scanner.Get (sym);
			IF sym = Base.sym_of THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No OF after ARRAY')
				END
			END;
		WHILE sym = Base.sym_array DO
			Base.New_typ (tp.base, Base.type_array);
			tp := tp.base;
			tp.len := -1;
			tp.size := -1;
			INC (typ.size, Base.Word_size);
			Scanner.Get (sym);
			IF sym = Base.sym_of THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No OF after ARRAY')
				END
			END;
		
		qualident (obj);
		IF obj.class = Base.class_type THEN
			tp2 := obj.type
		ELSE
			Scanner.Mark ('Identifier is not a type');
			tp2 := Base.int_type
			END;
		IF typ = NIL THEN
			typ := tp2
		ELSE
			tp.base := tp2
			END
		END FormalType;

	PROCEDURE FPSection (VAR parblksize : INTEGER);
		VAR
			first, obj : Base.Object;
			tp : Base.Type;
			cls, par_size : INTEGER;
			read_only : BOOLEAN;
		BEGIN
		IF sym = Base.sym_var THEN
			Scanner.Get (sym);
			cls := Base.class_ref
		ELSE
			cls := Base.class_var
			END;
		ident (first, cls);			
		WHILE sym = Base.sym_comma DO
			Scanner.Get (sym);
			ident (obj, cls);
			IF first = Base.guard THEN
				first := obj
				END
			END;
			
		IF sym = Base.sym_colon THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No colon after identifier list')
			END;
			
		FormalType (tp);
		par_size := Base.Word_size;
		read_only := FALSE;
		IF ~ Base.Is_scalar_type (tp) THEN
			IF cls = Base.class_var THEN
				cls := Base.class_ref;
				read_only := TRUE;
			ELSE
				IF tp.form = Base.type_record THEN
					par_size := Base.Word_size * 2
					END
				END
			END;
		IF (tp.form = Base.type_array) & (tp.len = -1) THEN
			par_size := tp.size
			END;
		
		WHILE first # Base.guard DO
			first.class := cls;
			first.val := parblksize;
			first.type := tp;
			first.lev := Base.cur_lev;
			first.flag := {Base.flag_param};
			IF read_only THEN
				first.flag := first.flag + {Base.flag_readonly}
				END;
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
			ELSE
				Scanner.Mark ('Superfluous semicolon')
				END
			END
		END;
	IF sym = Base.sym_rparen THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No closing )')
		END;
	IF sym = Base.sym_colon THEN
		Scanner.Get (sym);
		qualident (obj);
		IF (obj.class = Base.class_type) & Base.Is_scalar_type (obj.type) THEN
			result_typ := obj.type
		ELSE
			Scanner.Mark ('Invalid result type for function');
			result_typ := Base.int_type
			END
		END
	END FormalParameters;

PROCEDURE ArrayType (VAR typ : Base.Type; defobj : Base.Object);

	PROCEDURE length (VAR typ : Base.Type; defobj : Base.Object);
		VAR
			len : Base.Item;
		BEGIN
		expression (len);
		IF len.mode = Base.class_const THEN
			IF len.a > 0 THEN
				typ.len := len.a
			ELSE
				Scanner.Mark ('Non-positive array length is not allowed');
				typ.len := 1
				END
		ELSE
			Scanner.Mark ('Array length must be const');
			typ.len := 1
			END;
			
		IF sym = Base.sym_of THEN
			Scanner.Get (sym);
			type (typ.base, defobj);
		ELSIF sym = Base.sym_comma THEN
			Scanner.Get (sym);
			length (typ.base, defobj);
		ELSE
			Scanner.Mark ('OF or , expected');
			typ.base := Base.int_type
			END;
		typ.size := typ.len * typ.base.size
		END length;
		
	BEGIN (* ArrayType *)
	Base.New_typ (typ, Base.type_array);
	Scanner.Get (sym);
	length (typ, defobj)
	END ArrayType;

PROCEDURE RecordType (VAR typ : Base.Type; defobj : Base.Object);
	VAR
		obj : Base.Object;

	PROCEDURE FieldListSequence (VAR fields_size : INTEGER; defobj : Base.Object);

		PROCEDURE FieldList (VAR fields_size : INTEGER; defobj : Base.Object);
			VAR
				first : Base.Object;
				tp : Base.Type;
			BEGIN
			IdentList (first, Base.class_field);
			IF sym = Base.sym_colon THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No colon after identifier list')
				END;
			type (tp, defobj);
			WHILE first # Base.guard DO
				first.val := fields_size;
				first.type := tp;
				first.lev := Base.cur_lev;
				first := first.next;
				INC (fields_size, tp.size)
				END
			END FieldList;
	
		BEGIN (* FieldListSequence *)
		fields_size := 0;
		FieldList (fields_size, defobj);
		WHILE sym = Base.sym_semicolon DO
			Scanner.Get (sym);
			IF sym = Base.sym_ident THEN
				FieldList (fields_size, defobj)
			ELSE
				Scanner.Mark ('There must be a field list after ; in record definition')
				END
			END
		END FieldListSequence;
		
	BEGIN (* RecordType *)
	Base.New_typ (typ, Base.type_record);
	typ.size := 0;
	typ.len := 0;
	
	Scanner.Get (sym);
	IF sym = Base.sym_lparen THEN
		qualident (obj);
		IF obj = defobj THEN
			Scanner.Mark ('Recursive definition')
		ELSIF (obj.class = Base.class_type)
		& (obj.type.form = Base.type_record) THEN
			typ.base := obj.type;
			typ.size := obj.type.size;
			typ.len := obj.type.len + 1
		ELSE
			Scanner.Mark ('Invalid record base type')
			END;
		IF sym = Base.sym_rbrace THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No closing )')
			END
		END;
		
	Base.Open_scope (NIL);
	IF sym = Base.sym_ident THEN
		FieldListSequence (typ.size, defobj)
		END;
	IF sym = Base.sym_end THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No END for record definition')
		END;
	typ.fields := Base.top_scope.next;
	Base.Close_scope
	END RecordType;

PROCEDURE PointerType (VAR typ : Base.Type; defobj : Base.Object);
	BEGIN
	Base.New_typ (typ, Base.type_pointer);
	typ.size := Base.Word_size;
	
	Scanner.Get (sym);
	IF sym = Base.sym_to THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No TO in pointer definition')
		END;
		
	IF sym = Base.sym_record THEN
		RecordType (typ.base, NIL)
	ELSE
		type (typ.base, defobj)
		END;
	IF typ.base.form # Base.type_record THEN
		Scanner.Mark ('Record type expected');
		typ.base := Base.nilrecord_type
		END
	END PointerType;

PROCEDURE ProcedureType (VAR typ : Base.Type);
	BEGIN
	Base.New_typ (typ, Base.type_procedure);
	typ.size := Base.Word_size;
	
	Base.Open_scope (NIL);
	Scanner.Get (sym);
	IF sym = Base.sym_lparen THEN	
		FormalParameters (typ.len, typ.base);
		END;
	typ.fields := Base.top_scope.next;
	Base.Close_scope
	END ProcedureType;

PROCEDURE StrucType (VAR typ : Base.Type; defobj : Base.Object);
	BEGIN
	IF sym = Base.sym_array THEN
		ArrayType (typ, defobj)
	ELSIF sym = Base.sym_record THEN
		RecordType (typ, defobj)
	ELSIF sym = Base.sym_pointer THEN
		PointerType (typ, defobj)
	ELSIF sym = Base.sym_procedure THEN
		ProcedureType (typ)
	ELSE
		Scanner.Mark ('Expect a type definition')
		END
	END StrucType;
	
PROCEDURE type (VAR typ : Base.Type; defobj : Base.Object);
	VAR
		obj : Base.Object;
	BEGIN
	typ := Base.int_type;
	IF sym = Base.sym_ident THEN
		qualident (obj);
		IF obj = defobj THEN
			Scanner.Mark ('Recursive definition')
		ELSIF obj.class = Base.class_type THEN
			typ := obj.type
		ELSE
			Scanner.Mark ('This identifier is not a type')
			END
	ELSIF (sym = Base.sym_array) OR (sym = Base.sym_record)
	OR (sym = Base.sym_pointer) OR (sym = Base.sym_procedure) THEN
		StrucType (typ, defobj)
	ELSE
		Scanner.Mark ('Expect a type or type definition')
		END
	END type;

PROCEDURE DeclarationSequence (VAR vars_size : INTEGER);

	PROCEDURE ConstDeclaration;
		VAR
			obj : Base.Object;
			x : Base.Item;
		BEGIN
		identdef (obj, Base.class_const);
		IF sym = Base.sym_equal THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No = in const declaration')
			END;
		expression (x);
		IF x.mode = Base.class_const THEN
			obj.val := x.a;
			obj.type := x.type
		ELSIF (x.mode = Base.class_var) & (x.type.form = Base.type_string) THEN
			obj.flag := x.flag;
			obj.class := x.mode;
			obj.lev := x.lev;
			obj.val := x.a;
			obj.type := x.type
		ELSE
			Scanner.Mark ('Const expected')
			END
		END ConstDeclaration;
		
	PROCEDURE TypeDeclaration;
		VAR
			obj : Base.Object;
		BEGIN
		identdef (obj, Base.class_type);
		IF sym = Base.sym_equal THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No = in type declaration')
			END;
		StrucType (obj.type, obj)
		END TypeDeclaration;
	
	PROCEDURE VariableDeclaration (VAR vars_size : INTEGER);
		VAR
			first : Base.Object;
			tp : Base.Type;
		BEGIN
		IdentList (first, Base.class_var);
		IF sym = Base.sym_colon THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No : in variable declaration')
			END;	
		type (tp, NIL);
		WHILE first # Base.guard DO
			first.val := vars_size;
			first.type := tp;
			first.lev := Base.cur_lev;
			first := first.next;
			INC (vars_size, tp.size)
			END	
		END VariableDeclaration;
		
	PROCEDURE ProcedureDeclaration;
		VAR
			proc : Base.Object;
			
		PROCEDURE ProcedureHeading (VAR proc : Base.Object);
			BEGIN
			Scanner.Get (sym);
			identdef (proc, Base.class_proc);
			Base.Open_scope (proc.name);
			Base.Inc_level (1);
			IF sym = Base.sym_lparen THEN
				FormalParameters (proc.parblksize, proc.type)
			ELSE
				proc.parblksize := 0
				END
			END ProcedureHeading;
			
		PROCEDURE ProcedureBody (proc : Base.Object);
			VAR
				locblksize : INTEGER;
				x : Base.Item;
			BEGIN
			locblksize := 0;
			DeclarationSequence (locblksize);
			
			Generator.Enter (proc, locblksize);
			IF sym = Base.sym_begin THEN
				Scanner.Get (sym);
				StatementSequence
				END;
				
			IF sym = Base.sym_return THEN
				Scanner.Get (sym);
				expression (x);
				IF proc.type = NIL THEN
					Scanner.Mark ('Proper procedure do not need RETURN')
				ELSE
					IF (x.type = proc.type)
					OR (x.type.form = Base.type_pointer)
					& (proc.type.form = Base.type_pointer)
					& Base.Is_extension_type (x.type, proc.type) THEN
						Generator.Return_value (x)
					ELSE
						Scanner.Mark ('Return value type is incompatible')
						END
					END
			ELSIF proc.type # NIL THEN
				Scanner.Mark ('No return value for function procedure')
				END;
				
			IF sym = Base.sym_end THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No END for procedure body')
				END;
			Generator.Return (proc, locblksize);
			
			proc.dsc := Base.top_scope.next;
			Base.Inc_level (-1);
			Base.Close_scope
			END ProcedureBody;
			
		BEGIN (* ProcedureDeclaration *)
		ProcedureHeading (proc);
		IF sym = Base.sym_semicolon THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No ; after procedure heading')
			END;
		ProcedureBody (proc);
		IF sym = Base.sym_ident THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('Procedure declaration must end with name of procedure')
			END;
		END ProcedureDeclaration;

	BEGIN (* DeclarationSequence *)
	vars_size := 0;
	IF sym = Base.sym_const THEN
		Scanner.Get (sym);
		WHILE sym = Base.sym_ident DO
			ConstDeclaration;
			IF sym = Base.sym_semicolon THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No ; after const declaration')
				END;
			END;
		END;
	IF sym = Base.sym_type THEN
		Scanner.Get (sym);
		WHILE sym = Base.sym_ident DO
			TypeDeclaration;
			IF sym = Base.sym_semicolon THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No ; after type declaration')
				END;
			END;
		END;
	IF sym = Base.sym_var THEN
		Scanner.Get (sym);
		WHILE sym = Base.sym_ident DO
			VariableDeclaration (vars_size);
			IF sym = Base.sym_semicolon THEN
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('No ; after variable declaration')
				END;
			END;
		END;
	WHILE sym = Base.sym_procedure DO
		ProcedureDeclaration;
		IF sym = Base.sym_semicolon THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No ; after procedure declaration')
			END;
		END;
	END DeclarationSequence;
	
PROCEDURE ActualParameters (VAR x : Base.Item);
	VAR
		param : Base.Object;
		
	PROCEDURE Parameter (VAR proc : Base.Item; VAR param : Base.Object);
		VAR
			y, par_item : Base.Item;
			is_var_param : BOOLEAN;
		BEGIN
		expression (y);
		CASE Base.Check_parameter (param, y) OF
			0: Generator.Normal_parameter (y, param.val) |
			1: Generator.Open_array_parameter (y, param) |
			2: Generator.Reference_parameter (y, param.val) |
			3: Generator.Record_variable_parameter (y, param.val) |
			9: Generator.String_parameter (y, proc, param) |
			4: Scanner.Mark ('Formal parameter is variable but actual is read-only') |
			5: Scanner.Mark ('Formal parameter is variable but actual is not') |
			6: Scanner.Mark ('Formal type and actual type are incompatible') |
			7: Scanner.Mark ('Invalid parameter') |
			8: Scanner.Mark ('Actual type is not an extension of formal type')
			END;
		param := param.next
		END Parameter;
		
	BEGIN (* ActualParameters *)
	Scanner.Get (sym);
	IF x.mode = Base.class_proc THEN
		param := x.proc.dsc
	ELSE
		param := x.type.fields
		END;
	IF ~ (Base.flag_param IN param.flag) THEN
		IF sym = Base.sym_rparen THEN
			Scanner.Get (sym)
		ELSIF (sym = Base.sym_ident) OR (sym = Base.sym_number) THEN
			Scanner.Mark ('This procedure does not need any parameters')
		ELSE
			Scanner.Mark ('No closing )')
			END
	ELSE
		Parameter (x, param);
		WHILE sym = Base.sym_comma DO
			Scanner.Get (sym);
			IF ~ (Base.flag_param IN param.flag) THEN
				Scanner.Mark ('This procedure does not need more parameters')
			ELSE
				Parameter (x, param)
				END
			END;
		IF Base.flag_param IN param.flag THEN
			Scanner.Mark ('Not enough actual parameters')
			END;
		IF sym = Base.sym_rparen THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No closing )')
			END
		END
	END ActualParameters;
	
PROCEDURE ProperStandProc (VAR x : Base.Item);

	PROCEDURE SProc_LoadLibrary (VAR x : Base.Item);
		VAR
			y, z : Base.Item;
		BEGIN
		END SProc_LoadLibrary;

	BEGIN (* ProperStandProc *)
	CASE x.a OF
		0: SProc_LoadLibrary (x)
		END
	END ProperStandProc;
	
PROCEDURE selector (VAR x : Base.Item);
	VAR
		exit : BOOLEAN;
		obj : Base.Object;
		y : Base.Item;
	BEGIN
	exit := FALSE;
	WHILE ~ exit DO
		IF ~ Base.Has_value (x) OR (x.type.form = Base.type_procedure) THEN
			exit := TRUE
		ELSIF sym = Base.sym_period THEN
			IF x.type.form = Base.type_pointer THEN
				Generator.Deref (x)
				END;
			IF x.type.form = Base.type_record THEN
				Scanner.Get (sym);
				IF sym = Base.sym_ident THEN
					Base.Find_field (obj, Scanner.id, x.type);
					IF obj # Base.guard THEN
						Generator.Field (x, obj)
					ELSE
						Scanner.Mark ('Record field not found')
						END;
					Scanner.Get (sym)
				ELSE
					Scanner.Mark ('Expect a record field identifier')
					END
			ELSE
				Scanner.Mark ('Not a record or pointer but found . selector')
				END
		ELSIF sym = Base.sym_lbrak THEN
			IF x.type.form = Base.type_array THEN
				Scanner.Get (sym);
				expression (y);
				IF ~ Base.Has_value (y) OR (y.type.form # Base.type_integer) THEN
					Scanner.Mark ('Invalid array index')
				ELSE
					Generator.Index (x, y)
					END;
				IF sym = Base.sym_rbrak THEN
					Scanner.Get (sym)
				ELSE
					Scanner.Mark ('No closing ]')
					END
			ELSE
				Scanner.Mark ('Not an array but found [ selector')
				END
		ELSIF sym = Base.sym_arrow THEN
			IF x.type.form = Base.type_pointer THEN
				Generator.Deref (x);
				Scanner.Get (sym)
			ELSE
				Scanner.Mark ('Not a pointer but found ^ selector')
				END
		ELSIF sym = Base.sym_lparen THEN
			IF (x.type.form = Base.type_pointer) 
			OR (x.type.form = Base.type_record)
			& (x.mode = Base.class_ref)
			& ~ (Base.flag_readonly IN x.flag) THEN
				Scanner.Get (sym);
				qualident (obj);
				IF (obj.class = Base.class_type) 
				& (obj.type.form = x.type.form)
				& Base.Is_extension_type (obj.type, x.type) THEN
					Generator.Type_guard (x, obj.type)
				ELSE
					Scanner.Mark ('Invalid or incompatible type in type guard')
					END;
				IF sym = Base.sym_rparen THEN
					Scanner.Get (sym)
				ELSE
					Scanner.Mark ('No closing )')
					END
			ELSE
				Scanner.Mark ('Not a pointer or record var-param but found ( selector')
				END
		ELSE
			exit := TRUE
			END
		END
	END selector;
	
PROCEDURE designator (VAR x : Base.Item);
	VAR
		obj : Base.Object;
	BEGIN
	qualident (obj);
	IF obj = Base.guard THEN
		Scanner.Mark ('Identifier not defined or invisible to this scope');
		Generator.Make_const (x, Base.int_type, 0)
	ELSE
		Generator.Make_item (x, obj);
		selector (x)
		END
	END designator;
	
PROCEDURE set (VAR x : Base.Item);
	BEGIN
	(* Implement later *)
	END set;
	
PROCEDURE factor (VAR x : Base.Item);
	BEGIN
	IF sym = Base.sym_number THEN
		Generator.Make_const (x, Base.int_type, Scanner.val);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_ident THEN
		designator (x);
		IF sym = Base.sym_lparen THEN
			IF (x.mode = Base.class_proc)
			OR Base.Is_variable (x) & (x.type.form = Base.type_procedure) THEN
				IF (x.type = NIL)
				OR (x.mode # Base.class_proc) & (x.type.base = NIL) THEN
					Scanner.Mark ('Not able to call proper procedure in expression')
				ELSE
					Generator.Prepare_to_call (x);
					ActualParameters (x);
					Generator.Call (x);
					Generator.Cleanup_after_call (x);
					Generator.Get_return_value (x)
					END
			ELSE
				Scanner.Mark ('Found ( but designator is not a procedure')
				END
			END
	ELSIF sym = Base.sym_string THEN
		Generator.Make_string (x, Scanner.id);
		Scanner.Get (sym)
	ELSIF sym = Base.sym_lbrace THEN
		set (x)
	ELSIF sym = Base.sym_lparen THEN
		Scanner.Get (sym);
		expression (x);
		IF sym = Base.sym_rparen THEN
			Scanner.Get (sym)
		ELSE
			Scanner.Mark ('No closing )')
			END
	ELSIF sym = Base.sym_not THEN
		Scanner.Get (sym);
		factor (x);
		Check_operator (Base.sym_not, x);
		Generator.Op1 (Base.sym_not, x)
		END
	END factor;
	
PROCEDURE term (VAR x : Base.Item);
	VAR
		op : INTEGER;
		y : Base.Item;
	BEGIN
	factor (x);
	WHILE (sym >= Base.sym_times) & (sym <= Base.sym_and) DO
		op := sym;
		Check_operator (op, x);
		
		IF Base.Is_variable (x) THEN Generator.load (x) END;
		IF op = Base.sym_and THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym);
		factor (y);
		Check_operator (op, y);
		
		IF x.type.form = y.type.form THEN
			Generator.Op2 (op, x, y)
		ELSE
			Scanner.Mark ('Incompatible types')
			END
		END
	END term;
	
PROCEDURE SimpleExpression (VAR x : Base.Item);
	VAR
		op : INTEGER;
		y : Base.Item;
	BEGIN
	IF sym = Base.sym_plus THEN
		Scanner.Get (sym);
		term (x);
		Check_operator (Base.sym_plus, x)
	ELSIF sym = Base.sym_minus THEN
		Scanner.Get (sym);
		term (x);
		Check_operator (Base.sym_minus, x);
		Generator.Op1 (Base.sym_minus, x)
	ELSE
		term (x)
		END;
		
	WHILE (sym >= Base.sym_plus) & (sym <= Base.sym_or) DO
		op := sym;
		Check_operator (op, x);
		
		IF Base.Is_variable (x) THEN Generator.load (x) END;
		IF op = Base.sym_or THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym);
		term (y);
		Check_operator (op, y);
		
		IF x.type.form = y.type.form THEN
			Generator.Op2 (op, x, y)
		ELSE
			Scanner.Mark ('Incompatible types')
			END
		END
	END SimpleExpression;
	
PROCEDURE expression (VAR x : Base.Item);
	VAR
		op, x_form : INTEGER;
		y : Base.Item;
	BEGIN
	SimpleExpression (x);
	IF (sym >= Base.sym_equal) & (sym <= Base.sym_is) THEN
		Generator.Finish_cond (x);
		op := sym;
		Check_operator (op, x);
		
		IF (op # Base.sym_is) & Base.Is_variable (x) THEN
			Generator.load (x)
			END;
		
		Scanner.Get (sym);
		SimpleExpression (y);

		IF op = Base.sym_in THEN
			IF y.type # Base.set_type THEN
				Scanner.Mark ('Expected a SET value')
			ELSE
				Generator.Membership_test (x, y)
				END
		ELSIF op = Base.sym_is THEN
			IF y.mode # Base.class_type THEN
				Scanner.Mark ('Expected a type in type test')
			ELSIF (y.type.form # x.type.form)
			OR ~ Base.Is_extension_type (y.type, x.type) THEN
				Scanner.Mark ('Incompatible type in type test')
			ELSE
				Generator.Type_test (x, y.type)
				END
		ELSIF (op = Base.sym_equal) OR (op = Base.sym_not_equal) THEN
			IF Comparable (x, y) THEN Generator.Relation (op, x, y) END
		ELSE
			IF Base.Have_ordering_relations (x, y) THEN
				Generator.Relation (op, x, y)
			ELSE
				Scanner.Mark ('Invalid comparison')
				END
			END
		END
	END expression;
	
PROCEDURE statement;
	VAR
		x, y : Base.Item;
	BEGIN
	IF sym = Base.sym_ident THEN
		designator (x);
		IF sym = Base.sym_becomes THEN
			Scanner.Get (sym);
			expression (y);
			IF Assignable (x, y) THEN
				Generator.Store (x, y)
				END
		ELSIF x.mode = Base.class_proc THEN
			IF x.type # NIL THEN
				Scanner.Mark ('Function procedure must be called in expression')
				END;
			Generator.Prepare_to_call (x);
			IF sym = Base.sym_lparen THEN
				ActualParameters (x)
			ELSIF x.proc.parblksize > 0 THEN
				Scanner.Mark ('Not enough actual parameters')
				END;
			Generator.Call (x);
			Generator.Cleanup_after_call (x)
		ELSIF x.mode = Base.class_sproc THEN
			IF x.type # NIL THEN
				Scanner.Mark ('Function procedure must be called in expression')
			ELSE
				ProperStandProc (x)
				END
		ELSIF Base.Has_value (x) & (x.type.form = Base.type_procedure) THEN
			IF x.type.base # NIL THEN
				Scanner.Mark ('Function procedure must be called in expression')
				END;
			Generator.Prepare_to_call (x);
			IF sym = Base.sym_lparen THEN
				ActualParameters (x)
			ELSE
				Scanner.Mark ('Parameter list (even empty) expected')
				END;
			Generator.Call (x);
			Generator.Cleanup_after_call (x)
		ELSE
			Scanner.Mark ('Invalid statement')
			END
	(* ELSIF sym = Base.sym_if THEN
		IfStatement
	ELSIF sym = Base.sym_while THEN
		WhileStatement
	ELSIF sym = Base.sym_repeat THEN
		RepeatStatement
	ELSIF sym = Base.sym_for THEN
		ForStatement
	ELSIF sym = Base.sym_case THEN
		CaseStatement *)
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
	VAR
		vars_size : INTEGER;
		modid : Base.String;
	BEGIN
	Scanner.Get (sym);
	IF sym = Base.sym_module THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No MODULE keyword')
		END;
	IF sym = Base.sym_ident THEN
		modid := Scanner.id;
		Scanner.Get (sym)
	ELSE
		modid := Base.Make_string ('ERROR_MODULE_NAME');
		Scanner.Mark ('No module name')
		END;
	IF sym = Base.sym_semicolon THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No ; after module name')
		END;
	
	Base.Init (modid);
	Generator.Init (modid);

	vars_size := 0;
	DeclarationSequence (vars_size);

	Generator.Module_init;
	IF sym = Base.sym_begin THEN
		Scanner.Get (sym);
		StatementSequence
		END;
	Generator.End_module_init;

	IF sym = Base.sym_end THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No END for module')
		END;
	IF sym = Base.sym_ident THEN
		IF ~ Base.Str_equal2 (modid, Scanner.id) THEN
			Scanner.Mark ('Module identifier do not match')
			END;
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No module identifier after END')
		END;
	IF sym # Base.sym_period THEN
		Scanner.Mark ('No ending .')
		END;
		
	Generator.Finish;
	END Module;

END Parser.