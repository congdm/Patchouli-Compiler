MODULE Parser;

IMPORT
	Base, Scanner, Generator;

CONST
	success = Base.success; failed = Base.failed;

VAR
	sym : INTEGER;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Assignable (dst, src : Base.Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
		dst_form : INTEGER;
	BEGIN
	result := TRUE;
	IF ~ Base.Is_variable (dst) THEN
		Scanner.Mark ('Assignment destination is not a variable');
		result := FALSE
	ELSE
		dst_form := dst.type.form;
		IF dst_form = Base.type_procedure
		& (src.mode = Base.class_proc) THEN
		& ~ Base.Is_compatible_proc (dst.type.fields, src.proc.dsc) THEN
			Scanner.Mark ('Assignment with incompatible procedure');
			result := FALSE
		ELSIF ~ Base.Has_value (src) THEN
			Scanner.Mark ('Invalid assignment source');
			result := FALSE
		ELSIF src.type = dst.type THEN
			IF (dst_form = Base.type_array) & (dst.type.len = -1) THEN
				Scanner.Mark ('Assignment source is open array');
				result := FALSE
				END
		ELSIF (src.type = Base.nil_type)
		& ~ (dst_form IN {Base.type_pointer, Base.type_procedure}) THEN
			Scanner.Mark ('NIL is only assignable to pointer or procedure variable');
			result := FALSE
		ELSIF dst_form # src.type.form THEN
			Scanner.Mark ('Assignment source and destination are not compatible');
			result := FALSE
		ELSIF (dst_form IN {Base.type_pointer, Base.type_record})
		& ~ Base.Is_extension_type (src, dst) THEN
			Scanner.Mark ('Assignment source must be a extension of destination');
			result := FALSE
		ELSIF dst_form = Base.type_integer THEN
			(* Ok, no problem *)
		ELSE
			Scanner.Mark ('Assignment source and destination are not compatible');
			result := FALSE
			END
		END
	END Assignable;
	
PROCEDURE Comparable (x, y : Base.Item) : BOOLEAN;
	VAR
		result : BOOLEAN;
	BEGIN
	result := TRUE;
	IF x.mode = Base.class_proc THEN
		IF y.mode = Base.class_proc THEN
			IF ~ Base.Is_compatible_proc (x.proc.dsc, y.proc.dsc)
				Scanner.Mark ('Comparision between incompatible procedure');
				result := FALSE
				END
		ELSIF Base.Is_variable (y) & (y.type.form = Base.type_procedure) THEN
			IF ~ Base.Is_compatible_proc (x.proc.dsc, y.type.fields)
				Scanner.Mark ('Comparision between incompatible procedure');
				result := FALSE
				END
		ELSIF y.type # Base.nil_type THEN
			Scanner.Mark ('Procedure is only comparable with procedure or NIL');
			result := FALSE
			END
	ELSIF y.mode = Base.class_proc THEN
		IF Base.Is_variable (x) & (x.type.form = Base.type_procedure) THEN
			IF ~ Base.Is_compatible_proc (x.type.fields, y.proc.dsc)
				Scanner.Mark ('Comparision between incompatible procedure');
				result := FALSE
				END
		ELSIF x.type # Base.nil_type THEN
			Scanner.Mark ('Procedure is only comparable with procedure or NIL');
			result := FALSE
			END
	ELSIF ~ Base.Has_value (x) OR ~ Base.Has_value (y) THEN
		Scanner.Mark ('Invalid comparision')
		result := FALSE
	ELSIF x.type = y.type THEN
		(* Ok, no problem *)
	ELSIF (x.type = Base.nil_type)
	& ~ (y.type.form IN {Base.type_pointer, Base.type_procedure})
	OR (y.type = Base.nil_type)
	& ~ (y.type.form IN {Base.type_pointer, Base.type_procedure}) THEN
		Scanner.Mark ('NIL is only comparable with pointer, procedure or NIL');
		result := FALSE
	ELSIF x.type.form # y.type.form THEN
		Scanner.Mark ('Comparision between incompatible value');
		result := FALSE
	ELSIF (x.type.form = Base.type_pointer)
	& ~ Base.Is_extension_type (x.type, y.type)
	& ~ Base.Is_extension_type (y.type, x.type) THEN
		Scanner.Mark ('Comparision between unrelated pointer type');
		result := FALSE
	ELSIF (x.type.form = Base.type_procedure)
	& ~ Base.Is_compatible_proc (x.type.fields, y.type.fields) THEN
		Scanner.Mark ('Comparision between incompatible procedure type');
		result := FALSE
		END;
	RETURN result
	END Comparable;

PROCEDURE ^ expression (VAR x : Base.Item);
PROCEDURE ^ type (VAR typ : Base.Type; defozzzbj : Base.Object);

PROCEDURE qualident (VAR obj : Base.Object);
	BEGIN
	IF sym = Base.sym_ident THEN
		Base.Find_obj (obj, Scanner.id, FALSE)
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
			cls := Base.class_par
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
				cls := Base.class_par;
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
	IF sym = Base.sym_rbrace THEN
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
			IF len.a >= 0 THEN
				typ.len := len.a
			ELSE
				Scanner.Mark ('Negative array length is not allowed');
				typ.len := 0
				END
		ELSE
			Scanner.Mark ('Array length must be const');
			typ.len := 0
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
	IF sym = Base.sym_lbrace THEN
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
	IF sym = Base.sym_lbrace THEN	
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
			identdef (proc, Base.class_proc);
			Base.Open_scope (proc.name);
			Base.Inc_level (1);
			IF sym = Base.sym_lbrace THEN
				FormalParameters (proc.parblksize, proc.type)
			ELSE
				proc.parblksize := 0
				END;
			END ProcedureHeading;
			
		PROCEDURE ProcedureBody (proc : Base.Object);
			VAR
				locblksize : INTEGER;
				x : Base.Item;
			BEGIN
			DeclarationSequence (locblksize);
			
			Generator.Enter (proc, locblksize);
			IF sym = Base.sym_begin THEN
				Scanner.Get (sym);
				StatementSequence;
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
			Generator.Return (proc, locblksize);
			
			Base.Inc_level (-1);
			Base.Close_scope;
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
		
		Scanner.Get (sym);
		SimpleExpression (y);
		x_form := x.type.form;
		IF (op = Base.sym_equal) OR (op = Base.sym_not_equal) THEN
			IF (y.type = Base.nil_type) & (x_form # Base.type_pointer)
			& (x_form # Base.type_procedure) THEN
				Scanner.Mark ('Invalid comparision')
			
		END
	END expression;
	
PROCEDURE statement;
	VAR
		x, y : Base.Item;
	BEGIN
	IF sym = Base.sym_ident THEN
		designator (x);
		Scanner.Get (sym);
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
			IF sym = Base.sym_lbrace THEN
				ActualParameters (x)
			ELSIF x.proc.parblksize > 0 THEN
				Scanner.Mark ('Not enough parameters')
				END;
			Generator.Call (x);
			Generator.Cleanup_after_call (x)
		ELSIF (x.type # NIL) & (x.type.form = Base.type_procedure) THEN
			IF x.type.base # NIL THEN
				Scanner.Mark ('Function procedure must be called in expression')
				END;
			Generator.Prepare_to_call (x);
			IF sym = Base.sym_lbrace THEN
				ActualParameters (x)
			ELSE
				Scanner.Mark ('Parameter list (even empty) expected')
				END;
			Generator.Call (x);
			Generator.Cleanup_after_call (x)
		ELSE
			Scanner.Mark ('Invalid statement')
			END
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
	ELSE
		Scanner.Mark ('Invalid statement')
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

PROCEDURE Module* (VAR modid : Base.String);
	VAR
		vars_size : INTEGER;
	BEGIN
	IF sym = Base.sym_module THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No MODULE keyword')
		END;
	IF sym = Base.sym_ident THEN
		modid := Scanner.id;
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No module identifiers')
		END;

	DeclarationSequence (vars_size);

	IF sym = Base.sym_begin THEN
		Generator.Module_init;
		Scanner.Get (sym);
		StatementSequence;
		Generator.End_module_init
		END;

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
		END
	END Module;

END Parser.
