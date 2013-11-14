MODULE Parser;

IMPORT
	Base, Scanner, Generator;

CONST
	success = Base.success; failed = Base.failed;

VAR
	sym : INTEGER;

PROCEDURE ^ expression (VAR x : Base.Item);

PROCEDURE qualident (VAR obj : Base.Object);
	BEGIN
	IF sym = Base.sym_ident THEN
		Base.Find_obj (obj, Scanner.id, FALSE)
	ELSE
		Scanner.Mark ('Identifier expected');
		obj := Base.guard
		END
	END qualident;

PROCEDURE identdef (VAR obj : Base.Object; class : INTEGER);
	BEGIN
	IF Base.New_obj (obj, Scanner.id, class) = failed THEN
		Scanner.Mark ('Duplicate identifer');
		obj.class := class
		END;
	Scanner.Get (sym)
	END identdef;

PROCEDURE IdentList (VAR first : Base.Object; class : INTEGER);
	VAR
		obj : Base.Object;
	BEGIN
	identdef (first, class);
	WHILE sym = Base.sym_comma DO
		BEGIN
		Scanner.Get (sym);
		IF sym = Base.sym_ident THEN
			identdef (obj.next, class)
		ELSE
			Scanner.Mark ('No identifier after , in identifier list')
			END
		END
	END IdentList;

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

PROCEDURE ^ type (VAR typ : Base.Type; defobj : Base.Object);

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
			Scanner.Mark ('Array length must be const')
			typ.len := 0
			END;
			
		IF sym = Base.sym_of THEN
			Scanner.Get (sym);
			type (typ.base, defobj);
		ELSIF sym = Base.sym_comma THEN
			Scanner.Get (sym);
			length (typ.base, defobj);
		ELSE
			Scanner.Mark ('OF or , expected')
			typ.base := Base.int_type;
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
				fields_size := fields_size + tp.size
				END
			END FieldList;
	
		BEGIN (* FieldListSequence *)
		fields_size := 0;
		FieldList (fields_size, defobj);
		WHILE sym = Base.sym_semicolon DO
			BEGIN
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
		BEGIN
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
		IF sym = Base.rbrace THEN
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
	typ.dsc := Base.top_scope.next;
	Base.Close_scope
	END RecordType;

PROCEDURE StrucType (VAR typ : Base.Type; name : Base.String);
	BEGIN
	IF sym = Base.sym_array THEN
		ArrayType (typ, name)
	ELSIF sym = Base.sym_record THEN
		RecordType (typ, name)
	ELSIF sym = Base.sym_pointer THEN
		PointerType (typ, name)
	ELSIF sym = Base.sym_procedure THEN
		ProcedureType (typ, name)
	ELSE
		Scanner.Mark ('Expect a type definition')
		END
	END StrucType;

PROCEDURE TypeDeclaration;
	BEGIN
	identdef (obj, Base.class_type);
	IF sym = Base.sym_equal THEN
		Scanner.Get (sym)
	ELSE
		Scanner.Mark ('No = in type declaration')
		END;
	StrucType (obj.type, obj.name)
	END TypeDeclaration;

PROCEDURE DeclarationSequence (VAR vars_size : INTEGER);
	BEGIN
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
			VariableDeclaration;
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
			Scanner.Mark ('Module identifier don't match')
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
