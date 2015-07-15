MODULE Parser;

IMPORT
	Sys, Base, SymTable, Scanner, Generator := Generator3, Console;
	
CONST
	classes_Variable = Base.classes_Variable;
	classes_Value = Base.classes_Value;
	
	(* Error message *)
	errTooMuchParam = 'Superflous , or too much actual parameters';
	errTooLittleParam = 'Missing , or not enough actual parameters';
	errInvalidRightHand = 'Invalid right hand side of relation';
	errWrongType = 'Incompatible type';

VAR
	sym : INTEGER;
	defobj : Base.Object;
	hasExport : BOOLEAN;
	addressTypeList : Base.Type;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Check_val (VAR x : Base.Item);
	CONST err1 = 'Not a valid value';
BEGIN
	IF x.mode IN classes_Value + {Base.class_proc} THEN (* Ok *)
	ELSE Scanner.Mark (err1);
		IF x.type = NIL THEN x.type := Base.int_type END;
		x.mode := Base.class_var; x.lev := 0; x.a := 0
	END
END Check_val;

PROCEDURE Check_var (VAR x : Base.Item; can_be_readonly : BOOLEAN);
	CONST err1 = 'Not a variable';
		err2 = 'This variable is readonly';
BEGIN
	IF x.mode IN classes_Variable THEN
		IF can_be_readonly OR ~ x.readonly THEN (* Ok *)
		ELSE Scanner.Mark (err2); x.readonly := FALSE
		END
	ELSE Scanner.Mark (err1); Generator.Free_item (x);
		IF x.type = NIL THEN x.type := Base.int_type END;
		x.mode := Base.class_var; x.lev := 0; x.a := 0
	END
END Check_var;

PROCEDURE Check_int (VAR x : Base.Item);
BEGIN
	Check_val (x);
	IF x.type.form = Base.type_integer THEN (* Ok *)
	ELSE Scanner.Mark ('Not an integer value'); x.type := Base.int_type
	END
END Check_int;

PROCEDURE Check_real (VAR x : Base.Item);
BEGIN
	Check_val (x);
	IF x.type.form = Base.type_real THEN (* Ok *)
	ELSE Scanner.Mark ('Not a real value'); x.type := Base.real_type
	END
END Check_real;

PROCEDURE Check_bool (VAR x : Base.Item);
BEGIN
	Check_val (x);
	IF x.type = Base.bool_type THEN (* Ok *)
	ELSE Scanner.Mark ('Not a BOOLEAN value'); x.type := Base.bool_type
	END
END Check_bool;

PROCEDURE Check_set (VAR x : Base.Item);
BEGIN
	Check_val (x);
	IF x.type = Base.set_type THEN (* Ok *)
	ELSE Scanner.Mark ('Not a SET value'); x.type := Base.set_type
	END
END Check_set;

PROCEDURE Check_set_element (VAR x : Base.Item);
	CONST limit = Base.set_size_limit;
BEGIN
	Check_int (x);
	IF (x.mode # Base.class_const) OR (x.a >= 0) & (x.a <= limit) THEN (* Ok *)
	ELSE Scanner.Mark ('This number is not in SET type range'); x.a := 0
	END
END Check_set_element;

PROCEDURE Check_pointer (VAR x : Base.Item);
BEGIN
	Check_val (x);
	IF x.type.form = Base.type_pointer THEN (* Ok *)
	ELSE Scanner.Mark ('Not a pointer value'); x.type := Base.guardPointer_type
	END
END Check_pointer;

PROCEDURE Check_scalar (VAR x : Base.Item);
BEGIN
	Check_val (x);
	IF x.type.form IN Base.types_Scalar THEN (* Ok *)
	ELSE Scanner.Mark ('Not a scalar value'); x.type := Base.int_type
	END
END Check_scalar;

PROCEDURE Check_global_proc (VAR x : Base.Item);
BEGIN
	IF (x.mode # Base.class_proc) OR (x.lev <= 0) THEN (* Ok *)
	ELSE Scanner.Mark ('Not a global procedure')
	END
END Check_global_proc;

PROCEDURE Check_compatible_proc (xtype, ytype : Base.Type);
BEGIN
	IF Base.Compatible_proc (xtype, ytype) THEN (* Ok *)
	ELSE Scanner.Mark ('Procedure signatures mismatch')
	END
END Check_compatible_proc;

PROCEDURE Check_assignment_dest (VAR x : Base.Item);
BEGIN
	Check_var (x, FALSE);
	IF (x.type.form # Base.type_array) OR (x.type.len > 0) THEN (* Ok *)
	ELSE Scanner.Mark ('Open array cannot be assigned');
		x.type := Base.int_type
	END
END Check_assignment_dest;

PROCEDURE Check_assignment (VAR xtype : Base.Type; VAR y : Base.Item);
	CONST err2 = 'Source type is not an extension of dest';
	VAR xform, yform : INTEGER;
BEGIN
	IF xtype = y.type THEN (* Ok *)
	ELSE xform := xtype.form; yform := y.type.form;
		IF {xform, yform} = {Base.type_integer} THEN (* Ok *)
		ELSIF (xform = Base.type_char) & (yform = Base.type_string)
		& (y.type.len <= 2) THEN
			Generator.Make_const (y, Base.char_type, y.type.charVal)
		ELSIF (yform = Base.type_string) & (xform = Base.type_array)
		& (xtype.base = Base.char_type) THEN
			IF xtype.len >= y.type.len - 1 THEN (* Ok *)
			ELSE Scanner.Mark ('String is too long')
			END
		ELSIF (yform = Base.type_nil) & (xform IN Base.types_Address) THEN
			(* Ok *)
		ELSIF {xform, yform} = {Base.type_pointer} THEN
			IF Base.Is_extension (y.type.base, xtype.base) THEN (* Ok *)
			ELSE Scanner.Mark (err2)
			END
		ELSIF {xform, yform} = {Base.type_record} THEN
			IF Base.Is_extension (y.type, xtype) THEN (* Ok *)
			ELSE Scanner.Mark (err2)
			END
		ELSIF {xform, yform} = {Base.type_procedure} THEN
			Check_global_proc (y); Check_compatible_proc (xtype, y.type)
		ELSIF {xform, yform} = {Base.type_address} THEN
			IF xtype.base = y.type.base THEN (* OK *)
			ELSE Scanner.Mark ('Incompatible address types')
			END
		ELSE Scanner.Mark ('Invalid assignment');
			xtype := Base.int_type; y.type := xtype
		END
	END
END Check_assignment;
	
PROCEDURE Check_operand (op : INTEGER; VAR x : Base.Item);
	CONST group1 = {Base.type_integer, Base.type_set, Base.type_real};
BEGIN
	Check_val (x);
	IF (op = Scanner.plus) OR (op = Scanner.minus) OR (op = Scanner.times) THEN
		IF ~ (x.type.form IN group1) THEN
			Scanner.Mark ('Not a numberic or SET value');
			x.type := Base.int_type
		END
	ELSIF op = Scanner.slash THEN
		IF ~ (x.type.form IN {Base.type_set, Base.type_real}) THEN
			Scanner.Mark ('/ only compatible with REAL and SET');
			x.type := Base.set_type
		END
	ELSIF (op = Scanner.div) OR (op = Scanner.mod) THEN
		IF ~ (x.type.form = Base.type_integer) THEN
			Scanner.Mark ('DIV and MOD only compatible with INTEGER');
			x.type := Base.int_type
		END
	ELSIF (op = Scanner.not) OR (op = Scanner.or) OR (op = Scanner.and) THEN
		IF ~ (x.type.form = Base.type_boolean) THEN
			Scanner.Mark ('~, & and OR only compatible with BOOLEAN');
			x.type := Base.int_type
		END
	ELSE ASSERT(FALSE)
	END
END Check_operand;
	
PROCEDURE Check_left_hand_side (rel : INTEGER; VAR x : Base.Item);
	CONST err1 = 'Invalid left hand side of relation';
		group1 = Base.types_Numberic + {Base.type_char};
BEGIN
	Check_val (x);
	IF (rel = Scanner.equal) & (rel = Scanner.not_equal) THEN
		IF (x.type.form IN Base.types_Scalar) OR Base.Is_string (x.type) THEN
			(* Ok *)
		ELSE Scanner.Mark (err1);
			IF x.mode # Base.class_proc THEN x.type := Base.int_type
			ELSE Generator.Make_const (x, Base.int_type, 0)
			END
		END
	ELSIF (rel >= Scanner.less) & (rel <= Scanner.less_equal) THEN
		IF (x.type.form IN group1) OR Base.Is_string (x.type) THEN
		ELSIF (x.type = Base.set_type)
		& ((rel = Scanner.less_equal) OR (rel = Scanner.greater_equal)) THEN
			(* Ok *)
		ELSE Scanner.Mark (err1); x.type := Base.int_type
		END
	ELSIF rel = Scanner.is THEN
		IF Base.Type_test_applicable (x) THEN (* Ok *)
		ELSE Scanner.Mark (err1); x.type := Base.guardPointer_type
		END
	ELSIF rel = Scanner.in THEN Check_set_element (x)
	END
END Check_left_hand_side;

PROCEDURE Check_type_test (VAR x, y : Base.Item);
	VAR xtype, ytype : Base.Type;
BEGIN
	IF y.mode = Base.class_type THEN
		xtype := x.type; ytype := y.type;
		IF ytype.form = xtype.form THEN
			IF xtype.form = Base.type_pointer THEN
				xtype := xtype.base; ytype := ytype.base
			END;
			IF Base.Is_extension (ytype, xtype) THEN (* Ok *)
			ELSE Scanner.Mark ('Not an extension type'); y.type := xtype
			END
		ELSE Scanner.Mark ('Incompatible type'); y.type := xtype
		END
	ELSE Scanner.Mark ('Not a type'); y.mode := Base.class_type; y.type := xtype
	END;
END Check_type_test;

PROCEDURE Check_comparison (VAR x, y : Base.Item; equality : BOOLEAN);
	VAR xform, yform : INTEGER; xbase, ybase : Base.Type;
BEGIN
	IF x.type = y.type THEN (* Ok *)
	ELSE xform := x.type.form; yform := y.type.form;
		IF {xform, yform} = {Base.type_integer} THEN (* Ok *)
		ELSIF (xform = Base.type_char)
		& (yform = Base.type_string) & (y.type.len <= 2) THEN
			Generator.Make_const (y, Base.char_type, y.type.charVal)
		ELSIF (yform = Base.type_char)
		& (xform = Base.type_string) & (x.type.len <= 2) THEN
			Generator.Make_const (x, Base.char_type, x.type.charVal)
		ELSIF (xform = Base.type_array)
			& (x.type.base = Base.char_type)
			& (yform = Base.type_string)
			OR (yform = Base.type_array)
			& (y.type.base = Base.char_type)
			& (xform = Base.type_string) THEN (* Ok *)
		ELSIF equality THEN
			IF (xform IN Base.types_Address) & (yform = Base.type_nil) THEN
				(* Ok *)
			ELSIF {xform, yform} = {Base.type_pointer} THEN
				xbase := x.type.base; ybase := y.type.base;
				IF Base.Is_extension (xbase, ybase)
					OR Base.Is_extension (ybase, xbase) THEN (* Ok *)
				ELSE Scanner.Mark ('Unrelated pointer types')
				END
			ELSIF {xform, yform} = {Base.type_procedure} THEN
				Check_global_proc (y); Check_compatible_proc (x.type, y.type)
			ELSIF {xform, yform} = {Base.type_address} THEN
				IF x.type.base = y.type.base THEN (* Ok *)
				ELSE Scanner.Mark ('Incompatible address types')
				END
			ELSIF (yform IN Base.types_Address) & (xform = Base.type_nil) THEN
				(* Ok *)
			ELSE Scanner.Mark (errWrongType); y.type := x.type
			END
		ELSE Scanner.Mark (errWrongType); y.type := x.type
		END
	END
END Check_comparison;
	
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
	CONST err1 = 'Identifier expected';
BEGIN
	IF sym = Scanner.ident THEN
		SymTable.Find_obj (obj, Scanner.id); Scanner.Get (sym);
		IF obj.class = Base.class_module THEN
			Check (Scanner.period, 'Expect . after module identifiers');
			IF sym = Scanner.ident THEN
				SymTable.Find_import_object (obj, Scanner.id, obj);
				Scanner.Get (sym)
			ELSE Scanner.Mark (err1); obj := Base.guard
			END
		END
	ELSE Scanner.Mark (err1); obj := Base.guard
	END
END qualident;

PROCEDURE ident (VAR obj : Base.Object; class : INTEGER);
BEGIN
	IF sym = Scanner.ident THEN
		SymTable.New_obj (obj, Scanner.id, class);
		Scanner.Get (sym)
	ELSE Scanner.Mark ('Identifier expected'); obj := Base.guard
	END
END ident;

PROCEDURE identdef (VAR obj : Base.Object; class : INTEGER);
BEGIN
	ident (obj, class);
	IF sym = Scanner.times THEN
		IF obj.lev = 0 THEN obj.export := TRUE; hasExport := TRUE
		ELSE Scanner.Mark ('Cannot export non-global identifiers')
		END;
		Scanner.Get (sym)
	END
END identdef;

PROCEDURE IdentList (VAR first : Base.Object; class : INTEGER);
	VAR obj : Base.Object;
BEGIN
	identdef (first, class);
	WHILE sym = Scanner.comma DO
		Scanner.Get (sym); identdef (obj, class);
		IF first = Base.guard THEN first := obj END
	END
END IdentList;

PROCEDURE Check_export (obj : Base.Object);
BEGIN
	IF ~ hasExport OR obj.export OR (obj.type.mod # -1) THEN (* Ok *)
	ELSE Scanner.Mark ('This type is not exported')
	END
END Check_export;
	
PROCEDURE FormalType (VAR typ : Base.Type);
	VAR obj : Base.Object;
BEGIN
	IF sym = Scanner.array THEN
		Base.New_typ (typ, Base.type_array);
		typ.len := -1;
		
		Scanner.Get (sym); Check (Scanner.of, 'No OF after ARRAY');
		FormalType (typ.base);
		
		IF (typ.base.form = Base.type_array) & (typ.base.len < 0) THEN
			typ.size := typ.base.size + Base.Word_size; typ.base.size := -1
		ELSE typ.size := Base.Word_size * 2
		END
	ELSE
		qualident (obj);
		IF (obj # Base.guard) & (obj.class = Base.class_type) THEN
			typ := obj.type; Check_export (obj)
		ELSE Scanner.Mark ('Type not found'); typ := Base.int_type
		END
	END
END FormalType;

PROCEDURE FormalParameters (VAR parblksize : INTEGER; VAR rtype : Base.Type);
	VAR obj : Base.Object;

	PROCEDURE FPSection (VAR parblksize : INTEGER);
		VAR first, obj : Base.Object; tp : Base.Type;
			cls, par_size : INTEGER; read_only, open_array : BOOLEAN;
	BEGIN
		IF sym # Scanner.var THEN cls := Base.class_var
		ELSE Scanner.Get (sym); cls := Base.class_ref
		END;
		
		ident (first, cls);			
		WHILE sym = Scanner.comma DO
			Scanner.Get (sym); ident (obj, cls);
			IF first = Base.guard THEN first := obj END
		END;
		Check (Scanner.colon, 'No colon after identifier list');
		
		par_size := Base.Word_size;
		read_only := FALSE; open_array := FALSE;
		FormalType (tp);
		IF tp.form IN Base.types_Scalar THEN (* Do nothing *)
		ELSE
			IF (cls = Base.class_var) & ((tp.size > 8)
				OR ~ (tp.size IN {1, 2, 4, 8}) OR ~ SymTable.importSystem) THEN
				cls := Base.class_ref; read_only := TRUE
			ELSIF tp.form = Base.type_record THEN
				par_size := Base.Word_size * 2
			END;
			IF (tp.form = Base.type_array) & (tp.len < 0) THEN
				par_size := tp.size; open_array := TRUE
			END
		END;
		
		WHILE first # Base.guard DO
			first.class := cls;
			first.val := parblksize + 16;
			first.type := tp;
			first.param := TRUE;
			first.readonly := read_only;
			IF open_array THEN first.val2 := SHORT(first.val) + 8 END;
			
			first := first.next;
			parblksize := parblksize + par_size
		END
	END FPSection;
		
BEGIN (* FormalParameters *)
	parblksize := 0;

	Scanner.Get (sym);
	IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN
		FPSection (parblksize);
		WHILE sym = Scanner.semicolon DO
			Scanner.Get (sym);
			IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN
				FPSection (parblksize)
			ELSE Scanner.Mark ('Superfluous semicolon')
			END
		END
	END;
	
	Check (Scanner.rparen, 'No closing )');
	
	IF sym = Scanner.colon THEN
		Scanner.Get (sym); qualident (obj);
		IF (obj # Base.guard) & (obj.class = Base.class_type) THEN
			IF obj.type.form IN Base.types_Scalar THEN
				rtype := obj.type; Check_export (obj)
			ELSIF SymTable.importSystem & (obj.type.size <= 8)
				& (obj.type.size IN {1, 2, 4, 8}) THEN
				rtype := obj.type; Check_export (obj)
			ELSE
				Scanner.Mark ('Invalid function result type');
				rtype := Base.int_type
			END
		ELSE Scanner.Mark ('Type not found'); rtype := Base.int_type
		END
	END
END FormalParameters;

PROCEDURE ArrayType (VAR typ : Base.Type);

	PROCEDURE length (typ : Base.Type);
		CONST err1 = 'Non-positive array length is not allowed';
			err2 = 'Array length must be const';
		VAR len : Base.Item;
	BEGIN
		expression (len);
		IF len.mode = Base.class_const THEN
			IF len.a > 0 THEN typ.len := SHORT (len.a)
			ELSE Scanner.Mark (err1); typ.len := 1
			END
		ELSE Scanner.Mark (err2); typ.len := 1
		END
	END length;
		
BEGIN (* ArrayType *)
	Base.New_typ (typ, Base.type_array);	
	Scanner.Get (sym); length (typ);
	
	IF sym = Scanner.comma THEN ArrayType (typ.base)
	ELSE Check (Scanner.of, 'OF expected'); type (typ.base)
	END;
	
	typ.size := typ.len * typ.base.size;
	typ.num_ptr := typ.len * typ.base.num_ptr;
	typ.alignment := typ.base.alignment;
	Base.Adjust_alignment (typ.size, typ.alignment);
	Generator.Check_varsize (typ.size, FALSE)
END ArrayType;

PROCEDURE RecordType (VAR typ : Base.Type);
	VAR obj : Base.Object;

	PROCEDURE FieldListSequence (typ : Base.Type);
		
		PROCEDURE FieldList (typ : Base.Type);
			VAR first : Base.Object; tp : Base.Type; offset : INTEGER;
		BEGIN
			IF sym = Scanner.ident THEN
				hasExport := FALSE; IdentList (first, Base.class_field);
				Check (Scanner.colon, 'No colon after identifier list');
				type (tp);
				
				IF tp.alignment > typ.alignment THEN
					typ.alignment := tp.alignment
				END;
				offset := typ.size;
				Base.Adjust_alignment (offset, tp.alignment);
				
				WHILE first # Base.guard DO
					first.val := offset;
					first.type := tp;
					first := first.next;
					
					INC (offset, tp.size);
					IF tp.num_ptr > 0 THEN INC (typ.num_ptr, tp.num_ptr) END
				END;
				typ.size := offset
			ELSE Scanner.Mark ('Superflous semicolon')
			END
		END FieldList;
		
	BEGIN (* FieldListSequence *)
		FieldList (typ);
		WHILE sym = Scanner.semicolon DO
			Scanner.Get (sym); FieldList (typ)
		END
	END FieldListSequence;
		
	PROCEDURE BaseType (typ : Base.Type);
		VAR obj : Base.Object; tp : Base.Type;
	BEGIN
		qualident (obj);
		
		IF (obj # Base.guard) & (obj.class = Base.class_type) THEN
			IF obj = defobj THEN Scanner.Mark ('Circular definition')
			ELSIF obj.type.form = Base.type_record THEN tp := obj.type
			ELSE Scanner.Mark ('Invalid record base type')
			END
		ELSE Scanner.Mark ('Not a type')
		END;
		
		IF tp # NIL THEN
			IF tp.len < Base.max_extension - 1 THEN
				typ.base := tp;
				typ.size := tp.size;
				typ.num_ptr := tp.num_ptr;
				typ.len := tp.len + 1;
				typ.alignment := tp.alignment
			ELSE Scanner.Mark ('Extension level too deep (compiler limit)')
			END
		END
	END BaseType;
		
BEGIN (* RecordType *)
	Base.New_typ (typ, Base.type_record);
	typ.size := 0; typ.len := 0; typ.alignment := 0; typ.num_ptr := 0;

	Scanner.Get (sym);
	IF sym = Scanner.lparen THEN
		Scanner.Get (sym); BaseType (typ);
		Check (Scanner.rparen, 'No closing )')
	END;
		
	SymTable.Open_scope ('');
	IF sym = Scanner.ident THEN
		FieldListSequence (typ);
		Base.Adjust_alignment (typ.size, typ.alignment)
	END;
	Generator.Check_varsize (typ.size, FALSE);
	Generator.Alloc_typedesc (typ);
	
	Check (Scanner.end, 'No END for record definition');
	typ.fields := SymTable.top_scope.next;
	SymTable.Close_scope
END RecordType;

PROCEDURE PointerType (VAR typ : Base.Type);
	VAR obj : Base.Object;
BEGIN
	Base.New_typ (typ, Base.type_pointer);
	typ.size := Base.Word_size; typ.num_ptr := 1;
	typ.alignment := Base.Word_size;
	
	Scanner.Get (sym); Check (Scanner.to, 'No TO in pointer definition');
		
	IF sym = Scanner.record THEN RecordType (typ.base)
	ELSE
		qualident (obj);
		IF obj = Base.guard THEN
			SymTable.Register_undef_type (typ, obj.name, hasExport)
		ELSIF (obj.class = Base.class_type)
		& (obj.type.form = Base.type_record) THEN
			typ.base := obj.type; Check_export (obj)
		ELSE Scanner.Mark ('Record type expected');
			typ.base := Base.guardRecord_type
		END
	END
END PointerType;

PROCEDURE ProcedureType (VAR typ : Base.Type);
BEGIN
	Base.New_typ (typ, Base.type_procedure);
	typ.size := Base.Word_size; typ.alignment := Base.Word_size;
	
	SymTable.Open_scope ('');
	Scanner.Get (sym);
	IF sym = Scanner.lparen THEN FormalParameters (typ.len, typ.base) END;
	typ.fields := SymTable.top_scope.next;
	SymTable.Close_scope
END ProcedureType;

PROCEDURE AddressType (VAR typ : Base.Type);
	VAR obj : Base.Object;
BEGIN
	Base.New_typ (typ, Base.type_address);
	typ.size := Base.Word_size; typ.alignment := Base.Word_size;
	typ.next := addressTypeList; addressTypeList := typ;
	
	Scanner.Get (sym); Check (Scanner.of, 'ADDRESS OF expected');
		
	IF sym = Scanner.record THEN RecordType (typ.base)
	ELSE
		qualident (obj);
		IF obj = Base.guard THEN
			SymTable.Register_undef_type (typ, obj.name, hasExport)
		ELSIF (obj.class = Base.class_type) THEN
			typ.base := obj.type; Check_export (obj)
		ELSE Scanner.Mark ('Not a defined type'); typ.base := Base.int_type
		END
	END
END AddressType;

PROCEDURE Get_address_type_of (basetp : Base.Type) : Base.Type;
	VAR p : Base.Type;
BEGIN
	p := addressTypeList;
	WHILE (p # NIL) & (p.base # basetp) DO p := p.next
	END;
	IF p = NIL THEN Base.New_typ (p, Base.type_address);
		p.size := Base.Word_size; p.alignment := p.size; p.base := basetp;
		p.next := addressTypeList; addressTypeList := p
	END;
	RETURN p
END Get_address_type_of;
	
PROCEDURE type (VAR typ : Base.Type);
	CONST err2 = 'This identifier is not a type';
		ptrTypes = {Base.type_pointer, Base.type_address};
	VAR obj : Base.Object;
BEGIN
	typ := Base.int_type;
	IF sym = Scanner.ident THEN
		qualident (obj);
		IF obj = Base.guard THEN Scanner.Mark ('Undefined type')
		ELSIF obj.class # Base.class_type THEN Scanner.Mark (err2)
		ELSIF (obj # defobj) OR (obj.type.form IN ptrTypes) THEN
			typ := obj.type; Check_export (obj)
		ELSE Scanner.Mark ('Circular definition')
		END
	ELSIF sym = Scanner.array THEN ArrayType (typ)
	ELSIF sym = Scanner.record THEN RecordType (typ)
	ELSIF sym = Scanner.pointer THEN PointerType (typ)
	ELSIF sym = Scanner.procedure THEN ProcedureType (typ)
	ELSIF sym = Scanner.address THEN AddressType (typ)
	ELSE Scanner.Mark ('Expect a type or type definition')
	END
END type;

PROCEDURE DeclarationSequence (VAR varsize : INTEGER);

	PROCEDURE ConstDeclaration;
		VAR obj : Base.Object; x : Base.Item;
	BEGIN
		identdef (obj, Base.class_head); (* Defense again circular definition *)
		Check (Scanner.equal, 'No = in const declaration');
		expression (x);
		
		IF x.mode = Base.class_const THEN
			obj.class := Base.class_const;
			obj.type := x.type;
			obj.val := x.a
		ELSIF (x.mode = Base.class_var) & (x.type.form = Base.type_string) THEN
			obj.readonly := TRUE;
			obj.class := Base.class_var;
			obj.lev := x.lev;
			obj.type := x.type;
			obj.val := x.a
		ELSE
			Scanner.Mark ('Expect a const expression');
			obj.class := Base.class_const;
			obj.type := Base.int_type;
			obj.val := 0
		END
	END ConstDeclaration;
		
	PROCEDURE TypeDeclaration;
		VAR obj : Base.Object; error : INTEGER;
	BEGIN
		hasExport := FALSE; identdef (obj, Base.class_type);
		Check (Scanner.equal, 'No = in type declaration');
		defobj := obj; type (obj.type);
		IF obj.type.obj = NIL THEN obj.type.obj := obj
		END;
		IF (obj.type.form = Base.type_record)
			& (SymTable.undef_ptr_list # NIL) THEN
			SymTable.Check_undef_list (obj)
		END
	END TypeDeclaration;
	
	PROCEDURE VariableDeclaration (VAR varsize : INTEGER);
		VAR first : Base.Object; tp : Base.Type;
	BEGIN
		hasExport := FALSE; IdentList (first, Base.class_var);
		Check (Scanner.colon, 'No colon after identifier list');
		defobj := NIL; type (tp);
				
		Base.Adjust_alignment (varsize, tp.alignment);
		WHILE first # Base.guard DO
			INC (varsize, tp.size);
			first.val := -varsize;
			first.type := tp;
			first := first.next
		END
	END VariableDeclaration;
		
	PROCEDURE ProcedureDeclaration;
		VAR proc : Base.Object;
			
		PROCEDURE ProcedureHeading (VAR proc : Base.Object);
			VAR typ : Base.Type;
				
			PROCEDURE Copy_param_list (proc : Base.Object);
				VAR src, first, dst : Base.Object;
			BEGIN
				first := NIL; src := SymTable.top_scope.next;
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
			Base.New_typ (typ, Base.type_procedure);
			typ.size := Base.Word_size; typ.alignment := Base.Word_size;
			
			SymTable.Open_scope (proc.name);
			SymTable.Inc_level (1);
			IF sym = Scanner.lparen THEN FormalParameters (typ.len, typ.base)
			ELSE typ.len := 0
			END;
			Copy_param_list (proc); typ.fields := proc.dsc; proc.type := typ
		END ProcedureHeading;
			
		PROCEDURE ProcedureBody (proc : Base.Object);
			VAR locblksize : INTEGER; x : Base.Item; restype : Base.Type;
		BEGIN
			locblksize := 0; proc.val := Generator.ip;
			DeclarationSequence (locblksize);
			Generator.Enter (proc, locblksize);
			IF sym = Scanner.begin THEN
				Scanner.Get (sym); StatementSequence
			END;
			
			restype := proc.type.base;
			IF sym = Scanner.return THEN
				Scanner.Get (sym); expression (x);
				IF restype # NIL THEN Check_assignment (restype, x)
				ELSE Scanner.Mark ('Proper procedure do not need RETURN')
				END;
				Generator.load (x)
			ELSIF restype # NIL THEN
				Scanner.Mark ('No return value for function procedure')
			END;
			
			Check (Scanner.end, 'No END for procedure body');
			Generator.Return;
			
			SymTable.Inc_level (-1);
			SymTable.Close_scope
		END ProcedureBody;
			
	BEGIN (* ProcedureDeclaration *)
		hasExport := FALSE; ProcedureHeading (proc);
		Check (Scanner.semicolon, 'No ; after procedure heading');
		ProcedureBody (proc);
		IF sym = Scanner.ident THEN
			IF proc.name = Scanner.id THEN (* Ok *)
			ELSE Scanner.Mark ('Wrong name of procedure')
			END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('Procedure must end with its name')
		END
	END ProcedureDeclaration;

BEGIN (* DeclarationSequence *)
	IF sym = Scanner.const THEN
		Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			ConstDeclaration;
			Check (Scanner.semicolon, 'No ; after const declaration')
		END
	END;
	IF sym = Scanner.type THEN
		Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			TypeDeclaration;
			Check (Scanner.semicolon, 'No ; after type declaration')
		END;
		IF SymTable.undef_ptr_list # NIL THEN SymTable.Cleanup_undef_list END
	END;
	IF sym = Scanner.var THEN
		Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			VariableDeclaration (varsize);
			Check (Scanner.semicolon, 'No ; after variable declaration')
		END;
		Generator.Check_varsize (varsize, SymTable.cur_lev = 0)
	END;
	IF (sym = Scanner.procedure) & (SymTable.cur_lev = 1) THEN
		Generator.Placeholder_proc
	END;
	WHILE sym = Scanner.procedure DO
		ProcedureDeclaration;
		Check (Scanner.semicolon, 'No ; after procedure declaration')
	END
END DeclarationSequence;

PROCEDURE Parameter (VAR pinfo : Generator.ProcInfo; par : Base.Object);
	CONST err1 = 'Not compatible with formal type';
	VAR x : Base.Item; ftype, xtype : Base.Type;
BEGIN
	expression (x);
	IF par.class = Base.class_var THEN
		Check_val (x); Check_assignment (par.type, x);
		Generator.Value_param (x, pinfo)
	ELSE
		ftype := par.type; xtype := x.type; Check_var (x, par.readonly);
		IF (ftype.form = Base.type_array) & (ftype.len < 0) THEN
			IF (xtype.form IN {Base.type_array, Base.type_string})
			& Base.Compatible_array (xtype, ftype) THEN
				Generator.Open_array_param (x, pinfo, ftype)
			ELSIF (ftype.base = Base.byte_type) & SymTable.importSystem THEN
				Base.byteArray_type.len := x.type.size;
				Base.byteArray_type.size := x.type.size;
				x.type := Base.byteArray_type;
				Generator.Open_array_param (x, pinfo, ftype)
			ELSE Scanner.Mark (err1); Generator.Free_item (x)
			END
		ELSIF ~ par.readonly THEN
			IF xtype = ftype THEN Generator.Ref_param (x, pinfo)
			ELSIF {ftype.form, xtype.form} = {Base.type_record} THEN
				IF Base.Is_extension (xtype, ftype) THEN (* Ok *)					
				ELSE Scanner.Mark ('Actual type is not an extension of formal')
				END;
				Generator.Record_var_param (x, pinfo)
			ELSE Scanner.Mark (err1); Generator.Free_item (x)
			END
		ELSE Check_assignment (ftype, x); Generator.Ref_param (x, pinfo)
		END
	END
END Parameter;

PROCEDURE Skip_params;
	VAR x : Base.Item;
BEGIN
	REPEAT Scanner.Get (sym); expression (x); Generator.Free_item (x)
	UNTIL sym # Scanner.comma
END Skip_params;
	
PROCEDURE ActualParameters (VAR x : Base.Item; VAR pinfo : Generator.ProcInfo);
	VAR par : Base.Object;	
BEGIN
	par := x.type.fields; Scanner.Get (sym);
	IF sym # Scanner.rparen THEN
		IF par # Base.guard THEN Parameter (pinfo, par); par := par.next
		ELSE Scanner.Mark ('This procedure does not need parameters')
		END;
		WHILE (par # Base.guard) & (sym = Scanner.comma) DO
			Scanner.Get (sym); Parameter (pinfo, par); par := par.next
		END;
		IF sym = Scanner.comma THEN Scanner.Mark (errTooMuchParam); Skip_params
		END
	END;
	IF par # Base.guard THEN Scanner.Mark (errTooLittleParam) END;
	Check (Scanner.rparen, 'No closing )')
END ActualParameters;

PROCEDURE ProcedureCall (VAR x : Base.Item; proper : BOOLEAN);
	CONST err1 = 'Function procedure must be called in expression';
		err2 = 'Proper procedure cannot be called in expression';
	VAR pinfo : Generator.ProcInfo; has_error : BOOLEAN; rtype : Base.Type;
BEGIN
	pinfo.parblksize := x.type.len;
	rtype := x.type.base; pinfo.rtype := rtype;
	
	has_error := TRUE;
	IF proper THEN
		IF rtype = NIL THEN has_error := FALSE ELSE Scanner.Mark (err1) END
	ELSIF rtype # NIL THEN has_error := FALSE
	ELSE Scanner.Mark (err2)
	END;
	
	Generator.Prepare_to_call (x, pinfo);
	IF sym = Scanner.lparen THEN ActualParameters (x, pinfo)
	ELSIF x.type.len > 0 THEN Scanner.Mark ('Not enough parameters')
	END;
	Generator.Call (x, pinfo);
	
	IF has_error THEN
		IF proper THEN Generator.Free_item (x)
		ELSE Generator.Make_const (x, Base.int_type, 0)
		END
	END
END ProcedureCall;

PROCEDURE StandProc (VAR x : Base.Item);
	VAR y : Base.Item;
	
	PROCEDURE SProc_INC (op : INTEGER);
		VAR x, y, z : Base.Item;
	BEGIN
		expression (x); Check_var (x, FALSE); Check_int (x);
		IF sym # Scanner.comma THEN
			IF op = Scanner.plus THEN Generator.SProc_INC (x, 1)
			ELSE Generator.SProc_INC (x, -1)
			END
		ELSE
			Generator.load2 (x, y);
			Scanner.Get (sym); expression (z);
			Generator.Op2 (op, y, z); Generator.Store (x, y)
		END
	END SProc_INC;
	
	PROCEDURE SProc_INCL (op : INTEGER);
		VAR x, y, z, t : Base.Item;
	BEGIN
		expression (x); Check_var (x, FALSE); Check_set (x);
		Check (Scanner.comma, errTooLittleParam);
		Generator.load2 (x, y); Generator.Make_const (z, Base.set_type, 0);
		expression (t); Check_set_element (t);
		Generator.Set2 (z, t); Generator.Op2 (op, y, z); Generator.Store (x, y)
	END SProc_INCL;
	
	PROCEDURE SProc_NEW;
		VAR x : Base.Item;
	BEGIN
		expression (x); Check_var (x, FALSE); Check_pointer (x);
		Generator.SProc_NEW (x)
	END SProc_NEW;
	
	PROCEDURE SProc_ASSERT;
		VAR x : Base.Item;
	BEGIN
		expression (x); Check_bool (x); Generator.Assert (x)
	END SProc_ASSERT;
	
	PROCEDURE SProc_DISPOSE;
		VAR x, proc : Base.Item; pinfo : Generator.ProcInfo;
	BEGIN
		Generator.SProc_DISPOSE (proc, pinfo);
		expression (x); Check_pointer (x); Generator.Value_param (x, pinfo);
		Generator.Call (proc, pinfo)
	END SProc_DISPOSE;
		
	PROCEDURE SProc_GET;
		VAR x, y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_var (y, FALSE); Check_scalar (y);
		Generator.SProc_GET (x, y)
	END SProc_GET;
	
	PROCEDURE SProc_PUT;
		VAR x, y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_scalar (y);
		Generator.SProc_PUT (x, y)
	END SProc_PUT;
	
	PROCEDURE SProc_COPY;
		VAR x, y, z : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_int (y);
		IF y.mode IN classes_Variable THEN Generator.load (y) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (z); Check_int (z);
		Generator.SProc_COPY (x, y, z)
	END SProc_COPY;
		
	PROCEDURE SProc_LoadLibrary (adr : INTEGER);
		VAR x, y, proc : Base.Item; pinfo : Generator.ProcInfo;
	BEGIN
		expression (x); Check_var (x, FALSE);
		IF (x.type = Base.int_type) THEN (* Ok *)
		ELSE Scanner.Mark ('Not an INTEGER variable'); x.type := Base.int_type
		END;

		proc.mode := Base.class_proc; proc.type := Base.int_type;
		proc.lev := -2; proc.a := adr; pinfo.parblksize := 8;
		pinfo.rtype := Base.int_type; Generator.Prepare_to_call (proc, pinfo);
		
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_var (y, TRUE);
		IF (y.type.form = Base.type_string)
			OR (y.type.form = Base.type_array)
			& (y.type.base = Base.char_type) THEN (* Ok *)
		ELSE Scanner.Mark ('Not a string or char array')
		END;
		Generator.Ref_param (y, pinfo);
		
		Generator.Call (proc, pinfo); Generator.Store (x, proc)
	END SProc_LoadLibrary;
		
	PROCEDURE SProc_GetProcAddress (adr : INTEGER);
		VAR x, y, z, proc : Base.Item; pinfo : Generator.ProcInfo;
	BEGIN
		expression (x); Check_var (x, FALSE);
		IF x.type.form IN {Base.type_procedure, Base.type_address} THEN (* Ok *)
		ELSE Scanner.Mark ('Expect a procedure or address variable');
			x.type := Base.int_type
		END;
		
		proc.mode := Base.class_proc; proc.type := Base.int_type;
		proc.lev := -2; proc.a := adr; pinfo.parblksize := 16;
		pinfo.rtype := Base.int_type; Generator.Prepare_to_call (proc, pinfo);
		
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_int (y); Generator.Value_param (y, pinfo);
		
		Check (Scanner.comma, errTooLittleParam);
		expression (z);
		IF (z.type.form = Base.type_integer)
			OR (z.type.form = Base.type_address)
			& (z.type.base = Base.byte_type) THEN (* Ok *)
		ELSE Scanner.Mark ('Not an integer or ADDRESS OF BYTE value');
			z.type := Base.int_type
		END;
		Generator.Value_param (z, pinfo);
		
		Generator.Call (proc, pinfo); Generator.Store (x, proc)
	END SProc_GetProcAddress;

BEGIN (* StandProc *)
	Check (Scanner.lparen, 'Not enough parameter or missing (');
	IF x.type = NIL THEN
		CASE SHORT (x.a) OF
			0: SProc_INC (Scanner.plus) |
			1: SProc_INC (Scanner.minus) |
			2: SProc_INCL (Scanner.plus) |
			3: SProc_INCL (Scanner.minus) |
			4: SProc_NEW |
			5: SProc_ASSERT |
			8: SProc_DISPOSE |
			100: SProc_GET |
			101: SProc_PUT |
			102: SProc_COPY |
			103: SProc_LoadLibrary (x.b) |
			104: SProc_GetProcAddress (x.b)
		END;
		IF sym = Scanner.comma THEN
			Scanner.Mark (errTooMuchParam); Skip_params
		END;
		Check (Scanner.rparen, 'No closing )')
	ELSE Scanner.Mark ('Function procedure must be called in expression')
	END
END StandProc;

PROCEDURE StandFunc (VAR x : Base.Item);
	VAR funcno : INTEGER; rtype : Base.Type; y : Base.Item;

	PROCEDURE SFunc_ABS (VAR x : Base.Item);
	BEGIN expression (x); Check_val (x);
		IF x.type.form IN Base.types_Numberic THEN (* Ok *)
		ELSE Scanner.Mark ('Expect a numberic value'); x.type := Base.int_type
		END;
		Generator.SFunc_ABS (x)
	END SFunc_ABS;
		
	PROCEDURE SFunc_ODD (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x); Generator.SFunc_ODD (x)
	END SFunc_ODD;
		
	PROCEDURE SFunc_LEN (VAR x : Base.Item);
	BEGIN expression (x); Check_var (x, TRUE);
		IF x.type.form = Base.type_array THEN (* Ok *)
		ELSE Scanner.Mark ('Not an array'); x.type := Base.guardArray_type
		END;
		Generator.SFunc_LEN (x)
	END SFunc_LEN;
	
	PROCEDURE SFunc_SHIFT (shf : INTEGER; VAR x : Base.Item);
		VAR y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_int (y);
		Generator.SFunc_SHIFT (shf, x, y)
	END SFunc_SHIFT;
	
	PROCEDURE SFunc_FLOOR (VAR x : Base.Item);
	BEGIN
		expression (x); Check_real (x); Generator.SFunc_FLOOR (x)
	END SFunc_FLOOR;
	
	PROCEDURE SFunc_FLT (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x); Generator.SFunc_FLT (x)
	END SFunc_FLT;
		
	PROCEDURE SFunc_ORD (VAR x : Base.Item);
		CONST valid_types = {Base.type_char, Base.type_set, Base.type_boolean};
		VAR no_error : BOOLEAN;
	BEGIN
		expression (x); Check_val (x);
		IF x.type.form IN valid_types THEN (* Do nothing *)
		ELSIF (x.type.form = Base.type_string) & (x.type.len <= 2) THEN
			x.mode := Base.class_const; x.a := x.type.charVal
		ELSE Scanner.Mark ('Not a character, set or boolean value');
			x.type := Base.int_type
		END;
		IF x.mode IN classes_Variable THEN Generator.load (x) END
	END SFunc_ORD;
		
	PROCEDURE SFunc_CHR (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END
	END SFunc_CHR;
		
	PROCEDURE SFunc_ADR (VAR x : Base.Item);
	BEGIN
		expression (x); Check_var (x, TRUE); Generator.SFunc_ADR (x)
	END SFunc_ADR;
		
	PROCEDURE SFunc_SIZE (VAR x : Base.Item);
		VAR size : INTEGER;
	BEGIN
		expression (x);
		IF x.mode = Base.class_type THEN size := x.type.size
		ELSE Scanner.Mark ('Expect a type identifier');
			Generator.Free_item (x); size := 0
		END;
		Generator.Make_const (x, Base.int_type, size)
	END SFunc_SIZE;
	
	PROCEDURE SFunc_BIT (VAR x : Base.Item);
		VAR y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_set_element (y);
		Generator.SFunc_BIT (x, y)
	END SFunc_BIT;
		
	PROCEDURE SFunc_VAL (VAR x : Base.Item);
		VAR y : Base.Item;
			cast_type : Base.Type;
	BEGIN
		expression (x);
		IF (x.mode = Base.class_type) & (x.type.form IN Base.types_Scalar) THEN
			cast_type := x.type
		ELSE Scanner.Mark ('Expect a scalar type identifier');
			cast_type := Base.int_type; Generator.Free_item (x)
		END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_val (y); x := y; x.type := cast_type
	END SFunc_VAL;
	
	PROCEDURE SFunc_ADR2 (VAR x : Base.Item);
	BEGIN
		expression (x); Check_var (x, TRUE); Generator.SFunc_ADR (x);
		x.type := Get_address_type_of (x.type)
	END SFunc_ADR2;
	
	PROCEDURE SFunc_STRADR (VAR x : Base.Item);
	BEGIN
		expression (x); Check_var (x, TRUE);
		IF (x.type = Base.char_type) OR (x.type.form = Base.type_string)
			OR (x.type.form = Base.type_array) & (x.type.base = Base.char_type)
			THEN (* Ok *)
		ELSE Scanner.Mark ('Expect a string or character')
		END;
		Generator.SFunc_ADR (x); x.type := Get_address_type_of (Base.char_type)
	END SFunc_STRADR;
		
BEGIN (* StandFunc *)
	Scanner.Get (sym);
	IF x.type # NIL THEN
		funcno := SHORT (x.a); rtype := x.type;
		CASE funcno OF
			200: SFunc_ABS (x) |
			201: SFunc_ODD (x) |
			202: SFunc_LEN (x) |
			203 .. 205: SFunc_SHIFT (funcno - 203, x) |
			206: SFunc_FLOOR (x) |
			207: SFunc_FLT (x) |
			208: SFunc_ORD (x) |
			209: SFunc_CHR (x) |
			300: SFunc_ADR (x) |
			301: SFunc_SIZE (x) |
			302: SFunc_BIT (x) |
			303: SFunc_VAL (x) |
			310: SFunc_ADR2 (x) |
			311: SFunc_STRADR (x) |
		END;
		IF (funcno # 303) & (funcno # 200) & (funcno # 310) & (funcno # 311)
			THEN x.type := rtype 
		END;
		IF sym = Scanner.comma THEN Scanner.Mark (errTooMuchParam); Skip_params
		END;
		Check (Scanner.rparen, 'No closing )')
	ELSE Scanner.Mark ('Proper procedure cannot be called in expression');
		Generator.Make_const (x, Base.int_type, 0)
	END
END StandFunc;
	
PROCEDURE selector (VAR x : Base.Item);
		
	PROCEDURE Field_selector (VAR x : Base.Item);
		VAR obj : Base.Object; tp : Base.Type;
	BEGIN
		IF x.type.form IN Base.types_Pointer THEN Generator.Deref (x) END;
		Scanner.Get (sym);
		IF x.type.form = Base.type_record THEN
			IF sym = Scanner.ident THEN
				tp := x.type;
				REPEAT SymTable.Find_field (obj, Scanner.id, tp); tp := tp.base
				UNTIL (obj # Base.guard) OR (tp = NIL);
				IF obj # Base.guard THEN Generator.Field (x, obj)
				ELSE Scanner.Mark ('Record field not found')
				END;
				Scanner.Get (sym)
			ELSE Scanner.Mark ('Expect a record field identifier')
			END
		ELSE Scanner.Mark ('Not a record or pointer but found . selector');
			IF sym = Scanner.ident THEN Scanner.Get (sym) END
		END
	END Field_selector;
	
	PROCEDURE Element_selector (VAR x : Base.Item);
		VAR y : Base.Item;
	BEGIN
		IF x.type.form = Base.type_address THEN Generator.Deref (x)
		END;
		IF x.type.form = Base.type_array THEN
			REPEAT Scanner.Get (sym); expression (y); Check_int (y);
				IF x.type.form = Base.type_array THEN Generator.Index (x, y)
				ELSE Scanner.Mark ('Wrong dimension')
				END
			UNTIL sym # Scanner.comma
		ELSE
			Scanner.Mark ('Not an array but found [ selector');
			REPEAT Scanner.Get (sym); expression (y); Generator.Free_item (y)
			UNTIL sym # Scanner.comma
		END;
		Check (Scanner.rbrak, 'No closing ]')
	END Element_selector;
	
	PROCEDURE Deref_selector (VAR x : Base.Item);
	BEGIN
		Scanner.Get (sym);
		IF x.type.form IN Base.types_Pointer THEN Generator.Deref (x)
		ELSE Scanner.Mark ('Not a pointer but found ^ selector')
		END
	END Deref_selector;
	
	PROCEDURE Type_guard_selector (VAR x : Base.Item);
		VAR obj : Base.Object; noerror : BOOLEAN; xform : INTEGER;
			xtype, ytype : Base.Type;
	BEGIN
		ASSERT (Base.Type_test_applicable (x));
		xtype := x.type; xform := xtype.form;
		Scanner.Get (sym); qualident (obj);	
		IF obj.class = Base.class_type THEN ytype := obj.type;
			IF xform = ytype.form THEN
				IF xform = Base.type_pointer THEN
					ytype := ytype.base; xtype := xtype.base
				END;
				IF Base.Is_extension (ytype, xtype) THEN (* Ok *)
				ELSE Scanner.Mark ('Not an extension type'); ytype := xtype
				END;
				Generator.Type_test (x, ytype, TRUE)
			ELSE Scanner.Mark (errWrongType); Generator.Free_item (x)
			END
		ELSE Scanner.Mark ('Not a type'); Generator.Free_item (x)
		END;
		Check (Scanner.rparen, 'No closing )')
	END Type_guard_selector;
		
BEGIN
	Check_val (x);
	IF sym = Scanner.period THEN Field_selector (x)
	ELSIF sym = Scanner.lbrak THEN Element_selector (x)
	ELSIF sym = Scanner.arrow THEN Deref_selector (x)
	ELSIF sym = Scanner.lparen THEN Type_guard_selector (x)
	ELSE ASSERT(FALSE)
	END
END selector;
	
PROCEDURE designator (VAR x : Base.Item);
	CONST classes_Proc = {Base.class_proc, Base.class_sproc};
	VAR obj : Base.Object; exit, is_procedure : BOOLEAN;
BEGIN
	qualident (obj);
	IF obj # Base.guard THEN Generator.Make_item (x, obj); exit := FALSE;
		REPEAT
			IF (sym = Scanner.lparen) & Base.Type_test_applicable (x) THEN
				selector (x)
			ELSIF (sym = Scanner.period) OR (sym = Scanner.lbrak)
				OR (sym = Scanner.arrow) THEN selector (x)
			ELSE exit := TRUE
			END
		UNTIL exit
	ELSE Scanner.Mark ('Identifier not defined or invisible to this scope');
		Generator.Make_const (x, Base.int_type, 0)
	END
END designator;
	
PROCEDURE set (VAR x : Base.Item);

	PROCEDURE element (VAR x : Base.Item);
		VAR y, z : Base.Item;
	BEGIN
		expression (y); Check_set_element (y);
		IF sym = Scanner.upto THEN
			IF y.mode # Base.class_const THEN Generator.load (y) END;
			Scanner.Get (sym); expression (z); Check_set_element (z);
			Generator.Set3 (x, y, z)
		ELSE Generator.Set2 (x, y)
		END
	END element;

BEGIN (* set *)
	Scanner.Get (sym); Generator.Make_const (x, Base.set_type, 0);
	IF sym # Scanner.rbrace THEN element (x);
		WHILE sym = Scanner.comma DO Scanner.Get (sym); element (x) END;
		Generator.Set1 (x); Check (Scanner.rbrace, 'No closing }')
	ELSE Scanner.Get (sym)
	END
END set;
	
PROCEDURE factor (VAR x : Base.Item);
BEGIN
	IF sym = Scanner.number THEN
		Generator.Make_const (x, Scanner.typeOfVal, Scanner.val);
		Scanner.Get (sym)
	ELSIF sym = Scanner.ident THEN
		designator (x);
		IF sym = Scanner.lparen THEN
			IF x.mode = Base.class_sproc THEN StandFunc (x)
			ELSIF (x.mode = Base.class_proc) OR (x.mode IN classes_Value)
			& (x.type.form = Base.type_procedure) THEN
				ProcedureCall (x, FALSE)
			ELSE Scanner.Mark ('Found ( but designator is not a procedure')
			END
		END
	ELSIF sym = Scanner.string THEN
		Generator.Make_string (x, Scanner.str); Scanner.Get (sym)
	ELSIF sym = Scanner.nil THEN
		Generator.Make_const (x, Base.nil_type, 0); Scanner.Get (sym)
	ELSIF sym = Scanner.true THEN
		Generator.Make_const (x, Base.bool_type, 1); Scanner.Get (sym)
	ELSIF sym = Scanner.false THEN
		Generator.Make_const (x, Base.bool_type, 0); Scanner.Get (sym)
	ELSIF sym = Scanner.lbrace THEN set (x)
	ELSIF sym = Scanner.lparen THEN Scanner.Get (sym); expression (x);
		Check (Scanner.rparen, 'No closing )')
	ELSIF sym = Scanner.not THEN Scanner.Get (sym); factor (x);
		Check_operand (Scanner.not, x); Generator.Op1 (Scanner.not, x)
	END
END factor;
	
PROCEDURE term (VAR x : Base.Item);
	CONST err1 = 'Incompatible types';
	VAR op : INTEGER; y : Base.Item;
BEGIN
	factor (x);
	WHILE (sym >= Scanner.times) & (sym <= Scanner.and) DO
		op := sym; Check_operand (op, x);
		
		IF x.mode IN Base.cls_Variable THEN Generator.load (x) END;
		IF op = Scanner.and THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym); factor (y); Check_operand (op, y);
		
		IF x.type.form = y.type.form THEN Generator.Op2 (op, x, y)
		ELSE Scanner.Mark (err1); Generator.Free_item (y)
		END
	END
END term;
	
PROCEDURE SimpleExpression (VAR x : Base.Item);
	CONST err1 = 'Incompatible types';
	VAR op : INTEGER; y : Base.Item;
BEGIN
	IF sym = Scanner.plus THEN
		Scanner.Get (sym); term (x); Check_operand (Scanner.plus, x)
	ELSIF sym = Scanner.minus THEN
		Scanner.Get (sym); term (x); Check_operand (Scanner.minus, x);
		Generator.Op1 (Scanner.minus, x)
	ELSE term (x)
	END;
		
	WHILE (sym >= Scanner.plus) & (sym <= Scanner.or) DO
		op := sym; Check_operand (op, x);
		
		IF x.mode IN Base.cls_Variable THEN Generator.load (x) END;
		IF op = Scanner.or THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym); term (y); Check_operand (op, y);
		
		IF x.type.form = y.type.form THEN Generator.Op2 (op, x, y)
		ELSE Scanner.Mark (err1); Generator.Free_item (y)
		END
	END
END SimpleExpression;
	
PROCEDURE expression (VAR x : Base.Item);
	VAR op, x_form : INTEGER;
		y : Base.Item;
		
	PROCEDURE Check_for_string_const (VAR x, y : Base.Item);
	BEGIN
		IF (x.mode IN classes_Value) & (x.type = Base.char_type)
			& (y.type # Base.char_type) THEN
			Generator.Make_const (y, Base.char_type, y.type.charVal)
		ELSIF (y.mode IN classes_Value) & (y.type = Base.char_type)
			& (x.type # Base.char_type) THEN
			Generator.Make_const (x, Base.char_type, x.type.charVal)
		END
	END Check_for_string_const;
		
BEGIN (* expression *)
	SimpleExpression (x);
	IF (sym >= Scanner.equal) & (sym <= Scanner.is) THEN
		op := sym; Check_left_hand_side (op, x);
		
		IF x.mode = Base.mode_cond THEN Generator.load (x)
		ELSIF x.mode IN classes_Variable THEN
			IF (op # Scanner.is) & (x.type.form IN Base.types_Scalar) THEN
				Generator.load (x)
			END
		END;
		
		Scanner.Get (sym); SimpleExpression (y);

		IF op = Scanner.in THEN
			Check_set (y); Generator.Membership (x, y)
		ELSIF op = Scanner.is THEN
			Check_type_test (x, y); Generator.Type_test (x, y.type, FALSE)
		ELSIF (op = Scanner.equal) OR (op = Scanner.not_equal) THEN
			Check_comparison (x, y, TRUE);
			Generator.Comparison (op, x, y)
		ELSIF x.type # Base.set_type THEN
			Check_comparison (x, y, FALSE);
			Generator.Comparison (op, x, y)
		ELSE
			Check_set (y); Generator.Inclusion (op, x, y)
		END
	END
END expression;

(*	
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
		
	Check (Scanner.becomes, 'Expect :=');
	expression (beg); Check_int (beg);
	
	Check (Scanner.to, 'No TO in FOR statement');
	expression (end); Check_int (end);
	
	IF sym = Scanner.by THEN
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
	
	Check (Scanner.do, 'No DO in FOR statement');
	L := 0; Generator.Begin_FOR (x, beg, end, rel, L, SHORT (inc.a));
	StatementSequence;
	Check (Scanner.end, 'No END for FOR statement');
	Generator.End_FOR (x, rel, inc, L)
END ForStatement;
*)
	
PROCEDURE StatementSequence;
	CONST err1 = 'Invalid statement'; err3 = 'Missing semicolon?';
		err4 = 'No THEN after IF condition';
		err5 = 'No THEN after ELSIF condition';
		err6 = 'No END for structured statement';
		err7 = 'No DO after WHILE condition';
		err8 = 'No DO after ELSIF condition';
		err9 = 'No UNTIL for REPEAT statement';
	VAR x, y : Base.Item; L : INTEGER;
BEGIN
	REPEAT
		IF ~ ((sym = Scanner.ident)
				OR (sym >= Scanner.if) & (sym <= Scanner.for)
				OR (sym >= Scanner.semicolon)) THEN
			Scanner.Mark (err1);
			REPEAT Scanner.Get (sym)
			UNTIL (sym = Scanner.ident) OR (sym >= Scanner.if)
		END;
		
		IF sym = Scanner.ident THEN
			designator (x);
			IF sym = Scanner.becomes THEN Check_assignment_dest (x);
				Scanner.Get (sym); expression (y); Check_val (y);
				Check_assignment (x.type, y); Generator.Store (x, y)
			ELSIF x.mode = Base.class_proc THEN ProcedureCall (x, TRUE)
			ELSIF x.mode = Base.class_sproc THEN StandProc (x)
			ELSIF (sym = Scanner.lparen) & (x.mode IN classes_Value)
			& (x.type.form = Base.type_procedure) THEN
				ProcedureCall (x, TRUE)
			ELSE Scanner.Mark (err1);
				REPEAT Scanner.Get (sym)
				UNTIL (sym >= Scanner.if) & (sym <= Scanner.case)
					OR (sym >= Scanner.semicolon)
			END
		ELSIF sym = Scanner.if THEN
			L := 0; Scanner.Get (sym); expression (x); Check_bool (x);
			Generator.CFJump (x); Check (Scanner.then, err4);
			StatementSequence;
			WHILE sym = Scanner.elsif DO
				Generator.FJump (L); Generator.Fix_link (SHORT (x.a));
				Scanner.Get (sym); expression (x); Check_bool (x);
				Generator.CFJump (x); Check (Scanner.then, err5);
				StatementSequence
			END;			
			IF sym = Scanner.else THEN
				Generator.FJump (L); Generator.Fix_link (SHORT (x.a));
				Scanner.Get (sym); StatementSequence
			ELSE Generator.Fix_link (SHORT (x.a))
			END;
			Generator.Fix_link (L); Check (Scanner.end, err6)
		ELSIF sym = Scanner.while THEN
			L := Generator.pc;
			Scanner.Get (sym); expression (x); Check_bool (x);
			Generator.CFJump (x); Check (Scanner.do, err7);
			StatementSequence;
			WHILE sym = Scanner.elsif DO
				Generator.BJump (L); Generator.Fix_link (SHORT (x.a));
				Scanner.Get (sym); expression (x); Check_bool (x);
				Generator.CFJump (x); Check (Scanner.do, err8);
				StatementSequence
			END;
			Generator.BJump (L); Generator.Fix_link (SHORT (x.a));
			Check (Scanner.end, err6)
		ELSIF sym = Scanner.repeat THEN
			L := Generator.pc; Scanner.Get (sym); StatementSequence;
			Check (Scanner.until, err9);
			expression (x); Check_bool (x); Generator.CBJump (x, L)
		ELSIF sym = Scanner.for THEN
			Scanner.Mark ('FOR statement not supported yet!')
			(* Implement later *)
		ELSIF sym = Scanner.case THEN
			Scanner.Mark ('CASE statement not supported yet!')
			(* Implement later *)
		END;
		
		IF sym = Scanner.semicolon THEN Scanner.Get(sym)
		ELSIF sym < Scanner.semicolon THEN Scanner.Mark (err3)
		END
	UNTIL sym > Scanner.semicolon
END StatementSequence;

PROCEDURE ImportList;

	PROCEDURE import;
		VAR modul : Base.Object; sym_name : Base.String; error : BOOLEAN;
	BEGIN
		ident (modul, Base.class_module);
		IF sym = Scanner.becomes THEN
			Scanner.Get (sym);
			IF sym = Scanner.ident THEN
				sym_name := Scanner.id; Scanner.Get (sym)
			ELSE Scanner.Mark ('Expect an identifier'); sym_name := modul.name
			END
		ELSE sym_name := modul.name
		END;
		SymTable.Find_module_symfile (modul, sym_name)
	END import;

BEGIN (* ImportList *)
	Scanner.Get (sym); import;
	WHILE sym = Scanner.comma DO Scanner.Get (sym); import END;
	Check (Scanner.semicolon, 'No ending semicolon');
	SymTable.Import_modules
END ImportList;

PROCEDURE Module*;
	VAR modid : Base.String;
		varsize : INTEGER;
BEGIN
	Base.Reset_compiler_flag;
	
	Scanner.Get (sym); Check (Scanner.module, 'No MODULE keyword');
	IF sym = Scanner.ident THEN modid := Scanner.id; Scanner.Get (sym)
	ELSE modid := '@'; Scanner.Mark ('No module name')
	END;
	Check (Scanner.semicolon, 'No ; after module name');
	
	IF modid # '@' THEN
		Base.Init;
		SymTable.Init (modid);
		Generator.Init (modid);
		
		IF sym = Scanner.import THEN
			ImportList
		END;

		varsize := 0;
		DeclarationSequence (varsize);

		Generator.Enter (NIL, 0);
		IF sym = Scanner.begin THEN
			Scanner.Get (sym);
			StatementSequence
		END;
		Generator.Return;

		Check (Scanner.end, 'No END for module');
		IF sym = Scanner.ident THEN
			IF modid # Scanner.id THEN Scanner.Mark ('Wrong module name') END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No module identifier after END')
		END;
		Check (Scanner.period, 'No ending .');
		
		Generator.Finish
	END
END Module;

END Parser.