MODULE Parser2;

IMPORT
	Sys, Base, Scanner, Generator := GeneratorWin64v2, Console;
	
CONST
	classes_Variable = Base.classes_Variable;
	classes_Value = Base.classes_Value;
	
	(* Error message *)
	errTooMuchParam = 'Superflous , or too much actual parameters';
	errTooLittleParam = 'Missing , or not enough actual parameters';

VAR
	sym : INTEGER;
	defobj : Base.Object;
	hasExport : BOOLEAN;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Assignable (VAR dst, src : Base.Item) : BOOLEAN;
	VAR result : BOOLEAN;
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
	IF (op = Scanner.plus) OR (op = Scanner.minus)
	OR (op = Scanner.times) THEN
		IF ~ (x.type.form IN {Base.type_integer, Base.type_set, Base.type_real}) THEN
			Scanner.Mark ('+, - and * only compatible with numberic types and SET');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF op = Scanner.slash THEN
		IF ~ (x.type.form IN {Base.type_set, Base.type_real}) THEN
			Scanner.Mark ('/ only compatible with REAL and SET');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.set_type, 0)
		END
	ELSIF (op = Scanner.div) OR (op = Scanner.mod) THEN
		IF ~ (x.type.form = Base.type_integer) THEN
			Scanner.Mark ('DIV and MOD only compatible with INTEGER');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF (op = Scanner.not) OR (op = Scanner.or)
	OR (op = Scanner.and) THEN
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
		Scanner.Mark ('Expect a SET expression'); Generator.Free_item (x);
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
	IF (rel = Scanner.equal) & (rel = Scanner.not_equal) THEN
		IF ~ Base.Equality_applied (x) THEN
			Scanner.Mark ('Invalid operand');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF (rel >= Scanner.less) & (rel <= Scanner.less_equal) THEN
		IF Base.Comparison_applied (x) THEN
			(* Ok *)
		ELSIF ((rel = Scanner.less_equal) OR (rel = Scanner.greater_equal))
		& (x.mode IN Base.cls_HasValue) & (x.type = Base.set_type) THEN
			(* Ok *)
		ELSE
			Scanner.Mark ('Invalid operand');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	ELSIF rel = Scanner.is THEN
		IF ~ Base.Type_test_applied (x) THEN
			Scanner.Mark ('Expected a pointer or record varparam in type test');
			Generator.Free_item (x);
			Generator.Make_const (x, Base.nil_type, 0)
		END
	ELSIF rel = Scanner.in THEN
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
		Generator.Free_item (x); Generator.Free_item (y);
		Generator.Make_const (x, Base.nil_type, 0);
		y.mode := Base.class_type; y.type := Base.nilrecord_type
	END
END Check_type_test;

PROCEDURE Check_equal_comparison (VAR x, y : Base.Item);
	VAR fail : BOOLEAN;
BEGIN
	IF ~ Base.Equality_applied (y) THEN
		Scanner.Mark ('Invalid operand'); fail := TRUE
	ELSIF ~ Base.Equalable (x, y) THEN
		Scanner.Mark ('Invalid comparison'); fail := TRUE
	ELSE fail := FALSE
	END;
	IF fail THEN
		Generator.Free_item (x); Generator.Free_item (y);
		Generator.Make_const (x, Base.int_type, 0);
		Generator.Make_const (y, Base.int_type, 0)
	END
END Check_equal_comparison;

PROCEDURE Check_order_comparison (VAR x, y : Base.Item);
	VAR fail : BOOLEAN;
BEGIN
	IF ~ Base.Comparison_applied (y) THEN
		Scanner.Mark ('Invalid operand'); fail := TRUE
	ELSIF ~ Base.Comparable (x, y) THEN
		Scanner.Mark ('Invalid comparison'); fail := TRUE
	ELSE fail := FALSE
	END;
	IF fail THEN
		Generator.Free_item (x); Generator.Free_item (y);
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
	CONST err1 = 'Identifier expected';
	VAR modul : Base.Object;
BEGIN
	IF sym = Scanner.ident THEN
		Base.Find_obj (obj, Scanner.id); Scanner.Get (sym);
		IF obj.class = Base.class_module THEN
			Check (Scanner.period, 'Expect . after module identifiers');
			IF sym = Scanner.ident THEN
				modul := obj; Base.Find_in_module (obj, Scanner.id, modul);
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
		Base.New_obj (obj, Scanner.id, class);
		IF obj = Base.guard THEN
			Scanner.Mark ('Duplicate identifer definition')
		END;
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
	IF ~ hasExport OR obj.export OR obj.type.predefined THEN (* Ok *)
	ELSIF obj.type.form IN Base.types_HasExt THEN
		Scanner.Mark ('This type is not exported')
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
		IF sym # Scanner.var THEN cls := Base.class_var
		ELSE Scanner.Get (sym); cls := Base.class_ref
		END;
		
		ident (first, cls);			
		WHILE sym = Scanner.comma DO
			Scanner.Get (sym); ident (obj, cls);
			IF first = Base.guard THEN first := obj END
		END;
		
		Check (Scanner.colon, 'No colon after identifier list');
			
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
		IF (obj # Base.guard) & (obj.class = Base.class_type)
		& (obj.type.form IN Base.types_Scalar) THEN
			rtype := obj.type; Check_export (obj)
		ELSE
			Scanner.Mark ('Type not found or invalid result type');
			rtype := Base.int_type
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
	Generator.Check_varsize (typ.size, FALSE)
END ArrayType;

PROCEDURE RecordType (VAR typ : Base.Type);
	VAR obj : Base.Object;

	PROCEDURE FieldListSequence (typ : Base.Type);

		PROCEDURE FieldList (typ : Base.Type);
			VAR first : Base.Object; tp : Base.Type;
				offset : INTEGER;
		BEGIN
			hasExport := FALSE;
			IdentList (first, Base.class_field);
			Check (Scanner.colon, 'No colon after identifier list');
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
		WHILE sym = Scanner.semicolon DO
			Scanner.Get (sym);
			IF sym = Scanner.ident THEN FieldList (typ)
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
		
	Base.Open_scope ('');
	IF sym = Scanner.ident THEN
		FieldListSequence (typ);
		Base.Adjust_alignment (typ.size, typ.alignment)
	END;
	Generator.Check_varsize (typ.size, FALSE);
	
	Check (Scanner.end, 'No END for record definition');
	typ.fields := Base.top_scope.next;
	Base.Close_scope
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
			Base.Register_undefined_pointer_type (typ, obj.name)
		ELSIF (obj.class = Base.class_type)
		& (obj.type.form = Base.type_record) THEN
			typ.base := obj.type; Check_export (obj)
		ELSE Scanner.Mark ('Record type expected');
			typ.base := Base.nilrecord_type
		END
	END
END PointerType;

PROCEDURE ProcedureType (VAR typ : Base.Type);
BEGIN
	Base.New_typ (typ, Base.type_procedure);
	typ.size := Base.Word_size; typ.alignment := Base.Word_size;
	
	Base.Open_scope ('');
	Scanner.Get (sym);
	IF sym = Scanner.lparen THEN FormalParameters (typ.len, typ.base) END;
	typ.fields := Base.top_scope.next;
	Base.Close_scope
END ProcedureType;

PROCEDURE StrucType (VAR typ : Base.Type);
BEGIN
	IF sym = Scanner.array THEN ArrayType (typ)
	ELSIF sym = Scanner.record THEN RecordType (typ)
	ELSIF sym = Scanner.pointer THEN PointerType (typ)
	ELSIF sym = Scanner.procedure THEN ProcedureType (typ)
	ELSE Scanner.Mark ('Expect a type definition'); typ := Base.int_type
	END
END StrucType;
	
PROCEDURE type (VAR typ : Base.Type);
	CONST err2 = 'This identifier is not a type';
	VAR obj : Base.Object;
BEGIN
	typ := Base.int_type;
	IF sym = Scanner.ident THEN
		qualident (obj);
		IF obj = Base.guard THEN Scanner.Mark ('Undefined type')
		ELSIF obj.class # Base.class_type THEN Scanner.Mark (err2)
		ELSIF (obj # defobj) OR (obj.type.form # Base.type_pointer) THEN
			typ := obj.type; Check_export (obj)
		ELSE Scanner.Mark ('Circular definition')
		END
	ELSIF (sym = Scanner.array) OR (sym = Scanner.record)
	OR (sym = Scanner.pointer) OR (sym = Scanner.procedure) THEN
		StrucType (typ)
	ELSE Scanner.Mark ('Expect a type or type definition')
	END
END type;

PROCEDURE DeclarationSequence (VAR varsize : INTEGER);

	PROCEDURE ConstanstDeclaration;
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
			obj.val := x.a;
			obj.val2 := x.b
		ELSE
			Scanner.Mark ('Expect a const expression');
			obj.class := Base.class_const;
			obj.type := Base.int_type;
			obj.val := 0
		END
	END ConstanstDeclaration;
		
	PROCEDURE TypeDeclaration;
		VAR obj : Base.Object;
	BEGIN
		hasExport := FALSE;
		identdef (obj, Base.class_type);
		Check (Scanner.equal, 'No = in type declaration');
		defobj := obj; StrucType (obj.type);
		obj.type.named := TRUE;
		
		IF obj.type.form = Base.type_record THEN
			Generator.Alloc_typedesc (obj.type);
			IF Base.undef_ptr_list # NIL THEN
				Base.Check_undefined_pointer_list (obj)
			END
		END
	END TypeDeclaration;
	
	PROCEDURE VariableDeclaration (VAR varsize : INTEGER);
		VAR first : Base.Object; tp : Base.Type;
	BEGIN
		hasExport := FALSE;
		IdentList (first, Base.class_var);
		Check (Scanner.colon, 'No colon after identifier list');
		defobj := NIL; type (tp);
				
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
			IF sym = Scanner.lparen THEN
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
			IF sym = Scanner.begin THEN
				Scanner.Get (sym);
				StatementSequence
			END;
				
			IF sym = Scanner.return THEN
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
			
			Check (Scanner.end, 'No END for procedure body');
			Generator.Return;
			Base.Inc_level (-1);
			Base.Close_scope
		END ProcedureBody;
			
	BEGIN (* ProcedureDeclaration *)
		hasExport := FALSE;
		ProcedureHeading (proc);
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
			ConstanstDeclaration;
			Check (Scanner.semicolon, 'No ; after const declaration')
		END
	END;
	IF sym = Scanner.type THEN
		Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			TypeDeclaration;
			Check (Scanner.semicolon, 'No ; after type declaration')
		END;
		IF Base.undef_ptr_list # NIL THEN
			Base.Cleanup_undefined_pointer_list;
			Scanner.Mark ('There are pointer types with undefined base type')
		END
	END;
	IF sym = Scanner.var THEN
		Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			VariableDeclaration (varsize);
			Check (Scanner.semicolon, 'No ; after variable declaration')
		END;
		Generator.Check_varsize (varsize, Base.cur_lev = 0)
	END;
	WHILE sym = Scanner.procedure DO
		ProcedureDeclaration;
		Check (Scanner.semicolon, 'No ; after procedure declaration')
	END
END DeclarationSequence;

PROCEDURE Str_to_const (VAR x : Base.Item);
BEGIN
	IF (x.mode = Base.class_var) & (x.type.form = Base.type_string) THEN
		Generator.Make_const (x, Base.char_type, x.b)
	END
END Str_to_const;

PROCEDURE Parameter (VAR pinfo : Generator.ProcInfo; VAR par : Base.Object);
	VAR y : Base.Item;
BEGIN
	expression (y);
	IF par # Base.guard THEN
		CASE Base.Check_parameter (par, y) OF
			0: Str_to_const (y); Generator.Value_param (y, pinfo) |
			1: Generator.Open_array_param (y, pinfo, par.type) |
			2: Generator.Ref_param (y, pinfo) |
			3: Generator.Record_var_param (y, pinfo) |
			9: Generator.String_param (y, pinfo, par.type) |
			4: Scanner.Mark ('Formal is variable but actual is read-only') |
			5: Scanner.Mark ('Formal is variable but actual is not') |
			6: Scanner.Mark ('Formal and actual type are incompatible') |
			7: Scanner.Mark ('Invalid parameter') |
			8: Scanner.Mark ('Actual is not an extension of formal')
		END;
		par := par.next
	ELSE Scanner.Mark ('This procedure do not need parameters')
	END
END Parameter;

PROCEDURE Skip_params;
	VAR x : Base.Item;
BEGIN
	REPEAT
		Scanner.Get (sym); expression (x); Generator.Free_item (x)
	UNTIL sym # Scanner.comma
END Skip_params;
	
PROCEDURE ActualParameters (VAR x : Base.Item; VAR pinfo : Generator.ProcInfo);
	VAR par : Base.Object;	
BEGIN
	IF x.mode = Base.class_proc THEN par := x.obj.dsc
	ELSE par := x.type.fields
	END;
	Scanner.Get (sym);
	IF sym # Scanner.rparen THEN
		Parameter (pinfo, par);
		WHILE (sym = Scanner.comma) & (par # Base.guard) DO
			Scanner.Get (sym); Parameter (pinfo, par) 
		END;
		IF sym = Scanner.comma THEN
			Scanner.Mark (errTooMuchParam); Skip_params
		END
	END;
	IF par # Base.guard THEN Scanner.Mark (errTooLittleParam) END;
	Check (Scanner.rparen, 'No closing )')
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
	IF x.mode = Base.class_proc THEN
		pinfo.parblksize := x.obj.parblksize; rtype := x.type
	ELSE pinfo.parblksize := x.type.len; rtype := x.type.base
	END;
	pinfo.rtype := rtype;
	
	has_error := TRUE;
	IF proper THEN
		IF rtype = NIL THEN has_error := FALSE ELSE Scanner.Mark (err1) END
	ELSIF rtype # NIL THEN has_error := FALSE
	ELSE Scanner.Mark (err2)
	END;
	
	Generator.Prepare_to_call (x, pinfo);
	IF sym = Scanner.lparen THEN ActualParameters (x, pinfo)
	ELSIF x.obj.parblksize > 0 THEN Scanner.Mark ('Not enough parameters')
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
			ELSE Generator.SProc_NEW (x)
			END
		ELSE Scanner.Mark ('Expect a pointer variable')
		END
	END SProc_NEW;
	
	PROCEDURE SProc_DISPOSE;
		VAR
			x : Base.Item;
	BEGIN
		expression (x);
		IF (x.mode IN Base.cls_HasValue)
		& (x.type.form = Base.type_pointer) THEN Generator.SProc_DISPOSE (x)
		ELSE Scanner.Mark ('Expect a pointer')
		END
	END SProc_DISPOSE;
	*)
		
	PROCEDURE SProc_GET;
		VAR x, y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		Check (Scanner.comma, errTooLittleParam);
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
		Check (Scanner.comma, errTooLittleParam);
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
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_int (y);
		Check (Scanner.comma, errTooLittleParam);
		expression (z); Check_int (z);
		
		Generator.SProc_COPY (x, y, z)
	END SProc_COPY;
		
	PROCEDURE SProc_LoadLibrary (adr : INTEGER);
		VAR
			x, y, proc : Base.Item;
			pinfo : Generator.ProcInfo;
			no_error : BOOLEAN;
	BEGIN
		expression (x);
		IF (x.mode IN classes_Variable) & (x.type = Base.int_type) THEN
			no_error := TRUE;
			IF ~ x.readonly THEN
			ELSE Scanner.Mark ('Cannot modify read only variable')
			END
		ELSE
			Scanner.Mark ('Expect an INTEGER variable');
			Generator.Free_item (x); no_error := FALSE
		END;
		
		IF no_error THEN
			proc.mode := Base.class_var; proc.type := Base.int_type;
			proc.lev := -1; proc.a := adr;
			pinfo.parblksize := 8; pinfo.rtype := Base.int_type;	
			Generator.Prepare_to_call (proc, pinfo);
		END;
		
		Check (Scanner.comma, errTooLittleParam);
		expression (y);
		
		IF no_error THEN
			IF (y.mode IN classes_Value) & Base.Is_string (y.type) THEN
				Generator.Ref_param (y, pinfo)
			ELSE Scanner.Mark ('Expect a string or character array');
			END;
			Generator.Call (proc, pinfo); Generator.Store (x, proc)
		ELSE Generator.Free_item (y)
		END
	END SProc_LoadLibrary;
		
	PROCEDURE SProc_GetProcAddress (adr : INTEGER);
		VAR
			x, y, z, proc : Base.Item;
			pinfo : Generator.ProcInfo;
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
		
		IF no_error THEN
			proc.mode := Base.class_var; proc.type := Base.int_type;
			proc.lev := -1; proc.a := adr;
			pinfo.parblksize := 16; pinfo.rtype := Base.int_type;	
			Generator.Prepare_to_call (proc, pinfo);
		END;
		
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_int (y);
		
		IF no_error THEN Generator.Value_param (y, pinfo)
		ELSE Generator.Free_item (y)
		END;
		
		Check (Scanner.comma, errTooLittleParam);
		expression (z); Check_int (z);
		
		IF no_error THEN
			Generator.Value_param (z, pinfo);
			Generator.Call (proc, pinfo); Generator.Store (x, proc)
		ELSE Generator.Free_item (y)
		END
	END SProc_GetProcAddress;

BEGIN (* StandProc *)
	Check (Scanner.lparen, 'Not enough parameter or missing (');
	IF x.type = NIL THEN
		CASE SHORT (x.a) OF
			(* 4: SProc_NEW |
			8: SProc_DISPOSE | *)
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
	VAR funcno : INTEGER; rtype : Base.Type;
		y : Base.Item;

	PROCEDURE SFunc_ABS (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF (x.mode IN classes_Value) & (x.type.form IN Base.types_Numberic) THEN
			Generator.SFunc_ABS (x)
		ELSE
			Scanner.Mark ('Expect a numberic value'); Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
	END SFunc_ABS;
		
	PROCEDURE SFunc_ODD (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x); Generator.SFunc_ODD (x)
	END SFunc_ODD;
		
	PROCEDURE SFunc_LEN (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF (x.mode IN classes_Variable) & (x.type.form = Base.type_array) THEN
			Generator.SFunc_LEN (x)
		ELSE
			Scanner.Mark ('Expect an array'); Generator.Free_item (x);
			Generator.Make_const (x, Base.int_type, 0)
		END
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
		
	PROCEDURE SFunc_ORD (VAR x : Base.Item);
		CONST
			valid_types = {Base.type_char, Base.type_set, Base.type_boolean};
		VAR no_error : BOOLEAN;
	BEGIN
		expression (x); no_error := TRUE;
		IF (x.mode IN classes_Value) THEN
			IF x.type.form IN valid_types THEN
				IF x.mode IN classes_Variable THEN Generator.load (x) END;
			ELSIF (x.type.form = Base.type_string) & (x.type.len <= 2) THEN
				x.mode := Base.class_const; x.a := x.b
			ELSE no_error := FALSE;
			END
		ELSE no_error := FALSE
		END;
		IF no_error THEN (* Do nothing *) ELSE
			Scanner.Mark ('Expect a character, set or boolean value');
			Generator.Free_item (x); Generator.Make_const (x, Base.int_type, 0)
		END
	END SFunc_ORD;
		
	PROCEDURE SFunc_CHR (VAR x : Base.Item);
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END
	END SFunc_CHR;
		
	PROCEDURE SFunc_ADR (VAR x : Base.Item);
	BEGIN
		expression (x);
		IF x.mode IN classes_Variable THEN Generator.SFunc_ADR (x)
		ELSE
			Scanner.Mark ('Expect a variable');
			Generator.Make_const (x, Base.int_type, 0)
		END
	END SFunc_ADR;
		
	PROCEDURE SFunc_SIZE (VAR x : Base.Item);
		VAR size : INTEGER;
	BEGIN
		expression (x);
		IF x.mode = Base.class_type THEN size := x.type.size
		ELSE Scanner.Mark ('Expect a type identifier'); size := 0
		END;
		Generator.Make_const (x, Base.int_type, size)
	END SFunc_SIZE;
	
	PROCEDURE SFunc_BIT (VAR x : Base.Item);
		VAR y : Base.Item;
	BEGIN
		expression (x); Check_int (x);
		IF x.mode IN classes_Variable THEN Generator.load (x) END;
		Check (Scanner.comma, errTooLittleParam);
		expression (y); Check_int (y);
		Generator.SFunc_BIT (x, y)
	END SFunc_BIT;
		
	PROCEDURE SFunc_VAL (VAR x : Base.Item);
		VAR y : Base.Item;
			cast_type : Base.Type;
	BEGIN
		expression (x);
		IF (x.mode = Base.class_type) & (x.type.form IN Base.types_Scalar) THEN
			cast_type := x.type
		ELSE
			Scanner.Mark ('Expect a scalar type identifier');
			cast_type := Base.int_type; Generator.Free_item (x)
		END;
		
		Check (Scanner.comma, errTooLittleParam);
		expression (y);
		IF y.mode IN classes_Value THEN x := y; x.type := cast_type
		ELSE Scanner.Mark ('Expect a value'); Generator.Free_item (y);
			Generator.Make_const (x, cast_type, 0)
		END
	END SFunc_VAL;
		
BEGIN (* StandFunc *)
	Scanner.Get (sym);
	IF x.type # NIL THEN
		funcno := SHORT (x.a); rtype := x.type;
		CASE funcno OF
			200: SFunc_ABS (x) |
			201: SFunc_ODD (x) |
			202: SFunc_LEN (x) |
			203 .. 205: SFunc_SHIFT (funcno - 203, x) |
		(*	206: SFunc_FLOOR (x) |
			207: SFunc_FLT (x) | *)
			208: SFunc_ORD (x) |
			209: SFunc_CHR (x) |
			300: SFunc_ADR (x) |
			301: SFunc_SIZE (x) |
			302: SFunc_BIT (x) |
			303: SFunc_VAL (x)
		END;
		IF (funcno # 303) & (funcno # 200) THEN x.type := rtype END;
		IF sym = Scanner.comma THEN
			Scanner.Mark (errTooMuchParam); Skip_params
		END;
		Check (Scanner.rparen, 'No closing )')
	ELSE
		Scanner.Mark ('Proper procedure cannot be called in expression');
		Generator.Make_const (x, Base.int_type, 0)
	END
END StandFunc;
	
PROCEDURE selector (VAR x : Base.Item);
		
	PROCEDURE Field_selector (VAR x : Base.Item);
		VAR obj : Base.Object;
	BEGIN
		IF x.type.form = Base.type_pointer THEN Generator.Deref (x) END;
		Scanner.Get (sym);
		IF x.type.form = Base.type_record THEN
			IF sym = Scanner.ident THEN
				Base.Find_field (obj, Scanner.id, x.type);
				IF obj # Base.guard THEN Generator.Field (x, obj)
				ELSE Scanner.Mark ('Record field not found')
				END;
				Scanner.Get (sym)
			ELSE Scanner.Mark ('Expect a record field identifier')
			END
		ELSE
			Scanner.Mark ('Not a record or pointer but found . selector');
			IF sym = Scanner.ident THEN Scanner.Get (sym) END
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
			UNTIL sym # Scanner.comma
		ELSE
			Scanner.Mark ('Not an array but found [ selector');
			REPEAT
				Scanner.Get (sym);
				expression (y);
				Generator.Free_item (y)
			UNTIL sym # Scanner.comma
		END;
		Check (Scanner.rbrak, 'No closing ]')
	END Element_selector;
	
	PROCEDURE Deref_selector (VAR x : Base.Item);
	BEGIN
		Scanner.Get (sym);
		IF x.type.form = Base.type_pointer THEN Generator.Deref (x)
		ELSE Scanner.Mark ('Not a pointer but found ^ selector')
		END
	END Deref_selector;
	
	PROCEDURE Type_guard_selector (VAR x : Base.Item);
		VAR obj : Base.Object; valid : BOOLEAN; xform : INTEGER;
			xtype, ytype : Base.Type;
	BEGIN
		valid := (x.type.form = Base.type_pointer)
			OR (x.type.form = Base.type_record) & x.param & ~ x.readonly;
		IF valid THEN (* Ok *)
		ELSE Scanner.Mark ('Type guard is not applicable')
		END;
		
		Scanner.Get (sym); qualident (obj);
		IF valid THEN xform := x.type.form;
			IF (obj.class = Base.class_type) & (obj.type.form = xform) THEN
				xtype := x.type; ytype := obj.type;
				IF xform = Base.type_pointer THEN
					xtype := xtype.base; ytype := ytype.base
				END;
				IF Base.Is_extension (ytype, xtype) THEN
					Generator.Type_test (x, ytype, TRUE)
				ELSE Scanner.Mark ('Not an extension')
				END
			ELSE Scanner.Mark ('Invalid type in type guard')
			END	
		ELSE Scanner.Mark ('Type guard is not applicable')
		END;
		Check (Scanner.rparen, 'No closing )')
	END Type_guard_selector;
		
BEGIN
	IF ~ (x.mode IN Base.cls_HasValue) THEN
		Scanner.Mark ('Only array, pointer and record can have selector')
	ELSIF sym = Scanner.period THEN
		Field_selector (x)
	ELSIF sym = Scanner.lbrak THEN
		Element_selector (x)
	ELSIF sym = Scanner.arrow THEN
		Deref_selector (x)
	ELSIF sym = Scanner.lparen THEN
		Type_guard_selector (x)
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
					
			IF is_procedure & (sym = Scanner.lparen) THEN exit := TRUE
			ELSE
				IF (sym = Scanner.period) OR (sym = Scanner.lparen)
				OR (sym = Scanner.lbrak) OR (sym = Scanner.arrow) THEN
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
		IF sym = Scanner.upto THEN
			IF y.mode # Base.class_const THEN Generator.load (y) END;
			Scanner.Get (sym); expression (z); Check_set_element (z);
			Generator.Set3 (x, y, z)
		ELSE Generator.Set2 (x, y)
		END
	END element;

BEGIN (* set *)
	Scanner.Get (sym);
	Generator.Make_const (x, Base.set_type, 0);
	IF sym # Scanner.rbrace THEN
		element (x);
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
		Generator.Make_string (x);
		Scanner.Get (sym)
	ELSIF sym = Scanner.nil THEN
		Generator.Make_const (x, Base.nil_type, 0);
		Scanner.Get (sym)
	ELSIF sym = Scanner.true THEN
		Generator.Make_const (x, Base.bool_type, 1);
		Scanner.Get (sym)
	ELSIF sym = Scanner.false THEN
		Generator.Make_const (x, Base.bool_type, 0);
		Scanner.Get (sym)
	ELSIF sym = Scanner.lbrace THEN
		set (x)
	ELSIF sym = Scanner.lparen THEN
		Scanner.Get (sym); expression (x);
		Check (Scanner.rparen, 'No closing )')
	ELSIF sym = Scanner.not THEN
		Scanner.Get (sym); factor (x); Check_operator (Scanner.not, x);
		Generator.Op1 (Scanner.not, x)
	END
END factor;
	
PROCEDURE term (VAR x : Base.Item);
	CONST err1 = 'Incompatible types';
	VAR op : INTEGER; y : Base.Item;
BEGIN
	factor (x);
	WHILE (sym >= Scanner.times) & (sym <= Scanner.and) DO
		op := sym; Check_operator (op, x);
		
		IF x.mode IN Base.cls_Variable THEN Generator.load (x) END;
		IF op = Scanner.and THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym); factor (y); Check_operator (op, y);
		
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
		Scanner.Get (sym); term (x); Check_operator (Scanner.plus, x)
	ELSIF sym = Scanner.minus THEN
		Scanner.Get (sym); term (x); Check_operator (Scanner.minus, x);
		Generator.Op1 (Scanner.minus, x)
	ELSE term (x)
	END;
		
	WHILE (sym >= Scanner.plus) & (sym <= Scanner.or) DO
		op := sym; Check_operator (op, x);
		
		IF x.mode IN Base.cls_Variable THEN Generator.load (x) END;
		IF op = Scanner.or THEN Generator.Op1 (op, x) END;
		
		Scanner.Get (sym); term (y); Check_operator (op, y);
		
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
			Generator.Make_const (y, Base.char_type, y.b)
		ELSIF (y.mode IN classes_Value) & (y.type = Base.char_type)
				& (x.type # Base.char_type) THEN
			Generator.Make_const (x, Base.char_type, x.b)
		END
	END Check_for_string_const;
		
BEGIN (* expression *)
	SimpleExpression (x);
	IF (sym >= Scanner.equal) & (sym <= Scanner.is) THEN
		op := sym; Check_relation (op, x);
		
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
			Check_equal_comparison (x, y); Check_for_string_const (x, y);
			Generator.Comparison (op, x, y)
		ELSIF x.type # Base.set_type THEN
			Check_order_comparison (x, y); Check_for_string_const (x, y);
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
	CONST err1 = 'Invalid statement';
		err3 = 'Missing semicolon?';
		err4 = 'No THEN after IF condition';
		err5 = 'No THEN after ELSIF condition';
		err6 = 'No END for structured statement';
		err7 = 'No DO after WHILE condition';
		err8 = 'No DO after ELSIF condition';
		err9 = 'No UNTIL for REPEAT statement';
	VAR
		x, y : Base.Item;
		L : INTEGER;
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
			IF sym = Scanner.becomes THEN
				Scanner.Get (sym); expression (y);
				IF Assignable (x, y) THEN
					IF x.type = Base.char_type THEN Str_to_const (y) END;
					Generator.Store (x, y)
				END
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
		VAR modul : Base.Object;
	BEGIN
		ident (modul, Base.class_module);
		IF sym = Scanner.becomes THEN
			Scanner.Get (sym);
			IF sym = Scanner.ident THEN
				modul.realname := Scanner.id; Scanner.Get (sym)
			ELSE Scanner.Mark ('Expect an identifier');
				modul.realname := modul.name
			END
		ELSE modul.realname := modul.name
		END;
		Base.Import_module (modul)
	END import;

BEGIN (* ImportList *)
	Scanner.Get (sym);
	import;
	WHILE sym = Scanner.comma DO Scanner.Get (sym); import END;
	Check (Scanner.semicolon, 'No ending semicolon')
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
		Base.Init (modid);
		Generator.Init (modid);
		
		IF sym = Scanner.import THEN
			ImportList
		END;

		varsize := 0;
		DeclarationSequence (varsize);

		Generator.Enter (NIL, 0);
		Generator.Module_init;
		IF sym = Scanner.begin THEN
			Scanner.Get (sym);
			StatementSequence
		END;
		Generator.Module_exit;
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

END Parser2.