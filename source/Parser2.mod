MODULE Parser2;

IMPORT
	Strings, Console, Base, Scanner, SymTable, Generator;
	
CONST
	noSemicolonError = 'No ;';
	noEndError = 'END expected';
	noOfError = 'OF expected';
	noToError = 'TO expected';
	noThenError = 'THEN expected';
	noDoError = 'DO expected';
	notConstError = 'Not a constant value';
	superflousCommaError = 'Superflous ,';
	superflousSemicolonError = 'Superflous ;';
	superflousBarError = 'Superflous |';
	noColonError = 'No :';
	noPeriodError = 'No .';
	noEqualError = 'No =';
	noRParenError = 'No )';
	noLParenError = 'No (';
	invalidOperationError = 'Invalid operation';
	notScalarError = 'Not a scalar value';
	notStrError = 'Not a string';
	notPointerError = 'Not a pointer';
	notExtError = 'Not a extension of the other type';
	notPointerOrProcedureError = 'Not a pointer or procedure';
	notPointerOrRecordError = 'Not a pointer or record';
	notTypeError = 'Not a type';
	notPointerOrRecordTypeError = 'Not a pointer or record type';
	notRecordTypeError = 'Not a record type';
	notProcError = 'Not a procedure';
	notAddrError = 'Not an address';
	notGlobalProcError = 'Procedure is not global';
	notCompProcError = 'Incompatible procedures';
	notCompAddrError = 'Incompatible addresses';
	notCompTypeError = 'Incompatible types';
	stringTooLongError = 'String is longer than destination';
	invalidExtError = 'Invalid extension';
	noIdentError = 'Expect an identifier';
	tooMuchParamError = 'Too much parameters';
	tooLittleParamError = 'Not enough parameters';
	undefinedError = 'Undefined identifier';
	
TYPE
	UndefPtrList = POINTER TO RECORD
		typ: Base.Type; basename: Base.IdentStr; next: UndefPtrList
	END;
	
VAR
	sym*: INTEGER;
	identExport: BOOLEAN;
	defobj: Base.Object; defobjId: Base.IdentStr;
	expression: PROCEDURE (VAR x: Base.Item);
	type: PROCEDURE (VAR tp: Base.Type);
	Union: PROCEDURE (tp: Base.Type; VAR first: Base.Object);
	
	undefPtr: UndefPtrList;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Check (expected: INTEGER; err: ARRAY OF CHAR);
BEGIN IF sym = expected THEN Scanner.Get (sym) ELSE Scanner.Mark (err) END
END Check;

PROCEDURE MakeIntConst (VAR x: Base.Item);
BEGIN Generator.Clean_item (x); Generator.Make_const (x, Base.intType, 0)
END MakeIntConst;

PROCEDURE MakeConst (VAR x: Base.Item; tp: Base.Type);
BEGIN Generator.Clean_item (x); Generator.Make_const (x, tp, 0)
END MakeConst;

PROCEDURE CheckExport (VAR exported: BOOLEAN);
BEGIN
	IF sym = Scanner.times THEN Scanner.Get (sym);
		IF SymTable.curLev = 0 THEN exported := TRUE; identExport := TRUE
		ELSE Scanner.Mark ('Cannot export non-global')
		END
	END
END CheckExport;

PROCEDURE CheckExport2 (obj: Base.Object);
BEGIN
	IF identExport & ~ obj.export & (obj.type.mod = -1) THEN
		Scanner.Mark ('This type is not exported')
	END
END CheckExport2;

PROCEDURE CheckValue (VAR x: Base.Item);
BEGIN IF ~ (x.mode IN Base.clsValue) THEN Scanner.Mark ('Not a value') END
END CheckValue;

PROCEDURE CheckInt (VAR x: Base.Item);
BEGIN
	IF ~ (x.mode IN Base.clsValue) OR (x.type.form # Base.tInteger) THEN
		Scanner.Mark ('Not an integer'); MakeIntConst (x)
	END
END CheckInt;

PROCEDURE CheckReal (VAR x: Base.Item);
BEGIN
	IF ~ (x.mode IN Base.clsValue) OR (x.type.form # Base.tReal) THEN
		Scanner.Mark ('Not a real number'); MakeConst (x, Base.realType)
	END
END CheckReal;

PROCEDURE CheckSet (VAR x: Base.Item);
BEGIN
	IF ~ (x.mode IN Base.clsValue) OR (x.type # Base.setType) THEN
		Scanner.Mark ('Not a SET'); MakeConst (x, Base.setType)
	END
END CheckSet;

PROCEDURE CheckBool (VAR x: Base.Item);
BEGIN
	IF ~(x.mode IN Base.clsValue) OR (x.type # Base.boolType) THEN
		Scanner.Mark ('Not a BOOLEAN'); MakeConst (x, Base.boolType)
	END
END CheckBool;

PROCEDURE CheckChar (VAR x: Base.Item);
BEGIN
	IF x.mode IN Base.clsValue THEN
		IF (x.type.form = Base.tString) & (x.b <= 2) THEN
			Generator.Str_to_char (x)
		ELSIF x.type.form # Base.tChar THEN
			Scanner.Mark ('Not a CHAR'); MakeConst (x, Base.charType)
		END
	ELSE Scanner.Mark ('Not a CHAR'); MakeConst (x, Base.charType)
	END
END CheckChar;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE qualident (VAR obj: Base.Object);
BEGIN
	SymTable.Find (obj, Scanner.id);
	IF obj = Base.guard THEN Scanner.Mark (undefinedError) END;
	Scanner.Get (sym)
END qualident;

PROCEDURE designator (VAR x: Base.Item);
	VAR obj: Base.Object;
BEGIN qualident (obj);
	IF obj # Base.guard THEN Generator.Make_item (x, obj)
	ELSE MakeIntConst (x)
	END
END designator;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Expression *)

PROCEDURE StandFunc (VAR x: Base.Item);
	VAR funcno: INTEGER; rtype: Base.Type; y: Base.Item;

	PROCEDURE SFunc_ABS (VAR x: Base.Item);
	BEGIN expression (x); CheckValue (x);
		IF x.type.form IN Base.typeNumberic THEN (* Ok *)
		ELSE Scanner.Mark ('Expect a numberic value'); x.type := Base.intType
		END;
		Generator.SFunc_ABS (x)
	END SFunc_ABS;
		
	PROCEDURE SFunc_ODD (VAR x: Base.Item);
	BEGIN expression (x); CheckInt (x); Generator.SFunc_ODD (x)
	END SFunc_ODD;
		
	PROCEDURE SFunc_SHIFT (shf: INTEGER; VAR x: Base.Item);
		VAR y: Base.Item;
	BEGIN expression (x); CheckInt (x);
		Check (Scanner.comma, tooLittleParamError);
		expression (y); CheckInt (y); Generator.SFunc_SHIFT (shf, x, y)
	END SFunc_SHIFT;
	
	PROCEDURE SFunc_FLOOR (VAR x: Base.Item);
	BEGIN expression (x); CheckReal (x); Generator.SFunc_FLOOR (x)
	END SFunc_FLOOR;
	
	PROCEDURE SFunc_FLT (VAR x: Base.Item);
	BEGIN expression (x); CheckInt (x); Generator.SFunc_FLT (x)
	END SFunc_FLT;
		
	PROCEDURE SFunc_ORD (VAR x: Base.Item);
		CONST validTypes = {Base.tChar, Base.tSet, Base.tBoolean};
	BEGIN expression (x); CheckValue (x);
		IF x.type.form IN validTypes THEN (* Do nothing *)
		ELSE Scanner.Mark ('Expect CHAR, SET, or BOOLEAN'); MakeIntConst (x)
		END
	END SFunc_ORD;
		
	PROCEDURE SFunc_CHR (VAR x: Base.Item);
	BEGIN expression (x); CheckInt (x); Generator.SFunc_CHR (x)
	END SFunc_CHR;
		
	PROCEDURE SFunc_SIZE (VAR x: Base.Item);
		VAR size: INTEGER;
	BEGIN expression (x);
		IF x.mode = Base.cType THEN size := Generator.TypeSize(x.type)
		ELSE Scanner.Mark (notTypeError); size := Base.WordSize
		END;
		MakeIntConst (x); x.a := size
	END SFunc_SIZE;
	
BEGIN (* StandFunc *)
	Check (Scanner.lparen, noLParenError); funcno := x.a; rtype := x.type;
	IF funcno =	200 THEN SFunc_ABS (x)
	ELSIF funcno = 201 THEN SFunc_ODD (x)
	ELSIF (funcno >= 203) & (funcno <= 205) THEN SFunc_SHIFT (funcno - 203, x)
	ELSIF funcno = 206 THEN SFunc_FLOOR (x)
	ELSIF funcno = 207 THEN SFunc_FLT (x)
	ELSIF funcno = 208 THEN SFunc_ORD (x)
	ELSIF funcno = 209 THEN SFunc_CHR (x)
	ELSIF funcno = 301 THEN SFunc_SIZE (x)
	ELSE ASSERT(FALSE)
	END;
	IF (funcno # 303) & (funcno # 200) & (funcno # 310) & (funcno # 311) THEN
		x.type := rtype 
	END;
	funcno := 0;
	WHILE sym = Scanner.comma DO Scanner.Get (sym); expression (y);
		IF funcno = 0 THEN Scanner.Mark (tooMuchParamError); funcno := 1 END
	END;
	Check (Scanner.rparen, noRParenError)
END StandFunc;

PROCEDURE element (VAR x: Base.Item);
	VAR y, z: Base.Item;
BEGIN expression (y); CheckInt (y);
	IF sym = Scanner.upto THEN
		Scanner.Get (sym); expression (z); CheckInt (z);
		Generator.Set3 (x, y, z)
	ELSE Generator.Set2 (x, y)
	END
END element;

PROCEDURE set (VAR x: Base.Item);
BEGIN
	Scanner.Get (sym); Generator.Make_const (x, Base.setType, 0);
	IF sym # Scanner.rbrace THEN element (x);
		WHILE sym = Scanner.comma DO Scanner.Get (sym); element (x) END;
		Generator.Set1 (x); Check (Scanner.rbrace, 'No }')
	ELSE Scanner.Get (sym)
	END
END set;

PROCEDURE factor (VAR x: Base.Item);
	VAR obj: Base.Object;
BEGIN
	IF (sym < Scanner.char) OR (sym > Scanner.ident) THEN
		Scanner.Mark ('Expression expected');
		REPEAT Scanner.Get (sym)
		UNTIL (sym >= Scanner.char) & (sym <= Scanner.ident)
	END;
	IF sym = Scanner.ident THEN designator (x);
		IF x.mode = Base.cSFunc THEN StandFunc (x) END
	ELSIF sym = Scanner.int THEN
		Generator.Make_const (x, Base.intType, Scanner.ival); Scanner.Get (sym)
	ELSIF sym = Scanner.real THEN
		Generator.Make_const (x, Base.realType, Scanner.ival); Scanner.Get (sym)
	ELSIF sym = Scanner.char THEN
		Generator.Make_const (x, Base.charType, Scanner.ival); Scanner.Get (sym)
	ELSIF sym = Scanner.nil THEN
		Generator.Make_const (x, Base.nilType, 0); Scanner.Get (sym)
	ELSIF sym = Scanner.string THEN
		Generator.Make_const (x, Base.charType, ORD(Scanner.str[0]));
		IF Scanner.slen > 2 THEN Scanner.Mark ('String const not allowed') END;
		Scanner.Get (sym)
	ELSIF sym = Scanner.lparen THEN
		Scanner.Get (sym); expression (x); Check (Scanner.rparen, 'no )')
	ELSIF sym = Scanner.lbrace THEN set (x)
	ELSIF sym = Scanner.not THEN
		Scanner.Get (sym); factor (x); CheckBool (x); Generator.Not (x)
	ELSIF sym = Scanner.false THEN
		Generator.Make_const (x, Base.boolType, 0); Scanner.Get (sym)
	ELSIF sym = Scanner.true THEN
		Generator.Make_const (x, Base.boolType, 1); Scanner.Get (sym)
	ELSE Scanner.Mark('Not a factor'); MakeIntConst (x)
	END
END factor;

PROCEDURE term (VAR x: Base.Item);
	VAR op: INTEGER; y: Base.Item; 
BEGIN factor (x);
	WHILE (sym >= Scanner.times) & (sym <= Scanner.and) DO
		CheckValue (x); op := sym;
		IF (x.type.form = Base.tInteger) & (op # Scanner.and)
			& (op # Scanner.rdiv)
		OR (x.type.form IN {Base.tReal, Base.tSet})
			& ((op = Scanner.times) OR (op = Scanner.rdiv))
		OR (x.type = Base.boolType) & (op = Scanner.and) THEN
			IF x.type = Base.boolType THEN Generator.And1 (x) END
		ELSE Scanner.Mark (invalidOperationError)
		END;
		Scanner.Get (sym); factor (y);
		IF x.type.form = Base.tInteger THEN CheckInt (y);
			IF op = Scanner.times THEN Generator.Multiply (x, y, FALSE)
			ELSE Generator.Divide (op, x, y)
			END
		ELSIF x.type.form = Base.tReal THEN
			CheckReal (y); Generator.Multiply_real (op, x, y)
		ELSIF x.type = Base.setType THEN CheckSet (y);
			IF op = Scanner.times THEN Generator.Intersection (x, y)
			ELSE Generator.Symmetric_difference (x, y)
			END
		ELSIF x.type = Base.boolType THEN CheckBool (y); Generator.And2 (x, y)
		ELSE MakeIntConst (y); MakeIntConst (x)
		END
	END
END term;

PROCEDURE SimpleExpression (VAR x: Base.Item);
	CONST typeAddable = {Base.tInteger, Base.tReal, Base.tSet};
	VAR op: INTEGER; y: Base.Item;
BEGIN
	IF sym = Scanner.minus THEN Scanner.Get (sym); term (x);
		IF x.type.form IN typeAddable THEN Generator.Negate (x)
		ELSE Scanner.Mark ('Not an integer, real or set value')
		END
    ELSIF sym = Scanner.plus THEN Scanner.Get (sym); term (x)
    ELSE term (x)
    END;		
	WHILE (sym >= Scanner.plus) & (sym <= Scanner.or) DO
		CheckValue (x); op := sym;
		IF (x.type.form IN typeAddable) & (op # Scanner.or)
		OR (x.type = Base.boolType) & (op = Scanner.or) THEN
			IF x.type = Base.boolType THEN Generator.Or1 (x) END
		ELSE Scanner.Mark (invalidOperationError)
		END;
		Scanner.Get (sym); term (y);
		IF x.type.form = Base.tInteger THEN
			CheckInt (y); Generator.Add (op, x, y)
		ELSIF x.type.form = Base.tReal THEN
			CheckReal (y); Generator.Add_real (op, x, y)
		ELSIF x.type = Base.setType THEN CheckSet (y);
			IF op = Scanner.plus THEN Generator.Union (x, y)
			ELSE Generator.Negate (y); Generator.Intersection (x, y)
			END
		ELSIF x.type = Base.boolType THEN CheckBool (y); Generator.Or2 (x, y)
		ELSE MakeIntConst (y); MakeIntConst (x)
		END
	END
END SimpleExpression;

PROCEDURE expression0 (VAR x: Base.Item);
	CONST
		realRel = 0; intRel = 1; strRel = 2; setRel = 3; noRel = 4;
		typeHaveAllRel = {Base.tInteger, Base.tReal, Base.tChar};
	VAR
		y: Base.Item; xt: Base.Type; rel, relType: INTEGER;
BEGIN SimpleExpression (x);
	IF (sym >= Scanner.eql) & (sym <= Scanner.geq) THEN
		CheckValue (x); rel := sym; relType := intRel;
		IF (x.type.form IN typeHaveAllRel)
		OR (x.type = Base.setType) & (rel # Scanner.gtr) & (rel # Scanner.lss)
		OR (x.type = Base.boolType) & (rel <= Scanner.neq) THEN
			(* Ok *)
		ELSE Scanner.Mark (invalidOperationError); relType := noRel
		END;
		Scanner.Get (sym); SimpleExpression (y);
		IF relType # noRel THEN
			IF x.type.form = Base.tInteger THEN CheckInt (y)
			ELSIF x.type.form = Base.tReal THEN CheckReal (y);
				relType := realRel
			ELSIF x.type.form = Base.tChar THEN CheckChar (y)
			ELSIF x.type = Base.setType THEN CheckSet (y);
				IF rel > Scanner.neq THEN relType := setRel END
			ELSIF x.type = Base.boolType THEN CheckBool (y)
			END
		END;
		IF relType = intRel THEN Generator.Int_relation (rel, x, y)
		ELSIF relType = realRel THEN Generator.Real_relation (rel, x, y)
		ELSIF relType = setRel THEN Generator.Set_relation (rel, x, y)
		ELSE MakeIntConst (y); MakeConst (x, Base.boolType)
		END
	ELSIF sym = Scanner.in THEN
		CheckInt (x); Scanner.Get (sym); SimpleExpression (y);
		CheckSet (y); Generator.Member_test (x, y)
	END
END expression0;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Declaration *)

PROCEDURE IdentList (class: INTEGER; VAR first: Base.Object);
	VAR obj: Base.Object;
BEGIN
	SymTable.New (first, Scanner.id, class); identExport := FALSE;
	Scanner.Get (sym); CheckExport (first.export);
	WHILE sym = Scanner.comma DO Scanner.Get (sym);
		IF sym = Scanner.ident THEN SymTable.New (obj, Scanner.id, class);
			Scanner.Get (sym); CheckExport (obj.export);
			IF first = Base.guard THEN first := obj END
		ELSE Scanner.Mark (superflousCommaError)
		END
	END
END IdentList;

PROCEDURE FieldList (tp: Base.Type; VAR first: Base.Object);
	VAR fieldTp: Base.Type; field: Base.Object;
BEGIN
	IdentList (Base.cField, first); field := first;
	Check (Scanner.colon, noColonError); type (fieldTp);
	IF fieldTp.alignment > tp.alignment THEN
		tp.alignment := fieldTp.alignment
	END;
	WHILE field # Base.guard DO field.type := fieldTp;
		Generator.Align (tp.size, fieldTp.alignment);
		field.val := tp.size; tp.size := tp.size + fieldTp.size;
		tp.nptr := tp.nptr + fieldTp.nptr; field := field.next
	END
END FieldList;

PROCEDURE FieldListSequence (tp: Base.Type; VAR first: Base.Object);
	VAR field: Base.Object;
BEGIN
	IF sym = Scanner.ident THEN FieldList (tp, first)
	ELSE Union (tp, first)
	END;
	WHILE sym = Scanner.semicolon DO Scanner.Get (sym);
		IF sym = Scanner.ident THEN FieldList (tp, field)
		ELSIF sym = Scanner.union THEN Union (tp, field)
		ELSE Scanner.Mark (superflousSemicolonError)
		END
	END
END FieldListSequence;

PROCEDURE Union0 (tp: Base.Type; VAR first: Base.Object);
	VAR size, off, align, tpAlign: INTEGER;
		field: Base.Object;
BEGIN Scanner.Get (sym);
	IF (sym = Scanner.ident) OR (sym = Scanner.union) THEN
		off := tp.size; tpAlign := tp.alignment;
		tp.size := 0; tp.alignment := 1;
		FieldListSequence (tp, first);
		size := tp.size; align := tp.alignment;
		WHILE sym = Scanner.bar DO Scanner.Get (sym);
			IF (sym = Scanner.ident) OR (sym = Scanner.union) THEN
				tp.size := 0; tp.alignment := 1;
				FieldListSequence (tp, field);
				IF size < tp.size THEN size := tp.size END;
				IF align < tp.alignment THEN align := tp.alignment END
			ELSE Scanner.Mark (superflousBarError)
			END
		END;
		Generator.Align (off, align); field := first;
		WHILE field # Base.guard DO
			field.val := field.val + off; field := field.next
		END;
		tp.size := off + size;
		IF align > tpAlign THEN tp.alignment := align
		ELSE tp.alignment := tpAlign
		END
	END;
	Check (Scanner.end, noEndError)
END Union0;

PROCEDURE FormalType (VAR tp: Base.Type);
	VAR obj: Base.Object; len: INTEGER;
BEGIN tp := Base.intType;
	IF sym = Scanner.ident THEN qualident (obj);
		IF obj.class = Base.cType THEN tp := obj.type; CheckExport2 (obj)
		ELSE Scanner.Mark (notTypeError)
		END
	ELSIF sym = Scanner.array THEN
		Scanner.Get (sym); Check (Scanner.of, noOfError);
		Base.NewType (tp, Base.tArray); tp.len := -1; tp.base := Base.intType;
		IF sym = Scanner.ident THEN qualident (obj);
			IF obj.class = Base.cType THEN
				tp.base := obj.type; CheckExport2 (obj);
				IF tp.base = Base.byteType THEN tp.base := Base.sysByteType END
			ELSE Scanner.Mark (notTypeError)
			END
		ELSE Scanner.Mark (noIdentError)
		END
	ELSE Scanner.Mark (notTypeError)
	END
END FormalType;

PROCEDURE FPSection (VAR parblksize, nofpar: INTEGER);
	VAR cls, parSize: INTEGER; first, obj: Base.Object;
		readOnly, nilable: BOOLEAN;
		tp: Base.Type;
BEGIN nilable := FALSE;
	IF sym # Scanner.var THEN cls := Base.cVar
	ELSE Scanner.Get (sym); cls := Base.cRef
	END;
	IF sym = Scanner.lbrak THEN Scanner.Get (sym);
		IF sym = Scanner.nil THEN Scanner.Get (sym); nilable := TRUE END;
		Check (Scanner.rbrak, 'No ]')
	END;
	
	IF sym = Scanner.ident THEN
		SymTable.New (first, Scanner.id, cls); Scanner.Get (sym);
		IF sym = Scanner.times THEN Scanner.Mark ('Remove *') END;
		WHILE sym = Scanner.comma DO Scanner.Get (sym);
			IF sym = Scanner.ident THEN
				SymTable.New (obj, Scanner.id, cls); Scanner.Get (sym);
				IF first = Base.guard THEN first := obj END;
				IF sym = Scanner.times THEN Scanner.Mark ('Remove *') END;
			ELSE Scanner.Mark (superflousCommaError)
			END
		END
	ELSE Scanner.Mark (noIdentError); first := Base.guard
	END;
	
	Check (Scanner.colon, noColonError); FormalType (tp);
	parSize := Base.WordSize; readOnly := FALSE;
	IF tp.form IN Base.typeScalar THEN (* no change *)
	ELSIF (tp.form = Base.tArray) & (tp.len = -1) THEN
		IF cls = Base.cVar THEN readOnly := TRUE; cls := Base.cRef END
	ELSIF cls = Base.cVar THEN readOnly := TRUE;
		IF ~Generator.FitInReg(tp.size) THEN cls := Base.cRef END
	END;
		
	WHILE first # Base.guard DO first.val := parblksize;
		Generator.Fix_param_adr (first.val); first.type := tp;
		first.class := cls; first.param := TRUE; first.nilable := nilable;
		first.readonly := readOnly; first.tagged := FALSE;
		parblksize := parblksize + parSize; INC (nofpar); first := first.next
	END
END FPSection;

PROCEDURE FormalParameters (VAR tp: Base.Type);
	VAR obj: Base.Object;
BEGIN
	Base.NewType (tp, Base.tProcedure); tp.size := Base.WordSize;
	tp.alignment := Base.WordSize; tp.len := 0; tp.parblksize := 0;
	
	SymTable.OpenScope ('');
	IF sym = Scanner.lparen THEN Scanner.Get (sym);
		IF sym = Scanner.rparen THEN Scanner.Get (sym)
		ELSE FPSection (tp.parblksize, tp.len);
			WHILE sym = Scanner.semicolon DO Scanner.Get (sym);
				IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN
					FPSection (tp.parblksize, tp.len)
				ELSE Scanner.Mark (superflousSemicolonError)
				END
			END;
			Check (Scanner.rparen, noRParenError)
		END;
		IF sym = Scanner.colon THEN Scanner.Get (sym);
			IF sym = Scanner.ident THEN qualident (obj) ELSE obj := Base.guard
			END;
			IF obj.class = Base.cType THEN
				IF obj.type.form IN Base.typeScalar THEN
					tp.base := obj.type; CheckExport2 (obj)
				ELSE Scanner.Mark (notScalarError)
				END
			ELSE Scanner.Mark (notTypeError)
			END
		END
	END;
	tp.fields := SymTable.topScope.next; SymTable.CloseScope
END FormalParameters;

PROCEDURE RegisterUndefType (typ: Base.Type; basename: Base.IdentStr);
	VAR undef: UndefPtrList;
BEGIN
	NEW (undef); undef.typ := typ; Base.StrCopy(basename, undef.basename);
	undef.next := undefPtr; undefPtr := undef
END RegisterUndefType;
	
PROCEDURE CheckUndefList (obj: Base.Object);
	VAR p, prev: UndefPtrList;
BEGIN p := undefPtr;
	WHILE p # NIL DO
		IF p.basename = obj.name THEN p.typ.base := obj.type;
			IF p # undefPtr THEN prev.next := p.next ELSE undefPtr := p.next
			END
		ELSE prev := p
		END;
		p := p.next
	END
END CheckUndefList;
	
PROCEDURE CleanupUndefList;
	VAR msg: Base.String;
BEGIN
	WHILE undefPtr # NIL DO
		msg := 'Record type '; Strings.Append (undefPtr.basename, msg);
		Strings.Append (' is not defined', msg); Scanner.Mark (msg);
		undefPtr := undefPtr.next
	END
END CleanupUndefList;

PROCEDURE type0 (VAR tp: Base.Type);
	VAR obj, bfield: Base.Object; x: Base.Item; size, lev: INTEGER;
		id, str: Base.IdentStr;
BEGIN tp := Base.intType;
	IF (sym # Scanner.ident) & (sym < Scanner.array) THEN
		Scanner.Mark (notTypeError);
		REPEAT Scanner.Get (sym)
		UNTIL (sym = Scanner.ident) OR (sym >= Scanner.array)
	END;
	IF sym = Scanner.ident THEN qualident (obj);
		IF obj.class = Base.cType THEN tp := obj.type; CheckExport2 (obj)
		ELSE Scanner.Mark (notTypeError)
		END
	ELSIF sym = Scanner.array THEN
		Base.NewType (tp, Base.tArray);
		identExport := FALSE; Scanner.Get (sym);
		IF sym # Scanner.of THEN
			tp.len := 1; expression (x); CheckInt (x);
			IF x.mode = Base.cConst THEN
				IF x.a > 0 THEN tp.len := x.a
				ELSE Scanner.Mark ('Array length must be positive')
				END
			ELSE Scanner.Mark (notConstError); MakeIntConst (x)
			END;
			Check (Scanner.of, noOfError); type (tp.base);
			tp.size := tp.len * Generator.TypeSize(tp.base)
		ELSE
			tp.len := -1; Scanner.Get (sym); type (tp.base);
			IF (tp.base.form = Base.tArray) & (tp.len = -1) THEN
				Scanner.Mark ('Multidimension open array not allowed');
				tp.base := Base.intType
			END
		END;
		tp.alignment := tp.base.alignment
	ELSIF sym = Scanner.record THEN
		Base.NewType (tp, Base.tRecord); identExport := FALSE;
		tp.unsafe := TRUE; tp.len := 0; tp.size := 0; tp.alignment := 0;
		Scanner.Get (sym); SymTable.OpenScope ('');
		IF (sym = Scanner.ident) OR (sym = Scanner.union) THEN
			FieldListSequence (tp, obj)
		END;
		tp.fields := SymTable.topScope.next; SymTable.CloseScope;
		Check (Scanner.end, noEndError)
	(* ELSIF sym = Scanner.pointer THEN
		Base.NewType (tp, Base.tAddress); identExport := FALSE;
		tp.size := Base.WordSize; tp.alignment := Base.WordSize;
		tp.base := Base.intType;
		IF (defobj # NIL) & (defobj.type = tp) THEN
			Base.StrCopy (defobjId, defobj.name)
		END;
		Scanner.Get (sym); Check (Scanner.to, noToError);
		IF sym = Scanner.ident THEN SymTable.Find (obj, Scanner.id);
			IF obj # Base.guard THEN
				IF obj.class = Base.cType THEN tp.base := obj.type
				ELSE Scanner.Mark (notTypeError)
				END
			ELSE RegisterUndefType (tp, obj.name)
			END;
			Scanner.Get (sym)
		ELSIF sym = Scanner.record THEN type (tp.base)
		ELSIF sym = Scanner.array THEN
			Base.NewType (tp.base, Base.tArray); tp.base.len := -1;
			Scanner.Get (sym); Check (Scanner.of, noOfError);
			type (tp.base.base)
		ELSE Scanner.Mark (notTypeError)
		END *)
	ELSIF sym = Scanner.procedure THEN
		Scanner.Get (sym); FormalParameters (tp)
	ELSE Scanner.Mark (notTypeError)
	END
END type0;

PROCEDURE DeclarationSequence;
	VAR id: Base.IdentStr; obj, first, field: Base.Object; x: Base.Item;
		tp: Base.Type; exported: BOOLEAN; locblksize: INTEGER;
BEGIN
	IF (sym < Scanner.const) & (sym # Scanner.end) THEN
		Scanner.Mark ('Expect declaration section');
		REPEAT Scanner.Get (sym)
		UNTIL (sym >= Scanner.const) OR (sym = Scanner.end)
    END;
	IF sym = Scanner.const THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO Base.StrCopy (Scanner.id, id);
			Scanner.Get (sym); CheckExport (exported);
			Check (Scanner.eql, noEqualError); expression (x);
			IF x.mode # Base.cConst THEN
				Scanner.Mark (notConstError); MakeIntConst (x)
			END;
			SymTable.New (obj, id, Base.cConst); obj.export := exported;
			obj.val := x.a; obj.type := x.type; obj.lev := SymTable.curLev;
			Check (Scanner.semicolon, noSemicolonError)
		END
	END;
	IF sym = Scanner.type THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO Base.StrCopy (Scanner.id, id);
			SymTable.New (obj, 'TYPE', Base.cType);
			obj.lev := SymTable.curLev; Scanner.Get (sym);
			CheckExport (obj.export); identExport := FALSE;
			Check (Scanner.eql, noEqualError);
			defobj := obj; Base.StrCopy (id, defobjId);
			type (obj.type); tp := obj.type; defobj := NIL;
			Base.StrCopy (id, obj.name);
			IF tp.obj = NIL THEN tp.obj := obj END;
			IF tp.form = Base.tRecord THEN CheckUndefList (obj) END;
			Check (Scanner.semicolon, noSemicolonError)
		END;
		CleanupUndefList
	END;
	IF sym = Scanner.var THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO IdentList (Base.cVar, first);
			Check (Scanner.colon, noColonError); type (tp); obj := first;
			IF (tp.form = Base.tArray) & (tp.len = -1) THEN
				Scanner.Mark ('Open array variable not allowed');
				tp := Base.intType
			END;
			WHILE obj # Base.guard DO
				obj.lev := SymTable.curLev;
				obj.type := tp; obj := obj.next
			END;
			Check (Scanner.semicolon, noSemicolonError)
		END
	END;
	WHILE sym = Scanner.procedure DO
		identExport := FALSE; Scanner.Get (sym);
		IF sym = Scanner.ident THEN
			Base.StrCopy (Scanner.id, id); SymTable.New (obj, id, Base.cProc);
			Scanner.Get (sym); CheckExport (obj.export)
		ELSE Scanner.Mark (noIdentError); id[0] := 0X
		END;
		SymTable.OpenScope (id); SymTable.IncLevel (1);
		FormalParameters (tp); Check (Scanner.semicolon, noSemicolonError);
		IF id[0] # 0X THEN obj.type := tp END;
		SymTable.IncLevel (-1); SymTable.CloseScope
	END;
END DeclarationSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE import;
	VAR mod: Base.Object; symName: Base.IdentStr;
BEGIN SymTable.New (mod, Scanner.id, Base.cModule); Scanner.Get (sym);
	IF sym = Scanner.becomes THEN Scanner.Get (sym);
		IF sym = Scanner.ident THEN
			Base.StrCopy (Scanner.id, symName); Scanner.Get (sym)
		ELSE Scanner.Mark (noIdentError); Base.StrCopy (mod.name, symName)
		END
	ELSE Base.StrCopy (mod.name, symName)
	END;
	SymTable.Find_module_symfile (mod, symName)
END import;

PROCEDURE ImportList;
BEGIN Scanner.Get (sym);
	IF sym = Scanner.ident THEN import ELSE Scanner.Mark (noIdentError) END;
	WHILE sym = Scanner.comma DO Scanner.Get (sym);
		IF sym = Scanner.ident THEN import
		ELSE Scanner.Mark (superflousCommaError)
		END
	END;
	Check (Scanner.semicolon, noSemicolonError);
	IF Scanner.errcnt = 0 THEN SymTable.Import_modules END
END ImportList;

PROCEDURE Definition*;
	VAR modid: Base.IdentStr;
BEGIN
	Scanner.EnableDefinitionModuleMode; Base.ResetCompilerFlag;
	Scanner.Get (sym);
	IF sym = Scanner.ident THEN modid := Scanner.id; Scanner.Get (sym)
	ELSE modid := '@'; Scanner.Mark ('No definition module name')
	END;
	Check (Scanner.semicolon, noSemicolonError);
	IF modid # '@' THEN
		SymTable.Init (modid, TRUE); Generator.Init (modid);
		IF sym = Scanner.import THEN ImportList END;
		DeclarationSequence; Check (Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF modid # Scanner.id THEN Scanner.Mark ('Wrong module name') END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No module name after END')
		END;
		Check (Scanner.period, 'No ending .');
		Generator.Finish (TRUE)
	END
END Definition;

BEGIN
	expression := expression0; type := type0; Union := Union0
END Parser2.