MODULE Parser;

IMPORT
	Base, Scanner, SymTable, Generator;
	
CONST
	noSemicolonError = 'No ;';
	noEndError = 'END expected';
	noOfError = 'OF expected';
	noToError = 'TO expected';
	notConstError = 'Not a constant value';
	superflousCommaError = 'Superflous ,';
	superflousSemicolonError = 'Superflous ;';
	noColonError = 'No :';
	noEqualError = 'No =';
	noRParenError = 'No )';
	invalidOperationError = 'Invalid operation';
	notStrError = 'Not a string';
	notPointerError = 'Not a pointer';
	notExtError = 'Not a extension of the other type';
	notPointerOrProcedureError = 'Not a pointer or procedure';
	notPointerOrRecordError = 'Not a pointer or record';
	notTypeError = 'Not a type';
	notPointerOrRecordTypeError = 'Not a pointer or record type';
	notRecordTypeError = 'Not a record type';
	notProcError = 'Not a procedure';
	notGlobalProcError = 'Procedure is not global';
	notCompProcError = 'Incompatible procedures';
	notCompTypeError = 'Incompatible types';
	stringTooLongError = 'String is longer than destination';
	invalidExtError = 'Invalid extension';
	
VAR
	sym*: INTEGER;
	expression: PROCEDURE (VAR x: Base.Item);
	type: PROCEDURE (VAR tp: Base.Type; VAR tpObj: Base.Object);
	
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

PROCEDURE MakeVar (VAR x: Base.Item; tp: Base.Type);
BEGIN
	Generator.Clean_item (x); x.mode := Base.cVar; x.lev := 0; x.a := 0;
	x.type := tp
END MakeVar;

PROCEDURE CheckExport (VAR exported: BOOLEAN);
BEGIN (* stub *)
	IF sym = Scanner.times THEN Scanner.Get (sym) END
END CheckExport;

PROCEDURE CheckRecLevel (lev: INTEGER);
BEGIN
	IF lev = 0 THEN (* ok *)
	ELSE Scanner.Mark ('Pointer base type must be global')
	END
END CheckRecLevel;

PROCEDURE CheckValue (VAR x: Base.Item);
BEGIN
	IF x.mode IN Base.clsValue THEN (* ok *)
	ELSE Scanner.Mark ('Not a value')
	END
END CheckValue;

PROCEDURE CheckInt (VAR x: Base.Item);
BEGIN 
	IF (x.mode IN Base.clsValue) & (x.type.form = Base.tInteger) THEN (* ok *)
	ELSE Scanner.Mark ('Not an integer'); MakeIntConst (x)
	END
END CheckInt;

PROCEDURE CheckReal (VAR x: Base.Item);
BEGIN
	IF (x.mode IN Base.clsValue) & (x.type.form = Base.tReal) THEN (* ok *)
	ELSE Scanner.Mark ('Not a real number'); MakeConst (x, Base.realType)
	END
END CheckReal;

PROCEDURE CheckSet (VAR x: Base.Item);
BEGIN
	IF (x.mode IN Base.clsValue) & (x.type = Base.setType) THEN (* ok *)
	ELSE Scanner.Mark ('Not a SET'); MakeConst (x, Base.setType)
	END
END CheckSet;

PROCEDURE CheckBool (VAR x: Base.Item);
BEGIN
	IF (x.mode IN Base.clsValue) & (x.type = Base.boolType) THEN (* ok *)
	ELSE Scanner.Mark ('Not a BOOLEAN'); MakeConst (x, Base.boolType)
	END
END CheckBool;

PROCEDURE CheckChar (VAR x: Base.Item);
	VAR failed: BOOLEAN;
BEGIN failed := FALSE;
	IF x.mode IN Base.clsValue THEN
		IF x.type = Base.charType THEN (* ok *)
		ELSIF (x.type = Base.stringType) & (x.b <= 2) THEN
			Generator.Str_to_char (x)
		ELSE failed := TRUE
		END
	ELSE failed := TRUE
	END;
	IF ~ failed THEN (* ok *)
	ELSE Scanner.Mark ('Not a CHAR'); MakeConst (x, Base.charType)
	END
END CheckChar;

PROCEDURE CheckVar (VAR x: Base.Item; readonly: BOOLEAN);
BEGIN
	IF ~(x.mode IN Base.clsVariable) THEN
		Scanner.Mark ('Not a variable'); MakeVar (x, Base.intType)
	END;
	IF ~readonly & x.readonly THEN Scanner.Mark ('Read-only variable');
		IF x.type # Base.stringType THEN MakeVar (x, x.type)
		ELSE MakeVar (x, Base.intType)
		END
	END
END CheckVar;

PROCEDURE RecursiveIsExt (tpX, tpY: Base.Type) : BOOLEAN;
	RETURN (tpX = tpY)
	OR (tpX.len > tpY.len) & RecursiveIsExt(tpX.base, tpY)
END RecursiveIsExt;

PROCEDURE IsExt (tpX, tpY: Base.Type) : BOOLEAN;
BEGIN
	IF tpX.form = Base.tPointer THEN tpX := tpX.base END;
	IF tpY.form = Base.tPointer THEN tpY := tpY.base END;
	RETURN RecursiveIsExt(tpX, tpY)
END IsExt;

PROCEDURE SameTypes (tpX, tpY: Base.Type) : BOOLEAN;
	RETURN (tpX = tpY)
	OR ({tpX.form, tpY.form} = {Base.tArray}) & (tpX.len = 0) & (tpY.len = 0)
		& SameTypes (tpX.base, tpY.base)
END SameTypes;

PROCEDURE SameParams (parX, parY: Base.Object) : BOOLEAN;
	RETURN (parX = parY) (* = Base.guard *)
	OR (parX.class = parY.class) & (parX.readonly = parY.readonly)
		& SameTypes (parX.type, parY.type) & SameParams (parX.next, parY.next)
END SameParams;

PROCEDURE CompProc (tpX, tpY: Base.Type) : BOOLEAN;
	RETURN (tpX = tpY)
	OR (tpX.base = tpY.base) & (tpX.len = tpY.len)
		& SameParams (tpX.fields, tpY.fields)
END CompProc;

PROCEDURE CheckScalarAssignment (xtype: Base.Type; VAR y: Base.Item);
BEGIN
	IF xtype.form = Base.tInteger THEN CheckInt (y)
	ELSIF xtype.form = Base.tReal THEN CheckReal (y)
	ELSIF xtype = Base.setType THEN CheckSet (y)
	ELSIF xtype = Base.charType THEN CheckChar (y)
	ELSIF xtype = Base.boolType THEN CheckBool (y)
	ELSIF xtype.form = Base.tPointer THEN CheckValue (y);
		IF y.type.form = Base.tPointer THEN
			IF IsExt(y.type, xtype) THEN (* valid *)
			ELSE Scanner.Mark (notExtError)
			END
		ELSIF y.type # Base.nilType THEN (* valid *)
		ELSE Scanner.Mark (notPointerError); y.type := xtype
		END
	ELSIF xtype.form = Base.tProcedure THEN CheckValue (y);
		IF y.type.form = Base.tProcedure THEN
			IF CompProc (xtype, y.type) THEN (* valid *)
			ELSE Scanner.Mark (notCompProcError)
			END
		ELSIF y.type # Base.nilType THEN (* valid *)
		ELSE Scanner.Mark (notProcError); y.type := xtype
		END
	END
END CheckScalarAssignment;

PROCEDURE LoadVolatile (VAR x: Base.Item);
BEGIN
	IF x.mode IN Base.clsVariable + {Base.mCond} THEN Generator.load (x) END
END LoadVolatile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Procedure call *)

PROCEDURE ActualParameters (VAR x: Base.Item);
BEGIN
END ActualParameters;

PROCEDURE ProcedureCall (VAR x: Base.Item; proper: BOOLEAN);
BEGIN
END ProcedureCall;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE qualident (VAR obj: Base.Object);
BEGIN (* stub *)
	SymTable.Find (obj, Scanner.id);
	IF obj # Base.guard THEN (* ok *)
	ELSE Scanner.Mark ('Undefined identifier'); SymTable.Find (obj, 'INTEGER')
	END;
	Scanner.Get (sym)
END qualident;

PROCEDURE selector (VAR x: Base.Item);
END selector;

PROCEDURE designator (VAR x: Base.Item);
	VAR obj: Base.Object;
BEGIN
	qualident (obj);
	IF obj # Base.guard THEN Generator.Make_item (x, obj) ELSE MakeIntConst (x)
	END;
	selector (x)
END designator;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Expression *)

PROCEDURE StandFunc (VAR x: Base.Item);
END StandFunc;

PROCEDURE set (VAR x: Base.Item);
BEGIN
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
		IF x.mode = Base.cSFunc THEN StandFunc (x)
		ELSIF sym = Scanner.lparen THEN ProcedureCall (x, TRUE)
		ELSIF (x.mode = Base.cProc) & (x.lev > 0) THEN
			Scanner.Mark (notGlobalProcError)
		END
	ELSIF sym = Scanner.int THEN
		Generator.Make_const (x, Base.intType, Scanner.ival); Scanner.Get (sym)
	ELSIF sym = Scanner.real THEN
		Generator.Make_const (x, Base.realType, Scanner.ival); Scanner.Get (sym)
	ELSIF sym = Scanner.char THEN
		Generator.Make_const (x, Base.charType, Scanner.ival); Scanner.Get (sym)
	ELSIF sym = Scanner.nil THEN
		Generator.Make_const (x, Base.nilType, 0); Scanner.Get (sym)
	ELSIF sym = Scanner.string THEN
		Generator.Make_string (x); Scanner.Get (sym)
	ELSIF sym = Scanner.lparen THEN
		Scanner.Get (sym); expression (x); Check (Scanner.rparen, 'no )')
	ELSIF sym = Scanner.lbrace THEN
		Scanner.Get (sym); set (x); Check (Scanner.rbrace, 'no }')
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
			IF x.type # Base.boolType THEN LoadVolatile (x)
			ELSE Generator.And1 (x)
			END
		ELSE Scanner.Mark (invalidOperationError)
		END;
		Scanner.Get (sym); factor (y);
		IF x.type.form = Base.tInteger THEN
			CheckInt (y); Generator.Int_op2 (op, x, y)
		ELSIF x.type.form = Base.tReal THEN
			CheckReal (y); Generator.Real_op2 (op, x, y)
		ELSIF x.type = Base.setType THEN
			CheckSet (y); Generator.Set_op2 (op, x, y)
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
			IF x.type # Base.boolType THEN LoadVolatile (x)
			ELSE Generator.Or1 (x)
			END
		ELSE Scanner.Mark (invalidOperationError)
		END;
		Scanner.Get (sym); term (y);
		IF x.type.form = Base.tInteger THEN
			CheckInt (y); Generator.Int_op1 (op, x, y)
		ELSIF x.type.form = Base.tReal THEN
			CheckReal (y); Generator.Real_op1 (op, x, y)
		ELSIF x.type = Base.setType THEN
			CheckSet (y); Generator.Set_op1 (op, x, y)
		ELSIF x.type = Base.boolType THEN CheckBool (y); Generator.Or2 (x, y)
		ELSE MakeIntConst (y); MakeIntConst (x)
		END
	END
END SimpleExpression;

PROCEDURE _expression (VAR x: Base.Item);
	CONST
		realRel = 0; intRel = 1; strRel = 2; setRel = 3; noRel = 4;
		typeEqualityOnly = Base.typeAddress + {Base.tBoolean};
		typeHaveAllRel = {Base.tInteger,Base.tReal,Base.tChar,Base.tString};
	VAR
		y: Base.Item; xt: Base.Type; rel, relType: INTEGER;
BEGIN SimpleExpression (x);
	IF (sym >= Scanner.eql) & (sym <= Scanner.geq) THEN
		CheckValue (x); rel := sym; relType := intRel;
		IF (x.type.form IN typeHaveAllRel)
		OR (x.type = Base.setType) & (rel # Scanner.gtr) & (rel # Scanner.lss)
		OR (x.type.form IN typeEqualityOnly) & (rel <= Scanner.neq)
		OR (x.type.form = Base.tArray) & (x.type.base = Base.charType) THEN
			IF x.type.form IN Base.typeScalar THEN LoadVolatile (x) END
		ELSE Scanner.Mark (invalidOperationError); relType := noRel
		END;
		Scanner.Get (sym); SimpleExpression (y);
		IF relType # noRel THEN
			IF x.type.form = Base.tInteger THEN CheckInt (y)
			ELSIF x.type.form = Base.tReal THEN CheckReal (y);
				relType := realRel
			ELSIF x.type = Base.charType THEN CheckChar (y)
			ELSIF x.type = Base.setType THEN CheckSet (y);
				IF rel <= Scanner.neq THEN (* ok *) ELSE relType := setRel END
			ELSIF x.type = Base.boolType THEN CheckBool (y)
			ELSIF (x.type.form = Base.tArray) & (x.type.base = Base.charType)
			OR (x.type = Base.stringType) THEN CheckValue (y);
				IF (y.type = Base.stringType)
				OR (y.type.form = Base.tArray)
					& (y.type.base = Base.charType) THEN relType := strRel
				ELSIF (y.type = Base.charType) & (x.type = Base.stringType)
					& (x.b <= 2) THEN Generator.Str_to_char (x)
				ELSE Scanner.Mark (notStrError); relType := noRel
				END
			ELSIF x.type.form = Base.tPointer THEN CheckValue (y);
				IF y.type.form = Base.tPointer THEN 
					IF IsExt(x.type, y.type) OR IsExt(y.type, x.type) THEN
						(* valid *)
					ELSE Scanner.Mark (notExtError)
					END
				ELSIF y.type = Base.nilType THEN (* valid *)
				ELSE Scanner.Mark (notPointerError); relType := noRel
				END
			ELSIF x.type.form = Base.tProcedure THEN CheckValue (y);
				IF y.type.form = Base.tProcedure THEN 
					IF CompProc (x.type, y.type) THEN (* valid *)
					ELSE Scanner.Mark (notCompProcError)
					END
				ELSIF y.type = Base.nilType THEN (* valid *)
				ELSE Scanner.Mark (notProcError); relType := noRel
				END
			ELSE (* x is NIL *)
				IF (y.type = Base.nilType)
				OR (y.type.form IN Base.typeAddress) THEN (* valid *)
				ELSE Scanner.Mark (notPointerOrProcedureError); relType := noRel
				END
			END
		END;
		IF relType = intRel THEN Generator.Int_relation (rel, x, y)
		ELSIF relType = strRel THEN Generator.String_relation (rel, x, y)
		ELSIF relType = realRel THEN Generator.Real_relation (rel, x, y)
		ELSIF relType = setRel THEN Generator.Set_relation (rel, x, y)
		ELSE MakeIntConst (y); MakeConst (x, Base.boolType)
		END
	ELSIF sym = Scanner.in THEN
		CheckInt (x); LoadVolatile (x); Scanner.Get (sym); SimpleExpression (y);
		CheckSet (y); Generator.Member_test (x, y)
	ELSIF sym = Scanner.is THEN CheckValue (x);
		IF x.type.form IN Base.typeHasExt THEN (* valid *)
		ELSE Scanner.Mark (notPointerOrRecordError);
			MakeConst (x, Base.boolType)
		END;
		Scanner.Get (sym); SimpleExpression (y);
		IF x.type # Base.boolType THEN
			IF (y.mode = Base.cType) & (y.type.form IN Base.typeHasExt) THEN
				IF IsExt(x.type, y.type) OR IsExt(y.type, x.type) THEN
					(* valid *)
				ELSE Scanner.Mark (notExtError)
				END
			ELSE Scanner.Mark (notPointerOrRecordTypeError);
				MakeConst (y, x.type)
			END;
			Generator.Type_test (x, y.type, FALSE)
		ELSE MakeIntConst (y)
		END
	END
END _expression;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Declaration *)

PROCEDURE IdentList (class: INTEGER; VAR first: Base.Object);
	VAR obj: Base.Object;
BEGIN
	IF sym = Scanner.ident THEN
		SymTable.New (first, Scanner.id, class);
		Scanner.Get (sym); CheckExport (first.export);
		WHILE sym = Scanner.comma DO Scanner.Get (sym);
			IF sym = Scanner.ident THEN SymTable.New (obj, Scanner.id, class);
				Scanner.Get (sym); CheckExport (obj.export);
				IF first = Base.guard THEN first := obj END
			ELSE Scanner.Mark (superflousCommaError)
			END
		END
	ELSE first := Base.guard
	END
END IdentList;

PROCEDURE FieldList (tp: Base.Type);
	VAR fieldTp: Base.Type; field, bfield, fieldTpObj: Base.Object;
BEGIN
	IdentList (Base.cField, field); Check (Scanner.colon, noColonError);
	type (fieldTp, fieldTpObj);
	IF fieldTp.alignment > tp.alignment THEN
		tp.alignment := fieldTp.alignment
	END;
	WHILE field # Base.guard DO
		field.type := fieldTp; field.tpObj := fieldTpObj;
		Generator.Align (tp.size, fieldTp.alignment); field.val := tp.size;
		INC (tp.size, fieldTp.size); INC (tp.numPtr, fieldTp.numPtr);
		field := field.next
	END
END FieldList;

PROCEDURE FPSection (VAR parblksize, nofpar: INTEGER);
BEGIN
END FPSection;

PROCEDURE ProcedureType (VAR tp: Base.Type; VAR parblksize: INTEGER);
	VAR cls: INTEGER;
BEGIN Base.NewType (tp, Base.tProcedure);
	tp.size := Base.WordSize; tp.alignment := Base.WordSize;
	tp.len := 0; parblksize := 0;
	IF sym = Scanner.lparen THEN Scanner.Get (sym);
		IF sym = ORS.rparen THEN Scanner.Get(sym)
      ELSE FPSection(size, nofpar);
        WHILE sym = ORS.semicolon DO ORS.Get(sym); FPSection(size, nofpar) END ;
        Check(ORS.rparen, "no )")
      END ;
		Check (Scanner.rparen, noRParenError);
	END
END ProcedureType;

PROCEDURE _type (VAR tp: Base.Type; VAR tpObj: Base.Object);
	VAR obj, bfield: Base.Object; x: Base.Item; size: INTEGER;
BEGIN
	IF (sym # Scanner.ident) & (sym < Scanner.array) THEN
		Scanner.Mark (notTypeError);
		REPEAT Scanner.Get (sym)
		UNTIL (sym = Scanner.ident) OR (sym >= Scanner.array)
	END;
	tpObj := NIL;
	IF sym = Scanner.ident THEN qualident (obj);
		IF obj.class = Base.cType THEN tp := obj.type; tpObj := obj
		ELSE Scanner.Mark (notTypeError)
		END
	ELSIF sym = Scanner.array THEN Base.NewType (tp, Base.tArray);
		Scanner.Get (sym); expression (x); CheckInt (x);
		IF x.mode = Base.cConst THEN
			IF x.a > 0 THEN tp.len := x.a
			ELSE Scanner.Mark ('Array length must be positive'); tp.len := 1
			END
		ELSE Scanner.Mark (notConstError); MakeIntConst (x); tp.len := 1
		END;
		Check (Scanner.of, noOfError); type (tp.base, tp.baseTpObj);
		tp.alignment := tp.base.alignment; size := tp.base.size;
		Generator.Align (size, tp.alignment); tp.size := tp.len * size;
		tp.numPtr := tp.base.numPtr * tp.len
	ELSIF sym = Scanner.record THEN Base.NewType (tp, Base.tRecord);
		tp.len := 0; tp.size := 0; tp.alignment := 0; Scanner.Get (sym);
		IF sym = Scanner.lparen THEN
			IF SymTable.curLev = 0 THEN (* ok *)
			ELSE Scanner.Mark ('Extension of local types not implemented')
			END;
			qualident (obj);
			IF obj.class = Base.cType THEN
				IF obj.type.form = Base.tRecord THEN
					tp.base := obj.type; tp.baseTpObj := obj
				ELSIF obj.type.form = Base.tPointer THEN
					IF obj.type.base # Base.intType THEN
						tp.base := obj.type.base; tp.baseTpObj := obj
					ELSE Scanner.Mark ('Undefined record type')
					END
				ELSE Scanner.Mark (invalidExtError)
				END;
				IF tp.base # NIL THEN
					tp.len := tp.base.len + 1; tp.size := tp.base.size;
					tp.alignment := tp.base.alignment;
					tp.numPtr := tp.base.numPtr
				END;
				IF tp.len < Base.MaxExtension THEN (* ok *)
				ELSE Scanner.Mark ('Extension level too deep')
				END
			ELSE Scanner.Mark (notTypeError)
			END;
			Check (Scanner.rparen, noRParenError)
        END;
		SymTable.OpenScope ('');
		IF tp.base # NIL THEN bfield := tp.base.fields;
			WHILE bfield # Base.guard DO
				SymTable.New (obj, bfield.name, Base.cField); obj^ := bfield^;
				obj.lev := SymTable.curLev; bfield := bfield.next
			END
		END;
		IF sym = Scanner.ident THEN FieldList (tp);
			WHILE sym = Scanner.semicolon DO
				IF sym = Scanner.ident THEN FieldList (tp)
				ELSE Scanner.Mark (superflousSemicolonError)
				END
			END
		END;
		tp.fields := SymTable.topScope.next; SymTable.CloseScope;
		Check (Scanner.end, noEndError)
	ELSIF sym = Scanner.pointer THEN Check (Scanner.to, noToError);
		Base.NewType (tp, Base.tPointer); tp.size := Base.WordSize;
		tp.alignment := Base.WordSize; tp.numPtr := 1; tp.base := Base.intType;
		IF sym = Scanner.ident THEN SymTable.Find (obj, Scanner.id);
			IF obj # Base.guard THEN
				IF (obj.class = Base.cType) & (obj.type.form = Base.tRecord)
				THEN CheckRecLevel (obj.lev);
					tp.base := obj.type; tp.baseTpObj := obj;
				ELSE Scanner.Mark ('Invalid base type')
				END
			ELSE CheckRecLevel (SymTable.curLev);
				SymTable.RegisterUndefType (tp, obj.name, FALSE)
			END
		ELSIF sym = Scanner.record THEN
			CheckRecLevel (SymTable.curLev); type (tp.base, tp.baseTpObj)
		ELSE Scanner.Mark (notRecordTypeError)
		END
	ELSIF sym = Scanner.procedure THEN SymTable.OpenScope ('');
		Scanner.Get (sym); ProcedureType (tp, size); SymTable.CloseScope
	END
END _type;

PROCEDURE DeclarationSequence (VAR varsize: INTEGER);
	VAR id: Base.IdentStr;
		exported: BOOLEAN;
		x: Base.Item;
		obj, first, tpObj: Base.Object;
		tp: Base.Type;
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
			IF (x.mode = Base.cConst)
			OR (x.mode = Base.cVar) & (x.type = Base.stringType) THEN (* ok *)
			ELSE Scanner.Mark (notConstError); MakeIntConst (x)
			END;
			SymTable.New (obj, id, Base.cConst); obj.export := exported;
			obj.val := x.a; obj.val2 := x.b; obj.type := x.type;
			IF x.type.form = Base.tString THEN obj.lev := -1
			ELSE obj.lev := SymTable.curLev
			END;
			Check (Scanner.semicolon, noSemicolonError)
		END
	END;
	IF sym = Scanner.type THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO Base.StrCopy (Scanner.id, id);
			Scanner.Get (sym); CheckExport (exported);
			Check (Scanner.eql, noEqualError); type (tp, tpObj);
			SymTable.New (obj, id, Base.cType); obj.export := exported;
			obj.type := tp; obj.tpObj := tpObj; obj.lev := SymTable.curLev;
			IF tp.form = Base.tRecord THEN SymTable.CheckUndefList (obj) END;
			Check (Scanner.semicolon, noSemicolonError)
		END;
		SymTable.CleanupUndefList
	END;
	IF sym = Scanner.var THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			IdentList (Base.cVar, first); Check (Scanner.colon, noColonError);
			type (tp, tpObj); obj := first;
			WHILE obj # Base.guard DO
				obj.type := tp; obj.tpObj := tpObj; obj.lev := SymTable.curLev;
				varsize := varsize - tp.size;
				Generator.Align (varsize, tp.alignment); obj.val := varsize;
				obj := obj.next
			END;
			Check (Scanner.semicolon, noSemicolonError)
		END
	END;
	IF sym = Scanner.procedure THEN 
	END;
END DeclarationSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Statements *)

PROCEDURE StandProc (VAR x: Base.Item);
BEGIN
END StandProc;

PROCEDURE StatementSequence;
	VAR x, y, z, t: Base.Item; L: INTEGER; obj: Base.Object;
BEGIN
	REPEAT
		IF ~ ((sym = Scanner.ident)
			OR (sym >= Scanner.if) & (sym <= Scanner.for)
			OR (sym >= Scanner.semicolon)) THEN
			Scanner.Mark ('Statement expected');
			REPEAT Scanner.Get (sym)
			UNTIL (sym = Scanner.ident) OR (sym >= Scanner.if)
		END;
		IF sym = Scanner.ident THEN
			designator (x);
			IF sym = Scanner.becomes THEN CheckVar (x, FALSE);
				IF (x.type.form = Base.tArray) & (x.type.len = 0) THEN
					Scanner.Mark ('Open array assignment not supported')
				END;
				Scanner.Get (sym); expression (y);
				IF x.type.form IN Base.typeScalar THEN
					CheckScalarAssignment (x.type, y); Generator.Store (x, y)
				ELSIF (x.type.form # Base.tArray)
					OR (x.type.base # Base.charType) THEN CheckValue (y);
					IF y.type # x.type THEN Scanner.Mark (notCompTypeError);
						MakeIntConst (y); y.mode := Base.cVar; y.type := x.type
					END;
					Generator.Store_struct (x, y)
				ELSE (* Char array *)
					CheckValue (y);
					IF y.type = x.type THEN Generator.Store_struct (x, y)
					ELSIF y.type = Base.stringType THEN
						IF y.b <= x.type.len + 1 THEN
							Generator.Store_string (x, y)
						ELSE Scanner.Mark (stringTooLongError);
							MakeIntConst (y); MakeIntConst (x)
						END
					ELSE Scanner.Mark (notCompTypeError);
						MakeIntConst (y); MakeIntConst (x)
					END
				END
			ELSIF x.mode = Base.cSProc THEN StandProc (x)
			ELSIF (x.mode = Base.cProc) OR (sym = Scanner.lparen) THEN
				ProcedureCall (x, TRUE)
			ELSE Scanner.Mark ('Invalid statement'); Scanner.Get (sym)
			END
		ELSIF sym = Scanner.if THEN Scanner.Get (sym)
		ELSIF sym = Scanner.while THEN Scanner.Get (sym)
		ELSIF sym = Scanner.repeat THEN Scanner.Get (sym)
		ELSIF sym = Scanner.for THEN Scanner.Get (sym)
		ELSIF sym = Scanner.case THEN Scanner.Get (sym)
		END;
		IF sym = Scanner.semicolon THEN Scanner.Get(sym)
		ELSIF sym < Scanner.semicolon THEN Scanner.Mark (noSemicolonError)
		END
	UNTIL sym > Scanner.semicolon
END StatementSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ImportList;
BEGIN Scanner.Get (sym);
	IF sym = Scanner.ident THEN Scanner.Get (sym) END;
	WHILE sym = Scanner.comma DO Scanner.Get (sym);
		IF sym # Scanner.ident THEN Scanner.Mark (superflousCommaError)
		ELSE Scanner.Get (sym)
		END
	END;
	Check (Scanner.semicolon, noSemicolonError)
END ImportList;
	
PROCEDURE Module*;
	VAR modid: Base.IdentStr; varsize: INTEGER;
BEGIN
	Base.ResetCompilerFlag;
	Scanner.Get (sym); Check (Scanner.module, 'No MODULE keyword');
	IF sym = Scanner.ident THEN modid := Scanner.id; Scanner.Get (sym)
	ELSE modid := '@'; Scanner.Mark ('No module name')
	END;
	Check (Scanner.semicolon, noSemicolonError);
	IF modid # '@' THEN
		Base.Init; SymTable.Init (modid); Generator.Init (modid);
		IF sym = Scanner.import THEN ImportList END;
		varsize := 0; DeclarationSequence (varsize);
		Generator.Enter (NIL, 0);
		IF sym = Scanner.begin THEN Scanner.Get (sym); StatementSequence END;
		Generator.Return; Generator.Module_init; Generator.Module_exit;
		Check (Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF modid # Scanner.id THEN Scanner.Mark ('Wrong module name') END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No module identifier after END')
		END;
		Check (Scanner.period, 'No ending .'); Generator.Finish
	END
END Module;

BEGIN
	expression := _expression; type := _type
END Parser.