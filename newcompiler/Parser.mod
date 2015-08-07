MODULE Parser;

IMPORT
	Base, Scanner, SymTable, Generator;
	
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
	noColonError = 'No :';
	noPeriodError = 'No .';
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
	noIdentError = 'Expect an identifier';
	tooMuchParamError = 'Too much parameters';
	undefinedError = 'Undefined identifier';
	
VAR
	sym*: INTEGER;
	identExport: BOOLEAN;
	expression: PROCEDURE (VAR x: Base.Item);
	type: PROCEDURE (VAR tp: Base.Type);
	
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
	IF sym = Scanner.times THEN Scanner.Get (sym);
		IF SymTable.curLev = 0 THEN exported := TRUE; identExport := TRUE
		ELSE Scanner.Mark ('Cannot export non-global')
		END
	END
END CheckExport;

PROCEDURE CheckExport2 (exported: BOOLEAN);
BEGIN
	IF identExport & ~ exported THEN Scanner.Mark ('This type is not exported')
	END
END CheckExport2;

PROCEDURE CheckRecLevel (lev: INTEGER);
BEGIN IF lev # 0 THEN Scanner.Mark ('Pointer base type must be global') END
END CheckRecLevel;

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
	IF ~ (x.mode IN Base.clsValue) OR (x.type = Base.boolType) THEN
		Scanner.Mark ('Not a BOOLEAN'); MakeConst (x, Base.boolType)
	END
END CheckBool;

PROCEDURE CheckChar (VAR x: Base.Item);
BEGIN
	IF x.mode IN Base.clsValue THEN
		IF (x.type = Base.stringType) & (x.b <= 2) THEN
			Generator.Str_to_char (x)
		ELSIF x.type # Base.charType THEN
			Scanner.Mark ('Not a CHAR'); MakeConst (x, Base.charType)
		END
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

PROCEDURE CompArray (tpX, tpY: Base.Type) : BOOLEAN;
	RETURN (tpX.base = tpY.base)
	OR ({tpX.base.form, tpY.base.form} = {Base.tArray})
		& (tpX.base.len = 0) & CompArray (tpX.base, tpY.base)
END CompArray;

PROCEDURE CheckScalarAssignment (xtype: Base.Type; VAR y: Base.Item);
BEGIN
	IF xtype.form = Base.tInteger THEN CheckInt (y)
	ELSIF xtype.form = Base.tReal THEN CheckReal (y)
	ELSIF xtype = Base.setType THEN CheckSet (y)
	ELSIF xtype = Base.charType THEN CheckChar (y)
	ELSIF xtype = Base.boolType THEN CheckBool (y)
	ELSIF xtype.form = Base.tPointer THEN CheckValue (y);
		IF y.type.form = Base.tPointer THEN
			IF ~IsExt(y.type, xtype) THEN Scanner.Mark (notExtError) END
		ELSIF y.type # Base.nilType THEN
			Scanner.Mark (notPointerError); y.type := xtype
		END
	ELSIF xtype.form = Base.tProcedure THEN CheckValue (y);
		IF y.type.form = Base.tProcedure THEN
			IF ~CompProc (xtype, y.type) THEN Scanner.Mark (notCompProcError)
			END
		ELSIF y.type # Base.nilType THEN
			Scanner.Mark (notProcError); y.type := xtype
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

PROCEDURE Parameter (VAR call: Generator.ProcCall);
	VAR x: Base.Item; ftype: Base.Type; fpar: Base.Object;
BEGIN expression (x); INC (call.nofact);
	IF call.nofact <= call.nofpar THEN
		fpar := call.fpar; ftype := fpar.type;
		IF fpar.class = Base.cVar THEN
			IF ftype.form IN Base.typeScalar THEN
				CheckScalarAssignment (ftype, x)
			ELSE CheckValue (x);
				IF (x.type # ftype) THEN
					IF {ftype.form, x.type.form} # {Base.tRecord} THEN
						Scanner.Mark (notCompTypeError)
					ELSIF ~IsExt(x.type, ftype) THEN Scanner.Mark (notExtError)
					END
				END
			END;
			Generator.Value_param (x, call)
		ELSE CheckVar (x, fpar.readonly);
			IF (ftype.form = Base.tRecord) & (x.type.form = Base.tRecord) THEN
				IF IsExt(x.type, ftype) THEN
					IF ~fpar.readonly THEN Generator.Record_param (x, call)
					ELSE Generator.Ref_param (x, call)
					END
				ELSE Scanner.Mark (notExtError); MakeIntConst (x)
				END
			ELSIF (ftype.form = Base.tArray) & (ftype.len = 0)
				& (x.type.form = Base.tArray) & CompArray (ftype, x.type) THEN
				Generator.Array_param (x, call)
			ELSIF x.type = ftype THEN Generator.Ref_param (x, call)
			ELSE Scanner.Mark (notCompTypeError); MakeIntConst (x)
			END
		END;
		call.fpar := call.fpar.next
	ELSE
		IF call.nofact = call.nofpar + 1 THEN Scanner.Mark ('Too much params')
		END;
		MakeIntConst (x)
	END
END Parameter;

PROCEDURE ActualParameters (VAR call: Generator.ProcCall);
BEGIN Scanner.Get (sym);
	IF sym # Scanner.rparen THEN Parameter (call);
		WHILE sym = Scanner.comma DO Scanner.Get (sym);
			IF sym # Scanner.rparen THEN Parameter (call)
			ELSE Scanner.Mark (superflousCommaError)
			END
		END;
		Check (Scanner.rparen, noRParenError)
	END;
	IF call.nofact < call.nofpar THEN Scanner.Mark ('Not enough params') END
END ActualParameters;

PROCEDURE ProcedureCall (VAR x: Base.Item; proper: BOOLEAN);
	VAR call: Generator.ProcCall;
BEGIN
	IF proper & (x.type.base # NIL) THEN Scanner.Mark ('Need proper proc')
	ELSIF ~proper & (x.type.base = NIL) THEN Scanner.Mark ('Need function')
	END;
	Generator.Prepare_to_call (x, call);
	IF sym = Scanner.lparen THEN ActualParameters (call)
	ELSIF x.type.len > 0 THEN Scanner.Mark ('Need params')
	END;
	Generator.Call (call);
	IF ~proper THEN Generator.Return_value (x)
	ELSE MakeIntConst (x)
	END
END ProcedureCall;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE qualident (VAR obj: Base.Object);
BEGIN (* stub *)
	SymTable.Find (obj, Scanner.id);
	IF obj # Base.guard THEN
		IF obj.class = Base.cModule THEN
			Scanner.Mark ('Imported identifier not supported yet');
			Scanner.Get (sym); Check (Scanner.period, noPeriodError);
			IF sym # Scanner.ident THEN Scanner.Mark (noIdentError) END;
			SymTable.Find (obj, 'INTEGER')
		END;
	ELSE Scanner.Mark (undefinedError)
	END;
	Scanner.Get (sym)
END qualident;

PROCEDURE TypeTest (VAR x: Base.Item; guard: BOOLEAN);
	VAR y: Base.Item;
BEGIN
	Scanner.Get (sym); expression (y);
	(* If x is bool, that means there is an error before *)
	IF x.type # Base.boolType THEN
		IF (y.mode = Base.cType) & (y.type.form IN Base.typeHasExt) THEN
			IF ~IsExt(x.type, y.type) & ~IsExt(y.type, x.type) THEN
				Scanner.Mark (notExtError)
			END
		ELSE Scanner.Mark (notPointerOrRecordTypeError); MakeConst (y, x.type)
		END;
		Generator.Type_test (x, y.type, guard)
	ELSE MakeIntConst (y)
	END
END TypeTest;

PROCEDURE selector (VAR x: Base.Item);
	VAR exit: BOOLEAN; obj: Base.Object; tp: Base.Type; y: Base.Item;
BEGIN exit := FALSE;
	WHILE ~exit DO
		IF sym = Scanner.period THEN
			IF (x.mode IN Base.clsValue) & (x.type.form IN Base.typeHasExt) THEN
				IF x.type.form = Base.tPointer THEN Generator.Deref (x) END;
				tp := x.type; Scanner.Get (sym);
				IF sym = Scanner.ident THEN
					IF tp # Base.intType THEN
						SymTable.FindField (obj, Scanner.id, tp);
						IF obj # Base.guard THEN Generator.Field (x, obj)
						ELSE Scanner.Mark ('Field not found')
						END
					END;
					Scanner.Get (sym)
				ELSE Scanner.Mark (noIdentError)
				END
			ELSE Scanner.Mark (notPointerOrRecordError)
			END
		ELSIF sym = Scanner.lbrak THEN
			IF (x.mode IN Base.clsValue) & (x.type.form = Base.tArray) THEN
				Scanner.Get (sym); expression (y); CheckInt (y);
				Generator.Index (x, y);
				WHILE sym = Scanner.comma DO
					Scanner.Get (sym);
					IF sym # Scanner.rbrak THEN
						expression (y); CheckInt (y); Generator.Index (x, y)
					ELSE Scanner.Mark (superflousCommaError)
					END
				END;
				Check (Scanner.rbrak, 'No ]')
			ELSE Scanner.Mark ('Not an array')
			END
		ELSIF sym = Scanner.arrow THEN
			IF (x.mode IN Base.clsValue) & (x.type.form = Base.tPointer) THEN
				Generator.Deref (x)
			ELSE Scanner.Mark (notPointerError)
			END;
			Scanner.Get (sym)
		ELSIF (sym = Scanner.lparen) & (x.mode IN Base.clsValue)
			& (x.type.form IN Base.typeHasExt)
		THEN TypeTest (x, TRUE); Check (Scanner.rparen, noRParenError)
		ELSE exit := TRUE
		END
	END
END selector;

PROCEDURE designator (VAR x: Base.Item);
	VAR obj: Base.Object;
BEGIN
	qualident (obj);
	IF obj # Base.guard THEN Generator.Make_item (x, obj)
	ELSE MakeIntConst (x)
	END;
	selector (x)
END designator;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Expression *)

PROCEDURE StandFunc (VAR x: Base.Item);
END StandFunc;

PROCEDURE element (VAR x : Base.Item);
	VAR y, z : Base.Item;
BEGIN
	expression (y); CheckInt (y);
	IF sym = Scanner.upto THEN
		IF y.mode # Base.cConst THEN Generator.load (y) END;
		Scanner.Get (sym); expression (z); CheckInt (z);
		Generator.Set3 (x, y, z)
	ELSE Generator.Set2 (x, y)
	END
END element;

PROCEDURE set (VAR x : Base.Item);
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
		IF x.mode = Base.cSFunc THEN StandFunc (x)
		ELSIF sym = Scanner.lparen THEN
			IF (x.mode IN Base.clsValue) & (x.type.form = Base.tProcedure) THEN
				ProcedureCall (x, TRUE)
			ELSE Scanner.Mark (notProcError)
			END
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
				IF rel > Scanner.neq THEN relType := setRel END
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
					IF ~IsExt(x.type, y.type) & ~IsExt(y.type, x.type) THEN
						Scanner.Mark (notExtError)
					END
				ELSIF y.type # Base.nilType THEN
					Scanner.Mark (notPointerError); relType := noRel
				END
			ELSIF x.type.form = Base.tProcedure THEN CheckValue (y);
				IF y.type.form = Base.tProcedure THEN 
					IF ~CompProc (x.type, y.type) THEN
						Scanner.Mark (notCompProcError)
					END
				ELSIF y.type # Base.nilType THEN
					Scanner.Mark (notProcError); relType := noRel
				END
			(* else x is NIL *)
			ELSIF (y.type # Base.nilType) & ~(y.type.form IN Base.typeAddress)
			THEN Scanner.Mark (notPointerOrProcedureError); relType := noRel
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
		IF ~(x.type.form IN Base.typeHasExt) THEN
			Scanner.Mark (notPointerOrRecordError); MakeConst (x, Base.boolType)
		END;
		TypeTest (x, FALSE)
	END
END _expression;

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
		ELSIF sym = Scanner.if THEN
			L := 0; Scanner.Get (sym); expression (x); CheckBool (x);
			Generator.CFJump (x); Check (Scanner.then, noThenError);
			StatementSequence;
			WHILE sym = Scanner.elsif DO
				Generator.FJump (L); Generator.Fix_link (x.a);
				Scanner.Get (sym); expression (x); CheckBool (x);
				Generator.CFJump (x); Check (Scanner.then, noThenError);
				StatementSequence
			END;			
			IF sym = Scanner.else THEN
				Generator.FJump (L); Generator.Fix_link (x.a);
				Scanner.Get (sym); StatementSequence
			ELSE Generator.Fix_link (x.a)
			END;
			Generator.Fix_link (L); Check (Scanner.end, noEndError)
		ELSIF sym = Scanner.while THEN
			L := Generator.pc; Scanner.Get (sym); expression (x); CheckBool (x);
			Generator.CFJump (x); Check (Scanner.do, noDoError);
			StatementSequence;
			WHILE sym = Scanner.elsif DO
				Generator.BJump (L); Generator.Fix_link (x.a);
				Scanner.Get (sym); expression (x); CheckBool (x);
				Generator.CFJump (x); Check (Scanner.do, noDoError);
				StatementSequence
			END;
			Generator.BJump (L); Generator.Fix_link (x.a);
			Check (Scanner.end, noEndError)
		ELSIF sym = Scanner.repeat THEN
			L := Generator.pc; Scanner.Get (sym); StatementSequence;
			Check (Scanner.until, 'UNTIL expected'); expression (x);
			CheckBool (x); Generator.CBJump (x, L)
		ELSIF sym = Scanner.for THEN Scanner.Get (sym);
			IF sym = Scanner.ident THEN SymTable.Find (obj, Scanner.id);
				IF obj # Base.guard THEN Generator.Make_item (x, obj);
					CheckVar (x, FALSE); CheckInt (x)
				ELSE Scanner.Mark (undefinedError); MakeVar (x, Base.intType)
				END;
			ELSE Scanner.Mark (noIdentError); MakeVar (x, Base.intType)
			END;
			Check (Scanner.becomes, 'No :='); expression (y); CheckInt (y);
			Generator.Store (x, y); Check (Scanner.to, noToError);
			expression (z); CheckInt (z); Generator.For1 (z);
			IF sym = Scanner.by THEN
				Scanner.Get (sym); expression (t); CheckInt (t);
				IF t.mode # Scanner.const THEN
					Scanner.Mark (notConstError); MakeIntConst (t)
				END
			END;
			L := Generator.pc; Generator.For2 (x, z);
			Check (Scanner.do, noDoError); StatementSequence;
			Check (Scanner.end, noEndError); Generator.For3 (x, z, t.a, L)
		ELSIF sym = Scanner.case THEN
			Scanner.Mark ('CASE statement not supported yet!');
			Scanner.Get (sym)
			(* Implement later *)
		END;
		IF sym = Scanner.semicolon THEN Scanner.Get(sym)
		ELSIF sym < Scanner.semicolon THEN Scanner.Mark (noSemicolonError)
		END
	UNTIL sym > Scanner.semicolon
END StatementSequence;

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

PROCEDURE FieldList (tp: Base.Type);
	VAR fieldTp: Base.Type; field, bfield: Base.Object;
BEGIN
	IdentList (Base.cField, field); Check (Scanner.colon, noColonError);
	type (fieldTp);
	IF fieldTp.alignment > tp.alignment THEN
		tp.alignment := fieldTp.alignment
	END;
	WHILE field # Base.guard DO field.type := fieldTp;
		Generator.Align (tp.size, fieldTp.alignment); field.val := tp.size;
		INC (tp.size, fieldTp.size); INC (tp.numPtr, fieldTp.numPtr);
		field := field.next
	END
END FieldList;

PROCEDURE FormalType (VAR tp: Base.Type);
	VAR obj: Base.Object;
BEGIN tp := Base.intType;
	IF sym = Scanner.ident THEN qualident (obj);
		IF obj.class = Base.cType THEN tp := obj.type; CheckExport2 (obj.export)
		ELSE Scanner.Mark (notTypeError)
		END
	ELSIF sym = Scanner.array THEN
		Scanner.Get (sym); Check (Scanner.of, noOfError);
		Base.NewType (tp, Base.tArray); FormalType (tp.base); tp.len := 0;
		IF (tp.base.form # Base.tArray) OR (tp.base.len > 0) THEN
			tp.size := Base.WordSize * 2
		ELSE tp.size := tp.base.size + Base.WordSize
		END
	ELSE Scanner.Mark (notTypeError)
	END
END FormalType;

PROCEDURE FPSection (VAR parblksize, nofpar: INTEGER);
	VAR cls, parSize: INTEGER; first, obj: Base.Object;
		readOnly, openArray: BOOLEAN; tp: Base.Type;
BEGIN
	IF sym # Scanner.var THEN cls := Base.cVar
	ELSE Scanner.Get (sym); cls := Base.cRef
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
	parSize := Base.WordSize; readOnly := FALSE; openArray := FALSE;
	IF tp.form IN Base.typeScalar THEN (* no change *)
	ELSIF (tp.form = Base.tArray) & (tp.len = 0) THEN
		parSize := tp.size; openArray := TRUE;
		IF cls = Base.cVar THEN readOnly := TRUE; cls := Base.cRef END
	ELSIF cls = Base.cVar THEN readOnly := TRUE;
		IF ~ Generator.FitInReg (tp.size) THEN cls := Base.cRef END
	ELSIF (cls = Base.cRef) & (tp.form = Base.tRecord) THEN
		parSize := Base.WordSize * 2
	END;
		
	WHILE first # Base.guard DO first.val := parblksize;
		Generator.Fix_param_adr (first.val); first.type := tp;
		first.class := cls; first.param := TRUE; first.readonly := readOnly;
		IF openArray THEN first.val2 := first.val + Base.WordSize END;
		INC (parblksize, parSize); INC (nofpar); first := first.next
	END
END FPSection;

PROCEDURE FormalParameters (VAR tp: Base.Type; VAR parblksize: INTEGER);
	VAR obj: Base.Object;
BEGIN Base.NewType (tp, Base.tProcedure); tp.size := Base.WordSize;
	tp.alignment := Base.WordSize; tp.len := 0; parblksize := 0;
	IF sym = Scanner.lparen THEN Scanner.Get (sym);
		IF sym = Scanner.rparen THEN Scanner.Get (sym)
		ELSE FPSection (parblksize, tp.len);
			WHILE sym = Scanner.semicolon DO Scanner.Get (sym);
				IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN
					FPSection (parblksize, tp.len)
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
					tp.base := obj.type; CheckExport2 (obj.export)
				ELSE Scanner.Mark ('Result type must be scalar')
				END
			ELSE Scanner.Mark (notTypeError)
			END
		END
	END;
	tp.fields := SymTable.topScope.next
END FormalParameters;

PROCEDURE _type (VAR tp: Base.Type);
	VAR obj, bfield: Base.Object; x: Base.Item; size: INTEGER;
BEGIN tp := Base.intType;
	IF (sym # Scanner.ident) & (sym < Scanner.array) THEN
		Scanner.Mark (notTypeError);
		REPEAT Scanner.Get (sym)
		UNTIL (sym = Scanner.ident) OR (sym >= Scanner.array)
	END;
	IF sym = Scanner.ident THEN qualident (obj);
		IF obj.class = Base.cType THEN tp := obj.type; CheckExport2 (obj.export)
		ELSE Scanner.Mark (notTypeError)
		END
	ELSIF sym = Scanner.array THEN
		Base.NewType (tp, Base.tArray); identExport := FALSE;
		Scanner.Get (sym); expression (x); CheckInt (x);
		IF x.mode = Base.cConst THEN
			IF x.a > 0 THEN tp.len := x.a
			ELSE Scanner.Mark ('Array length must be positive'); tp.len := 1
			END
		ELSE Scanner.Mark (notConstError); MakeIntConst (x); tp.len := 1
		END;
		Check (Scanner.of, noOfError); type (tp.base);
		tp.alignment := tp.base.alignment; size := tp.base.size;
		Generator.Align (size, tp.alignment); tp.size := tp.len * size;
		tp.numPtr := tp.base.numPtr * tp.len
	ELSIF sym = Scanner.record THEN
		Base.NewType (tp, Base.tRecord); identExport := FALSE;
		tp.len := 0; tp.size := 0; tp.alignment := 0; Scanner.Get (sym);
		IF sym = Scanner.lparen THEN
			IF SymTable.curLev # 0 THEN
				Scanner.Mark ('Extension of local types not implemented')
			END;
			IF sym = Scanner.ident THEN qualident (obj)
			ELSE obj := Base.guard
			END;
			IF obj.class = Base.cType THEN
				IF obj.type.form = Base.tRecord THEN tp.base := obj.type
				ELSIF obj.type.form = Base.tPointer THEN
					IF obj.type.base # Base.intType THEN
						tp.base := obj.type.base
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
	ELSIF sym = Scanner.pointer THEN
		Base.NewType (tp, Base.tPointer); identExport := FALSE;
		tp.size := Base.WordSize; tp.alignment := Base.WordSize;
		tp.numPtr := 1; tp.base := Base.intType; Check (Scanner.to, noToError);
		IF sym = Scanner.ident THEN SymTable.Find (obj, Scanner.id);
			IF obj # Base.guard THEN
				IF (obj.class = Base.cType) & (obj.type.form = Base.tRecord)
				THEN CheckRecLevel (obj.lev); tp.base := obj.type
				ELSE Scanner.Mark ('Invalid base type')
				END
			ELSE CheckRecLevel (SymTable.curLev);
				SymTable.RegisterUndefType (tp, obj.name, FALSE)
			END
		ELSIF sym = Scanner.record THEN
			CheckRecLevel (SymTable.curLev); type (tp.base)
		ELSE Scanner.Mark (notRecordTypeError)
		END
	ELSIF sym = Scanner.procedure THEN SymTable.OpenScope ('');
		Scanner.Get (sym); FormalParameters (tp, size); SymTable.CloseScope
	ELSE Scanner.Mark (notTypeError)
	END
END _type;

PROCEDURE DeclarationSequence (VAR varsize: INTEGER);
	VAR id: Base.IdentStr; obj, first, field: Base.Object;
		x: Base.Item; tp: Base.Type;
		exported: BOOLEAN; parblksize, locblksize: INTEGER;
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
		WHILE sym = Scanner.ident DO
			identExport := FALSE; Base.StrCopy (Scanner.id, id);
			Scanner.Get (sym); CheckExport (exported);
			Check (Scanner.eql, noEqualError); type (tp);
			SymTable.New (obj, id, Base.cType); obj.export := exported;
			obj.type := tp; obj.lev := SymTable.curLev;
			IF tp.form = Base.tRecord THEN SymTable.CheckUndefList (obj) END;
			Check (Scanner.semicolon, noSemicolonError)
		END;
		SymTable.CleanupUndefList
	END;
	IF sym = Scanner.var THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO IdentList (Base.cVar, first);
			Check (Scanner.colon, noColonError); type (tp); obj := first;
			WHILE obj # Base.guard DO
				obj.lev := SymTable.curLev; DEC (varsize, tp.size);
				Generator.Align (varsize, tp.alignment); obj.val := varsize;
				obj.type := tp; obj := obj.next
			END;
			Check (Scanner.semicolon, noSemicolonError)
		END
	END;
	WHILE sym = Scanner.procedure DO identExport := FALSE; Scanner.Get (sym);
		IF sym = Scanner.ident THEN SymTable.New (obj, Scanner.id, Base.cProc);
			Scanner.Get (sym); CheckExport (obj.export)
		ELSE Scanner.Mark (noIdentError); obj := Base.guard
		END;
		SymTable.OpenScope ('');
		FormalParameters (tp, parblksize); SymTable.CloseScope;
		Check (Scanner.semicolon, noSemicolonError);
		IF obj # Base.guard THEN obj.type := tp; SymTable.OpenScope (Scanner.id)
		ELSE SymTable.OpenScope ('')
		END;
		SymTable.IncLevel (1); field := tp.fields;
		WHILE field # Base.guard DO
			SymTable.New (first, field.name, field.class);
			first^ := field^; first.lev := SymTable.curLev; field := field.next
		END;
		locblksize := 0; DeclarationSequence (locblksize);
		Generator.Enter (obj, locblksize);
		IF sym = Scanner.begin THEN Scanner.Get (sym); StatementSequence END;
		IF sym = Scanner.return THEN Scanner.Get (sym); expression (x);
			IF tp.base # NIL THEN CheckValue (x);
				CheckScalarAssignment (tp.base, x); Generator.load (x)
			ELSE Scanner.Mark ('Proper procedure cannot have RETURN')
			END
		END;
		Generator.Return; Check (Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF obj # Base.guard THEN
				IF obj.name # Scanner.id THEN
					Scanner.Mark ('Wrong procedure name')
				END
			END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No procedure name after END')
		END;
		Check (Scanner.semicolon, noSemicolonError);
		IF obj # Base.guard THEN obj.dsc := SymTable.topScope.next END;
		SymTable.IncLevel (-1); SymTable.CloseScope
	END;
END DeclarationSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ImportList;
	VAR obj: Base.Object;
BEGIN Scanner.Get (sym);
	IF sym = Scanner.ident THEN
		SymTable.New (obj, Scanner.id, Base.cModule); Scanner.Get (sym)
	END;
	WHILE sym = Scanner.comma DO Scanner.Get (sym);
		IF sym # Scanner.ident THEN Scanner.Mark (superflousCommaError)
		ELSE SymTable.New (obj, Scanner.id, Base.cModule); Scanner.Get (sym)
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