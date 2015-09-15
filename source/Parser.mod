MODULE Parser;

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
	AnonRecordType = POINTER TO RECORD
		tp: Base.Type; next: AnonRecordType
	END;
	
	UndefPtrList = POINTER TO RECORD
		typ: Base.Type; basename: Base.IdentStr; next: UndefPtrList
	END;
	
VAR
	sym*: INTEGER;
	identExport, isLibrary: BOOLEAN;
	defobj: Base.Object; defobjId: Base.IdentStr;
	expression: PROCEDURE (VAR x: Base.Item);
	type: PROCEDURE (VAR tp: Base.Type);
	Union: PROCEDURE (tp: Base.Type; VAR first: Base.Object);
	
	anonTypes: AnonRecordType;
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

PROCEDURE MakeVar (VAR x: Base.Item; tp: Base.Type);
BEGIN
	Generator.Clean_item (x); x.mode := Base.cVar; x.lev := 0; x.a := 0;
	x.type := tp
END MakeVar;

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

PROCEDURE CompAddress (tpX, tpY: Base.Type) : BOOLEAN;
	RETURN (tpX.base = tpY.base)
	OR ({tpX.base.form, tpY.base.form} = {Base.tAddress})
		& CompAddress (tpX.base, tpY.base)
END CompAddress;

PROCEDURE CheckScalarAssignment (xtype: Base.Type; VAR y: Base.Item);
BEGIN
	IF xtype.form = Base.tInteger THEN CheckInt (y)
	ELSIF xtype.form = Base.tReal THEN CheckReal (y)
	ELSIF xtype = Base.setType THEN CheckSet (y)
	ELSIF xtype.form = Base.tChar THEN CheckChar (y)
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
	ELSIF xtype.form = Base.tAddress THEN CheckValue (y);
		IF y.type.form = Base.tAddress THEN
			IF ~CompAddress (xtype, y.type) THEN
				Scanner.Mark (notCompAddrError)
			END
		ELSIF y.type # Base.nilType THEN
			Scanner.Mark (notAddrError); y.type := xtype
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

PROCEDURE Parameter (VAR c: Generator.ProcCall);
	VAR x: Base.Item; ftype: Base.Type; fpar: Base.Object;
BEGIN expression (x); INC (c.nofact);
	IF c.nofact <= c.nofpar THEN
		fpar := c.fpar; ftype := fpar.type;
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
			Generator.Value_param (x, c)
		ELSIF fpar.nilable & (x.type = Base.nilType) THEN
			Generator.Value_param (x, c)
		ELSE CheckVar (x, fpar.readonly);
			IF (ftype.form = Base.tRecord) & (x.type.form = Base.tRecord) THEN
				IF IsExt(x.type, ftype) THEN
					IF fpar.tagged THEN Generator.Record_param (x, c)
					ELSE Generator.Ref_param (x, c)
					END
				ELSE Scanner.Mark (notExtError); MakeIntConst (x)
				END
			ELSIF (ftype.form = Base.tArray) & (ftype.len = 0)
				& ((x.type.form = Base.tArray) & CompArray (ftype, x.type)
				OR (x.type = Base.stringType) & (ftype.base = Base.charType)
				OR (x.type = Base.string8Type) & (ftype.base = Base.char8Type))
				THEN Generator.Array_param (x, c)
			ELSIF (x.type = ftype)
			OR (ftype.form = Base.tArray)
				& ((ftype.base = Base.charType) & (x.type = Base.stringType)
				OR (ftype.base = Base.char8Type) & (x.type = Base.string8Type))
			OR (x.type.form = Base.tAddress) & (ftype.form = Base.tAddress)
				& CompAddress(x.type, ftype)
			OR (ftype.form = Base.tArray) & (ftype.len = -1)
				& ((x.type.form = Base.tArray) & (ftype.base = x.type.base)
				OR (x.type = Base.stringType) & (ftype.base = Base.charType)
				OR (x.type = Base.string8Type) & (ftype.base = Base.char8Type)
				OR (ftype.base = Base.sysByteType))
				THEN Generator.Ref_param (x, c)
			ELSIF (ftype.form = Base.tArray) & (ftype.len = 0)
				& (ftype.base = Base.sysByteType)
				THEN Generator.ByteArray_param (x, c)			
			ELSE Scanner.Mark (notCompTypeError); MakeIntConst (x)
			END
		END;
		c.fpar := c.fpar.next
	ELSE MakeIntConst (x);
		IF c.nofact = c.nofpar + 1 THEN Scanner.Mark (tooMuchParamError) END
	END
END Parameter;

PROCEDURE ActualParameters (VAR c: Generator.ProcCall);
BEGIN Scanner.Get (sym);
	IF sym # Scanner.rparen THEN Parameter (c);
		WHILE sym = Scanner.comma DO Scanner.Get (sym);
			IF sym # Scanner.rparen THEN Parameter (c)
			ELSE Scanner.Mark (superflousCommaError)
			END
		END;
		Check (Scanner.rparen, noRParenError)
	ELSE Scanner.Get (sym)
	END;
	IF c.nofact < c.nofpar THEN Scanner.Mark (tooLittleParamError) END
END ActualParameters;

PROCEDURE ProcedureCall (VAR x: Base.Item; proper: BOOLEAN);
	VAR c: Generator.ProcCall;
BEGIN
	IF proper & (x.type.base # NIL) THEN
		Scanner.Mark ('Need proper proc'); c.rtype := NIL
	ELSIF ~proper & (x.type.base = NIL) THEN
		Scanner.Mark ('Need function'); c.rtype := Base.intType
	ELSE c.rtype := x.type.base
	END;
	c.nofact := 0; c.nofpar := x.type.len; c.fpar := x.type.fields;
	Generator.Prepare_to_call (x, c);
	IF sym = Scanner.lparen THEN ActualParameters (c)
	ELSIF x.type.len > 0 THEN Scanner.Mark ('Need params')
	END;
	Generator.Call (c);
	IF ~proper THEN Generator.Return_value (x, c.rtype) END
END ProcedureCall;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE qualident (VAR obj: Base.Object);
BEGIN
	SymTable.Find (obj, Scanner.id);
	IF obj # Base.guard THEN
		IF obj.class = Base.cModule THEN
			Scanner.Get (sym); Check (Scanner.period, noPeriodError);
			IF sym = Scanner.ident THEN
				Base.StrCopy (Scanner.id, Base.guard.name); obj := obj.dsc;
				WHILE obj.name # Scanner.id DO obj := obj.next END;
				IF obj = Base.guard THEN Scanner.Mark (undefinedError) END
			ELSE Scanner.Mark (noIdentError); obj := Base.guard 
			END
		END
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
		IF (y.mode = Base.cType) & (y.type.form = x.type.form) THEN
			IF ~IsExt(x.type, y.type) & ~IsExt(y.type, x.type) THEN
				Scanner.Mark (notExtError); y.type := x.type
			END;
			IF y.type.form = Base.tPointer THEN
				Generator.Make_item (y, y.type.base.obj)
			END;
			Generator.Type_test (x, y, guard)
		ELSE
			IF y.mode = Base.cType THEN Scanner.Mark (notCompTypeError)
			ELSE Scanner.Mark (notTypeError)
			END;
			MakeIntConst (y); MakeConst (x, Base.boolType)
		END
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
			ELSE Scanner.Mark (notPointerOrRecordError); Scanner.Get (sym)
			END
		ELSIF sym = Scanner.lbrak THEN
			IF (x.mode IN Base.clsValue) & (x.type.form = Base.tAddress) THEN
				Generator.Deref (x)
			END;
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
			ELSE Scanner.Mark ('Not an array'); Scanner.Get (sym)
			END
		ELSIF sym = Scanner.arrow THEN
			IF (x.mode IN Base.clsValue)
				& (x.type.form IN {Base.tPointer, Base.tAddress})
			THEN Generator.Deref (x)
			ELSE Scanner.Mark (notPointerError)
			END;
			Scanner.Get (sym)
		ELSIF (sym = Scanner.lparen) & (x.mode IN Base.clsValue) &
			((x.type.form = Base.tPointer)
			OR (x.type.form = Base.tRecord) & x.tagged)
		THEN TypeTest (x, TRUE); Check (Scanner.rparen, noRParenError)
		ELSE exit := TRUE
		END
	END
END selector;

PROCEDURE designator (VAR x: Base.Item);
	VAR obj: Base.Object;
BEGIN qualident (obj);
	IF obj # Base.guard THEN Generator.Make_item (x, obj)
	ELSE MakeIntConst (x)
	END;
	selector (x)
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
		
	PROCEDURE SFunc_LEN (VAR x: Base.Item);
	BEGIN expression (x); CheckVar (x, TRUE);
		IF (x.type.form = Base.tString) OR (x.type.form = Base.tArray) THEN
			Generator.SFunc_LEN (x)
		ELSE Scanner.Mark ('Not an array'); MakeIntConst (x); x.a := 1
		END
	END SFunc_LEN;
	
	PROCEDURE SFunc_SHIFT (shf: INTEGER; VAR x: Base.Item);
		VAR y: Base.Item;
	BEGIN expression (x); CheckInt (x);	LoadVolatile (x);
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
		ELSIF (x.type.form = Base.tString) & (x.b <= 2) THEN
			Generator.Str_to_char (x)
		ELSE Scanner.Mark ('Expect CHAR, SET, or BOOLEAN'); MakeIntConst (x)
		END;
		IF ~(x.mode IN {Base.cConst, Base.mReg}) THEN Generator.load (x) END
	END SFunc_ORD;
		
	PROCEDURE SFunc_CHR (VAR x: Base.Item);
	BEGIN expression (x); CheckInt (x); Generator.SFunc_CHR (x)
	END SFunc_CHR;
		
	PROCEDURE SFunc_ADR (VAR x: Base.Item);
	BEGIN expression (x); CheckVar (x, TRUE); Generator.SFunc_ADR (x)
	END SFunc_ADR;
		
	PROCEDURE SFunc_SIZE (VAR x: Base.Item);
		VAR size: INTEGER;
	BEGIN expression (x);
		IF x.mode = Base.cType THEN size := x.type.size
		ELSE Scanner.Mark (notTypeError); size := Base.WordSize
		END;
		Generator.Align (size, x.type.alignment); MakeIntConst (x); x.a := size
	END SFunc_SIZE;
	
	PROCEDURE SFunc_BIT (VAR x: Base.Item);
		VAR y: Base.Item;
	BEGIN expression (x); CheckInt (x); LoadVolatile (x);
		Check (Scanner.comma, tooLittleParamError);
		expression (y); Generator.SFunc_BIT (x, y)
	END SFunc_BIT;
		
	PROCEDURE SFunc_VAL (VAR x: Base.Item);
		VAR castType: Base.Type;
	BEGIN expression (x);
		IF (x.mode # Base.cType) OR ~(x.type.form IN Base.typeScalar) THEN
			Scanner.Mark ('Not a scalar type'); MakeIntConst (x)
		END;
		castType := x.type; Check (Scanner.comma, tooLittleParamError);
		expression (x); CheckValue (x); Generator.SFunc_VAL (x, castType);
	END SFunc_VAL;
	
	PROCEDURE SFunc_ADR2 (VAR x: Base.Item);
	BEGIN
		expression (x); CheckVar (x, TRUE); Generator.SFunc_ADR (x);
		IF x.type = Base.stringType THEN x.type := Base.charType END;
		IF x.type.adrType = NIL THEN Base.NewAddressType (x.type) END;
		x.type := x.type.adrType
	END SFunc_ADR2;
	
	PROCEDURE SFunc_STRADR (VAR x: Base.Item);
	BEGIN
		expression (x); CheckVar (x, TRUE);
		IF (x.type # Base.stringType)
			& ((x.type.form # Base.tArray) OR (x.type.base # Base.charType))
		THEN Scanner.Mark (notStrError)
		END;
		Generator.SFunc_ADR (x); x.type := Base.charType;
		IF x.type.adrType = NIL THEN Base.NewAddressType (x.type) END;
		x.type := x.type.adrType
	END SFunc_STRADR;
		
BEGIN (* StandFunc *)
	Check (Scanner.lparen, noLParenError); funcno := x.a; rtype := x.type;
	IF funcno =	200 THEN SFunc_ABS (x)
	ELSIF funcno = 201 THEN SFunc_ODD (x)
	ELSIF funcno = 202 THEN SFunc_LEN (x)
	ELSIF (funcno >= 203) & (funcno <= 205) THEN SFunc_SHIFT (funcno - 203, x)
	ELSIF funcno = 206 THEN SFunc_FLOOR (x)
	ELSIF funcno = 207 THEN SFunc_FLT (x)
	ELSIF funcno = 208 THEN SFunc_ORD (x)
	ELSIF funcno = 209 THEN SFunc_CHR (x)
	ELSIF funcno = 300 THEN SFunc_ADR (x)
	ELSIF funcno = 301 THEN SFunc_SIZE (x)
	ELSIF funcno = 302 THEN SFunc_BIT (x)
	ELSIF funcno = 303 THEN SFunc_VAL (x)
	ELSIF funcno = 310 THEN SFunc_ADR2 (x)
	ELSIF funcno = 311 THEN SFunc_STRADR (x)
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
		LoadVolatile (y); Scanner.Get (sym);
		expression (z); CheckInt (z); Generator.Set3 (x, y, z)
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
		IF x.mode = Base.cSFunc THEN StandFunc (x)
		ELSIF sym = Scanner.lparen THEN
			IF (x.mode IN Base.clsValue) & (x.type.form = Base.tProcedure) THEN
				ProcedureCall (x, FALSE)
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
		IF ~Scanner.ansiStr THEN
			Generator.Make_string (x, Scanner.str, Scanner.slen)
		ELSE Generator.Make_string2 (x, Scanner.str, Scanner.slen)
		END;
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
			IF x.type # Base.boolType THEN LoadVolatile (x)
			ELSE Generator.And1 (x)
			END
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
			IF x.type # Base.boolType THEN LoadVolatile (x)
			ELSE Generator.Or1 (x)
			END
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
			ELSIF x.type.form = Base.tChar THEN CheckChar (y)
			ELSIF x.type = Base.setType THEN CheckSet (y);
				IF rel > Scanner.neq THEN relType := setRel END
			ELSIF x.type = Base.boolType THEN CheckBool (y)
			ELSIF (x.type.form = Base.tArray) & (x.type.base = Base.charType)
			OR (x.type = Base.stringType) THEN CheckValue (y);
				IF (y.type = Base.stringType)
				OR (y.type.form = Base.tArray)
					& (y.type.base = Base.charType)
				THEN relType := strRel
				ELSIF (y.type.form = Base.tChar)
					& (x.type = Base.stringType) & (x.b <= 2)
				THEN Generator.Str_to_char (x)
				ELSE Scanner.Mark (notStrError); relType := noRel
				END
			ELSIF (x.type.form = Base.tArray) & (x.type.base = Base.char8Type)
			OR (x.type = Base.string8Type) THEN CheckValue (y);
				IF (y.type = Base.string8Type)
				OR (y.type.form = Base.tArray)
					& (y.type.base = Base.char8Type)
				THEN relType := strRel
				ELSIF (y.type.form = Base.tChar)
					& (x.type = Base.string8Type) & (x.b <= 2)
				THEN Generator.Str_to_char (x)
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
			ELSIF x.type.form = Base.tAddress THEN CheckValue (y);
				IF y.type.form = Base.tAddress THEN
					IF ~CompAddress (x.type, y.type) THEN
						Scanner.Mark (notCompAddrError)
					END
				ELSIF y.type # Base.nilType THEN
					Scanner.Mark (notAddrError); relType := noRel
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
		CheckInt (x); LoadVolatile (x); Scanner.Get (sym);
		SimpleExpression (y); CheckSet (y); Generator.Member_test (x, y)
	ELSIF sym = Scanner.is THEN CheckValue (x);
		IF (x.type.form # Base.tPointer) &
			((x.type.form # Base.tRecord) OR ~x.tagged)
		THEN
			Scanner.Mark ('Type test not applicable');
			MakeConst (x, Base.boolType)
		END;
		TypeTest (x, FALSE)
	END
END expression0;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Statements *)

PROCEDURE StandProc (VAR x: Base.Item);
	VAR y: Base.Item; procno: INTEGER;
	
	PROCEDURE SProc_INC (op: INTEGER);
		VAR x, y, z: Base.Item;
	BEGIN
		expression (x); CheckVar (x, FALSE); CheckInt (x);
		IF sym # Scanner.comma THEN
			IF op = Scanner.plus THEN Generator.SProc_INC (x, 1)
			ELSE Generator.SProc_INC (x, -1)
			END
		ELSE y := x; Generator.Load_to_new_reg (y);
			Scanner.Get (sym); expression (z); CheckInt (z);
			Generator.Add (op, y, z); Generator.Store (x, y)
		END
	END SProc_INC;
	
	PROCEDURE SProc_INCL (op: INTEGER);
		VAR x, y, z: Base.Item;
	BEGIN
		expression (x); CheckVar (x, FALSE); CheckSet (x);
		y := x; Generator.Load_to_new_reg (y);
		Check (Scanner.comma, tooLittleParamError);
		expression (z); CheckInt (z); Generator.SProc_INCL (op, y, z);
		Generator.Store (x, y)
	END SProc_INCL;
	
	PROCEDURE SProc_NEW;
		VAR x: Base.Item;
	BEGIN expression (x); CheckVar (x, FALSE);
		IF x.type.form = Base.tPointer THEN
			IF (x.type.base # Base.intType) & (x.type.base.obj # NIL) THEN
				Generator.SProc_NEW (x)
			ELSE MakeIntConst (x)
			END
		ELSE Scanner.Mark (notPointerError); MakeIntConst (x)
		END
	END SProc_NEW;
	
	PROCEDURE SProc_ASSERT;
		VAR x: Base.Item;
	BEGIN expression (x); CheckBool (x); Generator.Assert (x)
	END SProc_ASSERT;
	
	PROCEDURE SProc_DISPOSE;
		VAR x: Base.Item; c: Generator.ProcCall;
	BEGIN Generator.SProc_DISPOSE (c); expression (x); CheckValue (x);
		IF x.type.form # Base.tPointer THEN Scanner.Mark (notPointerError) END;
		Generator.SProc_DISPOSE2 (c, x)
	END SProc_DISPOSE;
		
	PROCEDURE SProc_GET;
		VAR x, y: Base.Item;
	BEGIN expression (x); CheckInt (x); LoadVolatile (x);
		Check (Scanner.comma, tooLittleParamError);
		expression (y); CheckVar (y, FALSE);
		IF ~(y.type.form IN Base.typeScalar) THEN
			Scanner.Mark (notScalarError); y.type := Base.intType
		END;
		Generator.SProc_GET (x, y)
	END SProc_GET;
	
	PROCEDURE SProc_PUT;
		VAR x, y: Base.Item;
	BEGIN expression (x); CheckInt (x); LoadVolatile (x);
		Check (Scanner.comma, tooLittleParamError);
		expression (y); CheckValue (y);
		IF ~(y.type.form IN Base.typeScalar) THEN
			Scanner.Mark (notScalarError); MakeIntConst (y)
		END;
		Generator.SProc_PUT (x, y)
	END SProc_PUT;
	
	PROCEDURE SProc_COPY;
		VAR x, y, z : Base.Item;
	BEGIN
		expression (x); CheckInt (x); LoadVolatile (x);
		Check (Scanner.comma, tooLittleParamError);
		expression (y); CheckInt (y); LoadVolatile (y);
		Check (Scanner.comma, tooLittleParamError);
		expression (z); CheckInt (z); LoadVolatile (z);
		Generator.SProc_COPY (x, y, z)
	END SProc_COPY;
	
	PROCEDURE SProc_PACK;
		VAR x, y, z: Base.Item;
	BEGIN
		expression (x); CheckVar (x, FALSE); CheckReal (x);
		x.type := Base.card32Type; y := x; Generator.Load_to_new_reg (y);
		Check (Scanner.comma, tooLittleParamError);
		expression (z); CheckInt (z); Generator.SProc_PACK (x, y, z)
	END SProc_PACK;
	
	PROCEDURE SProc_UNPK;
		VAR x, y, z: Base.Item;
	BEGIN
		expression (x); CheckVar (x, FALSE); CheckReal (x);
		x.type := Base.card32Type; y := x; Generator.Load_to_new_reg (y);
		Check (Scanner.comma, tooLittleParamError);
		expression (z); CheckVar (z, FALSE); CheckInt (z);
		Generator.SProc_UNPK (x, y, z)
	END SProc_UNPK;
		
	PROCEDURE SProc_LoadLibrary;
		VAR x, y, proc: Base.Item; c: Generator.ProcCall; tp: Base.Type;
	BEGIN expression (x); CheckVar (x, FALSE);
		IF x.type # Base.intType THEN
			Scanner.Mark ('Not an INTEGER variable'); x.type := Base.intType
		END;

		proc.mode := Base.cProc; proc.type := Base.LoadLibraryFuncType;
		proc.lev := -2; proc.a := Base.LoadLibraryW;
		c.rtype := Base.intType; Generator.Prepare_to_call (proc, c);
		
		Check (Scanner.comma, tooLittleParamError);
		expression (y); CheckVar (y, TRUE); tp := y.type;
		IF (tp # Base.stringType)
			& ((tp.form # Base.tArray) OR (tp.base # Base.charType))
		THEN Scanner.Mark (notStrError)
		END;
		Generator.Ref_param (y, c); Generator.Call (c);
		Generator.Return_value (proc, c.rtype); Generator.Store (x, proc)
	END SProc_LoadLibrary;
		
	PROCEDURE SProc_GetProcAddress;
		VAR x, y, z, proc: Base.Item; c: Generator.ProcCall; tp: Base.Type;
	BEGIN expression (x); CheckVar (x, FALSE);
		IF ~(x.type.form IN {Base.tProcedure, Base.tAddress}) THEN
			Scanner.Mark ('Not a proc or ADDRESS'); x.type := Base.intType
		END;
		
		proc.mode := Base.cProc; proc.type := Base.GetProcAddressFuncType;
		proc.lev := -2; proc.a := Base.GetProcAddress;
		c.rtype := Base.intType; Generator.Prepare_to_call (proc, c);
		
		Check (Scanner.comma, tooLittleParamError);
		expression (y); CheckInt (y); Generator.Value_param (y, c);
		
		Check (Scanner.comma, tooLittleParamError);
		expression (z); CheckValue (z); tp := z.type;
		IF tp = Base.string8Type THEN Generator.Ref_param (z, c)
		ELSE CheckInt (z); Generator.Value_param (z, c)
		END;
		Generator.Call (c); Generator.Return_value (proc, c.rtype);
		Generator.Store (x, proc)
	END SProc_GetProcAddress;

BEGIN (* StandProc *)
	Check (Scanner.lparen, noLParenError); procno := x.a;
	IF procno = 0 THEN SProc_INC (Scanner.plus)
	ELSIF procno = 1 THEN SProc_INC (Scanner.minus)
	ELSIF procno = 2 THEN SProc_INCL (Scanner.plus)
	ELSIF procno = 3 THEN SProc_INCL (Scanner.minus)
	ELSIF procno = 4 THEN SProc_NEW
	ELSIF procno = 5 THEN SProc_ASSERT
	ELSIF procno = 6 THEN SProc_PACK
	ELSIF procno = 7 THEN SProc_UNPK
	ELSIF procno = 8 THEN SProc_DISPOSE
	ELSIF procno = 100 THEN SProc_GET
	ELSIF procno = 101 THEN SProc_PUT
	ELSIF procno = 102 THEN SProc_COPY
	ELSIF procno = 103 THEN SProc_LoadLibrary
	ELSIF procno = 104 THEN SProc_GetProcAddress
	END;
	procno := 0;
	WHILE sym = Scanner.comma DO Scanner.Get (sym); expression (y);
		IF procno = 0 THEN Scanner.Mark (tooMuchParamError); procno := 1 END
	END;
	Check (Scanner.rparen, noRParenError)
END StandProc;

PROCEDURE StatementSequence;
	VAR x, y, z, t: Base.Item; obj: Base.Object; orgtype: Base.Type;
		L, L2: INTEGER;
		
	PROCEDURE TypeCase (VAR x: Base.Item; VAR obj: Base.Object);
		VAR y: Base.Item; tp: Base.Type;
	BEGIN
		IF sym = Scanner.ident THEN
			Generator.Make_item (x, obj); expression (y); tp := y.type;
			IF y.mode # Base.cType THEN
				MakeIntConst (y); Scanner.Mark (notTypeError);
				y.type := x.type
			END;
			IF y.type.form # x.type.form THEN
				Scanner.Mark (notCompTypeError); y.type := x.type
			END;
			IF ~IsExt(x.type, y.type) & ~IsExt(y.type, x.type) THEN
				Scanner.Mark (notExtError); y.type := x.type
			END;
			tp := y.type; obj.type := tp;
			IF tp.form = Base.tPointer THEN
				Generator.Make_item (y, tp.base.obj)
			END;
			Generator.Type_test (x, y, FALSE); Generator.CFJump (x);
			Check (Scanner.colon, noColonError); StatementSequence
		ELSE Generator.Make_const (x, Base.boolType, 1); Generator.CFJump (x)
		END
	END TypeCase;
		
BEGIN (* StatementSequence *)
	REPEAT
		IF (sym # Scanner.ident) & ((sym < Scanner.if) OR (sym > Scanner.for))
			& (sym < Scanner.semicolon)
		THEN Scanner.Mark ('Statement expected');
			REPEAT Scanner.Get (sym)
			UNTIL (sym = Scanner.ident) OR (sym >= Scanner.if)
		END;
		IF sym = Scanner.ident THEN designator (x);
			IF sym = Scanner.becomes THEN CheckVar (x, FALSE);
				IF (x.type.form = Base.tArray) & (x.type.len = 0) THEN
					Scanner.Mark ('Open array assignment not supported')
				END;
				Scanner.Get (sym); expression (y);
				IF x.type.form IN Base.typeScalar THEN
					CheckScalarAssignment (x.type, y); Generator.Store (x, y)
				ELSIF (y.type = Base.stringType) & (x.type.form = Base.tArray)
					& (x.type.base = Base.charType)
				OR (y.type = Base.string8Type) & (x.type.form = Base.tArray)
					& (x.type.base = Base.char8Type)
				THEN
					IF y.b > x.type.len + 1 THEN
						Scanner.Mark (stringTooLongError); y.b := x.type.len
					END;
					Generator.Store_string (x, y)
				ELSE CheckValue (y);
					IF y.type # x.type THEN
						Scanner.Mark (notCompTypeError); MakeVar (y, x.type)
					END;
					Generator.Store_struct (x, y)
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
				Scanner.Get (sym)
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
			ELSE MakeIntConst (t); t.a := 1
			END;
			L := Generator.pc; Generator.For2 (x, z, t.a, L2);
			Check (Scanner.do, noDoError); StatementSequence;
			Check (Scanner.end, noEndError); Generator.For3 (x, t.a, L, L2)
		ELSIF sym = Scanner.case THEN Scanner.Get (sym);
			IF sym = Scanner.ident THEN qualident (obj);
				IF (obj.class IN {Base.cVar, Base.cRef})
					& ((obj.type.form = Base.tPointer)
					OR (obj.type.form = Base.tRecord) & obj.tagged)
				THEN Check (Scanner.of, noOfError);
					orgtype := obj.type; TypeCase (x, obj); L := 0;
					WHILE sym = Scanner.bar DO Scanner.Get (sym);
						Generator.FJump (L); Generator.Fixup (x);
						obj.type := orgtype; TypeCase (x, obj)
					END;
					Generator.Fixup (x); Generator.Fix_link (L)
				ELSE Scanner.Mark ('Need pointer or record var param')
				END;
				Check (Scanner.end, noEndError)
			ELSE Scanner.Mark (noIdentError)
			END
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
		Base.NewType (tp, Base.tArray); tp.len := 0; FormalType (tp.base);
		IF (tp.base.form # Base.tArray) OR (tp.base.len > 0) THEN
			tp.size := Base.WordSize * 2
		ELSE tp.size := tp.base.size + Base.WordSize
		END		
	ELSE Scanner.Mark (notTypeError)
	END
END FormalType;

PROCEDURE FPSection (VAR parblksize, nofpar: INTEGER);
	VAR cls, parSize: INTEGER; first, obj: Base.Object;
		readOnly, openArray, taggedRecord: BOOLEAN;
		tp: Base.Type;
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
	parSize := Base.WordSize; readOnly := FALSE;
	openArray := FALSE; taggedRecord := FALSE;
	IF tp.form IN Base.typeScalar THEN (* no change *)
	ELSIF (tp.form = Base.tArray) & (tp.len = 0) THEN
		parSize := tp.size; openArray := TRUE;
		IF cls = Base.cVar THEN readOnly := TRUE; cls := Base.cRef END
	ELSIF cls = Base.cVar THEN readOnly := TRUE;
		IF ~Generator.FitInReg(tp.size) THEN cls := Base.cRef END
	ELSIF cls = Base.cRef THEN
		IF (tp.form = Base.tRecord) & tp.extensible THEN
			parSize := Base.WordSize * 2; taggedRecord := TRUE
		END
	END;
		
	WHILE first # Base.guard DO first.val := parblksize;
		Generator.Fix_param_adr (first.val); first.type := tp;
		first.class := cls; first.param := TRUE; first.nilable := FALSE;
		first.readonly := readOnly; first.tagged := taggedRecord;
		IF openArray THEN first.val2 := first.val + Base.WordSize END;
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

PROCEDURE RecordBaseType (VAR tp: Base.Type);
	VAR obj: Base.Object;
BEGIN
	IF SymTable.curLev # 0 THEN
		Scanner.Mark ('Extension of non-global types not implemented')
	END;
	IF sym = Scanner.ident THEN qualident (obj) ELSE obj := Base.guard END;
	IF obj.class = Base.cType THEN
		IF obj.type.form = Base.tRecord THEN tp.base := obj.type
		ELSIF obj.type.form = Base.tPointer THEN
			IF obj.type.base # Base.intType THEN tp.base := obj.type.base
			ELSE Scanner.Mark ('Undefined record type')
			END
		ELSE Scanner.Mark (invalidExtError)
		END;
		IF tp.base # NIL THEN
			tp.len := tp.base.len + 1; tp.nptr := tp.base.nptr;
			tp.size := tp.base.size; tp.alignment := tp.base.alignment;
			IF tp.len >= Base.MaxExtension THEN
				Scanner.Mark ('Extension level too deep')
			END;
			IF ~tp.base.extensible THEN
				Scanner.Mark ('Not extensible record')
			END
		END
	ELSE Scanner.Mark (notTypeError)
	END
END RecordBaseType;

PROCEDURE type0 (VAR tp: Base.Type);
	VAR obj, bfield: Base.Object; x: Base.Item; size, lev: INTEGER;
		id, str: Base.IdentStr; anonType: AnonRecordType;
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
		tp.nptr := tp.len * tp.base.nptr
	ELSIF (sym = Scanner.record) OR (sym = Scanner.extensible) THEN
		Base.NewType (tp, Base.tRecord); identExport := FALSE;
		tp.len := 0; tp.size := 0; tp.alignment := 0;
		IF sym = Scanner.extensible THEN Scanner.Get (sym);
			IF sym # Scanner.record THEN Scanner.Mark ('RECORD expected') END;
			tp.extensible := TRUE
		END;
		IF (SymTable.curLev = 0) & ((defobj = NIL) OR (defobj.type # tp)) THEN
			NEW (anonType); anonType.tp := tp;
			anonType.next := anonTypes; anonTypes := anonType
		END;
		Scanner.Get (sym);
		IF sym = Scanner.lparen THEN Scanner.Get (sym);
			RecordBaseType (tp); Check (Scanner.rparen, noRParenError)
        END;
		SymTable.OpenScope ('');
		IF tp.base # NIL THEN bfield := tp.base.fields;
			WHILE bfield # Base.guard DO
				SymTable.New (obj, bfield.name, Base.cField); obj^ := bfield^;
				obj.next := Base.guard; obj.lev := SymTable.curLev;
				bfield := bfield.next
			END
		END;
		IF (sym = Scanner.ident) OR (sym = Scanner.union) THEN
			FieldListSequence (tp, obj)
		END;
		tp.fields := SymTable.topScope.next; SymTable.CloseScope;
		Check (Scanner.end, noEndError)
	ELSIF sym = Scanner.pointer THEN
		Base.NewType (tp, Base.tPointer); identExport := FALSE;
		tp.size := Base.WordSize; tp.alignment := Base.WordSize;
		tp.nptr := 1; tp.base := Base.intType; lev := SymTable.curLev;
		IF (defobj # NIL) & (defobj.type = tp) THEN
			Base.StrCopy (defobjId, defobj.name)
		END;
		Scanner.Get (sym); Check (Scanner.to, noToError);
		IF sym = Scanner.ident THEN SymTable.Find (obj, Scanner.id);
			IF obj # Base.guard THEN
				IF (obj.class = Base.cType) & (obj.type.form = Base.tRecord)
				THEN CheckRecLevel (obj.lev); tp.base := obj.type
				ELSE Scanner.Mark ('Invalid base type')
				END
			ELSE CheckRecLevel (lev); RegisterUndefType (tp, obj.name)
			END;
			Scanner.Get (sym)
		ELSIF (sym = Scanner.record) OR (sym = Scanner.extensible) THEN
			CheckRecLevel (lev); type (tp.base)
		ELSE Scanner.Mark (notRecordTypeError)
		END
	ELSIF sym = Scanner.procedure THEN
		Scanner.Get (sym); FormalParameters (tp)
	ELSIF sym = Scanner.address THEN
		Base.NewType (tp, Base.tAddress); tp.size := Base.WordSize;
		tp.alignment := Base.WordSize; Scanner.Get (sym);
		Check (Scanner.of, noOfError); type0 (tp.base)
	ELSE Scanner.Mark (notTypeError)
	END
END type0;

PROCEDURE DeclarationSequence (VAR varsize: INTEGER);
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
			IF (x.mode # Base.cConst) & (x.type.form # Base.tString) THEN
				Scanner.Mark (notConstError); MakeIntConst (x)
			END;
			SymTable.New (obj, id, Base.cConst); obj.export := exported;
			obj.val := x.a; obj.val2 := x.b; obj.type := x.type;
			IF x.type.form = Base.tString THEN
				obj.lev := x.lev; obj.class := x.mode
			ELSE obj.lev := SymTable.curLev
			END;
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
			IF tp.form = Base.tRecord THEN
				CheckUndefList (obj);
				IF obj.lev = 0 THEN Generator.Alloc_typedesc (tp, obj) END
			END;
			Check (Scanner.semicolon, noSemicolonError)
		END;
		CleanupUndefList
	END;
	IF sym = Scanner.var THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO IdentList (Base.cVar, first);
			Check (Scanner.colon, noColonError); type (tp); obj := first;
			WHILE obj # Base.guard DO
				obj.lev := SymTable.curLev; varsize := varsize + tp.size;
				Generator.Align (varsize, tp.alignment); obj.val := -varsize;
				obj.type := tp; obj := obj.next
			END;
			Check (Scanner.semicolon, noSemicolonError)
		END
	END;
	IF SymTable.curLev = 0 THEN
		Generator.Set_varsize (varsize);
		WHILE anonTypes # NIL DO
			SymTable.New (obj, '', Base.cType); obj.lev := 0;
			obj.type := anonTypes.tp; obj.type.obj := obj;
			Generator.Alloc_typedesc (obj.type, obj);
			anonTypes := anonTypes.next
		END
	ELSIF SymTable.curLev = 1 THEN
		IF sym = Scanner.procedure THEN Generator.Placeholder_proc END
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
		
		field := tp.fields;
		WHILE field # Base.guard DO
			SymTable.New (first, field.name, field.class);
			first^ := field^; first.next := Base.guard;
			first.lev := SymTable.curLev; field := field.next
		END;
		
		locblksize := 0; obj.val := Generator.ip;
		DeclarationSequence (locblksize);
		
		Generator.Enter (obj, locblksize);
		IF sym = Scanner.begin THEN
			Scanner.Get (sym); StatementSequence
		END;
		IF sym = Scanner.return THEN Scanner.Get (sym); expression (x);
			IF tp.base # NIL THEN CheckValue (x);
				CheckScalarAssignment (tp.base, x); Generator.load (x)
			ELSE Scanner.Mark ('Proper procedure cannot have RETURN')
			END
		END;
		Generator.Return;
		
		Check (Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF (id[0] # 0X) & (id # Scanner.id) THEN
				Scanner.Mark ('Wrong procedure name')
			END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No procedure name after END')
		END;
		Check (Scanner.semicolon, noSemicolonError);
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
	
PROCEDURE Module*;
	VAR modid: Base.IdentStr; varsize: INTEGER;
BEGIN
	Base.ResetCompilerFlag; Scanner.Get (sym);
	IF sym = Scanner.ident THEN modid := Scanner.id; Scanner.Get (sym)
	ELSE modid := '@'; Scanner.Mark ('No module name')
	END;
	Check (Scanner.semicolon, noSemicolonError);
	IF modid # '@' THEN
		SymTable.Init (modid, FALSE); Generator.Init (modid);
		IF sym = Scanner.import THEN ImportList END;
		IF Scanner.errcnt = 0 THEN
			varsize := 0; DeclarationSequence (varsize);
			
			Generator.Enter (NIL, 0);
			IF sym = Scanner.begin THEN
				Scanner.Get (sym); StatementSequence
			END;
			Generator.Return; Generator.Module_init;
			
			Check (Scanner.end, noEndError);
			IF sym = Scanner.ident THEN
				IF modid # Scanner.id THEN
					Scanner.Mark ('Wrong module name')
				END;
				Scanner.Get (sym)
			ELSE Scanner.Mark ('No module identifier after END')
			END;
			Check (Scanner.period, 'No ending .');
			Generator.Finish (FALSE)
		END
	END
END Module;

BEGIN
	expression := expression0; type := type0; Union := Union0
END Parser.