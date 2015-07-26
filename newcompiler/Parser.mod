MODULE Parser;

IMPORT
	Base, Scanner, SymTable, Generator;
	
CONST
	noSemicolonError = 'No ;';
	noEndError = 'No END';
	notConstError = 'Not a constant value';
	superflousCommaError = 'Superflous ,';
	noColonError = 'No :';
	invalidOperationError = 'Invalid operation';
	notStrError = 'Not a string';
	notPointerError = 'Not a pointer';
	notExtError = 'Not a extension of the other type';
	notPointerOrProcedureError = 'Not a pointer or procedure';
	notPointerOrRecordError = 'Not a pointer or record';
	notTypeError = 'Not a type';
	notPointerOrRecordTypeError = 'Not a pointer or record type';
	notProcError = 'Not a procedure';
	notGlobalProcError = 'Procedure is not global';
	notCompProcError = 'Incompatible procedures';
	
VAR
	sym*: INTEGER;
	expression: PROCEDURE (VAR x: Base.Item);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Check (expected: INTEGER; err: ARRAY OF CHAR);
BEGIN
	IF sym = expected THEN Scanner.Get (sym) ELSE Scanner.Mark (err) END
END Check;

PROCEDURE MakeIntConst (VAR x: Base.Item);
BEGIN Generator.Clean_item (x); Generator.Make_const (x, Base.intType, 0)
END MakeIntConst;

PROCEDURE CheckExport (VAR exported: BOOLEAN);
BEGIN (* stub *)
END CheckExport;

PROCEDURE CheckValue (VAR x: Base.Item);
BEGIN
	IF ~(x.mode IN Base.clsValue) THEN Scanner.Mark ('Not a value') END
END CheckValue;

PROCEDURE CheckInt (VAR x: Base.Item);
BEGIN CheckValue (x);
	IF ~(x.type.form = Base.tInteger) THEN
		Scanner.Mark ('Not an integer'); MakeIntConst (x)
	END
END CheckInt;

PROCEDURE CheckReal (VAR x: Base.Item);
BEGIN CheckValue (x);
	IF ~(x.type.form = Base.tReal) THEN
		Scanner.Mark ('Not a real'); MakeIntConst (x); x.type := Base.realType
	END
END CheckReal;

PROCEDURE CheckSet (VAR x: Base.Item);
BEGIN CheckValue (x);
	IF ~(x.type = Base.setType) THEN
		Scanner.Mark ('Not a SET'); MakeIntConst (x); x.type := Base.setType
	END
END CheckSet;

PROCEDURE CheckBool (VAR x: Base.Item);
BEGIN CheckValue (x);
	IF ~(x.type = Base.boolType) THEN Scanner.Mark ('Not a BOOLEAN');
		MakeIntConst (x); x.type := Base.boolType
	END
END CheckBool;

PROCEDURE CheckChar (VAR x: Base.Item);
BEGIN CheckValue (x);
	IF (x.type = Base.stringType) & (x.b <= 2) THEN Generator.Str_to_char (x)
	ELSIF ~(x.type = Base.charType) THEN Scanner.Mark ('Not a CHAR');
		MakeIntConst (x); x.type := Base.charType
	END
END CheckChar;

PROCEDURE CheckVar (VAR x: Base.Item; readonly: BOOLEAN);
BEGIN
	IF ~(x.mode IN Base.clsVariable) THEN
		Scanner.Mark ('Not a variable'); MakeIntConst (x); x.mode := Base.cVar
	END;
	IF ~readonly & x.readonly THEN Scanner.Mark ('Read-only variable');
		MakeIntConst (x); x.mode := Base.cVar
	END
END CheckVar;

PROCEDURE IsExt (tpX, tpY: Base.Type) : BOOLEAN;
	
	PROCEDURE RecursiveIsExt (tpX, tpY: Base.Type) : BOOLEAN;
		RETURN (tpX = tpY) OR (tpX.len > tpY.len)
			& RecursiveIsExt(tpX.base, tpY)
	END RecursiveIsExt;
	
BEGIN (* IsExt *)
	IF tpX.form = Base.tPointer THEN tpX := tpX.base END;
	IF tpY.form = Base.tPointer THEN tpY := tpY.base END;
	RETURN RecursiveIsExt(tpX, tpY)
END IsExt;

PROCEDURE CompProc (tpX, tpY: Base.Type) : BOOLEAN;
	RETURN TRUE
END CompProc;

PROCEDURE CheckAssignment (tpX: Base.Type; VAR y: Base.Item);
END CheckAssignment;

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
BEGIN SymTable.Find (obj, Scanner.id);
	IF obj = Base.guard THEN Scanner.Mark ('Undefined identifier');
		SymTable.Find (obj, 'INTEGER')
	END
	(* stub *)
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
		Generator.Make_string (x, Scanner.slen); Scanner.Get (sym)
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
		IF x.type.form = Base.tInteger THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF (op = Scanner.and) OR (op = Scanner.rdiv) THEN
				Scanner.Mark (invalidOperationError); op := Scanner.times
			END;
			Scanner.Get (sym); factor (y); CheckInt (y);
			Generator.Int_op2 (op, x, y)
		ELSIF x.type.form = Base.tReal THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF (op # Scanner.times) & (op # Scanner.rdiv) THEN
				Scanner.Mark (invalidOperationError); op := Scanner.times
			END;
			Scanner.Get (sym); factor (y); CheckReal (y);
			Generator.Real_op2 (op, x, y)
		ELSIF x.type = Base.setType THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF (op # Scanner.times) & (op # Scanner.rdiv) THEN
				Scanner.Mark (invalidOperationError); op := Scanner.times
			END;
			Scanner.Get (sym); factor (y); CheckSet (y);
			Generator.Set_op2 (op, x, y)
		ELSIF x.type = Base.boolType THEN
			IF op # Scanner.and THEN Scanner.Mark (invalidOperationError) END;
			Generator.And1 (x); Scanner.Get (sym); factor (y); CheckBool (y);
			Generator.And2 (x, y);
		ELSE Scanner.Mark (invalidOperationError); MakeIntConst (x);
			Scanner.Get (sym); factor (y); Generator.Clean_item (y)
		END
	END
END term;

PROCEDURE SimpleExpression (VAR x: Base.Item);
	VAR op: INTEGER; y: Base.Item;
BEGIN
	IF sym = Scanner.minus THEN Scanner.Get (sym); term (x);
		IF x.type.form IN {Base.tInteger, Base.tReal, Base.tSet} THEN
			Generator.Negate (x)
		ELSE Scanner.Mark ('Not an integer, real or set value');
			MakeIntConst (x)
		END
    ELSIF sym = Scanner.plus THEN Scanner.Get (sym); term (x)
    ELSE term (x)
    END;
		
	WHILE (sym >= Scanner.plus) & (sym <= Scanner.or) DO
		CheckValue (x); op := sym;
		IF x.type.form IN {Base.tInteger, Base.tReal, Base.tSet} THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF op = Scanner.or THEN
				Scanner.Mark (invalidOperationError); op := Scanner.plus
			END;
			Scanner.Get (sym); term (y);
			IF x.type.form = Base.tInteger THEN
				CheckInt (y); Generator.Int_op1 (op, x, y)
			ELSIF x.type.form = Base.tReal THEN
				CheckReal (y); Generator.Real_op1 (op, x, y)
			ELSE CheckSet (y); Generator.Set_op1 (op, x, y)
			END
		ELSIF x.type = Base.boolType THEN
			IF op # Scanner.or THEN Scanner.Mark (invalidOperationError) END;
			Generator.Or1 (x); Scanner.Get (sym); term (y); CheckBool (y);
			Generator.Or2 (x, y);
		ELSE Scanner.Mark (invalidOperationError); MakeIntConst (x);
			Scanner.Get (sym); term (y); Generator.Clean_item (y)
		END
	END
END SimpleExpression;

PROCEDURE _expression (VAR x: Base.Item);
	VAR y: Base.Item; rel: INTEGER; xt: Base.Type;
BEGIN SimpleExpression (x);
	IF (sym >= Scanner.eql) & (sym <= Scanner.geq) THEN
		CheckValue (x); rel := sym;
		IF x.type.form = Base.tInteger THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			Scanner.Get (sym); SimpleExpression (y); CheckInt (y);
			Generator.Int_relation (rel, x, y)
		ELSIF x.type = Base.setType THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF (rel = Scanner.gtr) OR (rel = Scanner.lss) THEN
				Scanner.Mark (invalidOperationError); rel := Scanner.eql
			END;
			Scanner.Get (sym); SimpleExpression (y); CheckSet (y);
			IF rel <= Scanner.neq THEN Generator.Int_relation (rel, x, y)
			ELSE Generator.Set_relation (rel, x, y)
			END
		ELSIF x.type.form = Base.tReal THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			Scanner.Get (sym); SimpleExpression (y); CheckReal (y);
			Generator.Real_relation (rel, x, y)
		ELSIF x.type = Base.boolType THEN
			IF x.mode IN {Base.mCond} + Base.clsVariable THEN Generator.load (x)
			END;
			IF rel > Scanner.neq THEN Scanner.Mark (invalidOperationError) END;
			Scanner.Get (sym); SimpleExpression (y); CheckBool (y);
			Generator.Int_relation (rel, x, y)
		ELSIF x.type = Base.charType THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			Scanner.Get (sym); SimpleExpression (y); CheckChar (y);
			Generator.Int_relation (rel, x, y)
		ELSIF x.type = Base.stringType THEN
			Scanner.Get (sym); SimpleExpression (y); CheckValue (y);
			IF (x.b <= 2) & (y.type = Base.charType) THEN
				Generator.Str_to_char (x); Generator.Int_relation (rel, x, y)
			ELSE
				IF (y.type # Base.stringType) & ((y.type.form # Base.tArray)
					OR (y.type.base # Base.charType)) THEN
					Scanner.Mark (notStrError); Generator.Clean_item (y); y := x
				END;
				Generator.String_relation (rel, x, y)
			END
		ELSIF (x.type.form = Base.tArray) & (x.type.base = Base.charType) THEN
			Scanner.Get (sym); SimpleExpression (y); CheckValue (y);
			IF (y.type # Base.stringType) & ((y.type.form # Base.tArray)
				OR (y.type.base # Base.charType)) THEN
				Scanner.Mark (notStrError); Generator.Clean_item (y); y := x
			END;
			Generator.String_relation (rel, x, y)
		ELSIF x.type.form = Base.tPointer THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF rel > Scanner.neq THEN Scanner.Mark (invalidOperationError) END;
			Scanner.Get (sym); SimpleExpression (y); CheckValue (y);
			IF y.type.form # Base.tPointer THEN
				IF y.type # Base.nilType THEN
					Scanner.Mark (notPointerError); y.type := x.type
				END
			ELSIF ~IsExt(x.type, y.type) OR ~IsExt(y.type, x.type) THEN
				Scanner.Mark (notExtError)
			END;
			Generator.Int_relation (rel, x, y)
		ELSIF x.type.form = Base.tProcedure THEN
			IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
			IF rel > Scanner.neq THEN Scanner.Mark (invalidOperationError) END;
			Scanner.Get (sym); SimpleExpression (y); CheckValue (y);
			IF y.type.form # Base.tProcedure THEN
				IF y.type # Base.nilType THEN
					Scanner.Mark (notProcError); y.type := x.type
				END
			ELSIF ~CompProc (x.type, y.type) THEN
				Scanner.Mark (notCompProcError)
			END;
			Generator.Int_relation (rel, x, y)
		ELSIF x.type = Base.nilType THEN
			Scanner.Get (sym); SimpleExpression (y); CheckValue (y);
			IF (y.type # Base.nilType) & ~ (y.type.form IN Base.typeAddress)
				THEN Scanner.Mark (notPointerOrProcedureError); y.type := x.type
			END;
			Generator.Int_relation (rel, x, y)
		ELSE Scanner.Mark (invalidOperationError); MakeIntConst (x);
			SimpleExpression (y); MakeIntConst (y);
			Generator.Int_relation (rel, x, y)
		END
	ELSIF sym = Scanner.in THEN
		CheckInt (x); IF x.mode IN Base.clsVariable THEN Generator.load (x) END;
		Scanner.Get (sym); SimpleExpression (y); CheckSet (y);
		Generator.Member_test (x, y)
	ELSIF sym = Scanner.is THEN CheckValue (x);
		IF ~ (x.type.form IN Base.typeHasExt) THEN
			Scanner.Mark (notPointerOrRecordError); MakeIntConst (x);
			x.type := Base.boolType; Scanner.Get (sym); SimpleExpression (y);
			Generator.Clean_item (y)
		ELSE Scanner.Get (sym); SimpleExpression (y);
			IF y.mode # Base.cType THEN
				Scanner.Mark (notTypeError); y.type := x.type
			ELSIF y.type.form IN Base.typeHasExt THEN
				Scanner.Mark (notPointerOrRecordTypeError); y.type := x.type
			ELSIF ~IsExt(x.type, y.type) OR ~IsExt(y.type, x.type) THEN
				Scanner.Mark (notExtError); y.type := x.type
			END;
			Generator.Type_test (x, y.type, FALSE)
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
		END;
		Check (Scanner.colon, noColonError)
	ELSE first := Base.guard
	END
END IdentList;

PROCEDURE type (VAR tp: Base.Type; VAR tpObj: Base.Object);
	VAR obj: Base.Object;
BEGIN
	IF (sym # Scanner.ident) & (sym < Scanner.array) THEN
		Scanner.Mark (notTypeError);
		REPEAT Scanner.Get (sym)
		UNTIL (sym = Scanner.ident) OR (sym >= Scanner.array)
	END;
	IF sym = Scanner.ident THEN qualident (obj);
		IF obj.class = Base.cType THEN tp := obj.type; tpObj := obj
		ELSE Scanner.Mark (notTypeError)
		END
	ELSE ASSERT (FALSE)
	END
END type;

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
			Check (Scanner.eql, 'No ='); expression (x); CheckValue (x);
			IF (x.mode = Base.cConst) OR (x.type.form = Base.tString) THEN
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
	IF sym = Scanner.type THEN
	END;
	IF sym = Scanner.var THEN Scanner.Get (sym);
		WHILE sym = Scanner.ident DO
			IdentList (Base.cVar, first); type (tp, tpObj); obj := first;
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
				Scanner.Get (sym); expression (y); CheckValue (y);
				CheckAssignment (x.type, y); Generator.Store (x, y)
			ELSIF x.mode = Base.cSProc THEN StandProc (x)
			ELSIF (x.mode = Base.cProc) OR (sym = Scanner.lparen) THEN
				ProcedureCall (x, TRUE)
			ELSE Scanner.Mark ('Invalid statement')
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
		Generator.Return;
		Generator.Module_init; Generator.Module_exit;

		Check (Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF modid # Scanner.id THEN Scanner.Mark ('Wrong module name') END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No module identifier after END')
		END;
		Check (Scanner.period, 'No ending .');
		
		Generator.Finish
	END
END Module;

BEGIN
	expression := _expression
END Parser.