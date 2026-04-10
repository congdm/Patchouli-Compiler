MODULE Base;

IMPORT
	Sys, S := Scn;

CONST
	MaxExtension* = 7;
	
	(* Object class *)
	cNull* = -1; cModule* = 0; cType* = 1;
	cNode* = 2; cVar* = 3; cConst* = 4; cProc* = 5;
	cField* = 6; cSProc* = 7; cSFunc* = 8;
	
	(* Type forms *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3; tReal* = 4;
	tPtr* = 5; tProc* = 6; tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;

	tStructs* = {tArray, tRec};
	tScalars* = {tInt, tBool, tSet, tChar, tReal, tPtr, tProc, tNil};

	tEqls* = {tBool, tSet, tPtr, tProc, tNil};
	tCmps* = {tInt, tReal, tChar, tStr};
	tAdds* = {tInt, tReal, tSet};
	tTimes* = {tInt, tReal, tSet};
	tRdivs* = {tReal, tSet};
	
	(* Node op *)
	opPar* = 100H; opCall* = 101H;
	opABS* = 102H; opODD* = 103H; opLEN* = 104H;
	opLSL* = 105H; opASR* = 106H; opROR* = 107H;
	opFLOOR* = 108H; opFLT* = 109H; opORD* = 10AH; opCHR* = 10BH;
	opADR* = 10CH; opSIZE* = 10DH; opBIT* = 10EH; opVAL* = 10FH;	
	opBitset* = 110H;
	
	opINC* = 111H; opDEC* = opINC + 1; opINCL* = opINC + 2;
	opEXCL* = opINC + 3; opNEW* = opINC + 4; opASSERT* = opINC + 5;
	opPACK* = opINC + 6; opUNPK* = opINC + 7;
	opEndOfStdProc* = opUNPK;

TYPE
	Object* = POINTER TO ObjDesc;
	Type* = POINTER TO TypeDesc;
	Node* = POINTER TO NodeDesc;
	Ident* = POINTER TO IdentDesc;
	Scope* = POINTER TO ScopeDesc;
	
	ObjDesc* = RECORD
		class*: BYTE; named*: BOOLEAN;
		type*: Type; ident*: Ident
	END ;
	Const* = POINTER TO RECORD (ObjDesc) END ;
	Field* = POINTER TO RECORD (ObjDesc) END ;
	Var* = POINTER TO RECORD (ObjDesc)
		expno*, lev*: INTEGER; ronly*: BOOLEAN
	END ;
	Par* = POINTER TO RECORD (Var) varpar*: BOOLEAN END ;
	Str* = POINTER TO RECORD (Var) len*: INTEGER END ;
	TempVar* = POINTER TO RECORD (Var) inited*: BOOLEAN END ;
	SProc* = POINTER TO RECORD (ObjDesc) id*: INTEGER END ;
	SFunc* = POINTER TO RECORD (ObjDesc) id*: INTEGER END ;
	
	Proc* = POINTER TO RECORD (ObjDesc)
		decl*: Ident; statseq*: Node; return*: Object
	END;
	
	ExtModule* = POINTER TO RECORD (ObjDesc)
		first*: Ident
	END ;
	
	TypeDesc* = RECORD
		predef*, isOpenArray*: BOOLEAN;
		spos*: INTEGER; base*: Type;
		form*, nfields*, lev*: INTEGER; len*: Sys.Int;
		fields*: Ident; obj*: Object
	END ;
	
	NodeDesc* = RECORD (ObjDesc)
		ronly*: BOOLEAN; spos*: INTEGER;
		op*: INTEGER; left*, right*: Object
	END ;
	
	IdentDesc = RECORD
		export*, used*: BOOLEAN; spos*: INTEGER;
		name*: S.Ident; obj*: Object; next*: Ident
	END ;
	
	ScopeDesc = RECORD first*, last: Ident; dsc*: Scope END ;
	
	ModuleId* = RECORD context*, name*: S.Ident END ;
	Module* = POINTER TO RECORD
		id*: ModuleId;
		system*: BOOLEAN; (* flags *)
		
		init*: Node; universe*: Scope;
		curLev*: INTEGER; topScope*: Scope;
		
		(* Predefined Types *)
		intType*, byteType*, realType*: Type;
		card16Type*, card32Type*, card64Type*: Type;
		int8Type*, int16Type*, int32Type*, int64Type*: Type;
		boolType*, setType*, charType*, nilType*, strType*: Type;
		noType*: Type
	END ;
	
	(* Parser *)

	UndefPtrList* = POINTER TO RECORD
		name*: S.Ident; tp*: Type; next*: UndefPtrList
	END ;
	
	Parser* = POINTER TO RECORD
		scn*: S.Scanner; sym*: INTEGER;
		mod*: Module;
		undefList*: UndefPtrList;
		externalIdentNotFound*: Ident;
		
		SystemStdFunc*: PROCEDURE (psr: Parser; f: SFunc): Object;
		SystemStdProc*: PROCEDURE (psr: Parser; f: SProc): Node;
		NewConst*: PROCEDURE (psr: Parser; t: Type; ival: Sys.Int): Const;
		NewConstR*: PROCEDURE (psr: Parser; rval: Sys.Real): Const;
		NewStr*: PROCEDURE (psr: Parser; str: S.Str; slen: INTEGER): Str;
		IsConstZero*: PROCEDURE (psr: Parser; x: Const): BOOLEAN;
		GetConstInt*: PROCEDURE (psr: Parser; x: Const; VAR res: Sys.Int);
		OpAbs*: PROCEDURE (psr: Parser; x: Const): Const;
		OpOdd*: PROCEDURE (psr: Parser; x: Const): Const;
		OpShift*: PROCEDURE (psr: Parser; op: INTEGER; x, y: Const): Const;
		OpFloor*: PROCEDURE (psr: Parser; x: Const): Const;
		OpFlt*: PROCEDURE (psr: Parser; x: Const): Const;
		OpChr*: PROCEDURE (psr: Parser; x: Const): Const;
		OpOrd*: PROCEDURE (psr: Parser; x: Const): Const;
		OpOrdChar*: PROCEDURE (psr: Parser; x: Str): Const;
		OpRangeSet*: PROCEDURE (psr: Parser; x, y: Const): Const;
		OpSingletonSet*: PROCEDURE (psr: Parser; x: Const): Const;
		OpAdd*: PROCEDURE (psr: Parser; op: INTEGER; x, y: Const): Const;
		OpNegateBool*: PROCEDURE (psr: Parser; x: Const): Const;
		OpMultiply*: PROCEDURE (psr: Parser; x, y: Const): Const;
		OpRDivide*: PROCEDURE (psr: Parser; x, y: Const): Const;
		OpIntDiv*: PROCEDURE (psr: Parser; op: INTEGER; x, y: Const): Const;
		OpAnd*: PROCEDURE (psr: Parser; x, y: Const): Const;
		OpNegate*: PROCEDURE (psr: Parser; x: Const): Const;
		OpOr*: PROCEDURE (psr: Parser; x, y: Const): Const;
		OpCompare*: PROCEDURE (psr: Parser; op: INTEGER; x, y: Object): Const;
		OpIn*: PROCEDURE (psr: Parser; x, y: Const): Const
	END ;
	
PROCEDURE InitConst*(x: Const);
BEGIN x.class := cConst
END InitConst;

PROCEDURE InitStr*(x: Str);
BEGIN x.class := cVar; x.ronly := TRUE; x.lev := 0;
END InitStr;

PROCEDURE InitVar*(x: Var);
BEGIN x.class := cVar; x.expno := -1; x.ronly := FALSE
END InitVar;

PROCEDURE InitType*(tp: Type; form: INTEGER);
BEGIN
	tp.predef := FALSE; tp.isOpenArray := FALSE; tp.spos := 0; 
	tp.form := form; tp.nfields := 0; tp.lev := 0; tp.len := Sys.IntZero
END InitType;

PROCEDURE NewType*(psr: Parser; form: INTEGER): Type;
	VAR tp: Type;
BEGIN NEW(tp); InitType(tp, form);
	RETURN tp
END NewType;

PROCEDURE NewPar*(psr: Parser; proc, type: Type; varpar: BOOLEAN): Par;
	VAR x: Par;
BEGIN
	NEW(x); InitVar(x); x.varpar := varpar;
	x.type := type; x.lev := psr.mod.curLev; INC(proc.nfields);
	RETURN x
END NewPar;

PROCEDURE NewField*(psr: Parser; rec, ftype: Type): Field;
	VAR x: Field;
BEGIN NEW(x); x.class := cField; x.type := ftype; INC(rec.nfields);
	RETURN x
END NewField;

PROCEDURE NewTypeObj*(psr: Parser; tp: Type): Object;
	VAR x: Object;
BEGIN NEW(x); x.class := cType; x.type := tp;
	RETURN x
END NewTypeObj;

PROCEDURE NewVar*(psr: Parser; tp: Type): Var;
	VAR x: Var;
BEGIN NEW(x); InitVar(x); x.lev := psr.mod.curLev; x.type := tp;
	RETURN x
END NewVar;

PROCEDURE NewProc*(psr: Parser): Proc;
	VAR x: Proc;
BEGIN NEW(x); x.class := cProc;
	RETURN x
END NewProc;

END Base.