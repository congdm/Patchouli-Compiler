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

TYPE
	Object* = POINTER TO ObjDesc;
	Type* = POINTER TO TypeDesc;
	Node* = POINTER TO NodeDesc;
	Ident* = POINTER TO IdentDesc;
	Scope* = POINTER TO ScopeDesc;
	
	ObjDesc* = RECORD class*: INTEGER; type*: Type; ident*: Ident END ;
	Const* = POINTER TO RECORD (ObjDesc) ival*: Sys.Int; rval*: Sys.Real END ;
	Field* = POINTER TO RECORD (ObjDesc) off*: Sys.Int END ;
	Var* = POINTER TO RECORD (ObjDesc)
		expno*, lev*: INTEGER; ronly*: BOOLEAN
	END ;
	Par* = POINTER TO RECORD (Var) varpar*: BOOLEAN END ;
	Str* = POINTER TO RECORD (Var) bufpos*, len*: Sys.Int END ;
	TempVar* = POINTER TO RECORD (Var) inited*: BOOLEAN END ;
	SProc* = POINTER TO RECORD (ObjDesc) id*: INTEGER END ;
	SFunc* = POINTER TO RECORD (ObjDesc) id*: INTEGER END ;
	
	TypeDesc* = RECORD
		predef*, isOpenArray*: BOOLEAN;
		spos*: Sys.Int; base*: Type;
		lev*, form*, nfpars*: INTEGER; len*: Sys.Int;
		fields*: Ident; obj*: Object
	END ;
	
	NodeDesc* = RECORD (ObjDesc)
		ronly*: BOOLEAN; spos*: Sys.Int;
		op*: INTEGER; left*, right*: Object
	END ;
	
	IdentDesc = RECORD
		export*, used*: BOOLEAN; spos*: Sys.Int;
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
	
	TypeCastList* = POINTER TO RECORD
		node*: Node; type*: Type; next*: TypeCastList
	END ;
	
	Parser* = POINTER TO RECORD
		scn*: S.Scanner; sym*: INTEGER;
		mod*: Module;
		undefList*: UndefPtrList;
		externalIdentNotFound*: Ident;
		typeCastStack*: TypeCastList
	END ;

END Base.