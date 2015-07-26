MODULE Base;

IMPORT
	Sys;
	
CONST
	WordSize* = 8; CharSize* = 2; MaxChar* = 65535; SetUpperLimit* = 64;
	MaxInt* = 9223372036854775807; MinInt* = -MaxInt - 1; MaxIdentLen* = 63;
	MaxStrLen* = 255; MaxExtension* = 8; MaxRecordTypes* = 512;
	
	(* Object class/Item mode *)
	cHead* = 0; cModule* = 1; cVar* = 2; cRef* = 3; cConst* = 4;
	cField* = 5; cType* = 6; cProc* = 7; cSProc* = 8; cSFunc* = 9;
	mReg* = 10; mRegI* = 11; mCond* = 12; mXreg* = 13;
	
	clsVariable* = {cVar, cRef, mRegI};
	clsValue* = clsVariable	+ {cConst, mReg, mCond, cProc, mXreg};

	(* Type form *)
	tInteger* = 0; tBoolean* = 1; tSet* = 2; tChar* = 3;
	tReal* = 4; tPointer* = 5; tProcedure* = 6;
	tArray* = 7; tRecord* = 8; tString* = 9; tNil* = 10;
	tAddress* = 11;
	
	typeSimple* = {tInteger, tBoolean, tSet, tReal, tChar};
	typeAddress* = {tPointer, tProcedure, tAddress};
	typePointer* = {tPointer, tAddress};
	typeScalar* = typeSimple + typeAddress;
	typeNumberic* = {tInteger, tReal};
	typeCharacter* = {tChar, tString};
	typeHasExt* = {tRecord, tPointer};
	
	(* System procedures *)
	ExitProcess* = -56;
	LoadLibraryW* = -48;
	GetProcAddress* = -40;
	GetProcessHeap* = -32;
	HeapAlloc* = -24;
	HeapFree* = -16;
	
TYPE
	IdentStr* = ARRAY MaxIdentLen + 1 OF CHAR;
	String* = ARRAY MaxStrLen + 1 OF CHAR;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjectDesc;
	
	TypeDesc* = RECORD
		ref*, mod*: INTEGER;
		tdAdr*, expno*: INTEGER;
		
		form*, size*, len*, numPtr*, alignment*: INTEGER;
		base*: Type; baseTpObj*: Object;
		fields*: Object
	END;
	
	ObjectDesc* = RECORD
		param*, readonly*, export* : BOOLEAN;
		name* : IdentStr;
		class*, lev* : INTEGER;
		type* : Type; tpObj*: Object;
		next*, dsc* : Object;
		val*, val2* : INTEGER
	END;
	
	Item* = RECORD
		readonly*, param* : BOOLEAN;
		mode*, lev* : INTEGER;
		obj* : Object; type* : Type;
		r*, a*, b*, c* : INTEGER
	END;

VAR
	guard* : Object;
	
	(* Predefined Types *)
	intType*, byteType*, wordType*, dwordType*: Type;
	boolType*, setType*, charType*, nilType*: Type;
	realType*, longrealType*: Type;
	byteArrayType*, stringType*: Type;
	
	predefinedTypes*: ARRAY 32 OF Type;
	preTypeNo*: INTEGER;

	stringData: ARRAY 65536 OF CHAR;
	strPos: INTEGER;
	
	CompilerFlag* : RECORD
		arrayCheck*, overflowCheck*, typeCheck*, nilCheck*: BOOLEAN;
		alignment*, main*, console*: BOOLEAN
	END;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE StrEqual* (s1, s2: ARRAY OF CHAR) : BOOLEAN;
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(s1)) & (i < LEN(s2)) & (s1[i] # 0X) & (s1[i] = s2[i]) DO
		INC (i)
	END;
	RETURN (i < LEN(s1)) & (i < LEN(s2)) & (s1[i] = s2[i])
		OR (LEN(s1) = LEN(s2)) & (i = LEN(s1))
		OR (i = LEN(s2)) & (s1[i] = 0X)
		OR (i = LEN(s1)) & (s2[i] = 0X)
END StrEqual;

PROCEDURE StrCopy* (src: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(dst) - 1) & (i < LEN(src)) & (src[i] # 0X) DO
		dst[i] := src[i]; INC(i)
	END;
	dst[i] := 0X
END StrCopy;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE NewType* (VAR typ: Type; form: INTEGER);
BEGIN
	NEW (typ);
	typ.form := form;
	typ.mod := -1;
	typ.ref := -1;
	typ.numPtr := 0
END NewType;

PROCEDURE NewPredefinedType (VAR typ: Type; form, size: INTEGER);
BEGIN
	NewType (typ, form);
	typ.mod := -2;
	typ.size := size;
	typ.alignment := size;
	preTypeNo := preTypeNo + 1;
	predefinedTypes[preTypeNo] := typ;
	typ.ref := preTypeNo
END NewPredefinedType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteInt* (VAR f: Sys.FileHandle; n: INTEGER);
	VAR finish: BOOLEAN; b: BYTE;
BEGIN
	REPEAT
		b := n MOD 128; finish := (n >= -64) & (n < 64);
		IF finish THEN b := b + 128 ELSE n := n DIV 128
		END;
		Sys.Write_byte (f, b)
	UNTIL finish
END WriteInt;

PROCEDURE ReadInt* (VAR f: Sys.FileHandle; VAR n: INTEGER);
	VAR finish: BOOLEAN; i, b: INTEGER; k: INTEGER;
BEGIN
	n := 0; i := 1; k := 1; b := 0; 
	REPEAT
		Sys.Read_byte (f, b);
		IF i < 10 THEN finish := b >= 128; b := b MOD 128; n := n + b * k;
			k := k * 128; i := i + 1;
			IF finish & (b >= 64) THEN n := n + (-1 * k)
			END
		ELSIF i = 10 THEN finish := TRUE;
			IF b = 127 THEN n := n + MinInt
			END
		ELSE ASSERT (FALSE); finish := TRUE
		END
	UNTIL finish
END ReadInt;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE SetCompilerFlag* (pragma : ARRAY OF CHAR);
BEGIN
	IF StrEqual(pragma,'MAIN') THEN CompilerFlag.main := TRUE
	ELSIF StrEqual(pragma,'CONSOLE') THEN
		CompilerFlag.main := TRUE; CompilerFlag.console := TRUE
	END
END SetCompilerFlag;

PROCEDURE ResetCompilerFlag*;
BEGIN
	CompilerFlag.arrayCheck := TRUE;
	CompilerFlag.overflowCheck := TRUE;
	CompilerFlag.typeCheck := TRUE;
	CompilerFlag.nilCheck := TRUE;
	CompilerFlag.alignment := TRUE;
	CompilerFlag.main := FALSE;
	CompilerFlag.console := FALSE
END ResetCompilerFlag;
	
PROCEDURE Init*;
BEGIN
	strPos := 0
END Init;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

BEGIN
	NEW (guard); guard.class := cHead;
	preTypeNo := 0; predefinedTypes[0] := NIL;
	
	NewPredefinedType (intType, tInteger, WordSize);
	NewPredefinedType (boolType, tBoolean, 1);
	NewPredefinedType (setType, tSet, WordSize);
	NewPredefinedType (byteType, tInteger, 1);
	NewPredefinedType (charType, tChar, CharSize);
	NewPredefinedType (nilType, tNil, WordSize);
	NewPredefinedType (realType, tReal, 4);
	NewPredefinedType (longrealType, tReal, 8);
	
	NewPredefinedType (wordType, tInteger, 2);
	NewPredefinedType (dwordType, tInteger, 4);
	
	NewPredefinedType (stringType, tString, CharSize);
	
	NewPredefinedType (byteArrayType, tArray, 1);
	byteArrayType.base := byteType; byteArrayType.baseTpObj := NIL;
	byteArrayType.len := 1
END Base.