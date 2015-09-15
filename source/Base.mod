MODULE Base;

IMPORT
	Console, Sys;
	
CONST
	WordSize* = 8; CharSize* = 2; MaxChar* = 65535; SetUpperLimit* = 64;
	MaxInt* = 9223372036854775807; MinInt* = -MaxInt - 1; MaxIdentLen* = 63;
	MaxStrLen* = 255; MaxExtension* = 8; MaxRecordTypes* = 512;
	MaxModules* = 256; MaxExportTypes* = 1024;
	
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
	typeAddress* = {tPointer, tProcedure, tAddress, tNil};
	typePointer* = {tPointer, tAddress};
	typeScalar* = typeSimple + typeAddress;
	typeNumberic* = {tInteger, tReal};
	typeCharacter* = {tChar, tString};
	typeHasExt* = {tRecord, tPointer};
	
	(* Win32 specifics *)
	HeapHandle* = -64;
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
		modname*: POINTER TO RECORD s*: IdentStr END;
		extensible*: BOOLEAN;
		form*, size*, len*, nptr*, alignment*, parblksize*: INTEGER;
		base*, adrType*: Type;
		obj*, fields*: Object
	END;
	
	ObjectDesc* = RECORD
		nilable*, tagged*, param*, readonly*, export*: BOOLEAN;
		name*: IdentStr;
		class*, lev*, expno*: INTEGER;
		type*: Type;
		next*, dsc*: Object;
		val*, val2*: INTEGER
	END;
	
	Item* = RECORD
		readonly*, param*, tagged* : BOOLEAN;
		mode*, lev* : INTEGER;
		obj* : Object; type* : Type;
		r*, a*, b*, c* : INTEGER
	END;

VAR
	guard* : Object;
	
	(* Predefined Types *)
	intType*, int8Type*, int16Type*, int32Type*: Type;
	byteType*, card16Type*, card32Type*: Type;
	boolType*, setType*, charType*, char8Type*, nilType*: Type;
	realType*, longrealType*: Type;
	stringType*, string8Type*: Type;
	sysByteType*, byteArrayType*: Type;
	
	LoadLibraryFuncType*: Type;
	GetProcAddressFuncType*: Type;
	HeapAllocFuncType*: Type;
	HeapFreeFuncType*: Type;
	
	predefinedTypes*: ARRAY 32 OF Type;
	preTypeNo*: INTEGER;
	
	CplFlag* : RECORD
		overflowCheck*, divideCheck*, arrayCheck*: BOOLEAN;
		typeCheck*, nilCheck*: BOOLEAN;
		main*, console*: BOOLEAN
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
	typ.adrType := NIL;
	typ.nptr := 0
END NewType;

PROCEDURE NewPredefinedType (VAR typ: Type; form, size: INTEGER);
BEGIN
	NewType (typ, form);
	typ.mod := -2;
	typ.size := size;
	typ.alignment := size;
	INC (preTypeNo); predefinedTypes[preTypeNo] := typ;
	typ.ref := preTypeNo
END NewPredefinedType;

PROCEDURE NewAddressType* (bas: Type);
	VAR tp: Type;
BEGIN
	NewType (tp, tAddress); tp.size := WordSize; tp.alignment := WordSize;
	tp.base := bas; bas.adrType := tp
END NewAddressType;

PROCEDURE IsSignedType* (typ: Type) : BOOLEAN;
	RETURN (typ = int8Type) OR (typ = int16Type) OR (typ = int32Type)
END IsSignedType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteInt* (VAR f: Sys.FileHandle; n: INTEGER);
	VAR finish: BOOLEAN; b: INTEGER;
BEGIN
	REPEAT b := n MOD 128; finish := (n >= -64) & (n < 64);
		IF finish THEN b := b + 128 ELSE n := n DIV 128 END;
		Sys.Write_byte (f, b)
	UNTIL finish
END WriteInt;

PROCEDURE ReadInt* (VAR f: Sys.FileHandle; VAR n: INTEGER);
	VAR finish: BOOLEAN; i, b, k: INTEGER;
BEGIN n := 0; i := 1; k := 1;
	REPEAT Sys.Read_byte (f, b);
		IF i < 10 THEN
			finish := b >= 128; b := b MOD 128; n := n + b * k;
			IF i # 9 THEN k := k * 128 END; INC (i);
			IF finish & (b >= 64) THEN
				IF i # 9 THEN n := n + (-1 * k) ELSE n := n + MinInt END
			END
		ELSIF i = 10 THEN
			finish := TRUE; IF b = 127 THEN n := n + MinInt END
		ELSE ASSERT(FALSE)
		END
	UNTIL finish
END ReadInt;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE SetCompilerFlag* (pragma: ARRAY OF CHAR);
BEGIN
	IF StrEqual(pragma,'MAIN') THEN CplFlag.main := TRUE
	ELSIF StrEqual(pragma,'CONSOLE') THEN
		CplFlag.main := TRUE; CplFlag.console := TRUE
	END
END SetCompilerFlag;

PROCEDURE ResetCompilerFlag*;
BEGIN
	CplFlag.divideCheck := TRUE;
	CplFlag.arrayCheck := TRUE;
	CplFlag.typeCheck := TRUE;
	CplFlag.nilCheck := TRUE;
	CplFlag.overflowCheck := FALSE;
	CplFlag.main := FALSE;
	CplFlag.console := FALSE
END ResetCompilerFlag;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

BEGIN
	NEW (guard); guard.class := cHead;
	preTypeNo := 0; predefinedTypes[0] := NIL;
	
	NewPredefinedType (intType, tInteger, WordSize);
	NewPredefinedType (int8Type, tInteger, 1);
	NewPredefinedType (int16Type, tInteger, 2);
	NewPredefinedType (int32Type, tInteger, 4);
	
	NewPredefinedType (byteType, tInteger, 1);
	NewPredefinedType (card16Type, tInteger, 2);
	NewPredefinedType (card32Type, tInteger, 4);
	
	NewPredefinedType (boolType, tBoolean, 1);
	NewPredefinedType (setType, tSet, WordSize);
	
	NewPredefinedType (charType, tChar, CharSize);
	NewPredefinedType (char8Type, tChar, 1);
	
	NewPredefinedType (nilType, tNil, WordSize);
	
	NewPredefinedType (realType, tReal, 4);
	NewPredefinedType (longrealType, tReal, 8);
	
	NewPredefinedType (stringType, tString, CharSize);
	stringType.base := charType;
	NewPredefinedType (string8Type, tString, 1);
	string8Type.base := char8Type;
	
	NewPredefinedType (sysByteType, tInteger, 1);
	NewPredefinedType (byteArrayType, tArray, 1);
	byteArrayType.base := sysByteType;
	
	NewPredefinedType (LoadLibraryFuncType, tProcedure, WordSize);
	LoadLibraryFuncType.parblksize := WordSize;
	
	NewPredefinedType (GetProcAddressFuncType, tProcedure, WordSize);
	LoadLibraryFuncType.parblksize := WordSize * 2;
	
	NewPredefinedType (HeapAllocFuncType, tProcedure, WordSize);
	HeapAllocFuncType.parblksize := WordSize * 3;
	
	NewPredefinedType (HeapFreeFuncType, tProcedure, WordSize);
	HeapFreeFuncType.parblksize := WordSize * 3
END Base.