MODULE Base;

IMPORT
	SYSTEM, S := Scn;

CONST
	MaxExt* = 7; MaxRecTypes* = 512;
	MaxImpMod* = 256; MaxExpTypes* = 1024; MaxModLev* = 255;
	true* = 1; false* = 0;
	MaxInt = 9223372036854775807; MinInt = -MaxInt-1;

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

	(* Op codes *)
	opCall* = 100H; opPar* = 101H; opSproc* = 102H; opBitset* = 104H;
	opABS* = 110H; opODD* = 111H; opLEN* = 112H;
	opLSL* = 113H; opASR* = 114H; opROR* = 115H;
	opFLOOR* = 116H; opFLT* = 117H; opORD* = 118H; opCHR* = 119H;
	opADR* = 120H; opBIT* = 121H; opVAL* = 122H; opSIZE* = 123H;
	
	opINC* = 130H; opDEC* = 131H; opINCL* = 132H; opEXCL* = 133H;
	opNEW* = 134H; opASSERT* = 135H; opPACK* = 136H; opUNPK* = 137H;
	opGET* = 138H; opPUT* = 139H; opCOPY* = 140H;
	
	(* Object classes for symbol files *)
	cNull* = -1; cModule* = 0; cType* = 1;
	cNode* = 2; cVar* = 3; cConst* = 4; cProc* = 5;
	cField* = 6; cSProc* = 7; cSFunc* = 8;

TYPE
	ModuleKey* = ARRAY 2 OF INTEGER;
	ModuleId* = RECORD context*, name*: S.Ident END ;

	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
	Node* = POINTER TO NodeDesc;
	Ident* = POINTER TO IdentDesc;
	Scope* = POINTER TO ScopeDesc;
	
	ObjDesc* = RECORD ident*: Ident; type*: Type END ;
	
	Const* = POINTER TO RECORD (Object) END ;
	Var* = POINTER TO RECORD (Object) ronly*: BOOLEAN; lev*: INTEGER END ;
	Field* = POINTER TO RECORD (Object) END ;
	SProc* = POINTER TO RECORD (Object) id*: INTEGER END ;

	Proc* = POINTER TO RECORD (Object)
		lev*: INTEGER; decl*: Ident; statseq*: Node; return*: Object
	END;

	ObjList* = POINTER TO RECORD obj*: Object; next*: ObjList END ;
	TypeList* = POINTER TO RECORD type*: Type; next*: TypeList END ;
	ProcList* = POINTER TO RECORD obj*: Proc; next*: ProcList END ;
	StrList* = POINTER TO RECORD obj*: Str; next*: StrList END ;
	
	ImportedModule* = POINTER TO RECORD (ObjDesc)
		export*: BOOLEAN;
		id*: ModuleId; key*: ModuleKey;
		first*: Ident; 
		next*: Module
	END ;
	
	TypeDesc* = RECORD (ObjDesc)
		predef*, openArray*: BOOLEAN; (* flags *)	
		(* mod - module of origin, ref - export id *)
		mod*: ImportedModule; ref*: INTEGER;
		(* --- *)
		form*: INTEGER;
		recLev*: INTEGER; base*: Type;
		fields*: Ident; nfpar*: INTEGER
	END ;

	NodeDesc = RECORD (ObjDesc)
		ronly*: BOOLEAN;
		op*, spos*: INTEGER;
		left*, right*: Object
	END ;

	IdentDesc = RECORD
		export*: BOOLEAN; spos*, nUsed*: INTEGER;
		name*: S.Ident; obj*: Object; next*: Ident
	END ;
	ScopeDesc = RECORD first*, last: Ident; dsc*: Scope END ;

	Module* = POINTER TO RECORD
		id*: ModuleId; arch*: INTEGER;
		system*: BOOLEAN; (* flags *)
		
		init*: Node; universe*: Scope;
		curLev*: INTEGER; topScope*: B.Scope;
		expno*, refno*: INTEGER;
		
		strList*: StrList; recList*: TypeList;
		expList*, lastExp*: ObjList;
		imodList*: Module
	END ;

VAR
	externalIdentNotFound*: Ident;
	mod*: Module; iPsr*: IPsr;
	predefTypes: TypeList;

	impMod: Module;

	intType*, boolType*, setType*, realType*, byteType*: Type;
	charType*, strType*: Type;
	nilType*: Type;
	
	ExportType0: PROCEDURE(typ: Type);
	ImportType0: PROCEDURE(VAR typ: Type; imod: Module);

(* Symbols table *)

PROCEDURE OpenScope*;
	VAR scp: Scope;
BEGIN
	NEW(scp); scp.dsc := topScope; topScope := scp
END OpenScope;

PROCEDURE CloseScope*;
BEGIN
	topScope := topScope.dsc
END CloseScope;

PROCEDURE IncLev*(x: INTEGER);
BEGIN
	curLev := curLev + x
END IncLev;

PROCEDURE NewIdent0*(VAR ident: Ident; name: S.Ident);
	VAR prev, x: Ident;
BEGIN x := topScope.first;
	NEW(ident); ident.export := FALSE; ident.name := name; ident.nUsed := 0;
	WHILE x # NIL DO
		IF x # NIL THEN S.Mark('duplicated ident') END ;
		prev := x; x := x.next
	END ;
	IF prev # NIL THEN prev.next := ident ELSE topScope.first := ident END
END NewIdent0;

PROCEDURE NewIdent*(VAR ident: Ident);
BEGIN NewIdent0(ident, S.id); ident.spos := S.symPos
END NewIdent;

PROCEDURE InitNewObject(x: Object);
BEGIN
	(* placeholder *)
END InitNewObject;

PROCEDURE NewConst*(t: Type; val: INTEGER): Const;
	VAR c: Const;
BEGIN
	NEW(c); InitNewObject(c);
	c.type := t; c.value := val;
	RETURN c
END NewConst;

PROCEDURE NewTypeObj*(): TypeObj;
	VAR t: TypeObj;
BEGIN
	NEW(t); InitNewObject(t);
	RETURN t
END NewTypeObj;

PROCEDURE NewVar*(t: Type): Var;
	VAR v: Var;
BEGIN
	NEW(v); InitNewObject(v);
	v.type := t; v.lev := curLev; v.ronly := FALSE;
	RETURN v
END NewVar;

PROCEDURE NewPar*(proc: Type; t: Type; varpar: BOOLEAN): Par;
	VAR p: Par;
BEGIN
	NEW(p); InitNewObject(p);
	p.type := t; p.lev := curLev;
	p.varpar := varpar; INC(proc.len);
	p.ronly := ~varpar & (t.form IN tStructs);
	RETURN p
END NewPar;

PROCEDURE NewField*(VAR rec: TypeDesc; t: Type): Field;
	VAR f: Field;
BEGIN
	NEW(f); InitNewObject(f); f.type := t;
	INC(rec.nPtr, t.nPtr);
	INC(rec.nProc, t.nProc);
	INC(rec.nTraced, t.nTraced)
	RETURN f
END NewField;

PROCEDURE NewStr*(str: ARRAY OF CHAR; slen: INTEGER): Str;
	VAR x: Str; i: INTEGER; p: StrList;
BEGIN
	NEW(x); InitNewObject(x); x.ronly := TRUE;
	x.type := strType; x.lev := curLev; x.len := slen;
	IF x.lev >= -1 (* not imported str, need to alloc buffer *) THEN 
		IF curmod.strbufSize + slen >= LEN(curmod.strbuf) THEN
			S.Mark('too many strings'); x.bufpos := -1
		ELSE x.bufpos := curmod.strbufSize; INC(curmod.strbufSize, slen);
			FOR i := 0 TO slen-1 DO curmod.strbuf[x.bufpos+i] := str[i] END ;
			NEW(p); p.obj := x; p.next := curmod.strList; curmod.strList := p
		END
	ELSE x.bufpos := -1
	END ;
	RETURN x
END NewStr;

PROCEDURE NewProc*(): Proc;
	VAR x: Proc;
BEGIN
	NEW(x); InitNewObject(x); x.lev := curLev;
	RETURN x
END NewProc;

(* types *)

PROCEDURE NewType*(VAR t: Type; form: INTEGER);
BEGIN
	NEW(t); t.form := form; t.predef := FALSE; t.ref := -1;
	t.union := FALSE; t.untagged := FALSE;
	t.nPtr := 0; t.nProc := 0; t.nTraced := 0
END NewType;

PROCEDURE NewArray*(VAR t: Type; len: INTEGER);
BEGIN
	NewType(t, tArray); t.len := len;
END NewArray;

PROCEDURE CompleteArray*(VAR t: TypeDesc);
BEGIN
	IF t.base.form = tArray THEN CompleteArray(t.base^) END ;
	t.nPtr := t.len * t.base.nPtr;
	t.nTraced := t.len * t.base.nTraced;
	t.nProc := t.len * t.base.nProc
END CompleteArray;

PROCEDURE NewRecord*(VAR t: Type);
	VAR p: TypeList;
BEGIN
	NewType(t, tRec); t.len := 0;
	IF curLev >= 0 THEN
		NEW(p); p.type := t; p.next := curmod.recList; curmod.recList := p
	ELSIF curLev = -1 THEN ASSERT(FALSE)
	END
END NewRecord;

PROCEDURE ExtendRecord*(VAR rec: TypeDesc);
	VAR base: Type;
BEGIN
	IF rec.base # NIL THEN base := rec.base;
		rec.len := base.len + 1; rec.nPtr := base.nPtr;
		rec.nTraced := base.nTraced; rec.nProc := base.nProc
	END
END ExtendRecord;

PROCEDURE NewPointer*(VAR t: Type);
BEGIN
	NewType(t, tPtr); t.nPtr := 1; t.nTraced := 1
END NewPointer;

PROCEDURE NewProcType*(VAR t: Type);
BEGIN
	NewType(t, tProc); t.len := 0; t.nProc := 1
END NewProcType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Utilities *)

PROCEDURE Insert*(src: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR; VAR p: INTEGER);
	VAR i, j: INTEGER;
BEGIN i := p; j := 0;
	WHILE src[j] # 0X DO dst[i] := src[j]; INC(i); INC(j) END ;
	dst[i] := 0X; p := i
END Insert;

PROCEDURE Append*(src: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
	VAR i, j: INTEGER;
BEGIN
	i := 0; WHILE dst[i] # 0X DO INC(i) END ;
	j := 0; WHILE src[j] # 0X DO dst[i] := src[j]; INC(i); INC(j) END ;
	dst[i] := 0X
END Append;

PROCEDURE WriteInt(x: INTEGER);
END WriteInt;

PROCEDURE WriteNum(x: INTEGER);
END WriteNum;

PROCEDURE WriteBool(x: BOOLEAN);
END WriteBool;

PROCEDURE WriteByteStr(str: ARRAY OF CHAR);
END WriteByteStr;

PROCEDURE ReadInt(VAR x: INTEGER);
END ReadInt;

PROCEDURE ReadNum(VAR x: INTEGER);
END ReadNum;

PROCEDURE ReadBool(VAR x: BOOLEAN);
END ReadBool;

PROCEDURE ReadByteStr(VAR str: ARRAY OF CHAR);
END ReadByteStr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ModIdToStr*(id: ModuleId; VAR str: ARRAY OF CHAR);
	VAR slen: INTEGER;
BEGIN
	slen := 0; Insert(id.context, str, slen);
	Insert('.', str, slen); Insert(id.name, str, slen)
END ModIdToStr;

PROCEDURE ModIdToStr2*(id: ModuleId; VAR str: ARRAY OF CHAR; VAR pos: INTEGER);
BEGIN
	Insert(id.context, str, pos);
	Insert('.', str, pos); Insert(id.name, str, pos)
END ModIdToStr2;

PROCEDURE EqlModId*(x, y: ModuleId): BOOLEAN;
	RETURN (x.context = y.context) & (x.name = y.name)
END EqlModId;

PROCEDURE FindImportedMod(id: ModuleId): Module;
	VAR imod: Module;
BEGIN
	imod := curmod.imodList;
	WHILE (imod # NIL) & ~EqlModId(imod.id, id) DO imod := imod.next END ;
	RETURN imod
END FindImportedMod;

PROCEDURE WriteModId(x: ModuleId);
BEGIN
	WriteByteStr(x.context); WriteByteStr(x.name)
END WriteModId;

PROCEDURE ReadModId(VAR x: ModuleId);
BEGIN
	ReadByteStr(x.context); ReadByteStr(x.name)
END ReadModId;

(*
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Export symbol file *)

PROCEDURE NewExport(VAR exp: ObjList);
BEGIN
	NEW(exp); INC(curmod.expno);
	IF curmod.lastExp = NIL THEN curmod.expList := exp; curmod.lastExp := exp
	ELSE curmod.lastExp.next := exp; curmod.lastExp := exp
	END
END NewExport;

PROCEDURE WriteModkey(key: ModuleKey);
BEGIN
	WriteInt(key[0]); WriteInt(key[1])
END WriteModkey;

PROCEDURE ExportTypeRef(typ: Type);
BEGIN
	IF typ # NIL THEN
		IF typ.predef THEN
			WriteNum(1); WriteNum(typ.ref)
		ELSIF typ.mod = NIL (* internal (non-imported) type *) THEN
			WriteNum(2); WriteNum(typ.ref);
			IF typ.ref < 0 THEN ExportType0(typ) END
		ELSE (* external (imported) type *)
			WriteNum(3); WriteModId(typ.mod.id);
			WriteBool(~typ.mod.export);
			IF ~typ.mod.export THEN
				WriteModkey(typ.mod.key); typ.mod.export := TRUE
			END ;
			WriteNum(typ.ref);
			IF typ.ref < 0 THEN ExportType0(typ) END
		END
	ELSE WriteNum(0)
	END
END ExportTypeRef;

PROCEDURE ExportProc(typ: Type);
	VAR p: Ident; x: Par;
BEGIN
	WriteNum(typ.len); ExportTypeRef(typ.base); p := typ.fields;
	WHILE p # NIL DO
		x := p.obj(Par); WriteNum(cVar); WriteByteStr(p.name);
		WriteBool(x.varpar); ExportTypeRef(x.type); p := p.next
	END ;
	WriteNum(cNull)
END ExportProc;
	
PROCEDURE ExportType(typ: Type);
	VAR f: Ident; ftyp: Type; exp: ObjList;
BEGIN
	IF typ.mod = NIL THEN
		IF curmod.refno < MaxExpTypes THEN
			INC(curmod.refno); typ.ref := curmod.refno
		ELSE S.Mark('too many exported types')
		END
	ELSE typ.ref := -typ.ref
	END ;
	WriteNum(-typ.ref); WriteNum(typ.form);
	IF typ.form = tRec THEN
		NewExport(exp); exp.obj := NewTypeObj(); exp.obj.type := typ;
		WriteNum(curmod.expno); 
		WriteBool(typ.union);
		ExportTypeRef(typ.base);
		f := typ.fields;
		WHILE f # NIL DO ftyp := f.obj.type;
			IF f.export OR (ftyp.nPtr > 0) OR (ftyp.nProc > 0) THEN
				WriteNum(cField);
				IF f.export THEN WriteByteStr(f.name)
				ELSE WriteByteStr(0X)
				END ;
				ExportTypeRef(ftyp)
			END ;
			f := f.next
		END ;
		WriteNum(cNull)
	ELSIF typ.form = tArray THEN
		WriteNum(typ.len);
		WriteBool(typ.untagged);
		ExportTypeRef(typ.base)
	ELSIF typ.form = tPtr THEN
		WriteNum(typ.nTraced);
		ExportTypeRef(typ.base)
	ELSIF typ.form = tProc THEN
		ExportProc(typ)
	END
END ExportType;

PROCEDURE WriteSymfile*;
	VAR ident: Ident; exp: ObjList; i, k, n, size: INTEGER;
		(* hash: Crypt.MD5Hash; chunk: ARRAY 64 OF BYTE; *)
		symfname: ARRAY 512 OF CHAR; x: Object;
BEGIN
	symfname := 0X; curmod.refno := 0; curmod.expno := 0;
	(* i := 0; Insert(srcPath, symfname, i);
	ModIdToStr2(mod.id, symfname, i); Insert('.sym', symfname, i); *)
	
	(*
	symfile := Files.New(symfname);
	Files.Set(rider, symfile, 16);
	Files.WriteNum(rider, modlev);
	*)
	
	imod := curmod.imodList;
	WHILE imod # NIL DO
		WriteNum(cModule); WriteModId(imod.id);
		WriteModkey(imod.key); imod := imod.next
	END;
	
	ident := curmod.universe.first;
	WHILE ident # NIL DO
		IF ident.export THEN x := ident.obj;
			IF x IS Const THEN
				WriteNum(cConst);
				WriteByteStr(ident.name);
				WriteNum(x(Const).value);
				ExportType(x.type)
			ELSIF x IS TypeObj THEN
				WriteNum(cType);
				WriteByteStr(ident.name);
				ExportType(x.type)
			ELSIF x IS Var THEN
				IF x IS Str THEN
					WriteNum(cConst);
					WriteByteStr(ident.name);
					NewExport(exp); exp.obj := x;
					WriteNum(curmod.expno); ExportType(x.type);
					WriteNum(x(Str).len)
				ELSE
					WriteNum(cVar);
					WriteByteStr(ident.name);
					NewExport(exp); exp.obj := x;
					WriteNum(curmod.expno); ExportType(x.type)
				END 
			ELSIF x IS Proc THEN
				WriteNum(cProc);
				WriteByteStr(ident.name);
				NewExport(exp); exp.obj := x;
				WriteNum(curmod.expno); ExportType(x.type)
			ELSE ASSERT(FALSE)
			END
		END;
		ident := ident.next
	END;
	WriteNum(cNull);
	
	(*
	size := Files.Pos(rider); Files.Set(rider, symfile, 0);
	Crypt.InitMD5Hash(hash); i := 0;
	REPEAT
		Files.ReadBytes(rider, chunk, LEN(chunk));
		k := LEN(chunk) - rider.res; INC(i, k);
		Crypt.MD5ComputeChunk(hash, SYSTEM.ADR(chunk), k)
	UNTIL i = size;
	
	Files.Set(rider, symfile, 0);
	modkey[0] := Crypt.MD5GetLowResult(hash);
	modkey[1] := Crypt.MD5GetHighResult(hash);
	WriteModkey(modkey);	
	
	IF S.errcnt = 0 THEN Files.Register(symfile) END
	*)
END WriteSymfile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import symbol file *)

PROCEDURE FindType(ref: INTEGER; types: TypeList): Type;
	VAR p: TypeList; typ: Type;
BEGIN p := types;
	WHILE (p # NIL) & (p.type.ref # ref) DO p := p.next END ;
	IF p # NIL THEN typ := p.type END ;
	RETURN typ
END FindType;

PROCEDURE NewImport(name: S.Ident; x: Object);
	VAR ident, p: Ident;
BEGIN
	NEW(ident); p := topScope.last;
	IF p # NIL THEN p.next := ident ELSE topScope.first := ident END ;
	topScope.last := ident; ident.export := FALSE;
	ident.obj := x; ident.name := name;
	IF x.ident = NIL THEN x.ident := ident END
END NewImport;

PROCEDURE ReadModkey(VAR key: ModuleKey);
BEGIN
	ReadInt(key[0]);
	ReadInt(key[1])
END ReadModkey;

PROCEDURE ImportTypeRef(VAR typ: Type);
	VAR n, ref: INTEGER; first: BOOLEAN;
		mod: Module; id: ModuleId; key: ModuleKey;
BEGIN
	ReadNum(n);
	IF n = 0 THEN typ := NIL
	ELSIF (n = 1) OR (n = 2) THEN ReadNum(ref);
		IF ref > 0 THEN
			IF n = 1 THEN typ := FindType(ref, predefTypes)
			ELSE typ := FindType(-ref, imod.types)
			END
		ELSE ImportType0(typ, imod)
		END
	ELSIF n = 3 THEN
		ReadModId(id); Files.ReadBool(first); mod := FindMod(id);
		IF first THEN ReadModkey(key);
			IF mod # NIL THEN
				IF (mod.key[0] # key[0]) OR (mod.key[1] # key[1]) THEN
					S.Mark('Modkey mismatched')
				END
			ELSE
				NEW(mod); mod.id := id; mod.key := key;
				mod.next := modList; modList := mod;
				mod.no := modno; DEC(modno); mod.import := FALSE
			END
		END ;
		ReadNum(ref);
		IF S.errcnt = 0 THEN
			IF ref < 0 THEN ImportType0(typ, mod)
			ELSE typ := FindType(-ref, mod.types)
			END
		END
	ELSE ASSERT(FALSE)
	END
END ImportTypeRef;

PROCEDURE AddToTypeList(typ: Type; mod: Module);
	VAR i: INTEGER; p: TypeList;
BEGIN
	NEW(p); p.next := mod.types;
	mod.types := p; p.type := typ
END AddToTypeList;

PROCEDURE ImportType(VAR typ: Type; mod: Module);
	VAR typ0: TypeDesc; form, ref, len: INTEGER;
		
	PROCEDURE ImportRecord(VAR typ: TypeDesc; new: BOOLEAN);
		VAR cls, off: INTEGER; fltype: Type; x: Field; name: S.IdStr;
	BEGIN
		ReadNum(typ.expno);
		ReadBool(typ.union);
		ImportTypeRef(typ.base);
		IF S.errcnt = 0 THEN
			ExtendRecord(typ); ReadNum(cls); OpenScope;
			WHILE (cls = cField) & (S.errcnt = 0) DO
				ReadByteStr(name); ImportTypeRef(fltype); ReadNum(cls);
				IF new & (S.errcnt = 0) THEN
					x := NewField(typ, fltype);
					NewImport(name, x)
				END
			END ;
			IF S.errcnt = 0 THEN ASSERT(cls = cNull) END ;
			typ.fields := topScope.first; CloseScope
		END
	END ImportRecord;
	
	PROCEDURE ImportArray(VAR typ: TypeDesc);
	BEGIN
		ReadBool(typ.untagged);
		ImportTypeRef(typ.base);
		IF S.errcnt = 0 THEN CompleteArray(typ) END
	END ImportArray;
	
	PROCEDURE ImportPointer(VAR typ: TypeDesc);
	BEGIN
		ReadNum(typ.nTraced); ImportTypeRef(typ.base)
	END ImportPointer;
	
	PROCEDURE ImportProc(VAR typ: TypeDesc; new: BOOLEAN);
		VAR cls: INTEGER; varpar: BOOLEAN;
			par: Ident; x: Par; xtype: Type; name: S.IdStr;
	BEGIN
		ReadNum(typ.len); ImportTypeRef(typ.base);
		IF S.errcnt = 0 THEN
			ReadNum(cls); OpenScope;
			WHILE (cls = cVar) & (S.errcnt = 0) DO
				ReadByteStr(name); ReadBool(varpar);
				ImportTypeRef(xtype); ReadNum(cls);
				IF new & (S.errcnt = 0) THEN
					x := NewPar(typ, xtype, varpar); NewImport(name, x)
				END
			END ;
			IF S.errcnt = 0 THEN ASSERT(cls = cNull) END ;
			typ.fields := topScope.first; CloseScope
		END
	END ImportProc;
		
BEGIN (* ImportType *)
	ReadNum(ref); ReadNum(form);
	IF (mod = imod) & mod.import THEN typ := NIL
	ELSE typ := FindType(ref, mod.types)
	END ;
	IF typ = NIL THEN
		IF form = tRec THEN
			typ := NewRecord(); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportRecord(typ^, TRUE)	
		ELSIF form = tArray THEN
			ReadNum(typ0.len);
			typ := NewArray(typ0.len); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportArray(typ^)
		ELSIF form = tPtr THEN
			typ := NewPointer(); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportPointer(typ^)
		ELSIF form = tProc THEN
			typ := NewProcType(); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportProc(typ^, TRUE)
		ELSE ASSERT(FALSE)
		END
	ELSE (* this type already exists, skip it *)
		IF form = tRec THEN ImportRecord(typ0, FALSE)
		ELSIF form = tArray THEN ImportArray(typ0)
		ELSIF form = tPtr THEN ImportPointer(typ0)
		ELSIF form = tProc THEN ImportProc(typ0, FALSE)
		ELSE ASSERT(FALSE)
		END
	END
END ImportType;

PROCEDURE Import(imodid: ModuleId): Module;
	VAR dep: Module; x: Object; key: ModuleKey; good: BOOLEAN;
		lev, val, cls, slen: INTEGER; tp: Type; depid: ModuleId;
		name: S.IdStr; str, msg: ARRAY 512 OF CHAR;
BEGIN
	(*
	Files.Set(rider, symfile, 0); imod := FindMod(imodid);
	ReadModkey(key); ReadNum(lev);
	
	
	IF imod = NIL THEN
		NEW(imod); imod.id := imodid; imod.adr := 0; imod.import := TRUE;
		imod.next := modList; modList := imod; imod.no := modno;
		DEC(modno); imod.key := key; imod.lev := lev; imod.export := FALSE;
		IF lev >= modlev THEN
			modlev := lev + 1;
			IF modlev > MaxModLev THEN S.Mark('Module level too high') END
		END
	ELSIF (key[0] = imod.key[0]) & (key[1] = imod.key[1]) THEN
		imod.adr := 0; imod.lev := lev; imod.export := FALSE
	ELSE S.Mark('Was imported with a different key')
	END;
	*)
	
	IF S.errcnt = 0 THEN
		OpenScope; curLev := imod.no; ReadNum(cls);
		WHILE (cls = cModule) & (S.errcnt = 0) DO
			ReadModId(depid); ReadModkey(key); dep := FindMod(depid);
			IF EqlModId(depid, modid) THEN S.Mark('Circular dependency')
			ELSIF dep # NIL THEN
				IF (dep.key[0] # key[0]) OR (dep.key[1] # key[1]) THEN
					msg := 'Module '; ModIdToStr(depid, str);
					Append(str, msg); Append(' was imported by ', msg);
					ModIdToStr(imodid, str); Append(str, msg);
					Append(' with a different key', msg); S.Mark(msg)
				END
			END ;
			ReadNum(cls)
		END ;
		WHILE (cls = cConst) & (S.errcnt = 0) DO
			ReadByteStr(name); ReadNum(val); ImportTypeRef(tp);
			IF S.errcnt = 0 THEN
				IF tp # strType THEN x := NewConst(tp, val)
				ELSE ReadNum(slen); x := NewStr('', slen); x(Str).expno := val
				END ;
				NewImport(name, x)
			END ;
			ReadNum(cls)
		END ;
		WHILE (cls = cType) & (S.errcnt = 0) DO
			ReadByteStr(name); ImportTypeRef(tp);
			IF S.errcnt = 0 THEN x := NewTypeObj(tp); NewImport(name, x) END ;
			ReadNum(cls)
		END ;
		WHILE (cls = cVar) & (S.errcnt = 0) DO
			ReadByteStr(name); ReadNum(val); ImportTypeRef(tp);
			IF S.errcnt = 0 THEN
				x := NewVar(tp); x(Var).ronly := TRUE;
				x(Var).expno := val; NewImport(name, x)
			END ;
			ReadNum(cls)
		END ;
		WHILE (cls = cProc) & (S.errcnt = 0) DO
			ReadByteStr(name); ReadNum(val); ImportTypeRef(tp);
			IF S.errcnt = 0 THEN
				x := NewProc();
				x(Proc).type := tp; tp.obj := x;
				x(Proc).expno := val; NewImport(name, x)
			END ;
			Files.ReadNum(rider, cls)
		END ;
		IF S.errcnt = 0 THEN ASSERT(cls = cNull) END ;
		curLev := 0; imod.import := TRUE;
		imod.first := topScope.first; CloseScope
	END ;
	RETURN imod
END Import;

PROCEDURE NewSystemModule*(modident: Ident);
	VAR sysmod: Module;
BEGIN
	NEW(sysmod);
	sysmod.lev := -1; sysmod.first := systemScope.first;
	modident.obj := sysmod; sysmod.ident := modident; curmod.system := TRUE
END NewSystemModule;

PROCEDURE NewModule0*(ident: Ident; id: ModuleId);
	VAR path, symfname: ARRAY 512 OF CHAR;
		x, i: INTEGER; found: BOOLEAN; newmod: Module;
	
	PROCEDURE GetPath(VAR path: ARRAY OF CHAR; VAR i: INTEGER);
		VAR j: INTEGER;
	BEGIN j := 0;
		WHILE (symPath[i] # 0X) & (symPath[i] # ';') DO
			path[j] := symPath[i]; INC(i); INC(j)
		END ;
		IF symPath[i] = ';' THEN INC(i) END ;
		IF path[j-1] # '\' THEN path[j] := '\'; INC(j) END ;
		path[j] := 0X
	END GetPath;
	
BEGIN (* NewModule0 *)
	newmod := FindMod(id);
	IF (newmod # NIL) & ~newmod.import THEN newmod := NIL END ;
	IF EqlModId(id, modid) THEN S.Mark('Cannot import self')
	ELSIF mod = NIL THEN
		i := 0; ModIdToStr2(id, symfname, i); Insert('.sym', symfname, i);
		symfile := Files.Old(symfname); found := symfile # NIL; i := 0;
		WHILE (symPath[i] # 0X) & ~found DO
			GetPath(path, i);
			IF path # 0X THEN
				Append(symfname, path);
				symfile := Files.Old(path);
				found := symfile # NIL
			END
		END ;
		IF found THEN ident.obj := Import(id)
		ELSE S.Mark('Symbol file not existed')
		END
	END
END NewModule0;

PROCEDURE NewModule*(ident: Ident; name: S.IdStr);
	VAR id: ModuleId;
BEGIN
	id.context := curmod.id.context; id.name := name; NewModule0(ident, id)
END NewModule;

(* Constanst folding *)

PROCEDURE CheckSetElement(VAR x: INTEGER);
BEGIN
	IF (x >= 0) & (x <= 63) THEN (*ok*)
	ELSE x := 0; S.Mark('set element must be >= 0 and <= 63')
	END
END CheckSetElement;

PROCEDURE ConstSingletonSet*(x: Object): Object;
	VAR val: INTEGER;
BEGIN
	val := x(Const).value; CheckSetElement(val);
	x := NewConst(setType, ORD({val}));
	RETURN x
END ConstSingletonSet;

PROCEDURE ConstRangeSet*(x, y: Object): Object;
	VAR beg, end: INTEGER;
BEGIN
	beg := x(Const).value; CheckSetElement(beg);
	end := y(Const).value; CheckSetElement(end);
	x := NewConst(setType, ORD({beg..end}));
	RETURN x
END ConstRangeSet;

PROCEDURE NegateConst*(x0: Object): Const;
	VAR x: Const; t: Type; v: INTEGER;
BEGIN t := x0.type; v := x0(Const).value;
	IF t = byteType THEN t := intType END ;
	IF t = intType THEN v := -v
	ELSIF t = realType THEN v := SYSTEM.VAL(INTEGER, -SYSTEM.VAL(REAL, v))
	ELSIF t = setType THEN v := ORD(-SYSTEM.VAL(SET, v))
	ELSIF t = boolType THEN v := (v + 1) MOD 2
	ELSE ASSERT(FALSE)
	END ;
	x := NewConst(t, v);
	RETURN x
END NegateConst;

PROCEDURE AbsConst*(x: Object): Object;
	VAR t: Type; v: INTEGER; v2: SET;
BEGIN t := x.type; v := x(Const).value;
	IF t = intType THEN
		IF v < 0 THEN v := -v END
	ELSIF t = byteType THEN t := intType
	ELSIF t = realType THEN
		v2 := SYSTEM.VAL(SET, v); EXCL(v2, 63); v := ORD(v2)
	END ;
	x := NewConst(t, v)
	RETURN x
END AbsConst;

PROCEDURE OddConst*(x: Object): Object;
	VAR v: INTEGER;
BEGIN
	v := x(Const).value; x := NewConst(boolType, v MOD 2);
	RETURN x
END OddConst;

PROCEDURE ShiftConst*(fid: INTEGER; x, y: Object): Object;
	VAR xv, yv: INTEGER;
BEGIN xv := x(Const).value; yv := y(Const).value;
	IF fid = opLSL THEN xv := LSL(xv, yv)
	ELSIF fid = opASR THEN xv := ASR(xv, yv)
	ELSIF fid = opROR THEN xv := ROR(xv, yv)
	ELSE ASSERT(FALSE)
	END ;
	x := NewConst(intType, xv);
	RETURN x
END ShiftConst;

PROCEDURE FloorConst*(x: Object): Object;
	VAR v, frac, exp, p: INTEGER; sign: BOOLEAN;
BEGIN
	IF x.type = realType THEN
		v := x(Const).value; frac := v MOD 10000000000000H;
		exp := v DIV 10000000000000H MOD 800H; sign := v < 0;
		IF exp = 0 (* subnormal *) THEN v := 0
		ELSIF exp = 7FFH (* Inf or NaN *) THEN S.Mark('float num too large')
		ELSE DEC(exp, 1023); INC(frac, 10000000000000H); p := 52;
			IF exp < 0 THEN v := 0 ELSIF exp = 0 THEN v := 1
			ELSE
				WHILE (p > 0) & (exp > 0) DO DEC(p); DEC(exp) END ;
				IF exp = 0 THEN v := ASR(frac, p)
				ELSIF exp <= 11 THEN v := LSL(frac, exp)
				ELSE S.Mark('float num too large')
				END
			END ;
			IF sign THEN v := -v END
		END
	END ;
	x := NewConst(intType, v);
	RETURN x
END FloorConst;

PROCEDURE FltConst*(x: Object): Object;
	CONST n52 = 10000000000000H;
	VAR v, exp, r: INTEGER; sign: BOOLEAN;
BEGIN v := x(Const).value;
	IF v = MinInt THEN v := -3C20000000000000H
	ELSIF v # 0 THEN exp := 52; r := 0;
		IF v < 0 THEN v := -v; sign := TRUE ELSE sign := FALSE END ;
		WHILE v < n52 DO v := v * 2; DEC(exp) END ;
		WHILE v >= n52 * 2 DO
			INC(r, LSL(v MOD 2, exp)); v := v DIV 2; INC(exp)
		END ;
		IF (exp > 0) & (r >= LSL(1, exp-1)) THEN INC(v);
			IF v >= n52 * 2 THEN v := v DIV 2; INC(exp) END
		END;
		INC(exp, 1023); v := v MOD n52 + exp * n52;
		IF sign THEN v := ORD(SYSTEM.VAL(SET, v) + {63}) END
	END ;
	x := NewConst(realType, v);
	RETURN x
END FltConst;

PROCEDURE OrdConst*(x: Const): Object;
BEGIN
	x := NewConst(intType, x.value);
	RETURN x
END OrdConst;

PROCEDURE IntToCharConst*(x: Const): Object;
BEGIN
	IF (x.value >= 0) OR (x.value <= 256) THEN (*ok*)
	ELSE S.Mark('char value should be 0 to 256')
	END ;
	x := NewConst(charType, x.value)
	RETURN x
END IntToCharConst;
	
PROCEDURE FoldConst*(op: INTEGER; x, y: Object): Object;
	VAR val, xval, yval, i, k: INTEGER; type: Type;
		r1, r2: REAL; xstr, ystr: Str; ch1, ch2: CHAR;
BEGIN
	IF (op >= S.eql) & (op <= S.in) THEN
		IF (x IS Const) & (y IS Const) & (x.type # realType) THEN
			xval := x(Const).value; yval := y(Const).value;
			IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
			OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
			OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
			OR (op = S.in) & (xval IN SYSTEM.VAL(SET,yval))
			THEN val := true ELSE val := false
			END
		ELSIF (x IS Const) & (y IS Const) & (x.type = realType) THEN
			xval := x(Const).value; yval := y(Const).value;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF (op = S.eql) & (r1 = r2) OR (op = S.neq) & (r1 # r2)
			OR (op = S.gtr) & (r1 > r2) OR (op = S.geq) & (r1 >= r2)
			OR (op = S.lss) & (r1 < r2) OR (op = S.leq) & (r1 <= r2)
			THEN val := true ELSE val := false
			END
		ELSIF (x IS Str) & (y IS Str) THEN
			xstr := x(Str); ystr := y(Str);
			IF (xstr.bufpos >= 0) & (ystr.bufpos >= 0) THEN
				i := xstr.bufpos; k := ystr.bufpos;
				ch1 := curmod.strbuf[i]; ch2 := curmod.strbuf[k];
				WHILE (ch1 = ch2) & (ch1 # 0X) DO
					INC(i); INC(k);
					ch1 := curmod.strbuf[i];
					ch2 := curmod.strbuf[k] 
				END ;
				IF (op = S.eql) & (ch1 = ch2) OR (op = S.neq) & (ch1 # ch2)
				OR (op = S.gtr) & (ch1 > ch2) OR (op = S.geq) & (ch1 >= ch2)
				OR (op = S.lss) & (ch1 < ch2) OR (op = S.leq) & (ch1 <= ch2)
				THEN val := true ELSE val := false
				END 
			END
		END ;
		type := boolType
	ELSIF (x IS Const) & (y IS Const) THEN
		xval := x(Const).value; yval := y(Const).value;
		IF x.type.form = tInt THEN type := intType;
			IF op = S.plus THEN val := xval + yval
			ELSIF op = S.minus THEN val := xval - yval
			ELSIF op = S.times THEN val := xval * yval
			ELSIF (op = S.div) OR (op = S.mod) THEN
				IF yval <= 0 THEN S.Mark('invalid divisor')
				ELSIF op = S.div THEN val := xval DIV yval
				ELSE val := xval MOD yval
				END
			END
		ELSIF x.type = setType THEN type := setType;
			IF op = S.plus THEN
				val := ORD(SYSTEM.VAL(SET, xval) + SYSTEM.VAL(SET, yval))
			ELSIF op = S.minus THEN
				val := ORD(SYSTEM.VAL(SET, xval) - SYSTEM.VAL(SET, yval))
			ELSIF op = S.times THEN
				val := ORD(SYSTEM.VAL(SET, xval) * SYSTEM.VAL(SET, yval))
			ELSIF op = S.rdiv THEN
				val := ORD(SYSTEM.VAL(SET, xval) / SYSTEM.VAL(SET, yval))
			END
		ELSIF x.type = realType THEN type := realType;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF op = S.plus THEN val := SYSTEM.VAL(INTEGER, r1 + r2)
			ELSIF op = S.minus THEN val := SYSTEM.VAL(INTEGER, r1 - r2)
			ELSIF op = S.times THEN val := SYSTEM.VAL(INTEGER, r1 * r2)
			ELSIF op = S.rdiv THEN val := SYSTEM.VAL(INTEGER, r1 / r2)
			END
		ELSIF x.type = boolType THEN type := boolType;
			IF op = S.or THEN
				IF (xval = true) OR (yval = true)
				THEN val := true ELSE val := false
				END
			ELSIF op = S.and THEN
				IF (xval = true) & (yval = true)
				THEN val := true ELSE val := false
				END
			END
		END
	END ;
	x := NewConst(type, val);
	RETURN x
END FoldConst;

(* Init *)

PROCEDURE Init*(modid: ModuleId);
BEGIN
	NEW(curmod); NEW(curmod.universe); curmod.id := modid;
	topScope := curmod.universe; curLev := 0;

	NEW(guard); NEW(externalIdentNotFound)
END Init;
*)

BEGIN
	(*ExportType0 := ExportType; ImportType0 := ImportType*)
END Base.