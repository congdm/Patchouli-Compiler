MODULE Base;
IMPORT
	SYSTEM, Strings, Crypt, Rtl, Out,
	S := Scanner;

CONST
	MaxExt* = 7; MaxRecTypes* = 512;
	MaxImpMod* = 256; MaxExpTypes* = 1024; MaxModLev* = 255;
	
	(* Object class *)
	cNull* = -1; cModule* = 0; cType* = 1;
	cNode* = 2; cVar* = 3; cConst* = 4; cProc* = 5;
	cField* = 6; cSProc* = 7; cSFunc* = 8;
	
	(* Type form *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3; tReal* = 4;
	tPtr* = 5; tProc* = 6; tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;
	
	typScalar* = {tInt, tBool, tSet, tChar, tReal, tPtr, tProc, tNil};
	typEql* = {tBool, tSet, tPtr, tProc, tNil};
	typCmp* = {tInt, tReal, tChar, tStr};
	
	(* SProc id *)
	spINC* = S.spINC; spDEC* = S.spDEC; spINCL* = S.spINCL; spEXCL* = S.spEXCL;
	spNEW* = 4; spASSERT* = 5; spPACK* = 6; spUNPK* = 7;
	sfABS* = 8; sfODD* = 9; sfLEN* = 10;
	sfLSL* = S.sfLSL; sfASR* = S.sfASR; sfROR* = S.sfROR;
	sfFLOOR* = 14; sfFLT* = 15; sfORD* = 16; sfCHR* = 17;
	
	spGET* = 18; spPUT* = 19; spCOPY* = 20;
	sfADR* = 23; sfSIZE* = 24; sfBIT* = 25; sfVAL* = 26;
	
TYPE
	ModuleKey* = ARRAY 2 OF INTEGER;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
    Node* = POINTER TO NodeDesc;
    Ident* = POINTER TO IdentDesc;
	
	ObjDesc* = RECORD class*: INTEGER; type*: Type; ident*: Ident END;
	Const* = POINTER TO RECORD (ObjDesc) val*: INTEGER END;
	Field* = POINTER TO RECORD (ObjDesc) off*: INTEGER END;
	Var* = POINTER TO RECORD (ObjDesc)
		adr*, expno*, lev*: INTEGER; ronly*: BOOLEAN
	END;
	Par* = POINTER TO RECORD (Var) varpar*: BOOLEAN END;
	Str* = POINTER TO RECORD (Var) bufpos*, len*: INTEGER END;
	TempVar* = POINTER TO RECORD (Var) inited*: BOOLEAN END;
	SProc* = POINTER TO RECORD (ObjDesc) id*: INTEGER END;
	
	Proc* = POINTER TO RECORD (ObjDesc)
		(* Generator independent fields*)
		expno*, lev*, nptr*: INTEGER;
		decl*: Ident; statseq*: Node; return*: Object;
		(* Generator dependent fields *)
		adr*, locblksize*: INTEGER; usedReg*, usedXReg*: SET;
		homeSpace*, stack*, fix*, lim*: INTEGER
	END;
	
	ObjList* = POINTER TO RECORD obj*: Object; next*: ObjList END;
	TypeList* = POINTER TO RECORD type*: Type; next*: TypeList END;
	ProcList* = POINTER TO RECORD obj*: Proc; next*: ProcList END;
	StrList* = POINTER TO RECORD obj*: Str; next*: StrList END;
	
	Module* = POINTER TO RECORD (ObjDesc)
		export*: BOOLEAN; path*: ARRAY 256 OF CHAR; name*: S.IdStr; 
		key*: ModuleKey; lev*, adr*, no*: INTEGER; next*: Module;
		first*, impList*: Ident; types*: TypeList
	END;
	
	NodeDesc* = RECORD (ObjDesc)
		(* Generator independent fields*)
		ronly*: BOOLEAN; op*, sPos*: INTEGER; left*, right*: Object;
		(* Generator dependent fields *)
		link*: Node; jmpSz*, jmpPc*: INTEGER; xRegUsed*, regUsed*: SET
	END;
	
	IdentDesc* = RECORD
		export*: BOOLEAN; name*: S.IdStr; obj*: Object; next*: Ident
	END;
	Scope* = POINTER TO RECORD first*: Ident; dsc*: Scope END;		
	
	TypeDesc* = RECORD
		notag*, mark, predef: BOOLEAN; form*: BYTE;
		len*, size*, align*, nptr*, parblksize*, nfpar*: INTEGER;
		base*: Type; fields*: Ident; obj*: Object;
		adr*, expno*, ref*: INTEGER; mod*: Module
	END;

VAR
	topScope*, universe*, systemScope: Scope;
	curLev*, modlev*: INTEGER; system*: BOOLEAN;
	modid*: S.IdStr; modkey*: ModuleKey;
	expList*, lastExp: ObjList; strList*: StrList; recList*: TypeList;
	
	(* Predefined Types *)
	intType*, byteType*, realType*: Type;
	card16Type*, card32Type*: Type;
	boolType*, setType*, charType*, nilType*, strType*: Type;
	noType*: Type; predefTypes: TypeList;
	
	Flag*: RECORD
		main*, console*, debug*, handle*, rtl*: BOOLEAN
	END;
	
	symfile: Rtl.File; imod, modList*: Module;
	refno, preTypeNo, expno*, modno*: INTEGER;
	
	strbufSize*: INTEGER;
	strbuf*: ARRAY 100000H OF CHAR;
	symPath, srcPath, sym: ARRAY 1024 OF CHAR;
	
	ExportType0: PROCEDURE(typ: Type);
	ImportType0: PROCEDURE(VAR typ: Type; mod: Module);

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE IsOpenArray*(tp: Type): BOOLEAN;
	RETURN (tp.form = tArray) & (tp.len < 0)
END IsOpenArray;

PROCEDURE IsNormalArray*(tp: Type): BOOLEAN;
	RETURN (tp.form = tArray) & (tp.len >= 0)
END IsNormalArray;

PROCEDURE IsStr*(t: Type): BOOLEAN;
	RETURN (t = strType) OR (t.form = tArray) & (t.base.form = tChar)
END IsStr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Symbol table *)

PROCEDURE NewVar*(tp: Type): Var;
	VAR v: Var;
BEGIN
	NEW(v); v.class := cVar; v.ronly := FALSE;
	v.type := tp; v.lev := curLev;
	RETURN v
END NewVar;

PROCEDURE NewConst*(tp: Type; val: INTEGER): Const;
	VAR c: Const;
BEGIN
	NEW(c); c.class := cConst;
	c.type := tp; c.val := val;
	RETURN c
END NewConst;

PROCEDURE NewPar*(VAR proc: TypeDesc; tp: Type; varpar: BOOLEAN): Par;
	VAR p: Par;
BEGIN
	NEW(p); p.class := cVar;
	p.type := tp; p.lev := curLev;
	p.varpar := varpar; INC(proc.nfpar);
	p.ronly := ~varpar & (tp.form IN {tArray, tRec});
	RETURN p
END NewPar;

PROCEDURE NewField*(VAR rec: TypeDesc; tp: Type): Field;
	VAR fld: Field;
BEGIN
	NEW(fld); fld.class := cField;
	fld.type := tp; rec.nptr := rec.nptr + tp.nptr;
	RETURN fld
END NewField;

PROCEDURE NewStr*(str: ARRAY OF CHAR; slen: INTEGER): Str;
	VAR x: Str; i: INTEGER; p: StrList;
BEGIN
	NEW(x); x.class := cVar; x.ronly := TRUE;
	x.type := strType; x.lev := curLev; x.len := slen;
	IF x.lev >= -1 (* need to alloc buffer *) THEN 
		IF strbufSize + slen >= LEN(strbuf) THEN
			S.Mark('too many strings'); x.bufpos := -1
		ELSE x.bufpos := strbufSize; strbufSize := strbufSize + slen;
			FOR i := 0 TO slen-1 DO strbuf[x.bufpos+i] := str[i] END;
			NEW(p); p.obj := x; p.next := strList; strList := p
		END
	ELSE x.bufpos := -1
	END;
	RETURN x
END NewStr;

PROCEDURE NewStr2*(str: ARRAY OF CHAR): Str;
	VAR slen: INTEGER;
BEGIN slen := 0; WHILE str[slen] # 0X DO INC(slen) END; INC(slen);
	RETURN NewStr(str, slen)
END NewStr2;

PROCEDURE NewProc*(): Proc;
	VAR p: Proc;
BEGIN
	NEW(p); p.class := cProc; p.lev := curLev;
	RETURN p
END NewProc;

PROCEDURE NewTypeObj*(tp: Type): Object;
	VAR x: Object;
BEGIN
	NEW(x); x.class := cType;
	x.type := tp; IF tp.obj = NIL THEN tp.obj := x END;
	RETURN x
END NewTypeObj;

PROCEDURE NewSProc*(id, cls: INTEGER): SProc;
	VAR x: SProc;
BEGIN
	NEW(x); x.class := cls;
	x.id := id; x.type := noType;
	RETURN x
END NewSProc;

PROCEDURE NewTempVar*(tp: Type): TempVar;
	VAR t: TempVar;
BEGIN
	NEW(t); t.class := cVar; t.ronly := FALSE;
	t.type := tp; t.lev := curLev; t.inited := FALSE;
	RETURN t
END NewTempVar;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewType*(VAR typ: Type; form: INTEGER);
BEGIN
	NEW(typ); typ.form := form; typ.nptr := 0; typ.ref := -1;
	typ.size := 0; typ.align := 0; typ.notag := FALSE
END NewType;

PROCEDURE NewArray*(len: INTEGER): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tArray); tp.len := len;
	RETURN tp
END NewArray;

PROCEDURE CompleteArray*(VAR tp: TypeDesc);
BEGIN
	IF tp.base.form = tArray THEN CompleteArray(tp.base^) END;
	tp.nptr := tp.len * tp.base.nptr
END CompleteArray;

PROCEDURE NewRecord*(): Type;
	VAR tp: Type; p: TypeList;
BEGIN
	NewType(tp, tRec); tp.len := 0;
	IF curLev >= 0 THEN
		NEW(p); p.type := tp; p.next := recList; recList := p
	ELSIF curLev = -1 THEN ASSERT(FALSE)
	END;
	RETURN tp
END NewRecord;

PROCEDURE ExtendRecord*(VAR recType: TypeDesc);
BEGIN
	IF recType.base # NIL THEN
		recType.len := recType.base.len + 1;
		recType.nptr := recType.base.nptr
	END
END ExtendRecord;

PROCEDURE NewPointer*(): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tPtr); tp.nptr := 1;
	RETURN tp
END NewPointer;

PROCEDURE NewProcType*(): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tProc); tp.nfpar := 0;
	RETURN tp
END NewProcType;

PROCEDURE NewPredefinedType(VAR typ: Type; form: INTEGER);
	VAR p: TypeList;
BEGIN
	NewType(typ, form); INC(preTypeNo);
	typ.predef := TRUE; typ.ref := preTypeNo;
	NEW(p); p.type := typ; p.next := predefTypes; predefTypes := p
END NewPredefinedType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*;
	VAR scp: Scope;
BEGIN NEW(scp); scp.dsc := topScope; topScope := scp
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;

PROCEDURE IncLev*(n: INTEGER);
BEGIN curLev := curLev + n
END IncLev;

PROCEDURE Enter(x: Object; name: S.IdStr);
	VAR ident: Ident;
BEGIN
	NEW(ident); ident.name := name; ident.export := FALSE;
	ident.obj := x; x.ident := ident;
	ident.next := topScope.first; topScope.first := ident
END Enter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Read/Write for symfile *)

PROCEDURE WriteInt(f: Rtl.File; n: INTEGER);
	VAR finish: BOOLEAN; b: INTEGER;
BEGIN
	REPEAT b := n MOD 128; finish := (n >= -64) & (n < 64);
		IF finish THEN b := b + 128 ELSE n := n DIV 128 END;
		Rtl.Write1(f, b)
	UNTIL finish
END WriteInt;

PROCEDURE ReadInt(f: Rtl.File; VAR n: INTEGER);
	CONST MaxInt = 9223372036854775807; MinInt = -MaxInt - 1;
	VAR finish: BOOLEAN; i, b, k: INTEGER;
BEGIN n := 0; i := 1; k := 1;
	REPEAT Rtl.Read1(f, b);
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

PROCEDURE WriteBool(f: Rtl.File; b: BOOLEAN);
BEGIN
	IF b THEN WriteInt(f, 1) ELSE WriteInt(f, 0) END
END WriteBool;

PROCEDURE ReadBool(f: Rtl.File; VAR b: BOOLEAN);
	VAR n: INTEGER;
BEGIN
	ReadInt(f, n); b := n = 1
END ReadBool;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Export symbol file *)

PROCEDURE NewExport(VAR exp: ObjList);
BEGIN
	NEW(exp); INC(expno);
	IF lastExp = NIL THEN expList := exp; lastExp := exp
	ELSE lastExp.next := exp; lastExp := exp
	END
END NewExport;

PROCEDURE DetectType(typ: Type);
BEGIN
	IF typ # NIL THEN
		IF typ.predef THEN
			WriteInt(symfile, 1); WriteInt(symfile, typ.ref)
		ELSIF typ.mod = NIL THEN
			WriteInt(symfile, 2); WriteInt(symfile, typ.ref);
			IF typ.ref < 0 THEN ExportType0(typ) END
		ELSE WriteInt(symfile, 3);
			Rtl.WriteStr(symfile, typ.mod.name); WriteInt(symfile, typ.ref);
			IF typ.ref < 0 THEN ExportType0(typ) END
		END
	ELSE WriteInt(symfile, 0)
	END
END DetectType;

PROCEDURE ExportProc(typ: Type);
	VAR par: Ident; x: Par;
BEGIN
	WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
	WriteInt(symfile, typ.parblksize); DetectType(typ.base);
	par := typ.fields;
	WHILE par # NIL DO x := par.obj(Par);
		WriteInt(symfile, x.class);
		Rtl.WriteStr(symfile, par.name);
		WriteBool(symfile, x.varpar);
		DetectType(x.type); par := par.next
	END;
	WriteInt(symfile, cType)
END ExportProc;
	
PROCEDURE ExportType(typ: Type);
	VAR fld: Ident; ftyp: Type; exp: ObjList;
BEGIN
	IF typ.mod = NIL THEN
		IF refno < MaxExpTypes THEN
			INC(refno); typ.ref := refno
		ELSE S.Mark('Too many exported types')
		END
	ELSE typ.ref := -typ.ref
	END;
	WriteInt(symfile, typ.ref); WriteInt(symfile, typ.form);
	IF typ.form = tRec THEN
		NewExport(exp); NEW(exp.obj); exp.obj.class := cType;
		exp.obj.type := typ; WriteInt(symfile, expno);
		WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
		DetectType(typ.base); fld := typ.fields;
		WHILE fld # NIL DO ftyp := fld.obj.type;
			IF fld.export OR (ftyp.nptr > 0) THEN
				WriteInt(symfile, cField);
				IF fld.export THEN Rtl.WriteStr(symfile, fld.name)
				ELSE Rtl.WriteStr(symfile, 0X)
				END;
				WriteInt(symfile, fld.obj(Field).off); DetectType(ftyp)
			END;
			fld := fld.next
		END;
		WriteInt(symfile, cType)
	ELSIF typ.form = tArray THEN
		WriteInt(symfile, typ.len); WriteBool(symfile, typ.notag);
		WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
		DetectType(typ.base)
	ELSIF typ.form = tPtr THEN
		WriteInt(symfile, typ.size);
		WriteInt(symfile, typ.align);
		DetectType(typ.base)
	ELSIF typ.form = tProc THEN
		ExportProc(typ)
	END
END ExportType;

PROCEDURE WriteModkey(key: ModuleKey);
BEGIN
	Rtl.Write8(symfile, key[0]);
	Rtl.Write8(symfile, key[1])
END WriteModkey;

PROCEDURE ScanExportTypes(first: Ident; form: INTEGER);
	VAR ident: Ident; type: Type;
	PROCEDURE ScanType(type: Type);
	BEGIN type.mark := TRUE;
		IF type.mod # NIL THEN type.mod.export := TRUE END;
		IF (type.base # NIL) & ~type.base.mark THEN ScanType(type.base) END;
		IF type.fields # NIL THEN ScanExportTypes(type.fields, type.form) END
	END ScanType;
BEGIN ident := first;
	WHILE ident # NIL DO type := ident.obj.type;
		IF (form = tProc) OR ident.export
		OR (form = tRec) & (type.nptr > 0) THEN 
			IF (type # NIL) & ~type.mark THEN ScanType(type) END
		END;
		ident := ident.next
	END
END ScanExportTypes;

PROCEDURE WriteSymfile*;
	VAR ident: Ident; exp: ObjList; i, k, n, size: INTEGER;
		hash: Crypt.MD5Hash; chunk: ARRAY 64 OF BYTE;
		symfname: ARRAY 512 OF CHAR; x: Object;
BEGIN
	refno := 0; expno := 0;
	ScanExportTypes(universe.first, -1);
	Rtl.Rewrite(symfile, '.tempSymfile');
	Rtl.Seek(symfile, 16); WriteInt(symfile, modlev);
	
	imod := modList;
	WHILE imod # NIL DO
		WriteInt(symfile, cModule); Rtl.WriteStr(symfile, imod.name);
		WriteModkey(imod.key); WriteInt(symfile, imod.lev);
		WriteBool(symfile, imod.export); imod := imod.next
	END;
	
	ident := universe.first;
	WHILE ident # NIL DO
		IF ident.export THEN x := ident.obj;
			IF x.class = cConst THEN
				WriteInt(symfile, cConst);
				Rtl.WriteStr(symfile, ident.name);
				WriteInt(symfile, x(Const).val);
				DetectType(x.type)
			ELSIF x.class = cType THEN
				WriteInt(symfile, cType);
				Rtl.WriteStr(symfile, ident.name);
				DetectType(x.type)
			ELSIF x.class = cVar THEN
				IF x IS Str THEN
					WriteInt(symfile, cConst);
					Rtl.WriteStr(symfile, ident.name);
					NewExport(exp); exp.obj := x;
					WriteInt(symfile, expno); DetectType(x.type);
					WriteInt(symfile, x(Str).len)
				ELSE
					WriteInt(symfile, cVar);
					Rtl.WriteStr(symfile, ident.name);
					NewExport(exp); exp.obj := x;
					WriteInt(symfile, expno); DetectType(x.type)
				END 
			ELSIF x.class = cProc THEN
				WriteInt(symfile, cProc);
				Rtl.WriteStr(symfile, ident.name);
				NewExport(exp); exp.obj := x;
				WriteInt(symfile, expno); DetectType(x.type)
			ELSE ASSERT(FALSE)
			END
		END;
		ident := ident.next
	END;
	WriteInt(symfile, cNull);
	
	size := Rtl.Pos(symfile); Rtl.Seek(symfile, 0);
	Crypt.InitMD5Hash(hash); i := 0;
	REPEAT k := 0;
		REPEAT Rtl.Read1(symfile, n); chunk[k] := n; INC(i); INC(k)
		UNTIL (i = size) OR (k = 64);
		Crypt.MD5ComputeChunk(hash, SYSTEM.ADR(chunk), k)
	UNTIL i = size;
	
	Rtl.Seek(symfile, 0);
	modkey[0] := Crypt.MD5GetLowResult(hash);
	modkey[1] := Crypt.MD5GetHighResult(hash);
	WriteModkey(modkey);
	Rtl.Close(symfile);
	
	IF S.errcnt = 0 THEN symfname := 0X;
		Strings.Append(srcPath, symfname);
		Strings.Append(modid, symfname);
		Strings.Append('.sym', symfname);
		Rtl.Delete(symfname); Rtl.Rename('.tempSymfile', symfname)
	ELSE Rtl.Delete('.tempSymfile')
	END
END WriteSymfile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import symbol file *)

PROCEDURE ModByLev(lev: INTEGER): Module;
	VAR mod: Module;
BEGIN mod := modList;
	WHILE (mod # NIL) & (mod.no # lev) DO
		mod := mod.next
	END;
	RETURN mod
END ModByLev;

PROCEDURE FindMod*(name: S.IdStr): Module;
	VAR i: INTEGER; mod: Module;
BEGIN mod := modList;
	WHILE (mod # NIL) & (mod.name # name) DO
		mod := mod.next
	END;
	RETURN mod
END FindMod;

PROCEDURE FindType(ref: INTEGER; types: TypeList): Type;
	VAR p: TypeList; typ: Type;
BEGIN p := types;
	WHILE (p # NIL) & (p.type.ref # ref) DO p := p.next END;
	IF p # NIL THEN typ := p.type END;
	RETURN typ
END FindType;

PROCEDURE NewImport(name: S.IdStr; x: Object);
	VAR ident, p: Ident;
BEGIN
	NEW(ident); p := topScope.first;
	IF p = NIL THEN topScope.first := ident
	ELSE WHILE (p.next # NIL) DO p := p.next END; p.next := ident
	END;
	ident.export := FALSE; ident.obj := x; ident.name := name;
	IF x.ident = NIL THEN x.ident := ident END
END NewImport;

PROCEDURE DetectTypeI(VAR typ: Type);
	VAR n, ref: INTEGER; name: S.IdStr; mod: Module;
BEGIN
	ReadInt(symfile, n);
	IF n = 0 THEN typ := NIL
	ELSIF (n = 1) OR (n = 2) THEN ReadInt(symfile, ref);
		IF ref > 0 THEN
			IF n = 1 THEN typ := FindType(ref, predefTypes)
			ELSE typ := FindType(ref, imod.types)
			END
		ELSE ImportType0(typ, imod)
		END
	ELSIF n = 3 THEN
		Rtl.ReadStr(symfile, name);
		ReadInt(symfile, ref); mod := FindMod(name);
		IF ref > 0 THEN typ := FindType(ref, mod.types)
		ELSE ImportType0(typ, mod)
		END
	ELSE ASSERT(FALSE)
	END
END DetectTypeI;

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
		ReadInt(symfile, typ.expno); typ.adr := 0;
		ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
		DetectTypeI(typ.base); ExtendRecord(typ);
		ReadInt(symfile, cls); OpenScope;
		WHILE cls # cType DO
			Rtl.ReadStr(symfile, name); ReadInt(symfile, off);
			DetectTypeI(fltype); ReadInt(symfile, cls);
			IF new THEN
				x := NewField(typ, fltype); x.off := off; NewImport(name, x)
			END
		END;
		typ.fields := topScope.first; CloseScope
	END ImportRecord;
	
	PROCEDURE ImportArray(VAR typ: TypeDesc);
	BEGIN
		ReadBool(symfile, typ.notag);
		ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
		DetectTypeI(typ.base); CompleteArray(typ)
	END ImportArray;
	
	PROCEDURE ImportPointer(VAR typ: TypeDesc);
	BEGIN
		ReadInt(symfile, typ.size);
		ReadInt(symfile, typ.align);
		DetectTypeI(typ.base)
	END ImportPointer;
	
	PROCEDURE ImportProc(VAR typ: TypeDesc; new: BOOLEAN);
		VAR cls: INTEGER; varpar: BOOLEAN;
			par: Ident; x: Par; xtype: Type; name: S.IdStr;
	BEGIN
		ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
		ReadInt(symfile, typ.parblksize); DetectTypeI(typ.base);
		ReadInt(symfile, cls); OpenScope;
		WHILE cls # cType DO
			Rtl.ReadStr(symfile, name); ReadBool(symfile, varpar);
			DetectTypeI(xtype); ReadInt(symfile, cls);
			IF new THEN
				x := NewPar(typ, xtype, varpar); NewImport(name, x)
			END
		END;
		typ.fields := topScope.first; CloseScope
	END ImportProc;
		
BEGIN (* ImportType *)
	ReadInt(symfile, ref); ReadInt(symfile, form);
	typ := FindType(ref, mod.types);
	IF form = tRec THEN
		IF typ = NIL THEN
			typ := NewRecord(); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportRecord(typ^, TRUE)
		ELSE ImportRecord(typ0, FALSE)
		END
	ELSIF form = tArray THEN
		ReadInt(symfile, typ0.len);
		IF typ = NIL THEN
			typ := NewArray(typ0.len); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportArray(typ^)
		ELSE ImportArray(typ0)
		END
	ELSIF form = tPtr THEN
		IF typ = NIL THEN
			typ := NewPointer(); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportPointer(typ^)
		ELSE ImportPointer(typ0)
		END
	ELSIF form = tProc THEN
		IF typ = NIL THEN
			typ := NewProcType(); typ.ref := ref;
			AddToTypeList(typ, mod); typ.mod := mod;
			ImportProc(typ^, TRUE)
		ELSE ImportProc(typ0, FALSE)
		END
	END
END ImportType;

PROCEDURE ReadModkey(VAR key: ModuleKey);
BEGIN
	Rtl.Read8(symfile, key[0]);
	Rtl.Read8(symfile, key[1])
END ReadModkey;

PROCEDURE Import(sympath: ARRAY OF CHAR; imodid: S.IdStr): Module;
	VAR dep: Module; key: ModuleKey; reExp, good: BOOLEAN;
		lev, val, cls, slen: INTEGER; name: S.IdStr;
		msg: ARRAY 512 OF CHAR; tp: Type; x: Object;
BEGIN
	Rtl.Reset(symfile, sympath); imod := modList;
	ReadModkey(key); ReadInt(symfile, lev); good := TRUE;
	WHILE (imod # NIL) & (imod.name # imodid) DO imod := imod.next END;
	IF imod = NIL THEN
		NEW(imod); imod.name := imodid; imod.adr := 0;
		imod.next := modList; modList := imod; imod.no := modno;
		DEC(modno); imod.key := key; imod.lev := lev; imod.export := FALSE;
		IF lev >= modlev THEN
			modlev := lev + 1;
			IF modlev > MaxModLev THEN S.Mark('Module level too high') END
		END
	ELSIF (key[0] = imod.key[0]) & (key[1] = imod.key[1]) THEN (* valid *)
	ELSE S.Mark('Was imported with a different key'); good := FALSE
	END;
	
	IF good THEN
		OpenScope; curLev := imod.no; ReadInt(symfile, cls);
		WHILE cls = cModule DO
			Rtl.ReadStr(symfile, name); ReadModkey(key); dep := FindMod(name);
			ReadInt(symfile, lev); ReadBool(symfile, reExp);
			IF name = modid THEN S.Mark('Circular dependency')
			ELSIF dep # NIL THEN
				IF (dep.key[0] # key[0]) OR (dep.key[1] # key[1])
				THEN msg := 'Module '; Strings.Append(name, msg);
					Strings.Append(' was imported by ', msg);
					Strings.Append(imodid, msg);
					Strings.Append(' with a different key', msg);
					S.Mark(msg)
				END
			ELSIF reExp THEN
				NEW(dep); dep.name := name; dep.adr := 0;
				dep.next := modList; modList := dep; dep.no := modno;
				DEC(modno); dep.key := key; dep.lev := lev
			END;
			ReadInt(symfile, cls)
		END;
		WHILE cls = cConst DO
			Rtl.ReadStr(symfile, name);
			ReadInt(symfile, val); DetectTypeI(tp);
			IF tp # strType THEN x := NewConst(tp, val)
			ELSE ReadInt(symfile, slen); x := NewStr('', slen);
				x(Str).adr := 0; x(Str).expno := val
			END;
			NewImport(name, x); ReadInt(symfile, cls)
		END;
		WHILE cls = cType DO
			Rtl.ReadStr(symfile, name);
			DetectTypeI(tp); x := NewTypeObj(tp);
			NewImport(name, x); ReadInt(symfile, cls)
		END;
		WHILE cls = cVar DO
			Rtl.ReadStr(symfile, name);
			ReadInt(symfile, val); DetectTypeI(tp);
			x := NewVar(tp); x(Var).ronly := TRUE;
			x(Var).expno := val; x(Var).adr := 0;
			NewImport(name, x); ReadInt(symfile, cls)
		END;
		WHILE cls = cProc DO
			Rtl.ReadStr(symfile, name);
			x := NewProc(); ReadInt(symfile, x(Proc).expno);
			x(Proc).adr := 0; DetectTypeI(x.type);
			NewImport(name, x); ReadInt(symfile, cls)
		END;
		ASSERT(cls = cNull); curLev := 0;
		imod.first := topScope.first; CloseScope
	END;
	Rtl.Close(symfile);
	RETURN imod
END Import;

PROCEDURE NewSystemModule*(modident: Ident);
	VAR mod: Module;
BEGIN
	NEW(mod); mod.name := 'SYSTEM'; system := TRUE;
	mod.lev := -1; mod.first := systemScope.first;
	modident.obj := mod; mod.ident := modident
END NewSystemModule;

PROCEDURE NewModule*(ident: Ident; name: S.IdStr);
	VAR path, symfname: ARRAY 512 OF CHAR;
		x, i, len: INTEGER; found: BOOLEAN;
	
	PROCEDURE GetPath(VAR path: ARRAY OF CHAR; VAR i: INTEGER): INTEGER;
		VAR j: INTEGER;
	BEGIN j := 0;
		WHILE (symPath[i] # 0X) & (symPath[i] # ';') DO
			path[j] := symPath[i]; INC(i); INC(j)
		END;
		IF symPath[i] = ';' THEN INC(i) END; path[j] := 0X;
		RETURN j
	END GetPath;
	
BEGIN (* NewModule *)
	symfname := name; Strings.Append('.sym', symfname);
	path := symfname; i := 0; found := Rtl.ExistFile(path);
	WHILE (symPath[i] # 0X) & ~found DO
		len := GetPath(path, i);
		IF len # 0 THEN
			IF path[len-1] # '\' THEN
				path[len] := '\'; path[len+1] := 0X
			END;
			Strings.Append(symfname, path); found := Rtl.ExistFile(path)
		END
	END;
	IF found THEN ident.obj := Import(path, name)
	ELSE S.Mark('Symbol file not existed')
	END;
END NewModule;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Compiler Flag *)

PROCEDURE SetCompilerFlag(pragma: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	IF pragma = 'MAIN' THEN Flag.main := TRUE
	ELSIF pragma = 'CONSOLE' THEN
		Flag.main := TRUE; Flag.console := TRUE
	ELSIF pragma = 'DEBUG' THEN Flag.debug := TRUE
	(*ELSIF Strings.Pos('RTL ', pragma, 0) = 0 THEN i := 0;
		WHILE pragma[i+4] # 0X DO Flag.rtl[i] := pragma[i+4]; INC(i) END*)
	ELSIF pragma = 'HANDLE' THEN Flag.handle := TRUE
	ELSIF pragma = 'POINTER' THEN Flag.handle := FALSE
	ELSIF pragma = 'RTL-' THEN Flag.rtl := FALSE
	END
END SetCompilerFlag;

PROCEDURE SetFlag*(flag: ARRAY OF CHAR);
BEGIN
	IF flag = 'handle' THEN Flag.handle := TRUE END
END SetFlag;

PROCEDURE InitCompilerFlag;
BEGIN
	Flag.main := FALSE; Flag.console := FALSE; Flag.debug := FALSE;
	Flag.handle := FALSE; Flag.rtl := TRUE
END InitCompilerFlag;

PROCEDURE SetSymPath*(path: ARRAY OF CHAR);
BEGIN
	symPath := path
END SetSymPath;

PROCEDURE SetSrcPath*(path: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	srcPath := path; i := 0; WHILE srcPath[i] # 0X DO INC(i) END;
	WHILE (i >= 0) & (srcPath[i] # '\') DO DEC(i) END; srcPath[i+1] := 0X
END SetSrcPath;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init*(modname: S.IdStr);
BEGIN
	NEW(universe); topScope := universe; curLev := -1;
	system := FALSE; modid := modname; modno := -2; strbufSize := 0;
	expList := NIL; lastExp := NIL; strList := NIL; recList := NIL;
	InitCompilerFlag;
	
	Enter(NewTypeObj(intType), 'INTEGER');
	Enter(NewTypeObj(byteType), 'BYTE');
	Enter(NewTypeObj(realType), 'REAL');
	Enter(NewTypeObj(setType), 'SET');
	Enter(NewTypeObj(boolType), 'BOOLEAN');
	Enter(NewTypeObj(charType), 'CHAR');
	
	Enter(NewSProc(spINC, cSProc), 'INC');
	Enter(NewSProc(spDEC, cSProc), 'DEC');
	Enter(NewSProc(spINCL, cSProc), 'INCL');
	Enter(NewSProc(spEXCL, cSProc), 'EXCL');
	Enter(NewSProc(spNEW, cSProc), 'NEW');
	Enter(NewSProc(spASSERT, cSProc), 'ASSERT');
	Enter(NewSProc(spPACK, cSProc), 'PACK');
	Enter(NewSProc(spUNPK, cSProc), 'UNPK');
	
	Enter(NewSProc(sfABS, cSFunc), 'ABS');
	Enter(NewSProc(sfODD, cSFunc), 'ODD');
	Enter(NewSProc(sfLEN, cSFunc), 'LEN');
	Enter(NewSProc(sfLSL, cSFunc), 'LSL');
	Enter(NewSProc(sfASR, cSFunc), 'ASR');
	Enter(NewSProc(sfROR, cSFunc), 'ROR');
	Enter(NewSProc(sfFLOOR, cSFunc), 'FLOOR');
	Enter(NewSProc(sfFLT, cSFunc), 'FLT');
	Enter(NewSProc(sfORD, cSFunc), 'ORD');
	Enter(NewSProc(sfCHR, cSFunc), 'CHR');
	
	OpenScope;
	Enter(NewSProc(spGET, cSProc), 'GET');
	Enter(NewSProc(spPUT, cSProc), 'PUT');
	Enter(NewSProc(spCOPY, cSProc), 'COPY');
	Enter(NewSProc(S.spLoadLibraryW, cSProc), 'LoadLibraryW');
	Enter(NewSProc(S.spGetProcAddress, cSProc), 'GetProcAddress');
	Enter(NewSProc(S.spINT3, cSProc), 'INT3');
	
	Enter(NewSProc(sfADR, cSFunc), 'ADR');
	Enter(NewSProc(sfSIZE, cSFunc), 'SIZE');
	Enter(NewSProc(sfBIT, cSFunc), 'BIT');
	Enter(NewSProc(sfVAL, cSFunc), 'VAL');
	Enter(NewSProc(S.sfNtCurrentTeb, cSFunc), 'NtCurrentTeb');
	
	Enter(NewTypeObj(byteType), 'BYTE');
	Enter(NewTypeObj(card16Type), 'CARD16');
	Enter(NewTypeObj(card32Type), 'CARD32');
	systemScope := topScope; CloseScope;
	
	curLev := 0
END Init;

PROCEDURE Cleanup*;
	VAR i: INTEGER;
BEGIN
	universe := NIL; topScope := NIL; systemScope := NIL; modList := NIL;
	expList := NIL; lastExp := NIL; strList := NIL; recList := NIL
END Cleanup;

BEGIN
	ExportType0 := ExportType; ImportType0 := ImportType;
	S.InstallSetCompilerFlag(SetCompilerFlag);

	preTypeNo := 0; curLev := -1;
	NewPredefinedType(intType, tInt);
	NewPredefinedType(byteType, tInt);
	NewPredefinedType(boolType, tBool);
	NewPredefinedType(setType, tSet);
	NewPredefinedType(charType, tChar);
	NewPredefinedType(nilType, tNil);
	NewPredefinedType(realType, tReal);
	NewPredefinedType(strType, tStr);
	NewPredefinedType(noType, tPtr); noType.base := intType;
	NewPredefinedType(card16Type, tInt);
	NewPredefinedType(card32Type, tInt)
END Base.