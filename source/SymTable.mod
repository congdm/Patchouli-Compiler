MODULE SymTable;

IMPORT
	SYSTEM, Strings, Console, Sys, Base, Scanner;

TYPE
	ModuleKey* = ARRAY 16 OF BYTE;
	
	TypeArray* = POINTER TO RECORD
		a: ARRAY Base.MaxExportTypes OF Base.Type
	END;
	
	Module* = POINTER TO ModuleDesc;
	ModuleArray* = POINTER TO RECORD
		len*: INTEGER;
		a*: ARRAY Base.MaxModules OF Module
	END;
	ModuleDesc* = RECORD
		lev*, modno*: INTEGER;
		name*: Base.IdentStr; key*: ModuleKey;
		dsc*: Base.Object; types*: TypeArray;
		importModules*: ModuleArray;
		next*: Module
	END;
	
VAR
	topScope*, procScope*, universe*: Base.Object;
	curLev*: INTEGER;
	
	refno, expno*, modno: INTEGER;
	importSystem*: BOOLEAN;
	
	moduleList*, imod: Module;
	module*: ModuleDesc;
	symfile: Sys.FileHandle;
	
	(* Forward decl procedure *)
	_Export_type : PROCEDURE (typ: Base.Type);
	_Import_type : PROCEDURE (
		VAR typ: Base.Type; typmod: INTEGER; modname: Base.IdentStr
	);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope* (name: Base.IdentStr);
	VAR scope: Base.Object;
BEGIN
	NEW (scope); scope.class := Base.cHead; scope.name := name;
	scope.dsc := topScope; scope.next := Base.guard; topScope := scope;
	IF name[0] # 0X THEN procScope := scope END
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc;
	IF topScope.name[0] # 0X THEN procScope := topScope END
END CloseScope;
	
PROCEDURE IncLevel* (x: INTEGER);
BEGIN curLev := curLev + x
END IncLevel;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE FindInScope (
	VAR obj: Base.Object; name: Base.IdentStr; scope: Base.Object
);
BEGIN Base.StrCopy(name, Base.guard.name); obj := scope.next;
	WHILE obj.name # name DO obj := obj.next END
END FindInScope;

PROCEDURE Find* (VAR obj: Base.Object; name: Base.IdentStr);
BEGIN
	IF procScope = universe THEN FindInScope (obj, name, universe)
	ELSIF procScope.name = name THEN FindInScope (obj, name, topScope.dsc)
	ELSE FindInScope (obj, name, procScope);
		IF obj = Base.guard THEN FindInScope (obj, name, universe) END
	END
END Find;
	
PROCEDURE FindField* (
	VAR obj: Base.Object; name: Base.IdentStr; record: Base.Type
);
BEGIN Base.StrCopy(name, Base.guard.name); obj := record.fields;
	WHILE obj.name # name DO obj := obj.next END
END FindField;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE New_module (
	VAR mod: Module; name: Base.IdentStr; key: ModuleKey; lev: INTEGER
);
BEGIN NEW (mod); Base.StrCopy (name, mod.name);
	mod.key := key; mod.lev := lev; mod.modno := -1; mod.dsc := Base.guard;
	mod.next := moduleList; moduleList := mod
END New_module;

PROCEDURE Find_module (VAR mod: Module; name: Base.IdentStr);
BEGIN mod := moduleList;
	WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END
END Find_module;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE New* (VAR obj: Base.Object; name: Base.IdentStr; class: INTEGER);
BEGIN
	Base.StrCopy(name, Base.guard.name); obj := topScope;
	IF name[0] # 0X THEN
		WHILE obj.next.name # name DO obj := obj.next END
	ELSE
		WHILE obj.next # Base.guard DO obj := obj.next END
	END;
	
	IF obj.next = Base.guard THEN
		NEW (obj.next); obj := obj.next;
		obj.export := FALSE; obj.readonly := FALSE; obj.param := FALSE;
		Base.StrCopy(name, obj.name); obj.class := class; obj.lev := curLev;
		obj.next := Base.guard; obj.val := 0; obj.val2 := 0; obj.expno := 0;
	ELSE Scanner.Mark ('Duplicated identifer definition'); obj := Base.guard
	END
END New;

PROCEDURE New_import_in (
	mod: Module; VAR obj: Base.Object; name: Base.IdentStr; cls: INTEGER
);
BEGIN
	NEW (obj); obj.readonly := TRUE; obj.param := FALSE; obj.export := FALSE;
	Base.StrCopy(name, obj.name); obj.class := cls; obj.lev := -2;
	obj.next := mod.dsc; mod.dsc := obj 
END New_import_in;

PROCEDURE New_import (VAR obj: Base.Object; name: Base.IdentStr; cls: INTEGER);
BEGIN New_import_in (imod, obj, name, cls)
END New_import;
	
PROCEDURE Enter (cl, n: INTEGER; name: Base.IdentStr; typ: Base.Type);
	VAR obj: Base.Object;
BEGIN
	NEW (obj); obj.readonly := FALSE; obj.param := FALSE;
	obj.class := cl; obj.val := n; Base.StrCopy(name, obj.name);
	obj.type := typ; obj.next := topScope.next; topScope.next := obj;
	IF cl = Base.cType THEN typ.obj := obj END
END Enter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import/Export *)

PROCEDURE Detect_typeI (VAR typ: Base.Type);
	VAR mod, ref: INTEGER; modname: Base.IdentStr; mod0: Module;
BEGIN
	Base.ReadInt (symfile, mod); Base.ReadInt (symfile, ref);
	IF mod = -2 THEN typ := Base.predefinedTypes[ref]
	ELSIF mod = -1 THEN
		IF ref < 0 THEN _Import_type (typ, -1, modname)
		ELSE typ := imod.types.a[ref]
		END
	ELSIF mod >= 0 THEN
		IF ref < 0 THEN _Import_type (typ, mod, modname)
		ELSE typ := imod.importModules.a[mod].types.a[ref]
		END
	ELSIF mod = -3 THEN
		Sys.Read_string (symfile, modname);
		IF ref < 0 THEN _Import_type (typ, -3, modname)
		ELSE Find_module (mod0, modname); typ := mod0.types.a[ref]
		END
	END
END Detect_typeI;

PROCEDURE Import_proc (VAR typ: Base.TypeDesc);
	VAR class, n: INTEGER; field: Base.Object;
		name: Base.IdentStr;
BEGIN
	Detect_typeI (typ.base);
	Base.ReadInt (symfile, typ.len);
	Base.ReadInt (symfile, typ.parblksize);
	typ.size := Base.WordSize;
	typ.alignment := Base.WordSize;
	
	OpenScope ('');
	Base.ReadInt (symfile, class);
	WHILE class # Base.cType DO
		Sys.Read_string (symfile, name);
		New (field, name, class); field.param := TRUE;
		Base.ReadInt (symfile, n); field.readonly := n = ORD(TRUE);
		Detect_typeI (field.type);
		Base.ReadInt (symfile, class)
	END;
	typ.fields := topScope.next;
	CloseScope
END Import_proc;

PROCEDURE Import_type (
	VAR typ: Base.Type; typmod: INTEGER; modname: Base.IdentStr
);
	VAR field, obj: Base.Object; itype: Base.TypeDesc; orgmod: Module;
		name: Base.IdentStr; form, class, ref, eno: INTEGER;
BEGIN itype.nptr := 0; 
	IF typmod = -1 THEN itype.mod := imod.modno ELSE itype.mod := -3 END;
	Base.ReadInt (symfile, ref); itype.ref := -ref - 1;
	Base.ReadInt (symfile, eno);
	Base.ReadInt (symfile, form); itype.form := form;
	
	IF typmod = -1 THEN Base.NewType (typ, form); imod.types.a[ref] := typ
	ELSE
		IF typmod >= 0 THEN orgmod := imod.importModules.a[typmod]
		ELSE Find_module (orgmod, modname)
		END;
		IF orgmod.modno < 0 THEN
			IF orgmod.types = NIL THEN NEW (orgmod.types) END;
			IF orgmod.types.a[ref] = NIL THEN
				IF typmod = -3 THEN NEW (itype.modname);
					Base.StrCopy (modname, itype.modname.s)
				END;
				Base.NewType (typ, form); orgmod.types.a[ref] := typ
			END
		END
	END;
	
	IF form = Base.tRecord THEN
		Detect_typeI (itype.base);
		Base.ReadInt (symfile, itype.len);
		Base.ReadInt (symfile, itype.size);
		Base.ReadInt (symfile, itype.nptr);
		Base.ReadInt (symfile, itype.alignment);
		
		OpenScope ('');
		Base.ReadInt (symfile, class);
		WHILE class = Base.cField DO
			Sys.Read_string (symfile, name);
			New (field, name, Base.cField);
			Detect_typeI (field.type);
			Base.ReadInt (symfile, field.val);
			Base.ReadInt (symfile, class)
		END; ASSERT (class = Base.cType);
		itype.fields := topScope.next;
		CloseScope
	ELSIF form = Base.tArray THEN
		Detect_typeI (itype.base);
		Base.ReadInt (symfile, itype.len);
		Base.ReadInt (symfile, itype.size);
		Base.ReadInt (symfile, itype.nptr);
		Base.ReadInt (symfile, itype.alignment)
	ELSIF form = Base.tPointer THEN
		Detect_typeI (itype.base);
		itype.size := Base.WordSize; itype.nptr := 1;
		itype.alignment := Base.WordSize
	ELSIF form = Base.tProcedure THEN
		Import_proc (itype)
	ELSIF itype.form = Base.tAddress THEN
		Detect_typeI (itype.base);
		itype.size := Base.WordSize;
		itype.alignment := Base.WordSize
	END;
	
	IF typmod = -1 THEN typ^ := itype;
		IF eno > 0 THEN
			New_import (obj, '', Base.cType); obj.type := typ;
			typ.obj := obj; obj.expno := eno; obj.val := 0
		END
	ELSE typ := orgmod.types.a[ref];
		IF typ.mod = -1 THEN typ^ := itype;
			IF eno > 0 THEN
				New_import_in (orgmod, obj, '', Base.cType); obj.type := typ;
				typ.obj := obj; obj.expno := eno; obj.val := 0
			END
		END
	END;
END Import_type;

PROCEDURE Read_module_key (VAR key: ModuleKey);
	VAR i, n: INTEGER;
BEGIN n := 0; i := 0;
	WHILE i < 16 DO Sys.Read_byte (symfile, n); key[i] := n; INC (i) END
END Read_module_key;

PROCEDURE Different_key (key1, key2: ModuleKey) : BOOLEAN;
	VAR i: INTEGER; result: BOOLEAN;
BEGIN i := 0; WHILE (i < 16) & (key1[i] = key2[i]) DO INC (i) END;
	RETURN i < 16
END Different_key;
	
PROCEDURE Import_symbols_file* (filename: ARRAY OF CHAR);
	VAR class, no, modlev: INTEGER; key: ModuleKey;
		obj: Base.Object; name: Base.IdentStr; mod: Module;
		errormsg: ARRAY 1024 OF CHAR;
BEGIN
	NEW (imod.types);
	Sys.Open (symfile, filename);
	Sys.Seek (symfile, 16); (* Skip module key *)
	Base.ReadInt (symfile, class); (* Skip module level *)
	
	Base.ReadInt (symfile, class);
	IF class = Base.cModule THEN NEW (imod.importModules) END;
	WHILE class # Base.cHead DO
		IF class = Base.cModule THEN
			Sys.Read_string (symfile, name); Read_module_key (key);
			Base.ReadInt (symfile, modlev); Base.ReadInt (symfile, no);
			IF name # module.name THEN mod := moduleList;
				WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END;
				IF mod = NIL THEN New_module (mod, name, key, modlev)
				ELSIF Different_key (key, mod.key) THEN
					errormsg := 'Module '; Strings.Append (name, errormsg);
					Strings.Append (' is imported from module ', errormsg);
					Strings.Append (imod.name, errormsg);
					Strings.Append (' with a different key', errormsg);
					Scanner.Mark (errormsg)
				END;
				IF no >= 0 THEN imod.importModules.a[no] := mod;
					IF no >= imod.importModules.len THEN
						imod.importModules.len := no + 1
					END
				END
			ELSE Scanner.Mark ('Circular import!')
			END
		ELSIF class = Base.cConst THEN
			Sys.Read_string (symfile, name);
			New_import (obj, name, Base.cConst);
			Base.ReadInt (symfile, obj.val); obj.expno := 0;
			Detect_typeI (obj.type)
		ELSIF class = Base.cType THEN
			Sys.Read_string (symfile, name);
			New_import (obj, name, Base.cType);
			Detect_typeI (obj.type);
			IF obj.type.obj = NIL THEN obj.type.obj := obj END;
			IF obj.type.form = Base.tRecord THEN
				Base.ReadInt (symfile, obj.expno); obj.val := 0
			END
		ELSIF class = Base.cVar THEN
			Sys.Read_string (symfile, name);
			New_import (obj, name, Base.cRef);
			Base.ReadInt (symfile, obj.val2);
			Base.ReadInt (symfile, obj.expno); obj.val := 0;
			Detect_typeI (obj.type)
		ELSIF class = Base.cProc THEN
			Sys.Read_string (symfile, name);
			New_import (obj, name, Base.cProc);
			Base.ReadInt (symfile, obj.expno); obj.val := 0;
			Base.NewType (obj.type, Base.tProcedure); Import_proc (obj.type^)
		ELSE ASSERT(FALSE)
		END;
		Base.ReadInt (symfile, class)
	END;
	Sys.Close (symfile)
END Import_symbols_file;

PROCEDURE Import_SYSTEM (mod: Base.Object);
BEGIN
	importSystem := TRUE; curLev := -2; OpenScope ('');
	
	Enter (Base.cType, 0, 'WORD', Base.wordType);
	Enter (Base.cType, 0, 'DWORD', Base.dwordType);
	
	Enter (Base.cSProc, 100, 'GET', NIL);
	Enter (Base.cSProc, 101, 'PUT', NIL);
	Enter (Base.cSProc, 102, 'COPY', NIL);
	Enter (Base.cSProc, 103, 'LoadLibraryW', NIL);
	Enter (Base.cSProc, 104, 'GetProcAddress', NIL);
	
	Enter (Base.cSFunc, 300, 'ADR', Base.intType);
	Enter (Base.cSFunc, 301, 'SIZE', Base.intType);
	Enter (Base.cSFunc, 302, 'BIT', Base.boolType);
	Enter (Base.cSFunc, 303, 'VAL', Base.intType);
	Enter (Base.cSFunc, 310, 'ADR2', Base.intType);
	Enter (Base.cSFunc, 311, 'STRADR', Base.intType);
	
	mod.dsc := topScope.next; mod.val := -1;
	CloseScope; curLev := 0
END Import_SYSTEM;

PROCEDURE Import_modules*;
	VAR filename: Base.String; i, min, minlev, n: INTEGER;
		importModules: ModuleArray; mod: Module; obj: Base.Object;
BEGIN
	(* Find the lowest level module to import first *)
	importModules := module.importModules; n := importModules.len; 
	REPEAT i := 0; min := -1;
		WHILE i < n DO mod := importModules.a[i];
			IF (mod.modno = -1) & ((min = -1) OR (mod.lev < minlev)) THEN
				min := i; minlev := mod.lev
			END;
			INC (i)
		END;
		IF min # -1 THEN
			imod := importModules.a[min]; imod.modno := min;
			filename[0] := 0X; Strings.Append (imod.name, filename);
			Strings.Append ('.sym', filename);
			Import_symbols_file (filename); obj := universe.next;
			WHILE obj # Base.guard DO
				IF (obj.class = Base.cModule) & (obj.val = min) THEN
					obj.dsc := imod.dsc; obj := Base.guard
				ELSE obj := obj.next
				END
			END
		END
	UNTIL min = -1
END Import_modules;

PROCEDURE Find_module_symfile* (mod: Base.Object; symName: Base.IdentStr);
	VAR filename: Base.String;
		importModules: ModuleArray; key: ModuleKey; newmod: Module;
		i, modlev: INTEGER;
BEGIN importModules := module.importModules;
	IF symName = 'SYSTEM' THEN Import_SYSTEM (mod)
	ELSIF importModules.len < LEN(importModules.a) THEN
		filename[0] := 0X; Strings.Append (symName, filename);
		Strings.Append ('.sym', filename);
		IF Sys.File_existed(filename) THEN
			Sys.Open (symfile, filename);
			Read_module_key (key); Base.ReadInt (symfile, modlev);
			New_module (newmod, symName, key, modlev);
			Sys.Close (symfile);
			i := importModules.len; importModules.a[i] := newmod;
			mod.val := i; INC (importModules.len);
			IF modlev >= module.lev THEN module.lev := modlev + 1 END
		ELSE Scanner.Mark ('Symbol file not found')
		END
	ELSE Scanner.Mark ('Compiler limit: Too many imported modules')
	END;
END Find_module_symfile;

(* -------------------------------------------------------------------------- *)

PROCEDURE Detect_type (typ : Base.Type);
BEGIN
	IF typ # NIL THEN
		Base.WriteInt (symfile, typ.mod); Base.WriteInt (symfile, typ.ref);
		IF typ.mod = -3 THEN Sys.Write_string (symfile, typ.modname.s) END;
		IF typ.ref < 0 THEN _Export_type (typ) END
	ELSE Base.WriteInt (symfile, -2); Base.WriteInt (symfile, 0)
	END
END Detect_type;

PROCEDURE Export_proc (typ: Base.Type);
	VAR field: Base.Object;
BEGIN
	Detect_type (typ.base);
	Base.WriteInt (symfile, typ.len);
	Base.WriteInt (symfile, typ.parblksize);
	
	field := typ.fields;
	WHILE field # Base.guard DO
		Base.WriteInt (symfile, field.class);
		Sys.Write_string (symfile, field.name);
		Base.WriteInt (symfile, ORD(field.readonly));
		Detect_type (field.type);
		field := field.next
	END;
	Base.WriteInt (symfile, Base.cType)
END Export_proc;
	
PROCEDURE Export_type (typ: Base.Type);
	VAR field: Base.Object; i: INTEGER; s: Base.String;
BEGIN
	IF typ.mod = -1 THEN
		IF refno < Base.MaxExportTypes THEN typ.ref := refno; INC (refno)
		ELSE Scanner.Mark ('Compiler limit: Too many exported types')
		END
	ELSE typ.ref := -typ.ref - 1
	END;
	Base.WriteInt (symfile, typ.ref);
	IF (typ.form # Base.tRecord) OR typ.obj.export THEN
		Base.WriteInt (symfile, 0)
	ELSE
		IF typ.obj.expno = 0 THEN INC (expno); typ.obj.expno := expno END;
		Base.WriteInt (symfile, typ.obj.expno)
	END;
	
	Base.WriteInt (symfile, typ.form);
	IF typ.form = Base.tRecord THEN
		Detect_type (typ.base);
		Base.WriteInt (symfile, typ.len);
		Base.WriteInt (symfile, typ.size);
		Base.WriteInt (symfile, typ.nptr);
		Base.WriteInt (symfile, typ.alignment);
		
		i := 0; field := typ.fields;
		WHILE field # Base.guard DO
			IF field.export OR (field.type.nptr > 0) THEN
				Base.WriteInt (symfile, Base.cField);
				IF ~field.export THEN
					Console.IntToString (i, s); INC (i);
					Sys.Write_string (symfile, s)
				ELSE Sys.Write_string (symfile, field.name)
				END;
				Detect_type (field.type);
				Base.WriteInt (symfile, field.val)
			END;
			field := field.next
		END;
		Base.WriteInt (symfile, Base.cType)
	ELSIF typ.form = Base.tArray THEN
		Detect_type (typ.base);
		Base.WriteInt (symfile, typ.len);
		Base.WriteInt (symfile, typ.size);
		Base.WriteInt (symfile, typ.nptr);
		Base.WriteInt (symfile, typ.alignment)
	ELSIF typ.form = Base.tPointer THEN
		Detect_type (typ.base)
	ELSIF typ.form = Base.tProcedure THEN
		Export_proc (typ)
	ELSIF typ.form = Base.tAddress THEN
		Detect_type (typ.base)
	END
END Export_type;

PROCEDURE Write_module_key (key: ModuleKey);
	VAR i : INTEGER;
BEGIN i := 0; WHILE i < 16 DO Sys.Write_byte (symfile, key[i]); INC (i) END
END Write_module_key;

PROCEDURE Write_symbols_file*;
	VAR obj: Base.Object; i, k: INTEGER; mod: Module;
BEGIN
	refno := 0; expno := 0;
	Sys.Rewrite (symfile, 'sym.temp_'); Sys.Seek (symfile, 16);
	Base.WriteInt (symfile, module.lev);
	
	mod := moduleList;
	WHILE mod # NIL DO
		Base.WriteInt (symfile, Base.cModule);
		Sys.Write_string (symfile, mod.name);
		Write_module_key (mod.key); Base.WriteInt (symfile, mod.lev);
		Base.WriteInt (symfile, mod.modno); mod := mod.next
	END;
	
	obj := universe.next;
	WHILE obj # Base.guard DO
		IF obj.export THEN
			IF obj.class = Base.cConst THEN
				Base.WriteInt (symfile, Base.cConst);
				Sys.Write_string (symfile, obj.name);
				Base.WriteInt (symfile, obj.val);
				Detect_type (obj.type)
			ELSIF obj.class = Base.cType THEN
				Base.WriteInt (symfile, Base.cType);
				Sys.Write_string (symfile, obj.name);
				Detect_type (obj.type);
				IF obj.type.form = Base.tRecord THEN
					INC (expno); obj.expno := expno;
					Base.WriteInt (symfile, expno)
				END
			ELSIF obj.class = Base.cVar THEN
				Base.WriteInt (symfile, Base.cVar);
				Sys.Write_string (symfile, obj.name);
				Base.WriteInt (symfile, obj.val2);
				INC (expno); obj.expno := expno;
				Base.WriteInt (symfile, expno);
				Detect_type (obj.type)
			ELSIF obj.class = Base.cProc THEN
				Base.WriteInt (symfile, Base.cProc);
				Sys.Write_string (symfile, obj.name);
				INC (expno); obj.expno := expno;
				Base.WriteInt (symfile, expno);
				Export_proc (obj.type)
			ELSE ASSERT(FALSE)
			END
		END;
		obj := obj.next
	END;
	Base.WriteInt (symfile, Base.cHead);
	Sys.Seek (symfile, 0); (*Sys.Calculate_MD5_hash (symfile, module.key);*)
	Sys.Seek (symfile, 0); Write_module_key (module.key);
	Sys.Close (symfile)
END Write_symbols_file;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Init* (modname : Base.IdentStr);
BEGIN
	module.lev := 0; Base.StrCopy (modname, module.name);
	NEW (module.importModules); NEW (module.types);
	
	importSystem := FALSE; NEW (universe);
	universe.class := Base.cHead; universe.name := 'MODULE';
	universe.next := Base.guard; topScope := universe; procScope := universe;
	
	Enter (Base.cType, 0, 'INTEGER', Base.intType);
	Enter (Base.cType, 0, 'BOOLEAN', Base.boolType);
	Enter (Base.cType, 0, 'SET', Base.setType);
	Enter (Base.cType, 0, 'BYTE', Base.byteType);
	Enter (Base.cType, 0, 'CHAR', Base.charType);
	Enter (Base.cType, 0, 'REAL', Base.realType);
	Enter (Base.cType, 0, 'LONGREAL', Base.realType);
	
	Enter (Base.cSProc, 0, 'INC', NIL);
	Enter (Base.cSProc, 1, 'DEC', NIL);
	Enter (Base.cSProc, 2, 'INCL', NIL);
	Enter (Base.cSProc, 3, 'EXCL', NIL);
	Enter (Base.cSProc, 4, 'NEW', NIL);
	Enter (Base.cSProc, 5, 'ASSERT', NIL);
	(*Enter (Base.cSproc, 6, 'PACK', NIL);*)
	(*Enter (Base.cSproc, 7, 'UNPK', NIL);*)
	
	Enter (Base.cSFunc, 200, 'ABS', Base.intType);
	Enter (Base.cSFunc, 201, 'ODD', Base.boolType);
	Enter (Base.cSFunc, 202, 'LEN', Base.intType);
	Enter (Base.cSFunc, 203, 'LSL', Base.intType);
	Enter (Base.cSFunc, 204, 'ASR', Base.intType);
	Enter (Base.cSFunc, 205, 'ROR', Base.intType);
	Enter (Base.cSFunc, 206, 'FLOOR', Base.intType);
	Enter (Base.cSFunc, 207, 'FLT', Base.realType);
	Enter (Base.cSFunc, 208, 'ORD', Base.intType);
	Enter (Base.cSFunc, 209, 'CHR', Base.charType)
END Init;
	
BEGIN
	_Export_type := Export_type;
	_Import_type := Import_type
END SymTable.