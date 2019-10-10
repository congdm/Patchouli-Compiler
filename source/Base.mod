MODULE Base;

IMPORT
	S := Scn;

TYPE
	Type* = POINTER TO TypeDesc;
	TypeDesc = RECORD
	END ;

	Object* = POINTER TO ObjDesc;
	ObjDesc = RECORD
		type*: Type
	END ;

	Const* = POINTER TO ConstDesc;
	ConstDesc = RECORD (ObjDesc)
		value*: INTEGER
	END ;

	Var* = POINTER TO VarDesc;
	VarDesc = RECORD (ObjDesc)
		lev*: INTEGER
	END ;

	Node* = POINTER TO NodeDesc;
	NodeDesc = RECORD (ObjDesc)
		op: INTEGER;
		left*, right*: Object
	END ;

	Ident* = POINTER TO IdentDesc;
	IdentDesc = RECORD
		name*: S.Ident; obj*: Object; next: Ident
	END ;

	Scope* = POINTER TO ScopeDesc;
	ScopeDesc = RECORD first*: Ident; dsc: Scope END ;

VAR
	mod*: POINTER TO RECORD
		id*: S.Ident;
		topScope*, universe*: Scope
	END ;

PROCEDURE NewIdent*(VAR ident: Ident; name: S.Ident);
	VAR prev, x: Ident;
BEGIN
	x := topScope.first; NEW(ident); ident.name := name;
	WHILE x # NIL DO
		IF x # NIL THEN S.Mark('duplicated ident') END ;
		prev := x; x := x.next
	END ;
	IF prev # NIL THEN prev.next := ident
	ELSE topScope.first := ident
	END
END NewIdent;

PROCEDURE MakeConst*(VAR x: Object; t: Type; val: INTEGER);
	VAR c: Const;
BEGIN
	NEW(c); c.type := t; c.value := val; x := c
END MakeConst;

PROCEDURE MakeTypeObj*(VAR x: Object);
BEGIN
END MakeTypeObj;

PROCEDURE MakeVar*(VAR x: Object; t: Type);
BEGIN
END MakeVar;

PROCEDURE Init*(modid: S.Ident);
BEGIN
	NEW(mod); NEW(mod.universe); mod.id := modid;
	mod.topScope := mod.universe
END Init;

END Base.
