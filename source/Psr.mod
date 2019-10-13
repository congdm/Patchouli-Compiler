MODULE Psr;

IMPORT
	S := Scn, B := Base;
	
VAR
	sym: INTEGER;
	type0: PROCEDURE(): B.Type;
	
PROCEDURE Check(expected: INTEGER);
BEGIN
	IF sym = expected THEN (* ok *) S.Get(sym)
	ELSIF sym = S.end THEN S.Mark('no END')
	ELSIF sym = S.semicolon THEN S.Mark('no ;')
	ELSIF sym = S.period THEN S.Mark('no .')
	ELSIF sym = S.eql THEN S.Mark('no =')
	ELSIF sym = S.colon THEN S.Mark('no :')
	ELSIF sym = S.rparen THEN S.Mark('no )')
	ELSIF sym = S.of THEN S.Mark('no OF')
	ELSIF sym = S.to THEN S.Mark('no TO')
	ELSE ASSERT(FALSE)
	END
END Check;

PROCEDURE CheckExport(VAR expo: BOOLEAN);
BEGIN
	IF sym = S.times THEN S.Get(sym);
		IF B.mod.curLev = 0 THEN expo := TRUE
		ELSE S.Mark('not exportable')
		END
	END
END CheckExport;

PROCEDURE CheckTypeObj(x: B.Object; VAR t: B.Type);
BEGIN t := B.mod.intType;
	IF x IS B.TypeObj THEN
		IF x.type # NIL THEN t := x.type END 
	ELSE S.Mark('not a type')
	END
END CheckTypeObj;

PROCEDURE MarkMissing(sym0: INTEGER);
BEGIN
	IF sym0 = S.ident THEN S.Mark('identifier expected')
	ELSE ASSERT(FALSE)
	END
END MarkMissing;

PROCEDURE MarkSflous(sym0: INTEGER);
BEGIN
	IF sym0 = S.comma THEN S.Mark('superfluous ,')
	ELSIF sym0 = S.semicolon THEN S.Mark('superfluous ;')
	ELSE ASSERT(FALSE)
	END
END MarkSflous;

PROCEDURE AddUndef(ptr: B.Type; recName: S.Ident);
END AddUndef;

PROCEDURE FixUndef(rec: B.Type; recName: S.Ident);
END FixUndef;

PROCEDURE CheckUndef;
END CheckUndef;

PROCEDURE qualident0(): B.Ident;
	RETURN NIL
END qualident0;

PROCEDURE qualident(): B.Object;
	RETURN NIL
END qualident;

PROCEDURE expression(): B.Object;
	RETURN NIL
END expression;

PROCEDURE ConstExpression(): B.Const;
	VAR x: B.Object; c: B.Const;
BEGIN x := expression();
	IF x IS B.Const THEN c := x(B.Const)
	ELSE S.Mark('not a const'); c := B.NewConst(B.mod.intType, 0)
	END ;
	RETURN c
END ConstExpression;

PROCEDURE StatementSequence(): B.Node;
	RETURN NIL
END StatementSequence;

PROCEDURE IdentList(): B.Ident;
	VAR fst, x: B.Ident;
BEGIN
	B.NewIdent(fst, S.id); S.Get(sym); CheckExport(fst.expo);
	WHILE sym = S.comma DO
		IF sym = S.ident THEN
			B.NewIdent(x, S.id); S.Get(sym); CheckExport(x.expo)
		ELSE MarkSflous(S.comma)
		END
	END ;
	RETURN fst
END IdentList;

PROCEDURE PointerType(typeObj: B.TypeObj): B.Type;
	VAR ptr, b: B.Type; id: S.Ident; x: B.Ident;
BEGIN
	B.NewPointer(ptr); S.Get(sym); Check(S.to);
	IF typeObj # NIL THEN typeObj.type := ptr (* forward defining *) END ;
	IF sym = S.ident THEN id := S.id; x := qualident0();
		IF x # NIL THEN
			IF x.obj # NIL THEN CheckTypeObj(x.obj, b);
				IF b.form = B.tRec THEN ptr.base := b
				ELSE S.Mark('not a record type')
				END
			ELSIF x = B.externalIdentNotFound THEN
				S.Mark('external identifier not found')
			ELSE S.Mark('identifier not defined yet')
			END
		ELSE (* undefined base type *) AddUndef(ptr, id)
		END
	ELSIF sym = S.record THEN ptr.base := type0()
	ELSE S.Mark('base type?')
	END ;
	RETURN ptr
END PointerType;

PROCEDURE length(): INTEGER;
	VAR x: B.Const;
BEGIN x := ConstExpression();
	IF x.value < 0 THEN S.Mark('Invalid array length'); x.value := 0 END ; 
	RETURN x.value
END length;

PROCEDURE BaseType(): B.Type;
	VAR b: B.Type; x: B.Object;
BEGIN
	IF sym = S.ident THEN
		x := qualident(); CheckTypeObj(x, b);
		IF b.form = B.tRec THEN (* ok *)
		ELSIF b.form = B.tPtr THEN
			IF b.base # NIL THEN b := b.base
			ELSE S.Mark('this type is not defined yet')
			END
		ELSE b := NIL; S.Mark('not record type')
		END ;
		IF (b # NIL) & (b.len >= B.MaxExt) THEN
			b := NIL; S.Mark('max extension limit reached')
		END
	ELSE MarkMissing(S.ident)
	END ;
	RETURN b
END BaseType;

PROCEDURE FieldList;
	VAR f: B.Ident; tp: B.Type;
BEGIN
	f := IdentList(); Check(S.colon); tp := type0();
	WHILE f # NIL DO f.obj := B.NewField(tp); f := f.next END
END FieldList;

PROCEDURE type(): B.Type;
	VAR t, arr: B.Type; x: B.Object; len: INTEGER;
BEGIN t := B.mod.intType;
	IF sym = S.ident THEN
		x := qualident(); CheckTypeObj(x, t); S.Get(sym)
	ELSIF sym = S.array THEN
		S.Get(sym); len := length(); B.NewArray(t, len); arr := t;
		WHILE sym = S.comma DO S.Get(sym);
			IF sym <= S.ident THEN
				len := length(); B.NewArray(arr.base, len); arr := arr.base
			ELSE MarkSflous(S.comma) 
			END
		END ;
		Check(S.of); arr.base := type()
	ELSIF sym = S.record THEN
		S.Get(sym); B.NewRecord(t);
		IF sym = S.lparen THEN
			S.Get(sym); t.base := BaseType(); Check(S.rparen);
			IF t.base # NIL THEN t.len := t.base.len + 1 END
		END ;
		B.OpenScope;
		IF sym = S.ident THEN FieldList;
			WHILE sym = S.semicolon DO S.Get(sym);
				IF sym = S.ident THEN FieldList;
				ELSE MarkSflous(S.semicolon)
				END
			END
		END ;
		t.fields := B.mod.topScope.first; B.CloseScope; Check(S.end)
	ELSIF sym = S.pointer THEN t := PointerType(NIL)
	ELSIF sym = S.procedure THEN
	END ;
	RETURN t
END type;

PROCEDURE DeclarationSequence;
	VAR id: B.Ident; x: B.Object; tp: B.Type;
BEGIN
	IF sym = S.const THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(id, S.id); S.Get(sym);
			CheckExport(id.expo); Check(S.eql);
			id.obj := ConstExpression(); Check(S.semicolon) 
		END
	END ;
	IF sym = S.type THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(id, S.id); S.Get(sym);
			CheckExport(id.expo); Check(S.eql); x := B.NewTypeObj();
			IF sym # S.pointer THEN x.type := type();
				IF x.type.form = B.tRec THEN FixUndef(x.type, id.name) END
			ELSE x.type := PointerType(x(B.TypeObj))
			END ;
			Check(S.semicolon)
		END ;
		CheckUndef
	END ;
	IF sym = S.var THEN S.Get(sym);
		WHILE sym = S.ident DO
			id := IdentList(); Check(S.colon); tp := type();
			WHILE id # NIL DO id.obj := B.NewVar(tp); id := id.next END ;
			Check(S.semicolon)
		END
	END ;
	WHILE sym = S.procedure DO
	END
END DeclarationSequence;

PROCEDURE ImportList;
END ImportList;
	
PROCEDURE Module*;
	VAR modid: S.Ident;
BEGIN S.Get(sym);
	IF sym = S.ident THEN modid := S.id
	ELSE modid := 0X; MarkMissing(S.ident)
	END ;
	IF S.errcnt = 0 THEN
		B.Init(modid); Check(S.semicolon);
		IF sym = S.import THEN ImportList END
	END ;
	IF S.errcnt = 0 THEN
		DeclarationSequence;
		IF sym = S.begin THEN
			S.Get(sym); B.mod.init := StatementSequence()
		END ;
		Check(S.end);
		IF sym = S.ident THEN
			IF S.id # modid THEN S.Mark('wrong module name') END ;
			S.Get(sym)
		ELSE MarkMissing(S.ident)
		END ;
		Check(S.period)
	END
END Module;

BEGIN
	type0 := type
END Psr.