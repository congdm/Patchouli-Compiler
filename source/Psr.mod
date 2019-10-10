MODULE Psr;

IMPORT
	S := Scn; B := Base;
	
VAR
	sym: INTEGER;
	
PROCEDURE Check(expected: INTEGER);
BEGIN
	IF sym = expected THEN (* ok *) S.Get(sym)
	ELSIF sym = S.semicolon THEN S.Mark('No ;')
	ELSIF sym = S.period THEN S.Mark('No .')
	ELSIF sym = S.eql THEN S.Mark('No =')
	ELSE ASSERT(FALSE)
	END
END Check;

PROCEDURE Missing(sym0: INTEGER);
BEGIN
	IF sym0 = S.ident THEN Mark('No ident')
	ELSE ASSERT(FALSE)
	END
END Missing;

PROCEDURE CheckExport(VAR expo: BOOLEAN);
BEGIN
	IF sym = S.times THEN S.Get(sym);
		IF B.mod.curLev = 0 THEN expo := TRUE
		ELSE S.Mark('not exportable')
		END
	END
END CheckExport;

PROCEDURE MarkSuperfluous(sym0: INTEGER);
BEGIN
	IF sym0 = S.comma THEN Mark('Superfluous ,')
	END
END MarkSuperfluous;

PROCEDURE expression(): B.Object;
	RETURN NIL
END expression;

PROCEDURE StatementSequence(): B.Node;
	RETURN NIL
END StatementSequence;

PROCEDURE DeclarationSequence;
	VAR id, fst: B.Ident; x: B.Object; tp: B.Type;
BEGIN
	IF sym = S.const THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(id, S.id); S.Get(sym);
			CheckExport(id.expo); Check(S.eql); x := expression();
			IF ~(x IS B.Const) THEN 
				S.Mark('not a const'); B.MakeConst(x, B.mod.intType, 0)
			END ;
			id.obj := x; Check(S.semicolon) 
		END
	END ;
	IF sym = S.type THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(id, S.id); S.Get(sym);
			CheckExport(id.expo); Check(S.eql);
			B.MakeTypeObj(x); x.type := type(); Check(S.semicolon)
		END
	END ;
	IF sym = S.var THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(fst, S.id); S.Get(sym); CheckExport(fst.expo);
			WHILE sym = S.comma DO
				IF sym = S.ident THEN
					B.NewIdent(id, S.id); S.Get(sym); CheckExport(id.expo)
				ELSE MarkSuperfluous(S.comma)
				END
			END ;
			Check(S.colon); tp := type(); id := fst;
			WHILE id # NIL DO B.MakeVar(id.obj, tp); id := id.next END ;
			Check(S.semicolon)
		END
	END ;
	WHILE sym = S.procedure DO
	END
END DeclarationSequence;
	
PROCEDURE Module*;
	VAR modid: S.Ident;
BEGIN S.Get(sym);
	IF sym = S.ident THEN modid := S.id
	ELSE modid := 0X; Missing(S.ident)
	END ;
	IF S.errcnt = 0 THEN
		B.Init(modid); Check(S.semicolon);
		IF sym = S.import THEN ImportList END
	END ;
	IF S.errcnt = 0 THEN
		DeclarationSequence;
		IF sym = S.begin THEN S.Get(sym); modinit := StatementSequence() END ;
		Check(S.end);
		IF sym = S.ident THEN
			IF S.id # modid THEN Mark('wrong module name') END ; S.Get(sym)
		ELSE Missing(S.ident)
		END ;
		Check(S.period)
	END
END Module;

END Psr.