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

PROCEDURE expression(): B.Object;
	RETURN NIL
END expression;

PROCEDURE StatementSequence(): B.Node;
	RETURN NIL
END StatementSequence;

PROCEDURE DeclarationSequence;
	VAR ident: B.Ident; x: B.Object;
BEGIN
	IF sym = S.const THEN S.Get(sym);
		WHILE sym = S.ident DO
			B.NewIdent(ident, S.id); Check(S.eql); x := expression();
			IF x IS B.Const THEN ident.obj := x
			ELSE S.Mark('not a const'); B.MakeConst(ident.obj, B.mod.intType, 0)
			END ;
			Check(S.semicolon) 
		END
	END ;
	IF sym = S.type THEN S.Get(sym);
		WHILE sym = S.ident DO
		END
	END ;
	IF sym = S.var THEN S.Get(sym);
		WHILE sym = S.ident DO
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