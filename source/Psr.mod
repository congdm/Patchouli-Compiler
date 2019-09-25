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
	ELSE ASSERT(FALSE)
	END
END Check;

PROCEDURE Missing(sym0: INTEGER);
BEGIN
	IF sym0 = S.ident THEN Mark('No ident')
	ELSE ASSERT(FALSE)
	END
END Missing;

PROCEDURE StatementSequence(): B.Node;
	RETURN NIL
END StatementSequence;

PROCEDURE DeclarationSequence;
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