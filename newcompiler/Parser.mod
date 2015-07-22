MODULE Parser;

IMPORT
	Base, Scanner;
	
CONST
	noSemicolonError = 'No ;';
	noEndError = 'No END';
	
PROCEDURE Check (expected: INTEGER; err: ARRAY OF CHAR);
BEGIN
	IF sym = expected THEN Scanner.Get (sym) ELSE Scanner.Mark (err) END
END Check;
	
PROCEDURE Module*;
	VAR modid: Base.String; varsize: INTEGER;
BEGIN
	Base.ResetCompilerFlag;
	
	Scanner.Get (sym); Check (Scanner.module, 'No MODULE keyword');
	IF sym = Scanner.ident THEN modid := Scanner.id; Scanner.Get (sym)
	ELSE modid := '@'; Scanner.Mark ('No module name')
	END;
	Check (Scanner.semicolon, noSemicolonError);
	
	IF modid # '@' THEN
		Base.Init; SymTable.Init (modid); Generator.Init (modid);
		(* IF sym = Scanner.import THEN ImportList END; *)
		varsize := 0; DeclarationSequence (varsize);

		Generator.Enter (NIL, 0);
		IF sym = Scanner.begin THEN Scanner.Get (sym); StatementSequence END;
		Generator.Return;
		Generator.Module_init; Generator.Module_exit;

		Check (Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF modid # Scanner.id THEN Scanner.Mark ('Wrong module name') END;
			Scanner.Get (sym)
		ELSE Scanner.Mark ('No module identifier after END')
		END;
		Check (Scanner.period, 'No ending .');
		
		Generator.Finish
	END
END Module;
	
END Parser.