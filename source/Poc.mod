MODULE Poc;
(*$CONSOLE*)

IMPORT
	Rtl, Out,
	Scanner, Base, Parser;
	
VAR
	srcfile: Rtl.File;
	str, fname: ARRAY 512 OF CHAR;
	len, sym, i: INTEGER;
	
BEGIN
	fname[0] := 0X; i := 1; Rtl.GetArg(str, len, i);
	WHILE str[0] # 0X DO
		IF (str[0] # '/') & (fname[0] = 0X) THEN fname := str
		ELSIF str[0] = '/' THEN
			IF str = '/h' THEN Base.SetFlag('handle') END
		END;
		INC(i); Rtl.GetArg(str, len, i)
	END;
	IF fname[0] # 0X THEN
		IF Rtl.ExistFile(fname) THEN
			Rtl.Reset(srcfile, fname); Scanner.Init(srcfile, 0);
			Rtl.Close(srcfile); Scanner.Get(sym);
			IF sym = Scanner.module THEN Parser.Module
			ELSE Scanner.Mark('MODULE?')
			END
		ELSE Out.String('File '); Out.String(fname); Out.String(' not found')
		END
	ELSE
		Out.String('Patchouli Oberon-07 Compiler v0.8d'); Out.Ln;
 		Out.String('Usage: Poc <inputfile>'); Out.Ln
	END
END Poc.