MODULE AyaCompile;
(*$CONSOLE*)

IMPORT
	ProgArgs, Console, Sys, Scanner, Parser, Parser2;
	
VAR
	srcfile: Sys.FileHandle;
	str: ARRAY 256 OF CHAR;
	len, sym: INTEGER;
	
BEGIN
	ProgArgs.GetArg (str, len, 1);
	IF str[0] # 0X THEN
		Sys.Open (srcfile, str);
		Scanner.Init (srcfile, 0); Scanner.Get (sym);
		IF sym = Scanner.module THEN Parser.Module
		ELSIF sym = Scanner.library THEN Parser2.Library
		ELSE Scanner.Mark ('Not a MODULE or LIBRARY')
		END;
		Sys.Close (srcfile)
	ELSE
		Console.WriteString ('AyaCompiler v0.6a for Oberon-07 language');
		Console.WriteLn; 
 		Console.WriteString ('Usage: AyaCompile <inputfile>');
		Console.WriteLn
	END
END AyaCompile.