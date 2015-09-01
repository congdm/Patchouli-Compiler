MODULE AyaCompile;
(*$CONSOLE*)

IMPORT
	ProgArgs, Console, Sys, Scanner, Parser;
	
VAR
	srcfile: Sys.FileHandle;
	str: ARRAY 256 OF CHAR;
	len: INTEGER;
	
BEGIN
	ProgArgs.GetArg (str, len, 1);
	IF str[0] # 0X THEN
		Sys.Open (srcfile, str);
		Scanner.Init (srcfile, 0); Parser.Module;
		Sys.Close (srcfile)
	ELSE
		Console.WriteString ('AyaCompiler v0.6a for Oberon-07 language');
		Console.WriteLn; 
 		Console.WriteString ('Usage: AyaCompile <inputfile>');
		Console.WriteLn 
	END
END AyaCompile.