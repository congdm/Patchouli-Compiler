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
	Sys.Open (srcfile, str);
	Scanner.Init (srcfile, 0); Parser.Module;
	Sys.Close (srcfile)
END AyaCompile.