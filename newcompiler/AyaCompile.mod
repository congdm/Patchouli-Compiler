MODULE AyaCompile;
(*$CONSOLE*)

IMPORT
	Sys, Scanner, Parser;
	
VAR
	srcfile: Sys.FileHandle;
	
BEGIN
	Sys.Open (srcfile, 'Parser.mod');
	Scanner.Init (srcfile, 0); Parser.Module;
	Sys.Close (srcfile)
END AyaCompile.