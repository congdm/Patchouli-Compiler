MODULE AyaCompile;

IMPORT
	Base, Scanner, Parser, CPmain, Console;
	
VAR
	in : Base.FileHandle;
	sym : INTEGER;
	a : ARRAY 256 OF CHAR;
	
BEGIN
Base.Open (in, 'in.txt');
Scanner.Init (in);
Parser.Module;
Base.Close (in)
END AyaCompile.