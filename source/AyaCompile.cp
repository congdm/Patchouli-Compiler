MODULE AyaCompile;

IMPORT
	Base, Scanner, CPmain, Console;
	
VAR
	in : Base.FileHandle;
	sym : INTEGER;
	a : ARRAY 256 OF CHAR;
	
BEGIN
Base.Open (in, 'in.txt');
Scanner.Init (in);
Scanner.Get (sym);
IF sym = Base.sym_module THEN
	a := 'MODULE'
ELSIF sym = Base.sym_number THEN
	Base.Int_to_string (Scanner.val, a);
	END;
Console.WriteString (a);
Base.Close (in)
END AyaCompile.