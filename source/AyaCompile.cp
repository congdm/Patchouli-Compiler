MODULE AyaCompile;

IMPORT
	Base, Scanner, Parser, CPmain, Console, ProgArgs;
	
VAR
	str : ARRAY 256 OF CHAR;
	args_num : INTEGER;
	
PROCEDURE Compile (filename : ARRAY OF CHAR);
	VAR
		in : Base.FileHandle;
	BEGIN
	Base.Open (in, filename);
	Scanner.Init (in);
	Parser.Module;
	Base.Close (in)
	END Compile;
	
PROCEDURE Valid_filename (filename : ARRAY OF CHAR) : BOOLEAN;
	BEGIN
	(* Implement later *)
	RETURN TRUE
	END Valid_filename;
	
BEGIN
args_num := ProgArgs.ArgNumber ();
IF args_num = 0 THEN
	Console.WriteString ('AyaCompiler for Oberon-07 language'); Console.WriteLn;
	Console.WriteString ('Usage: AyaCompile <inputfile>'); Console.WriteLn
ELSIF args_num = 1 THEN
	ProgArgs.GetArg (0, str);
	IF Valid_filename (str) THEN Compile (str)
	ELSE Console.WriteString ('Invalid input filename'); Console.WriteLn END
ELSE
	Console.WriteString ('Invalid number of arguments'); Console.WriteLn;
	Console.WriteString ('Run without arguments for usage info'); Console.WriteLn
	END
END AyaCompile.