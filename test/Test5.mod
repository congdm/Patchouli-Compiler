MODULE Test5;
(*$CONSOLE*)

IMPORT
	Console;
	
PROCEDURE MainProc (i : INTEGER);
	
	PROCEDURE SubProc;
	BEGIN
		Console.WriteString ('Printed by SubProc');
		Console.WriteLn;
		MainProc (-1)
	END SubProc;
	
	PROCEDURE RecursivePrint (n : INTEGER);
	BEGIN
		IF n = 0 THEN
			Console.WriteString ('Printed by RecursivePrint: ');
			RecursivePrint (1)
		ELSIF n < 10 THEN
			Console.Write (CHR(ORD('0') + n));
			Console.Write (' ');
			RecursivePrint (n + 1)
		ELSE Console.WriteLn
		END
	END RecursivePrint;
	
BEGIN
	IF i = 0 THEN
		SubProc; RecursivePrint (0)
	ELSIF i = -1 THEN
		Console.WriteString ('Printed by MainProc');
		Console.WriteLn
	END
END MainProc;

BEGIN
	MainProc (0)
END Test5.