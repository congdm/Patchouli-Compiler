MODULE Test7;
(*$CONSOLE*)

IMPORT
	SYSTEM, Console;
	
PROCEDURE Main;
	VAR real : REAL;
BEGIN
	real := 1.1;
	Console.WriteReal (real);
	Console.Write (' ');
	Console.WriteInt (SYSTEM.VAL (SYSTEM.DWORD, real));
	Console.WriteLn
END Main;
	
BEGIN
	Main
END Test7.