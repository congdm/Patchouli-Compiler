MODULE Test3;
(*$CONSOLE*)
IMPORT SYSTEM, Out;

VAR
	teb: INTEGER;
	stkBase, stkLimit: INTEGER;
	
PROCEDURE LargeStack;
	VAR x: ARRAY 5000H OF BYTE;
BEGIN
	x[0] := 1
END LargeStack;

BEGIN
	teb := SYSTEM.NtCurrentTeb();
	Out.String('Thread Information Block address: '); Out.Int(teb, 0); Out.Ln;
	SYSTEM.GET(teb+30H, teb);
	Out.String('Thread Information Block address: '); Out.Int(teb, 0);
	LargeStack
END Test3.