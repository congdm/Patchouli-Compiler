MODULE Test2;(*$CONSOLE*)
IMPORT Out;
TYPE Rec = RECORD a: ARRAY 8 OF BYTE END;
VAR x: Rec;

PROCEDURE Proc(y: Rec);
BEGIN
	Out.Int(y.a[0], 0)
END Proc;

BEGIN
	x.a[0] := 17; Proc(x)
END Test2.