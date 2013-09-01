MODULE Test4;

VAR
	a : ARRAY 10 OF INTEGER;
	b : INTEGER;
	
PROCEDURE Proc1 (VAR x : ARRAY OF INTEGER);
BEGIN
	x [b] := b;
END Proc1;

PROCEDURE Proc2 (VAR x : ARRAY OF INTEGER);
BEGIN
	Proc1 (x);
END Proc2;

BEGIN
	Proc1 (a);
	Proc2 (a);
END Test4.
