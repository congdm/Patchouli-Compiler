MODULE Test7;

TYPE
	IntDynArray = ARRAY OF INTEGER;
	DynArrayOfDynArray = ARRAY OF IntDynArray;
	
VAR
	x : IntDynArray;
	y : DynArrayOfDynArray;
	
PROCEDURE Proc1 (a : IntDynArray);
BEGIN
	a [0] := 0;
	a [1] := 0;
END Proc1;
	
BEGIN
	x [0] := 1;
	x [1] := 2;
END Test7.
