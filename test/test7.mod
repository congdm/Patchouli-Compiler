MODULE Test7;

TYPE
	IntDynArray = ARRAY OF INTEGER;
	DynArrayOfDynArray = ARRAY OF IntDynArray;
	
VAR
	x : IntDynArray;
	y : DynArrayOfDynArray;
	i : INTEGER;
	
PROCEDURE Proc1 (a : IntDynArray);
BEGIN
	a [0] := 0;
	a [1] := 0;
	a [i] := 5
END Proc1;

PROCEDURE Proc2 (a : DynArrayOfDynArray);
BEGIN
	a [0][0] := 1;
	a [2][2] := 9;
	a [i][i] := 5
END Proc2;

PROCEDURE Proc3 (VAR a : ARRAY OF INTEGER);
BEGIN
	a [0] := 1;
	a [10] := 2;
	a [i] := 5
END Proc3;
	
BEGIN
	x [0] := 1;
	x [1] := 2;
	y [0][0] := 0;
	y [1][1] := 1;
	
	Proc1 (x); Proc2 (y); Proc3 (x);
	
	y [0] := x;
END Test7.
