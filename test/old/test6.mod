MODULE Test6;

TYPE
	Arr20 = ARRAY 20 OF INTEGER;
	RecursiveArray = ARRAY 20 OF RecursiveArray; (* Error *)

VAR
	x : Arr20;
	y : ARRAY 5 OF ARRAY 5 OF INTEGER;

PROCEDURE Hello (a : ARRAY OF INTEGER; VAR b : ARRAY OF INTEGER);
BEGIN
	a [1] := 1; (* Error because assignment to value parameter *)
	b [2] := 2;
	b [LEN (b) - 1] := 3;
END Hello;

PROCEDURE Hello2 (VAR a : ARRAY OF ARRAY OF INTEGER);
VAR
	t : INTEGER;
BEGIN
	a [1][2] := 10;
	t := LEN (a);
	t := LEN (a[0]);
END Hello2;

PROCEDURE Hello3 (VAR a : ARRAY OF ARRAY OF INTEGER);
BEGIN
	Hello2 (a);
END Hello3;

BEGIN
	Hello (x, x);
	Hello2 (y);
END Test6.
