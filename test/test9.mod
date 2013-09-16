MODULE Test9;

TYPE
	P = PROCEDURE;
	Function = PROCEDURE (x : INTEGER) : INTEGER;
	
VAR
	proc : P;
	add : Function;
	i : INTEGER;

PROCEDURE Add (x : INTEGER) : INTEGER;
BEGIN
	x := x + x;
RETURN x END Add;

PROCEDURE Atk (p : P; a : Function);
BEGIN
	p ();
	a (i);
END Atk;
	
PROCEDURE Hello;
BEGIN
	Atk (Hello, Add)
END Hello;

BEGIN
	proc := NIL;
	proc := Hello;
	proc ();
	i := i + add (i);
	add := Add;
END Test9.
