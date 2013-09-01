MODULE Test3;

VAR
	x : SET;
	y, z : INTEGER;
	a, b : INT8;

BEGIN
	x := {y, z};
	x := x / {3, 4};
	y := y + a;
	IF a IN x THEN
		y := ORD (x);
	END;
	IF x <= x THEN
		x := x + {1};
	END;
END Test3.
