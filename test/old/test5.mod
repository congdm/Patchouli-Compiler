MODULE Test5;

VAR
	x, y, z : INTEGER;
	
BEGIN
	IF x = 0 THEN
		y := x
	ELSIF x > 0 THEN
		z := x
	ELSE
		y := z
	END;
	
	WHILE y > 0 DO
		y := x - y
	ELSIF y < 0 DO
		z := y;
		y := 0
	END;
	
	REPEAT
		z := z + y + x;
		x := x - 1;
	UNTIL x <= 0;
END Test5.
