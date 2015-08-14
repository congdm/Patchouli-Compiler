MODULE TestSource;
VAR x: INTEGER;
BEGIN
	IF x = 0 THEN x := 1
	ELSIF x > 5 THEN x := 0
	END;
	x := x DIV 20
END TestSource.