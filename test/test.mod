MODULE Test;

VAR
	s : ARRAY 20 OF INTEGER;
	len : INTEGER;
	
PROCEDURE WriteChar (c : INTEGER);
BEGIN
END WriteChar;
	
PROCEDURE WriteStr;
VAR i : INTEGER;
BEGIN
	i := len - 1;
	WHILE i >= 0 DO
		WriteChar (s[i]);
		i := i - 1;
	END;
END WriteStr;

PROCEDURE IntToStr (x : INTEGER);
VAR
	i, m : INTEGER;
BEGIN
	len := 0;
	IF x > 0 THEN
		i := 0; WHILE i < 20 DO s[i] := 0; i := i + 1; END;
		REPEAT
			m := x MOD 10;
			x := x DIV 10;
			s [len] := m + 48;
			len := len + 1;
		UNTIL x = 0;
	END;
END IntToStr;

PROCEDURE Add (VAR x : INTEGER; y : INTEGER);
BEGIN
	x := x + y;
END Add;

BEGIN
	IntToStr (45);
	WriteStr;
END Test.