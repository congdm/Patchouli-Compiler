MODULE Test6;
(*$CONSOLE*)
IMPORT Out;
TYPE
	R0 = RECORD END; P0 = POINTER TO R0;
	R1 = RECORD (R0) END; P1 = POINTER TO R1;
	R2 = RECORD (R1) END; P2 = POINTER TO R2;
	R3 = RECORD (R0) END; P3 = POINTER TO R3;
VAR
	x: P0; y: P2;
BEGIN
	NEW(y); x := y; NEW(x);
	CASE x OF
		P2: Out.String('x IS P2') |
		P1: Out.String('x IS P1') |
		P3: Out.String('x IS P3') |
		P0: Out.String('x IS P0')
	END
END Test6.