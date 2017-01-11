MODULE Test6;
(*$CONSOLE*)
IMPORT Out;
CONST MinInt = 8000000000000000H; MaxInt = MinInt - 1; MinusOne = -1;
TYPE
	R0 = RECORD END; P0 = POINTER TO R0;
	R1 = RECORD (R0) END; P1 = POINTER TO R1;
	R2 = RECORD (R1) END; P2 = POINTER TO R2;
	R3 = RECORD (R0) END; P3 = POINTER TO R3;
VAR
	x: P0; y: P2; int: INTEGER; ch: CHAR;
BEGIN
	NEW(y); x := y; NEW(x);
	CASE x OF
		P2: Out.String('x IS P2') |
		P1: Out.String('x IS P1') |
		P3: Out.String('x IS P3') |
		P0: Out.String('x IS P0')
	END;
	Out.Ln; int := 4364364;
	CASE int OF
		201 .. MaxInt: Out.String('200 < int') |
		100 .. 200: Out.String('99 < int <= 200') |
		1 .. 99: Out.String('0 < int <= 99') |
		0: Out.String('int = 0') |
		MinInt .. MinusOne: Out.String('int < 0')
	END;
	Out.Ln; Out.String('ch is '); ch := 90X;
	CASE ch OF
		0X .. 7FX: Out.String('ASCII') |
		80X .. 0FFX: Out.String('Extended ASCII') |
		100X .. 0D7FFX: Out.String('Unicode Code points below Surrogates') |
		0D800X .. 0DBFFX: Out.String('Low Surrogates') |
		0DC00X .. 0DFFFX: Out.String('High Surrogates') |
		0E000X .. 0F8FFX: Out.String('Private Use') |
		0F900X .. 0FFEFX: Out.String('Unicode Code points above Private Use') |
		0FFF0X .. 0FFFFX: Out.String('Specials')
	END
END Test6.