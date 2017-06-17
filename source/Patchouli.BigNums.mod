MODULE Patchouli.BigNums; (*$CONSOLE*)
IMPORT Out := [Oberon07.Out];
CONST bigNumSz = 2048; dSize = 60; intSize = 64; base = 1000000000000000000;
TYPE BigNum* = ARRAY bigNumSz DIV intSize OF INTEGER;
VAR Zero*: BigNum;

PROCEDURE Add*(VAR out: BigNum; x, y: BigNum);
VAR i, r0, carry: INTEGER; res: BigNum;
BEGIN
	carry := 0; i := 0;
	WHILE i < LEN(x) DO
		r0 := x[i] + y[i] + carry;
		IF r0 < base THEN carry := 0 ELSE carry := 1; DEC(r0, base) END;
		res[i] := r0; INC(i)
	END;
	out := res
END Add;

PROCEDURE Subtract*(VAR out: BigNum; x, y: BigNum);
VAR i, r0, borrow: INTEGER; res: BigNum;
BEGIN
	borrow := 0; i := 0;
	WHILE i < LEN(x) DO
		r0 := x[i] - y[i] - borrow;
		IF r0 < 0 THEN borrow := 1 ELSE borrow := 0; INC(r0, base) END;
		res[i] := r0; INC(i)
	END;
	out := res
END Subtract;

PROCEDURE Print*(x: BigNum);
VAR i, x0, t: INTEGER;
BEGIN
	i := LEN(x) - 1; WHILE (x[i] = 0) & (i > 0) DO DEC(i) END;
	Out.Int(x[i], 0); DEC(i);
	WHILE i >= 0 DO
		x0 := x[i]; t := base DIV 10;
		WHILE (x0 < t) & (t > 10) DO t := t DIV 10; Out.Char('0') END;
		Out.Int(x0, 0); DEC(i)
	END
END Print;

PROCEDURE Set0(VAR x: BigNum; v: INTEGER);
BEGIN x := Zero; x[0] := v MOD LSL(1, dSize)
END Set0;

PROCEDURE Init;
VAR i: INTEGER; x: BigNum;
BEGIN
	i := 0; WHILE i < LEN(Zero) DO Zero[i] := 0; INC(i) END;
	Set0(x, 1241); i := 0;
	WHILE i < 1000 DO
		Print(x); Out.Ln; Add(x, x, x); INC(i)
	END
END Init;

BEGIN Init
END BigNums.