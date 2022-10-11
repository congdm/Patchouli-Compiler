MODULE Sys;

TYPE
	Int* = ARRAY 8 OF BYTE;
	Real* = ARRAY 8 OF BYTE;
	Decimal* = ARRAY 21 OF BYTE;

VAR
	MaxInt*, MinInt*: Int;
	ZeroInt*: Int;
	MaxUnicode*: Int;
	
PROCEDURE SignInt*(x: Int): BOOLEAN;
BEGIN
	RETURN x[LEN(x)-1] > 127
END SignInt;
	
PROCEDURE CmpInt*(x, y: Int): INTEGER;
	VAR res, i: INTEGER;
BEGIN
	IF SignInt(x) = SignInt(y) THEN
		i := LEN(x)-1;
		WHILE (i > 0) & (x[i] = y[i]) DO DEC(i) END ;
		IF x[i] > y[i] THEN res := 1
		ELSIF x[i] < y[i] THEN res := -1
		ELSE res := 0
		END
	ELSIF SignInt(x) THEN res := -1
	ELSE res := 1
	END ;
	RETURN res
END CmpInt;
	
PROCEDURE AddInt*(VAR x: Int; y: Int);
	VAR i, carry, t: INTEGER;
BEGIN carry := 0; i := 0;
	WHILE i < LEN(x) DO
		t = x[i] + y[i] + carry;
		IF t <= 255 THEN carry := 0; x[i] := t
		ELSE carry = 1; x[i] := t - 256
		END ;
		INC(i)
	END
END AddInt;

PROCEDURE NegInt*(VAR x: Int);
	VAR i, carry: INTEGER;
BEGIN i := 0; carry := 0;
	WHILE i < LEN(x) DO
		x[i] := 256 - x[i] - carry; carry := 1; INC(i)
	END
END NegInt;

PROCEDURE SubInt*(VAR x: Int; y: Int);
	VAR z: Int;
BEGIN
	z := y; NegInt(z); AddInt(x, z)
END SubInt;

PROCEDURE LShiftLeft*(VAR x: Int; n: INTEGER);
	VAR i, t, carry: INTEGER;
BEGIN
	WHILE n >= 64 DO n := n - 64 END ;
	WHILE n > 0 DO
		i := 0; carry := 0;
		WHILE i < LEN(x) DO
			t := x[i] * 2;
			IF t > 255 THEN x[i] := t - 256 + carry; carry := 1
			ELSE x[i] := t + carry; carry := 0
			END ;
			INC(i)
		END ;
		DEC(n)
	END
END LShiftLeft;

PROCEDURE AShiftRight*(VAR x: Int; n: INTEGER);
	VAR i, t, carry: INTEGER; odd: BOOLEAN;
BEGIN
	WHILE n >= 64 DO n := n - 64 END ;
	WHILE n > 0 DO
		i := LEN(x)-1;
		IF x[i] < 128 THEN carry := 0 ELSE carry := 1 END ;
		WHILE i >= 0 DO
			odd := ODD(x[i]);
			x[i] := x[i] DIV 2 + carry*128;
			IF odd THEN carry := 1 ELSE carry := 0 END ;
			DEC(i)
		END ;
		DEC(n)
	END
END AShiftRight;

PROCEDURE MulInt*(VAR x: Int; y: Int);
	VAR z: Int;
BEGIN
END MulInt;

END Sys.