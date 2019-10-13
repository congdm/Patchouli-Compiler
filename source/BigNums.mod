MODULE BigNums;
CONST
	bigNumSz = 2048; intSize = 64;
	Base* = 1000000000000000000;
	DecimalDigitsPerDigits* = 18;
	MaxDigits* = bigNumSz DIV intSize;
	MaxDecimalDigits* = MaxDigits * DecimalDigitsPerDigits;
TYPE
	BigNum* = ARRAY MaxDigits OF INTEGER;
VAR
	Zero*, MaxNum*, One*: BigNum;
	Ten: ARRAY DecimalDigitsPerDigits OF INTEGER;

PROCEDURE SetDecimalDigit*(VAR x: BigNum; pos, d: INTEGER);
VAR i, j, k: INTEGER;
BEGIN
	ASSERT((pos >= 0) & (pos < MaxDecimalDigits) & (d >= 0) & (d < 10));
	i := pos DIV DecimalDigitsPerDigits; j := pos MOD DecimalDigitsPerDigits;
	k := x[i] DIV Ten[j] MOD 10; x[i] := x[i] + (d-k)*Ten[j]
END SetDecimalDigit;

PROCEDURE DecimalDigit*(x: BigNum; pos: INTEGER): INTEGER;
VAR i, j: INTEGER;
BEGIN
	ASSERT((pos >= 0) & (pos < MaxDecimalDigits));
	i := pos DIV DecimalDigitsPerDigits; j := pos MOD DecimalDigitsPerDigits;
	RETURN x[i] DIV Ten[j] MOD 10
END DecimalDigit;

PROCEDURE Set0*(VAR x: BigNum; v: INTEGER);
BEGIN ASSERT((v < Base) & (v >= 0)); x := Zero; x[0] := v
END Set0;

PROCEDURE Get0*(VAR x: BigNum): INTEGER;
RETURN x[0] MOD Base
END Get0;

PROCEDURE Add*(VAR res: BigNum; x, y: BigNum);
VAR i, r0, carry: INTEGER;
BEGIN carry := 0; i := 0;
	WHILE i < LEN(x) DO r0 := x[i] + y[i] + carry;
		IF r0 < Base THEN carry := 0 ELSE carry := 1; DEC(r0, Base) END;
		res[i] := r0; INC(i)
	END;
	ASSERT(res[LEN(res)-1] < Base)
END Add;

PROCEDURE Subtract*(VAR res: BigNum; x, y: BigNum);
VAR i, r0, borrow: INTEGER;
BEGIN
	borrow := 0; i := 0;
	WHILE i < LEN(x) DO
		r0 := x[i] - y[i] - borrow;
		IF r0 < 0 THEN borrow := 1; INC(r0, Base) ELSE borrow := 0 END;
		res[i] := r0; INC(i)
	END;
	ASSERT(res[LEN(res)-1] >= 0)
END Subtract;

PROCEDURE Complement*(VAR res: BigNum; x: BigNum);
VAR i, r0: INTEGER;
BEGIN
	res[0] := Base - x[0]; i := 1;
	WHILE i < LEN(x) DO
		res[i] := Base - x[i] - 1; INC(i)
	END
END Complement;

PROCEDURE DivideByTwo*(VAR res: BigNum; x: BigNum);
VAR i, r0, rem: INTEGER;
BEGIN
	i := LEN(x) - 1; rem := 0;
	WHILE i >= 0 DO
		r0 := x[i] + rem * Base; rem := r0 MOD 2; r0 := r0 DIV 2;
		res[i] := r0; DEC(i)
	END
END DivideByTwo;

PROCEDURE ModuloTwo*(x: BigNum): INTEGER;
RETURN x[0] MOD 2
END ModuloTwo;

PROCEDURE MultiplyByTen*(VAR res: BigNum; x: BigNum);
VAR i, c0, r0, carry: INTEGER;
BEGIN
	i := 0; carry := 0;
	WHILE i < LEN(x) DO
		r0 := x[i]; c0 := carry; carry := r0 DIV (Base DIV 10);
		res[i] := (r0 - carry * (Base DIV 10)) * 10 + c0; INC(i)
	END;
	ASSERT(x[LEN(x)-1] < Base)
END MultiplyByTen;

PROCEDURE DivideByTen*(VAR res: BigNum; x: BigNum);
VAR i, rem, r0: INTEGER;
BEGIN
	i := LEN(x) - 1; rem := 0;
	WHILE i >= 0 DO
		r0 := x[i] DIV 10 + rem * (Base DIV 10);
		rem := x[i] MOD 10; res[i] := r0; DEC(i)
	END
END DivideByTen;

PROCEDURE ModuloTen*(x: BigNum): INTEGER;
RETURN x[0] MOD 10
END ModuloTen;

PROCEDURE Compare*(x, y: BigNum): INTEGER;
VAR i, res: INTEGER;
BEGIN
	i := LEN(x) - 1; res := 0;
	WHILE (i >= 0) & (res = 0) DO
		IF x[i] < y[i] THEN res := -1 ELSIF x[i] > y[i] THEN res := 1 END;
		DEC(i)
	END;
	RETURN res
END Compare;

PROCEDURE Init;
VAR i: INTEGER; x: BigNum;
BEGIN
	i := 0; WHILE i < LEN(Zero) DO Zero[i] := 0; INC(i) END;
	One := Zero; One[0] := 1; Ten[0] := 1; i := 1;
	WHILE i < LEN(Ten) DO Ten[i] := Ten[i-1]*10; INC(i) END;
	i := 0; WHILE i < LEN(MaxNum) DO MaxNum[i] := Base - 1; INC(i) END
END Init;

BEGIN Init
END BigNums.
