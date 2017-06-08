MODULE Patchouli.BigNums;
CONST
	dSize = 30; (* 30-bit digit *)
	base = 1000000000;
TYPE BigNum* = ARRAY 16 OF INTEGER;

PROCEDURE Add*(VAR res: BigNum; x, y: BigNum);
CONST b30 = LSL(1, dSize);
VAR i, r0, x0, y0, carry, wp, bp: INTEGER;
BEGIN
	i := 0; WHILE i < LEN(BigNum) DO res[i] := 0; INC(i) END;
	carry := 0; i := 0; x0 := x[0] MOD b30; y0 := y[0] MOD b30;
	WHILE (x0 # 0) & (y0 # 0) DO
		r0 := x0 + y0 + carry;
		IF r0 >= base THEN carry := 1; DEC(r0, base) ELSE carry := 0 END;
		wp := i DIV 64; bp := i MOD 64; INC(res[wp], LSL(r0, bp));
		IF bp > 64 - dSize THEN r0 := ASR(r0, 64 - bp); INC(res[wp], r0) END;
		INC(i, dSize); wp := i DIV 64; bp := i MOD 64;
		IF bp <= 64 - dSize THEN
			x0 := ASR(x[wp], bp) MOD b30;
			y0 := ASR(y[wp], bp) MOD b30
		ELSE
		END
	END
END Add;

END BigNums.