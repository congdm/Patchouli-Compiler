MODULE Sys;

IMPORT
	Out := [Oberon07.Out], Files := [Oberon07.Files];

CONST
	SizeInt* = 8;
	SizeReal* = 8;
	ExpBias = 1023;
	ExpMax = 2047;

TYPE
	Int* = RECORD 
		data: ARRAY SizeInt OF BYTE
	END ;
	Real* = RECORD
		data: ARRAY SizeReal OF BYTE
	END ;
	Decimal* = RECORD
		data: ARRAY 21 OF BYTE
	END ;

	Char* = RECORD
		v: BYTE
	END ;
	
	File* = RECORD
		f: Files.File;
		r: Files.Rider
	END ;

VAR
	MaxInt*, MinInt*: Int;
	IntZero*, IntOne*, IntMinusOne*: Int;
	MaxUnicode: Int;
	MaxDecimalExp, MinDecimalExp: Int;
	
	IntPowOfTwo*: ARRAY 64 OF Int;
	Unicode80: Int;
	Unicode800: Int;
	Unicode10000: Int;
	Unicode110000: Int;
	UnicodeD800, UnicodeE000: Int;


PROCEDURE CopyIntToReal(x: Int; VAR r: Real);
	VAR i: INTEGER;
BEGIN
	ASSERT(SizeInt = SizeReal);
	r.data := x.data
END CopyIntToReal;

PROCEDURE CopyRealToInt(r: Real; VAR x: Int);
	VAR i: INTEGER;
BEGIN
	ASSERT(SizeInt = SizeReal);
	x.data := r.data
END CopyRealToInt;

PROCEDURE ZeroReal(VAR r: Real);
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(r.data)-1 DO r.data[i] := 0 END
END ZeroReal;

PROCEDURE IsZeroInt(x: Int): BOOLEAN;
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(x.data)) & (x.data[i] = 0) DO INC(i) END ;
	RETURN i = LEN(x.data)
END IsZeroInt;

PROCEDURE CmpUInt(x, y: Int): INTEGER;
	VAR i, res: INTEGER;
BEGIN
	i := LEN(x.data)-1;
	WHILE (i > 0) & (x.data[i] = y.data[i]) DO DEC(i) END ;
	IF x.data[i] > y.data[i] THEN res := 1
	ELSIF x.data[i] < y.data[i] THEN res := -1
	ELSE res := 0
	END
	RETURN res
END CmpUInt;

PROCEDURE GetSign(r: Real): BOOLEAN;
BEGIN
	RETURN r.data[LEN(r.data)-1] >= 128
END GetSign;

PROCEDURE GetExp(r: Real): INTEGER;
BEGIN
	RETURN (r.data[LEN(r.data)-1] MOD 128)*16 + r.data[LEN(r.data)-2] DIV 16
END GetExp;

PROCEDURE GetMant(r: Real; VAR m: Int);
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(m.data)-1 DO m.data[i] := 0 END ;
	FOR i := 0 TO LEN(r.data)-3 DO m.data[i] := r.data[i] END ;
	m.data[LEN(m.data)-2] := r.data[LEN(r.data)-2] MOD 16
END GetMant;

PROCEDURE SetMant(VAR r: Real; m: Int);
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(r.data)-3 DO r.data[i] := m.data[i] END ;
	r.data[LEN(r.data)-2] := (r.data[LEN(r.data)-2] DIV 16) * 16 + (m.data[LEN(m.data)-2] MOD 16)
END SetMant;

PROCEDURE SetExp(VAR r: Real; e: INTEGER);
	VAR hi, lo: INTEGER;
BEGIN
	hi := e DIV 16; lo := e MOD 16;
	r.data[LEN(r.data)-1] := (r.data[LEN(r.data)-1] DIV 128)*128 + hi;
	r.data[LEN(r.data)-2] := (r.data[LEN(r.data)-2] MOD 16) + lo*16
END SetExp;

PROCEDURE IsZeroReal(r: Real): BOOLEAN;
	VAR m: Int;
		res: BOOLEAN;
BEGIN
	IF GetExp(r) # 0 THEN res := FALSE
	ELSE GetMant(r, m); res := IsZeroInt(m)
	END ;
	RETURN res
END IsZeroReal;

PROCEDURE SetBit52(VAR m: Int);
BEGIN
	IF m.data[LEN(m.data)-2] < 16 THEN m.data[LEN(m.data)-2] := m.data[LEN(m.data)-2] + 16 END
END SetBit52;

PROCEDURE ClearBit52(VAR m: Int);
BEGIN
	IF m.data[LEN(m.data)-2] >= 16 THEN m.data[LEN(m.data)-2] := m.data[LEN(m.data)-2] - 16 END
END ClearBit52;

PROCEDURE HighestBit(x: Int): INTEGER;
	VAR i, b, bit, res: INTEGER;
BEGIN
	i := LEN(x.data)-1;
	WHILE (i >= 0) & (x.data[i] = 0) DO DEC(i) END ;
	IF i < 0 THEN res := -1
	ELSE
		b := x.data[i]; bit := 7;
		WHILE (bit > 0) & (b < (1 SHL bit)) DO DEC(bit) END ;
		res := i*8 + bit
	END ;
	RETURN res
END HighestBit;

PROCEDURE ShiftRightSafe(VAR x: Int; shfCnt: INTEGER);
BEGIN
	IF shfCnt >= 8*LEN(x.data) THEN x := IntZero
	ELSE LShiftRight0(x, shfCnt)
	END
END ShiftRightSafe;

PROCEDURE PackReal(sign: BOOLEAN; exp: INTEGER; mant: Int; VAR r: Real);
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(r.data)-1 DO r.data[i] := 0 END ;
	SetMant(r, mant);
	SetExp(r, exp);
	IF sign THEN r.data[LEN(r.data)-1] := r.data[LEN(r.data)-1] + 128 END
END PackReal;

PROCEDURE NormalizeMant(VAR mant: Int; VAR exp: INTEGER);
BEGIN
	WHILE CmpUInt(mant, IntPowOfTwo[53]) >= 0 DO
		LShiftRight0(mant, 1); INC(exp)
	END ;
	WHILE (CmpUInt(mant, IntPowOfTwo[52]) < 0) & (exp > 1) DO
		LShiftLeft0(mant, 1); DEC(exp)
	END ;
	IF (exp = 1) & (CmpUInt(mant, IntPowOfTwo[52]) < 0) THEN exp := 0 END ;
	IF exp <= 0 THEN
		LShiftRight0(mant, 1-exp);
		exp := 0
	END ;
	IF exp >= ExpMax THEN
		mant := IntZero; exp := ExpMax
	END
END NormalizeMant;

PROCEDURE RealMul10(VAR r: Real);
	VAR s: BOOLEAN; e: INTEGER; m: Int;
BEGIN
	IF ~IsZeroReal(r) THEN
		s := GetSign(r); e := GetExp(r);
		GetMant(r, m);
		IF e > 0 THEN SetBit52(m) ELSE e := 1 END ;
		MulIntByte(m, 10);
		NormalizeMant(m, e);
		IF e > 0 THEN ClearBit52(m) END ;
		PackReal(s, e, m, r)
	END
END RealMul10;

PROCEDURE RealDiv10(VAR r: Real);
	VAR s: BOOLEAN; e: INTEGER; m: Int; rem: INTEGER;
BEGIN
	IF ~IsZeroReal(r) THEN
		s := GetSign(r); e := GetExp(r);
		GetMant(r, m);
		IF e > 0 THEN SetBit52(m) ELSE e := 1 END ;
		DivIntByte(m, 10, rem);
		NormalizeMant(m, e);
		IF e > 0 THEN ClearBit52(m) END ;
		PackReal(s, e, m, r)
	END
END RealDiv10;
	
PROCEDURE INTEGERToInt*(x: INTEGER; VAR res: Int);
	VAR i: INTEGER;
BEGIN i := 0; res := IntZero;
	WHILE (x # 0) & (i < LEN(res.data)) DO
		res.data[i] := x MOD 256; x := x DIV 256; INC(i)
	END
END INTEGERToInt;
	
PROCEDURE SignInt*(x: Int): BOOLEAN;
	RETURN x.data[LEN(x.data)-1] > 127
END SignInt;
	
PROCEDURE CmpInt*(x, y: Int): INTEGER;
	VAR res, i: INTEGER;
BEGIN
	IF SignInt(x) = SignInt(y) THEN
		i := LEN(x.data)-1;
		WHILE (i > 0) & (x.data[i] = y.data[i]) DO DEC(i) END ;
		IF x.data[i] > y.data[i] THEN res := 1
		ELSIF x.data[i] < y.data[i] THEN res := -1
		ELSE res := 0
		END
	ELSIF SignInt(x) THEN res := -1
	ELSE res := 1
	END ;
	RETURN res
END CmpInt;

(* ======================================================================== *)
(* Op Int Byte *)

PROCEDURE CmpIntByte*(x: Int; y: BYTE): INTEGER;
	VAR res, i: INTEGER; foundHigh: BOOLEAN;
BEGIN
	IF SignInt(x) THEN res := -1
	ELSE
		i := LEN(x.data)-1;
		foundHigh := FALSE;
		WHILE (i > 0) & ~foundHigh DO
			IF x.data[i] # 0 THEN res := 1; foundHigh := TRUE
			ELSE DEC(i)
			END
		END ;
		IF ~foundHigh THEN
			IF x.data[0] > y THEN res := 1
			ELSIF x.data[0] < y THEN res := -1
			ELSE res := 0
			END
		END
	END ;
	RETURN res
END CmpIntByte;

PROCEDURE AddIntByte*(VAR x: Int; y: BYTE);
	VAR i, carry, t: INTEGER;
BEGIN
	i := 0; carry := y;
	WHILE (i < LEN(x.data)) & (carry # 0) DO
		t := x.data[i] + carry;
		IF t <= 255 THEN x.data[i] := t; carry := 0
		ELSE x.data[i] := t - 256; carry := 1
		END ;
		INC(i)
	END
END AddIntByte;

PROCEDURE SubIntByte*(VAR x: Int; y: BYTE);
	VAR i, borrow, t: INTEGER;
BEGIN
	i := 0; borrow := y;
	WHILE (i < LEN(x.data)) & (borrow # 0) DO
		t := x.data[i] - borrow;
		IF t >= 0 THEN x.data[i] := t; borrow := 0
		ELSE x.data[i] := t + 256; borrow := 1
		END ;
		INC(i)
	END
END SubIntByte;

PROCEDURE MulIntByte*(VAR x: Int; y: BYTE);
	VAR i, carry, t: INTEGER;
BEGIN
	i := 0; carry := 0;
	WHILE i < LEN(x.data) DO
		t := x.data[i]*y + carry;
		x.data[i] := t MOD 256; carry := t DIV 256;
		INC(i)
	END
END MulIntByte;

PROCEDURE DivIntByte*(VAR x: Int; y: BYTE; VAR rem: INTEGER);
	VAR i, t: INTEGER;
BEGIN
	rem := 0; i := LEN(x.data)-1;
	WHILE i >= 0 DO
		t := rem*256 + x.data[i];
		x.data[i] := t DIV y; rem := t MOD y;
		DEC(i)
	END
END DivIntByte;

PROCEDURE AndByte*(x: BYTE; y: BYTE): BYTE;
	VAR i, bitx, bity, k, res: INTEGER;
BEGIN k := 1; res := 0;
	FOR i := 1 TO 8 DO
		bitx := x MOD 2; bity := y MOD 2;
		x := x DIV 2; y := y DIV 2;
		bitx := bitx * bity;
		INC(res, bitx * k); k := k*2
	END ;
	RETURN res
END AndByte;

PROCEDURE OrByte*(x: BYTE; y: BYTE): BYTE;
	VAR i, bitx, bity, k, res: INTEGER;
BEGIN k := 1; res := 0;
	FOR i := 1 TO 8 DO
		bitx := x MOD 2; bity := y MOD 2;
		x := x DIV 2; y := y DIV 2;
		IF bitx + bity > 0 THEN bitx := 1 ELSE bitx := 0 END ;
		INC(res, bitx * k); k := k*2
	END ;
	RETURN res
END OrByte;

(* ======================================================================== *)

PROCEDURE LShiftLeftOne(VAR x: Int);
	VAR i, t, carry: INTEGER;
BEGIN
	i := 0; carry := 0;
	WHILE i < LEN(x.data) DO
		t := x.data[i] * 2;
		IF t > 255 THEN x.data[i] := t - 256 + carry; carry := 1
		ELSE x.data[i] := t + carry; carry := 0
		END ;
		INC(i)
	END
END LShiftLeftOne;

PROCEDURE LShiftRightOne(VAR x: Int);
	VAR i, t, carry: INTEGER; odd: BOOLEAN;
BEGIN
	i := LEN(x.data)-1; carry := 0;
	WHILE i >= 0 DO
		odd := ODD(x.data[i]); x.data[i] := x.data[i] DIV 2 + carry*128;
		IF odd THEN carry := 1 ELSE carry := 0 END ;
		DEC(i)
	END
END LShiftRightOne;

PROCEDURE AShiftRightOne(VAR x: Int);
	VAR i, t, carry: INTEGER; odd: BOOLEAN;
BEGIN
	i := LEN(x.data)-1;
	IF x.data[i] < 128 THEN carry := 0 ELSE carry := 1 END ;
	WHILE i >= 0 DO
		odd := ODD(x.data[i]); x.data[i] := x.data[i] DIV 2 + carry*128;
		IF odd THEN carry := 1 ELSE carry := 0 END ;
		DEC(i)
	END
END AShiftRightOne;

PROCEDURE LShiftLeft0*(VAR x: Int; shfCnt: INTEGER);
BEGIN
	IF shfCnt < 0 THEN (*nothing*)
	ELSIF shfCnt < 8*LEN(x.data) THEN
		WHILE shfCnt > 0 DO LShiftLeftOne(x); DEC(shfCnt) END
	ELSE x := IntZero
	END
END LShiftLeft0;

PROCEDURE LShiftLeft*(VAR x: Int; n: Int);
	VAR shfCnt: Int;
BEGIN
	IF SignInt(n) THEN (*nothing*)
	ELSIF CmpIntByte(n, 8*LEN(x.data)) < 0 THEN
		ASSERT(8*LEN(x.data) < 256);
		LShiftLeft0(x, n.data[0])
	ELSE x := IntZero
	END
END LShiftLeft;

PROCEDURE LShiftRight0*(VAR x: Int; shfCnt: INTEGER);
BEGIN
	IF shfCnt < 0 THEN (*nothing*)
	ELSIF shfCnt < 8*LEN(x.data) THEN
		WHILE shfCnt > 0 DO LShiftRightOne(x); DEC(shfCnt) END
	ELSE x := IntZero
	END
END LShiftRight0;

PROCEDURE LShiftRight*(VAR x: Int; n: Int);
BEGIN
	IF SignInt(n) THEN (*nothing*)
	ELSIF CmpIntByte(n, 8*LEN(x.data)) < 0 THEN
		ASSERT(8*LEN(x.data) < 256);
		LShiftRight0(x, n.data[0])
	ELSE x := IntZero
	END
END LShiftRight;

PROCEDURE AShiftRight0*(VAR x: Int; shfCnt: INTEGER);
	VAR i, t, carry: INTEGER; odd: BOOLEAN;
BEGIN
	IF shfCnt < 0 THEN (*nothing*)
	ELSIF shfCnt < 8*LEN(x.data) THEN
		WHILE shfCnt > 0 DO AShiftRightOne(x); DEC(shfCnt) END
	ELSIF SignInt(x) THEN x := IntMinusOne
	ELSE x := IntZero
	END
END AShiftRight0;

PROCEDURE AShiftRight*(VAR x: Int; n: Int);
BEGIN
	IF SignInt(n) THEN (*nothing*)
	ELSIF CmpIntByte(n, 8*LEN(x.data)) < 0 THEN
		ASSERT(8*LEN(x.data) < 256);
		AShiftRight0(x, n.data[0])
	ELSIF SignInt(x) THEN x := IntMinusOne
	ELSE x := IntZero
	END
END AShiftRight;
	
PROCEDURE AddInt*(VAR x: Int; y: Int);
	VAR i, carry, t: INTEGER;
BEGIN carry := 0; i := 0;
	WHILE i < LEN(x.data) DO
		t := x.data[i] + y.data[i] + carry;
		IF t <= 255 THEN carry := 0; x.data[i] := t
		ELSE carry := 1; x.data[i] := t - 256
		END ;
		INC(i)
	END
END AddInt;

PROCEDURE NegInt*(VAR x: Int);
	VAR i, carry, t: INTEGER;
BEGIN i := 0; carry := 1;
	WHILE i < LEN(x.data) DO
		t := 255 - x.data[i] + carry;
		IF t > 255 THEN x.data[i] := 0; carry := 1 ELSE x.data[i] := t; carry := 0 END ;
		INC(i)
	END
END NegInt;

PROCEDURE SubInt*(VAR x: Int; y: Int);
	VAR z: Int;
BEGIN
	z := y; NegInt(z); AddInt(x, z)
END SubInt;

PROCEDURE AndInt*(VAR x: Int; y: Int);
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE i < LEN(x.data) DO x.data[i] := AndByte(x.data[i], y.data[i]); INC(i) END
END AndInt;

PROCEDURE OrInt*(VAR x: Int; y: Int);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE i < LEN(x.data) DO x.data[i] := OrByte(x.data[i], y.data[i]); INC(i) END
END OrInt;

PROCEDURE MulInt*(VAR x: Int; y: Int);
	VAR ax, ay, res, tmp: Int; i: INTEGER; neg: BOOLEAN;
BEGIN
	ax := x; ay := y; neg := FALSE;
	IF SignInt(ax) THEN NegInt(ax); neg := ~neg END ;
	IF SignInt(ay) THEN NegInt(ay); neg := ~neg END ;
	res := IntZero;
	FOR i := 0 TO LEN(ay.data)-1 DO
		IF ay.data[i] # 0 THEN
			tmp := ax; MulIntByte(tmp, ay.data[i]);
			LShiftLeft0(tmp, i*8);
			AddInt(res, tmp)
		END
	END ;
	IF neg THEN NegInt(res) END ;
	x := res
END MulInt;

PROCEDURE RotRight*(VAR x: Int; n: Int);
	VAR shfCnt: INTEGER; hi, lo: Int;
BEGIN
	shfCnt := n.data[0] MOD (8*LEN(x.data));
	IF shfCnt # 0 THEN
		hi := x; lo := x;
		LShiftRight0(hi, shfCnt);
		LShiftLeft0(lo, 8*LEN(x.data) - shfCnt);
		OrInt(hi, lo); x := hi
	END
END RotRight;

PROCEDURE ModIntByte*(VAR x: Int; y: BYTE);
	VAR t: Int; rem: INTEGER;
BEGIN
	t := x; DivIntByte(t, y, rem);
	x := IntZero; x.data[0] := rem
END ModIntByte;

PROCEDURE AbsReal*(VAR x: Real);
BEGIN
	IF x.data[LEN(x.data)-1] >= 128 THEN x.data[LEN(x.data)-1] := x.data[LEN(x.data)-1] - 128 END
END AbsReal;

PROCEDURE FloorReal*(VAR x: Real);
	VAR s: BOOLEAN; e, i, fracBits: INTEGER; m: Int; hadFrac: BOOLEAN; one: Real;
BEGIN
	IF (GetExp(x) # ExpMax) & ~IsZeroReal(x) THEN
		s := GetSign(x); e := GetExp(x);
		IF e = 0 THEN
			IF s THEN IntToReal(IntMinusOne, x) ELSE ZeroReal(x) END
		ELSE
			DEC(e, ExpBias);
			IF e < 0 THEN
				IF s THEN IntToReal(IntMinusOne, x) ELSE ZeroReal(x) END
			ELSIF e < 52 THEN
				GetMant(x, m);
				fracBits := 52 - e;
				hadFrac := FALSE;
				i := 0;
				WHILE i < fracBits DO
					IF (m.data[i DIV 8] DIV (1 SHL (i MOD 8))) MOD 2 = 1 THEN
						hadFrac := TRUE
					END ;
					m.data[i DIV 8] := m.data[i DIV 8] - ((m.data[i DIV 8] DIV (1 SHL (i MOD 8))) MOD 2) * (1 SHL (i MOD 8));
					INC(i)
				END ;
				PackReal(s, e + ExpBias, m, x);
				IF s & hadFrac THEN
					IntToReal(IntMinusOne, one);
					AddReal(x, one)
				END
			END
		END
	END
END FloorReal;

PROCEDURE AddReal*(VAR x: Real; y: Real);
	VAR sx, sy, s: BOOLEAN; ex, ey, e: INTEGER;
		mx, my, tmp: Int; done: BOOLEAN;
BEGIN
	done := FALSE;
	ex := GetExp(x); ey := GetExp(y);
	IF ex = ExpMax THEN
		GetMant(x, mx);
		IF ~IsZeroInt(mx) THEN
			done := TRUE (* x is NaN *)
		ELSE
			(* x is Inf *)
			IF ey = ExpMax THEN
				GetMant(y, my);
				IF ~IsZeroInt(my) THEN x := y; done := TRUE (* y is NaN *)
				ELSIF GetSign(x) # GetSign(y) THEN
					mx := IntZero; mx.data[0] := 1; (* quiet NaN payload *)
					PackReal(FALSE, ExpMax, mx, x);
					done := TRUE
				ELSE
					done := TRUE
				END
			ELSE
				done := TRUE
			END
		END
	ELSIF ey = ExpMax THEN
		GetMant(y, my);
		x := y; done := TRUE
	END ;

	IF ~done THEN
		IF IsZeroReal(x) THEN x := y
		ELSIF IsZeroReal(y) THEN
		ELSE
			sx := GetSign(x); sy := GetSign(y);
			GetMant(x, mx); GetMant(y, my);
			IF ex > 0 THEN SetBit52(mx) ELSE ex := 1 END ;
			IF ey > 0 THEN SetBit52(my) ELSE ey := 1 END ;
			IF ex > ey THEN
				LShiftRight0(my, ex - ey); e := ex
			ELSIF ey > ex THEN
				LShiftRight0(mx, ey - ex); e := ey
			ELSE e := ex
			END ;
			IF sx = sy THEN
				AddInt(mx, my); s := sx
			ELSE
				IF CmpUInt(mx, my) >= 0 THEN
					tmp := mx; SubInt(tmp, my); mx := tmp; s := sx
				ELSE
					tmp := my; SubInt(tmp, mx); mx := tmp; s := sy
				END
			END ;
			IF IsZeroInt(mx) THEN ZeroReal(x)
			ELSE
				NormalizeMant(mx, e);
				IF e > 0 THEN ClearBit52(mx) END ;
				PackReal(s, e, mx, x)
			END
		END
	END
END AddReal;

PROCEDURE CodepointToStr*(x: Int; VAR res: ARRAY OF Char);
	VAR u, tmp, t80, t800, t10000, t110000: Int;
		len, b: INTEGER; ok: BOOLEAN;
BEGIN
	u := x; len := 0; ok := ~SignInt(u);
	IF ok THEN
		IF CmpInt(u, Unicode80) < 0 THEN len := 1
		ELSIF CmpInt(u, Unicode800) < 0 THEN len := 2
		ELSIF CmpInt(u, Unicode10000) < 0 THEN
			ok := (CmpInt(u, UnicodeD800) < 0) OR (CmpInt(u, UnicodeE000) >= 0);
			len := 3
		ELSIF CmpInt(u, Unicode110000) < 0 THEN len := 4
		ELSE ok := FALSE
		END
	END ;
	IF ~ok OR (LEN(res) < len+1) THEN
		res[0].v := 0
	ELSIF len = 1 THEN
		res[0].v := u.data[0] MOD 128; res[1].v := 0
	ELSIF len = 2 THEN
		tmp := u; LShiftRight0(tmp, 6); b := tmp.data[0] MOD 32;
		res[0].v := 192 + b;
		res[1].v := 128 + u.data[0] MOD 64; res[2].v := 0
	ELSIF len = 3 THEN
		tmp := u; LShiftRight0(tmp, 12); b := tmp.data[0] MOD 16;
		res[0].v := 224 + b;
		tmp := u; LShiftRight0(tmp, 6); b := tmp.data[0] MOD 64;
		res[1].v := 128 + b;
		res[2].v := 128 + (u.data[0] MOD 64); res[3].v := 0
	ELSE
		tmp := u; LShiftRight0(tmp, 18); b := tmp.data[0] MOD 8;
		res[0].v := 240 + b;
		tmp := u; LShiftRight0(tmp, 12); b := tmp.data[0] MOD 64;
		res[1].v := 128 + b;
		tmp := u; LShiftRight0(tmp, 6); b := tmp.data[0] MOD 64;
		res[2].v := 128 + b;
		res[3].v := 128 + (u.data[0] MOD 64); res[4].v := 0
	END
END CodepointToStr;

PROCEDURE CharToInt*(ch: Char; VAR res: Int);
BEGIN res := IntZero; res.data[0] := ch.v
END CharToInt;

PROCEDURE ByteToInt*(x: BYTE; VAR res: Int);
BEGIN res := IntZero; res.data[0] := x
END ByteToInt;

PROCEDURE DecToInt*(
	x: Decimal; len: INTEGER; VAR res: Int
): BOOLEAN;
	VAR i: INTEGER; t: Int; success: BOOLEAN;
BEGIN
	res := IntZero; success := TRUE;
	i := 0;
	WHILE (i < len) & success DO
		t := res; MulIntByte(t, 10); AddIntByte(t, x.data[i]);
		IF CmpInt(t, MaxInt) > 0 THEN
			success := FALSE; res := IntZero
		ELSE res := t
		END ;
		INC(i)
	END ;
	RETURN success
END DecToInt;

PROCEDURE DecToReal*(
	x: Decimal; intLen: INTEGER;
	f: Decimal; fracLen: INTEGER;
	e: Int; VAR res: Real
);
	VAR i: INTEGER; num, tmp: Int;
BEGIN
	num := IntZero;
	FOR i := 0 TO intLen-1 DO
		tmp := num; MulIntByte(tmp, 10); AddIntByte(tmp, x.data[i]);
		num := tmp
	END ;
	FOR i := 0 TO fracLen-1 DO
		tmp := num; MulIntByte(tmp, 10); AddIntByte(tmp, f.data[i]);
		num := tmp
	END ;
	IntToReal(num, res);
	FOR i := 1 TO fracLen DO RealDiv10(res) END ;
	IF ~SignInt(e)
		IF CmpInt(e, MaxDecimalExp) > 0 THEN e := MaxDecimalExp END ;
		WHILE CmpInt(e, 0) > 0 DO RealMul10(res); SubIntByte(e, 1) END
	ELSE
		IF CmpInt(e, MinDecimalExp) < 0 THEN e := MinDecimalExp END ;
		WHILE SignInt(e) DO RealDiv10(res); AddIntByte(e, 1) END
	END
END DecToReal;

PROCEDURE HexToReal*(x: Int; VAR res: Real);
BEGIN
	CopyIntToReal(x, res)
END HexToReal;

PROCEDURE IntToReal*(x: Int; VAR res: Real);
	VAR ax, mant: Int; pos, exp: INTEGER; s: BOOLEAN;
BEGIN
	IF IsZeroInt(x) THEN ZeroReal(res)
	ELSE
		ax := x; s := SignInt(ax);
		IF s THEN NegInt(ax) END ;
		pos := HighestBit(ax);
		exp := pos + ExpBias;
		mant := ax;
		IF pos < 52 THEN LShiftLeft0(mant, 52 - pos)
		ELSIF pos > 52 THEN LShiftRight0(mant, pos - 52)
		END ;
		ClearBit52(mant);
		PackReal(s, exp, mant, res)
	END
END IntToReal;

(* -------------------------------------------------------------------------- *)
(* File I/O. Standard I/O *)

PROCEDURE OpenFile*(VAR f: File; fname: ARRAY OF CHAR);
BEGIN
	f.f := Files.Old(fname);
	Files.Set(f.r, f.f, 0)
END OpenFile;

PROCEDURE CloseFile*(VAR f: File);
BEGIN
	Files.Close(f.f)
END CloseFile;

PROCEDURE SetFilePos*(VAR f: File; pos: INTEGER);
BEGIN
	Files.Set(f.r, f.f, pos)
END SetFilePos;

PROCEDURE FileEOF*(f: File): BOOLEAN;
	RETURN f.r.eof
END FileEOF;

PROCEDURE FilePos*(VAR f: File): INTEGER;
	RETURN Files.Pos(f.r)
END FilePos;

PROCEDURE FileReadCHAR*(VAR f: File; VAR ch: CHAR);
BEGIN
	Files.ReadChar(f.r, ch)
END FileReadCHAR;

PROCEDURE Write*(str: ARRAY OF CHAR);
BEGIN
	Out.String(str)
END Write;

PROCEDURE WriteLn*(str: ARRAY OF CHAR);
BEGIN
	Out.String(str); Out.Ln
END WriteLn;

PROCEDURE Init;
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO LEN(IntZero.data)-1 DO IntZero.data[i] := 0 END ;
	IntOne := IntZero; IntOne.data[0] := 1;
	FOR i := 0 TO LEN(IntMinusOne.data)-1 DO IntMinusOne.data[i] := 255 END ;
	IntPowOfTwo[0] := IntOne;
	FOR i := 1 TO LEN(IntPowOfTwo)-1 DO
		IntPowOfTwo[i] := IntPowOfTwo[i-1];
		MulIntByte(IntPowOfTwo[i], 2);
	END ;
	i := 0;
	WHILE i < LEN(MaxInt.data)-1 DO MaxInt.data[i] := 255; INC(i) END ; MaxInt.data[i] := 127; 
	MinInt := IntZero; MinInt.data[LEN(MinInt.data)-1] := 128;

	Unicode80 := IntPowOfTwo[7];
	Unicode800 := IntPowOfTwo[11];
	Unicode10000 := IntPowOfTwo[16];
	Unicode110000 := IntZero;
	Unicode110000.data[2] := 11H;
	UnicodeD800 := IntZero;
	UnicodeD800.data[1] := D8H;
	UnicodeE000 := IntZero;
	UnicodeE000.data[1] := E0H;

	MaxDecimalExp := IntZero;
	MaxDecimalExp.data[0] := 308 MOD 256;
	MaxDecimalExp.data[1] := 308 DIV 256;
	MinDecimalExp := MaxDecimalExp;	NegInt(MinDecimalExp)
END Init;

BEGIN Init
END Sys.
