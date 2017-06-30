MODULE Oberon07.Out;
(*$RTL-*)

IMPORT
	SYSTEM, Rtl, BigNums := [Patchouli.BigNums];

CONST
	STD_OUTPUT_HANDLE = -11;
	Kernel32 = 'Kernel32.dll';

TYPE
	Handle = INTEGER;
	Dword = SYSTEM.CARD32;
	Bool = SYSTEM.CARD32;

VAR
	GetStdHandle: PROCEDURE(nStdHandle: Dword): Handle;
	AllocConsole: PROCEDURE(): Bool;
	WriteFile: PROCEDURE(
		hFile, lpBuffer, nNumberOfBytesToWrite,
		lpNumberOfBytesWrite, lpOverlapped: INTEGER
	): Bool;
	
	wsprintfW: PROCEDURE(
		VAR lpOut: ARRAY [untagged] OF CHAR;
		lpFmt: ARRAY [untagged] OF CHAR;
		par1, par2: INTEGER
	): INTEGER;

PROCEDURE Open*;
	VAR hOut: INTEGER;
BEGIN
	hOut := GetStdHandle(STD_OUTPUT_HANDLE);
	IF hOut = 0 THEN ASSERT(AllocConsole() # 0) END
END Open;

PROCEDURE Char*(ch: CHAR);
	VAR utf8: ARRAY 8 OF BYTE; str: ARRAY 2 OF CHAR;
		i: INTEGER; bRes, dwByteWritten: INTEGER;
BEGIN
	str[0] := ch; str[1] := 0X; i := Rtl.UnicodeToUtf8(str, utf8);
	bRes := WriteFile(
		GetStdHandle(STD_OUTPUT_HANDLE), SYSTEM.ADR(utf8), i-1,
		SYSTEM.ADR(dwByteWritten), 0
	)
END Char;

PROCEDURE String*(str: ARRAY OF CHAR);
	VAR utf8: ARRAY 1024 OF BYTE;
		i: INTEGER; bRes, dwByteWritten: INTEGER;
BEGIN
	i := Rtl.UnicodeToUtf8(str, utf8); 
	bRes := WriteFile(
		GetStdHandle(STD_OUTPUT_HANDLE), SYSTEM.ADR(utf8), i-1,
		SYSTEM.ADR(dwByteWritten), 0
	)
END String;

PROCEDURE IntToDecStr(i: INTEGER; VAR str: ARRAY OF CHAR);
	VAR s: ARRAY 19 OF CHAR; j, k: INTEGER;
BEGIN
	IF i # 8000000000000000H THEN j := 0; k := 0;
		IF i < 0 THEN i := -i; str[k] := '-'; INC(k) END;
		REPEAT s[j] := CHR(ORD('0') + i MOD 10); i := i DIV 10; INC(j)
		UNTIL i = 0;
		WHILE j > 0 DO DEC(j); str[k] := s[j]; INC(k) END; str[k] := 0X
	ELSE str := '-9223372036854775808'
	END
END IntToDecStr;

PROCEDURE IntToHexStr(i: INTEGER; VAR str: ARRAY OF CHAR);
	VAR s: ARRAY 16 OF CHAR; j, k: INTEGER;
BEGIN
	j := 0; k := 0;
	REPEAT
		IF i MOD 16 < 10 THEN s[j] := CHR(ORD('0') + i MOD 16)
		ELSE s[j] := CHR(ORD('a') - 10 + i MOD 16)
		END;
		INC(j); i := i DIV 16
	UNTIL (i = 0) OR (i < 0) & (j = 16);
	WHILE j > 0 DO DEC(j); str[k] := s[j]; INC(k) END; str[k] := 0X
END IntToHexStr;

PROCEDURE Int*(i, n: INTEGER);
	VAR str: ARRAY 64 OF CHAR;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0)); IntToDecStr(i, str);
	i := 0; WHILE str[i] # 0X DO INC(i) END;
	IF i < n THEN str[n] := 0X; DEC(i); DEC(n);
		WHILE i >= 0 DO str[n] := str[i]; DEC(i); DEC(n) END;
		WHILE n >= 0 DO str[n] := 20X; DEC(n) END
	END;
	String(str)
END Int;

PROCEDURE Hex*(i, n: INTEGER);
	VAR str: ARRAY 64 OF CHAR;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0)); IntToHexStr(i, str);
	i := 0; WHILE str[i] # 0X DO INC(i) END;
	IF i < n THEN str[n] := 0X; DEC(i); DEC(n);
		WHILE i >= 0 DO str[n] := str[i]; DEC(i); DEC(n) END;
		WHILE n >= 0 DO str[n] := 20X; DEC(n) END
	END;
	String(str)
END Hex;

(*PROCEDURE BigNum*(x: BigNums.BigNum);
	VAR i, k: INTEGER; str: ARRAY BigNums.MaxDecimalDigits OF CHAR;
BEGIN
	i := BigNums.MaxDecimalDigits-1; k := 0;
	WHILE (i > 0) & (BigNums.DecimalDigit(x, i) = 0) DO DEC(i) END;
	REPEAT str[k] := CHR(BigNums.DecimalDigit(x, i)+30H); INC(k); DEC(i)
	UNTIL i < 0;
	str[k] := 0X; String(str)
END BigNum;*)

PROCEDURE RealToStr(x: REAL; VAR str: ARRAY OF CHAR);
	VAR
		x0, f, m0, m1, c, half, ten: BigNums.BigNum; i, e, u: INTEGER;
		quit: BOOLEAN; s: ARRAY 8 OF BYTE;
BEGIN
	i := SYSTEM.VAL(INTEGER, x);
	e := i DIV 10000000000000H MOD 2048 - 1023;
	i := i MOD 10000000000000H + 10000000000000H;
	BigNums.Set0(x0, i);
	
	f := BigNums.Zero; half := BigNums.Zero; m0 := BigNums.Zero;
	BigNums.SetDecimalDigit(half, BigNums.MaxDecimalDigits-1, 5); m1 := half;
	FOR i := 1 TO 52 DO
		BigNums.DivideByTwo(f, f); BigNums.DivideByTwo(m1, m1);
		IF BigNums.ModuloTwo(x0) = 1 THEN BigNums.Add(f, f, half) END;
		BigNums.DivideByTwo(x0, x0)
	END;
	
	WHILE e > 0 DO
		BigNums.Add(x0, x0, x0); BigNums.Add(m0, m0, m0);
		IF BigNums.Compare(f, half) >= 0 THEN
			BigNums.Add(x0, x0, BigNums.One);
			BigNums.Subtract(f, f, half)
		END;
		IF BigNums.Compare(m1, half) >= 0 THEN
			BigNums.Add(m0, m0, BigNums.One);
			BigNums.Subtract(m1, m1, half)
		END;
		BigNums.Add(f, f, f); BigNums.Add(m1, m1, m1); DEC(e)
	END;
	IF e < 0 THEN
		BigNums.DivideByTwo(f, f); BigNums.DivideByTwo(m1, m1);
		BigNums.Add(f, f, half); x0 := BigNums.Zero; INC(e)
	END;
	WHILE e < 0 DO
		BigNums.DivideByTwo(f, f); BigNums.DivideByTwo(m1, m1); INC(e)
	END;
	
	BigNums.Set0(ten, 10); e := 0;
	WHILE BigNums.Compare(x0, ten) >= 0 DO
		BigNums.DivideByTen(f, f); BigNums.DivideByTen(m1, m1);
		i := BigNums.ModuloTen(x0);
		IF i # 0 THEN
			BigNums.SetDecimalDigit(f, BigNums.MaxDecimalDigits-1, i)
		END;
		i := BigNums.ModuloTen(m0);
		IF i # 0 THEN
			BigNums.SetDecimalDigit(m1, BigNums.MaxDecimalDigits-1, i)
		END;
		BigNums.DivideByTen(x0, x0); BigNums.DivideByTen(m0, m0); INC(e)
	END;
	WHILE BigNums.Compare(x0, BigNums.Zero) = 0 DO
		i := BigNums.DecimalDigit(f, BigNums.MaxDecimalDigits-1);
		IF i # 0 THEN
			BigNums.SetDecimalDigit(x0, 0, i);
			BigNums.SetDecimalDigit(f, BigNums.MaxDecimalDigits-1, 0)
		END;
		BigNums.MultiplyByTen(f, f); BigNums.MultiplyByTen(m1, m1); DEC(e)
	END;
	
	str[0] := CHR(BigNums.ModuloTen(x0)+30H);
	str[1] := '.'; i := 2; quit := FALSE;
	REPEAT
		u := BigNums.DecimalDigit(f, BigNums.MaxDecimalDigits-1);
		BigNums.SetDecimalDigit(f, BigNums.MaxDecimalDigits-1, 0);
		BigNums.MultiplyByTen(f, f);
		IF BigNums.DecimalDigit(m1, BigNums.MaxDecimalDigits-1) = 0 THEN
			BigNums.MultiplyByTen(m1, m1); BigNums.Complement(c, m1);
			IF (BigNums.Compare(f, m1) >= 0) & (BigNums.Compare(f, c) <= 0)
			THEN str[i] := CHR(u+30H); INC(i)
			ELSE quit := TRUE
			END
		ELSE quit := TRUE
		END
	UNTIL quit;
	IF (BigNums.Compare(f, half) > 0)
	OR (BigNums.Compare(f, half) = 0) & ODD(u) THEN str[i] := CHR(u+1+30H)
	ELSE str[i] := CHR(u+30H)
	END; INC(i);
	
	IF e # 0 THEN
		str[i] := 'e'; INC(i);
		IF e > 0 THEN str[i] := '+' ELSE str[i] := '-'; e := -e END; INC(i);
		u := 0; REPEAT s[u] := e MOD 10; e := e DIV 10; INC(u) UNTIL e = 0;
		REPEAT DEC(u); str[i] := CHR(s[u]+30H); INC(i) UNTIL u = 0;
	END;
	str[i] := 0X
END RealToStr;

PROCEDURE Real*(x: REAL; n: INTEGER);
	CONST p = 52;
	VAR str, s: ARRAY 64 OF CHAR; ten: REAL;
		i, k, exp10, exp2, d, f, m: INTEGER; quit: BOOLEAN;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0));
	IF SYSTEM.VAL(INTEGER, x) = 0 THEN str := '0.0'; i := 3
	ELSIF SYSTEM.VAL(INTEGER, x) = 8000000000000000H THEN str := '-0.0'; i := 4
	ELSE RealToStr(x, str); i := 0; WHILE str[i] # 0X DO INC(i) END
	END;
	IF i < n THEN str[n] := 0X; DEC(i); DEC(n);
		WHILE i >= 0 DO str[n] := str[i]; DEC(i); DEC(n) END;
		WHILE n >= 0 DO str[n] := 20X; DEC(n) END
	END;
	String(str)
END Real;

PROCEDURE Ln*;
	VAR crlf: ARRAY 2 OF BYTE; dwByteWritten, bRes: INTEGER;
BEGIN
	crlf[0] := 13; crlf[1] := 10;
	bRes := WriteFile(
		GetStdHandle(STD_OUTPUT_HANDLE), SYSTEM.ADR(crlf), 2,
		SYSTEM.ADR(dwByteWritten), 0
	)
END Ln;

BEGIN
	Rtl.Import(GetStdHandle, Kernel32, 'GetStdHandle');
	Rtl.Import(AllocConsole, Kernel32, 'AllocConsole');
	Rtl.Import(WriteFile, Kernel32, 'WriteFile');
	Rtl.Import(wsprintfW, 'USER32.DLL', 'wsprintfW');
	Open
END Out.