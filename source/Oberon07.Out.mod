MODULE Oberon07.Out;
(*$RTL-*)

IMPORT
	SYSTEM, Rtl;

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

PROCEDURE Real*(x: REAL; n: INTEGER);
	CONST p = 52;
	VAR str, s: ARRAY 64 OF CHAR; ten: REAL;
		i, k, exp10, exp2, d, f, m: INTEGER; quit: BOOLEAN;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0));
	IF x = 0.0 THEN str := '0.0'; i := 3
	ELSE i := 0; exp10 := 0; ten := 1.0;
		IF x < 0.0 THEN str[i] := '-'; INC(i); x := -x END;
		IF x >= 10.0 THEN
			REPEAT INC(exp10); ten := ten * 10.0 UNTIL x / ten < 10.0;
			x := x / ten
		ELSIF x < 1.0 THEN
			REPEAT DEC(exp10); ten := ten * 10.0 UNTIL x * ten >= 1.0;
			x := x * ten
		END;
		Char(0DX); Char(0AX);
		UNPK(x, exp2); f := SYSTEM.VAL(INTEGER, x) MOD LSL(1, p) + LSL(1, p);
		ASSERT((exp2 >= 0) & (exp2 <= 4)); f := LSL(f, exp2);
		d := ASR(f, p); DEC(f, LSL(d, p)); str[i] := CHR(ORD('0') + d);
		INC(i); str[i] := '.'; INC(i); m := 5; quit := FALSE;
		REPEAT
			String('f = '); Int(f, 0);
			f := f * 10; d := ASR(f, p); DEC(f, LSL(d, p));
			String('; d = '); Int(d, 0);
			String('; f2 = '); Int(f, 0);
			String('; m = '); Int(m, 0); Char(0DX); Char(0AX);
			IF (f >= m) & (f <= LSL(1, p) - m) THEN
				str[i] := CHR(ORD('0') + d); INC(i); m := m * 10
			ELSE quit := TRUE
			END 
		UNTIL quit;
		IF (f > LSL(1, p-1)) OR (f = LSL(1, p-1)) & (d MOD 2 = 1) THEN
			str[i] := CHR(ORD('0') + d + 1); INC(i)
		ELSE str[i] := CHR(ORD('0') + d); INC(i)
		END;
		
		str[i] := 'e'; INC(i);
		IF exp10 < 0 THEN str[i] := '-'; INC(i); exp10 := -exp10 END; k := 0;
		REPEAT
			s[k] := CHR(ORD('0') + exp10 MOD 10);
			exp10 := exp10 DIV 10; INC(k)
		UNTIL exp10 = 0;
		WHILE k > 0 DO DEC(k); str[i] := s[k]; INC(i) END; str[i] := 0X;
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