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
	VAR str, s: ARRAY 64 OF CHAR; ten: REAL;
		i, k, exp, d: INTEGER;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0)); i := 0; exp := 0; ten := 1.0;
	IF x < 0.0 THEN str[i] := '-'; INC(i); x := -x END;
	WHILE ten * 10.0 <= x DO INC(exp); ten := ten * 10.0 END;
	WHILE x < 1.0 DO DEC(exp); x := x * 10.0 END; d := FLOOR(x / ten);
	str[i] := CHR(ORD('0') + d); INC(i); str[i] := '.'; INC(i);
	x := (x - FLT(d) * ten) * 10.0; k := 0;
	REPEAT
		d := FLOOR(x / ten); str[i] := CHR(ORD('0') + d); INC(i);
		INC(k); x := (x - FLT(d) * ten) * 10.0
	UNTIL (k = 16) OR (x = 0.0);
	str[i] := 'e'; INC(i);
	IF exp < 0 THEN str[i] := '-'; INC(i); exp := -exp END; k := 0;
	REPEAT s[k] := CHR(ORD('0') + exp MOD 10); exp := exp DIV 10; INC(k)
	UNTIL exp = 0;
	WHILE k > 0 DO DEC(k); str[i] := s[k]; INC(i) END; str[i] := 0X;
	
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