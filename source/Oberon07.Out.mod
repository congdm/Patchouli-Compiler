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

PROCEDURE Int*(i, n: INTEGER);
	VAR str: ARRAY 64 OF CHAR;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0)); i := wsprintfW(str, "%ld", i, 0);
	IF i < n THEN str[n] := 0X; DEC(i); DEC(n);
		WHILE i >= 0 DO str[n] := str[i]; DEC(i); DEC(n) END;
		WHILE n >= 0 DO str[n] := 20X; DEC(n) END
	END;
	String(str)
END Int;

PROCEDURE Hex*(i, n: INTEGER);
	VAR str: ARRAY 64 OF CHAR;
BEGIN
	ASSERT((n < LEN(str)) & (n >= 0)); i := wsprintfW(str, "%lx", i, 0);
	IF i < n THEN str[n] := 0X; DEC(i); DEC(n);
		WHILE i >= 0 DO str[n] := str[i]; DEC(i); DEC(n) END;
		WHILE n >= 0 DO str[n] := 20X; DEC(n) END
	END;
	String(str)
END Hex;

(*PROCEDURE Real (x: REAL; n: INTEGER);*)

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