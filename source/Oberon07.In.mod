MODULE Oberon07.In;
IMPORT
	SYSTEM;
VAR
	GetStdHandle: PROCEDURE(nStdHandle: SYSTEM.CARD32): INTEGER;
	ReadConsoleW: PROCEDURE(
		hConsoleInput, lpBuffer: INTEGER;
		nNumberOfCharsToRead: SYSTEM.CARD32;
		lpNumberOfCharsRead, pInputControl: INTEGER
	): SYSTEM.CARD32;
	
	hIn: INTEGER; buf: CHAR; bufLen: INTEGER;
	Done*: BOOLEAN;
	
PROCEDURE Open*;
BEGIN
	hIn := GetStdHandle(-10); (* STD_INPUT_HANDLE *)
	ASSERT(hIn # -1); Done := TRUE
END Open;

PROCEDURE GetCh;
	VAR nRead: SYSTEM.CARD32;
BEGIN
	IF Done & (bufLen = 0) THEN
		Done :=
			(ReadConsoleW(hIn, SYSTEM.ADR(buf), 1, SYSTEM.ADR(nRead), 0) # 0)
			& (nRead = 1);
		IF Done THEN bufLen := 1 END
	END
END GetCh;

PROCEDURE Char*(VAR ch: CHAR);
BEGIN GetCh; IF bufLen = 1 THEN ch := buf; bufLen := 0 ELSE ch := 0X END
END Char;

PROCEDURE Ln*; (* skip until CRLF and consume them *)
BEGIN
	GetCh; WHILE (bufLen = 1) & (buf # 0AX) DO bufLen := 0; GetCh END;
	IF bufLen = 1 THEN bufLen := 0 END
END Ln;

PROCEDURE Int*(VAR i: INTEGER);
	CONST MaxInt = 7FFFFFFFFFFFFFFFH; MinInt = -MaxInt - 1;
	VAR neg: BOOLEAN; x: INTEGER;
BEGIN i := 0; GetCh; neg := FALSE;
	IF (bufLen = 1) & (buf = '-') THEN neg := TRUE; bufLen := 0; GetCh END;
	WHILE (bufLen = 1) & (buf >= '0') & (buf <= '9') DO
		x := ORD(buf) - ORD('0');
		Done := (~neg & ((MaxInt - x) DIV 10 >= i))
			OR (neg & ((MinInt + x) DIV 10 <= i)); (* Check overflow *)
		IF ~neg THEN i := i * 10 + x ELSE i := i * 10 - x END;
		bufLen := 0; GetCh
	END
END Int;
	
PROCEDURE Init;
	VAR kern32: INTEGER;
	PROCEDURE Import(
		VAR proc: ARRAY OF SYSTEM.BYTE; lib: INTEGER; name: ARRAY OF CHAR
	);
		VAR procAdr, i: INTEGER; byteStr: ARRAY 256 OF BYTE;
	BEGIN
		IF lib # 0 THEN i := 0;
			WHILE name[i] # 0X DO
				byteStr[i] := ORD(name[i]); INC(i)
			END; byteStr[i] := 0;
			SYSTEM.GetProcAddress(procAdr, lib, SYSTEM.ADR(byteStr));
			SYSTEM.PUT(SYSTEM.ADR(proc), procAdr)
		ELSE SYSTEM.PUT(SYSTEM.ADR(proc), 0)
		END
	END Import;
BEGIN (* Init *)
	SYSTEM.LoadLibraryW(kern32, 'kernel32.dll');
	Import(GetStdHandle, kern32, 'GetStdHandle');
	Import(ReadConsoleW, kern32, 'ReadConsoleW')
END Init;
BEGIN Init; Open
END In.