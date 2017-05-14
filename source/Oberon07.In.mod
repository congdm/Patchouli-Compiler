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
	Done := Done
		& (ReadConsoleW(hIn, SYSTEM.ADR(buf), 1, SYSTEM.ADR(nRead), 0) # 0)
		& (nRead = 1);
	IF Done THEN bufLen := 1 ELSE bufLen := 0 END
END GetCh;

PROCEDURE Char*(VAR ch: CHAR);
BEGIN
	IF Done THEN
		IF bufLen = 0 THEN GetCh END;
		IF bufLen = 1 THEN bufLen := 0; ch := buf END
	END
END Char;

PROCEDURE Ln*; (* skip until CRLF and consume them *)
BEGIN
	IF Done THEN
		IF bufLen = 0 THEN GetCh END;
		WHILE (bufLen = 1) & (buf # 0AX) DO GetCh END;
		IF bufLen = 1 THEN bufLen := 0 END
	END
END Ln;

PROCEDURE IsBlank(ch: CHAR): BOOLEAN;
	RETURN (ch = ' ') OR (ch = 9X) OR (ch = 0AX) OR (ch = 0DX)
END IsBlank;

PROCEDURE IsDigit(ch: CHAR): BOOLEAN;
	RETURN (ch >= '0') & (ch <= '9')
END IsDigit;

PROCEDURE IsHexDigit(ch: CHAR): BOOLEAN;
	RETURN (ch >= 'A') & (ch <= 'F')
	OR (ch >= 'a') & (ch <= 'f')
END IsHexDigit;

PROCEDURE SkipBlank;
BEGIN
	IF bufLen = 0 THEN GetCh END;
	WHILE (bufLen = 1) & IsBlank(buf) DO GetCh END
END SkipBlank;

PROCEDURE Int*(VAR i: INTEGER);
	CONST MaxInt = 7FFFFFFFFFFFFFFFH; MinInt = -MaxInt - 1;
	VAR neg, isHex, decOverflow, hexOverflow, finished: BOOLEAN;
		x, hex, dec, dec0, digitCnt : INTEGER;
BEGIN
	IF Done THEN
		i := 0; neg := FALSE; isHex := FALSE; decOverflow := FALSE;
		finished := FALSE; hexOverflow := FALSE; digitCnt := 0;
		IF bufLen = 0 THEN GetCh END; SkipBlank;
		IF (bufLen = 1) & (buf = '-') THEN neg := TRUE; GetCh END;
		IF (bufLen = 1) & IsDigit(buf) THEN
			dec := ORD(buf) - ORD('0'); hex := dec; INC(digitCnt); GetCh
		ELSE Done := FALSE
		END;
		WHILE Done & ~finished DO INC(digitCnt);
			IF IsDigit(buf) THEN x := ORD(buf) - ORD('0');
				hex := hex * 16 + x; dec0 := dec * 10 + x;
				hexOverflow := hexOverflow OR (digitCnt > 16);
				decOverflow := decOverflow OR (digitCnt > 19)
					OR (dec0 < 0) & (dec0 # MinInt)
					OR (dec0 > 0) & ((dec0 - x) DIV 10 # dec);
				dec := dec0
			ELSIF IsHexDigit(buf) THEN isHex := TRUE; x := ORD(buf) + 10;
				IF (buf >= 'a') & (buf <= 'f')
				THEN DEC(x, ORD('a')) ELSE DEC(x, ORD('A'))
				END;
				hexOverflow := hexOverflow OR (digitCnt > 16);
				hex := hex * 16 + x
			ELSIF (buf = 'h') OR (buf = 'H') THEN
				isHex := TRUE; finished := TRUE; GetCh; 
				Done := Done & IsBlank(buf) & ~hexOverflow
			ELSE finished := TRUE;
				Done := Done & ~isHex & IsBlank(buf) & ~decOverflow
			END;
			IF ~finished THEN GetCh END
		END;
		IF Done THEN
			IF isHex THEN i := hex ELSE i := dec END;
			IF neg THEN i := -i END
		END
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