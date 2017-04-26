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
	
	hIn: INTEGER;
	Done*: BOOLEAN;
	
PROCEDURE Open*;
BEGIN
	hIn := GetStdHandle(-10); (* STD_INPUT_HANDLE *)
	ASSERT(hIn # -1); Done := TRUE
END Open;
	
PROCEDURE Char*(VAR ch: CHAR);
	VAR nRead: SYSTEM.CARD32;
BEGIN
	Done := ReadConsoleW(hIn, SYSTEM.ADR(ch), 1, SYSTEM.ADR(nRead), 0) # 0
END Char;
	
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