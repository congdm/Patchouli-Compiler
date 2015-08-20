MODULE Console;

IMPORT
	SYS := SYSTEM, Win := WinApi, Strings;
	
CONST
	nilAdr = 0; boolFalse = 0;
	
VAR
	stdin, stdout : Win.Handle;

PROCEDURE Write* (ch : CHAR);
	VAR nWritten : Win.Dword; r : Win.Bool;
BEGIN
	r := Win.WriteConsoleW(
		stdout, SYS.ADR(ch), 1, SYS.ADR2(nWritten), nilAdr
	);
	ASSERT (r # boolFalse); ASSERT (nWritten = 1)
END Write;

PROCEDURE WriteLn*;
	VAR str : ARRAY 2 OF CHAR; nWritten : Win.Dword; r : Win.Bool;
BEGIN
	str[0] := CHR(13); str[1] := CHR(10);
	r := Win.WriteConsoleW(
		stdout, SYS.ADR(str), 2, SYS.ADR2(nWritten), nilAdr
	);
	ASSERT (r # boolFalse); ASSERT (nWritten = 2)
END WriteLn;

PROCEDURE WriteString* (str : ARRAY OF CHAR);
	VAR nWritten : Win.Dword; r : Win.Bool;
BEGIN
	r := Win.WriteConsoleW(
		stdout, SYS.ADR(str), Strings.Length(str), SYS.ADR2(nWritten), nilAdr
	)
END WriteString;

PROCEDURE IntToString* (x : INTEGER; VAR str : ARRAY OF CHAR);
	CONST MIN_INT = -7FFFFFFFFFFFFFFFH - 1;
	VAR negative : BOOLEAN; s : ARRAY 32 OF CHAR; i, j : INTEGER;
BEGIN
	IF x # MIN_INT THEN 
		IF x < 0 THEN negative := TRUE; x := -x
		ELSE negative := FALSE
		END;
		
		i := 0;
		REPEAT
			s[i] := CHR(x MOD 10 + ORD('0'));
			INC (i); x := x DIV 10
		UNTIL x = 0;
		
		IF negative THEN str[0] := '-'; j := 0;
			WHILE j < i DO str[j + 1] := s[i - 1 - j]; j := j + 1 END;
			str[i + 1] := 0X
		ELSE j := 0;
			WHILE j < i DO str[j] := s[i - 1 - j]; j := j + 1 END;
			str [i] := 0X
		END
	ELSE str[0] := 0X; Strings.Append ('-9223372036854775808', str)
	END
END IntToString;

PROCEDURE WriteInt* (n : INTEGER);
	VAR str : ARRAY 32 OF CHAR;
BEGIN
	IntToString (n, str); WriteString (str)
END WriteInt;

PROCEDURE IntToHexString* (x: INTEGER; VAR str: ARRAY OF CHAR);
	VAR s: ARRAY 17 OF CHAR; i, j, n: INTEGER;
BEGIN i := 0;
	REPEAT n := x MOD 16; 
		IF n < 10 THEN s[i] := CHR(n + ORD('0'))
		ELSE s[i] := CHR(ORD('A') - 10 + n)
		END;
		INC (i); x := x DIV 16
	UNTIL (x = 0) OR (i = 16);
	j := 0; WHILE j < i DO str[j] := s[i - 1 - j]; INC (j) END; str[i] := 0X
END IntToHexString;

PROCEDURE WriteHex* (n: INTEGER);
	VAR str: ARRAY 17 OF CHAR;
BEGIN IntToHexString (n, str); WriteString (str)
END WriteHex;

(* Simple but not accurate *)
PROCEDURE RealToString (x : REAL; VAR str : ARRAY OF CHAR);
	VAR i, n, exp, exp10, frac, zeroNum : INTEGER;
		neg : BOOLEAN; ch : ARRAY 2 OF CHAR; str2 : ARRAY 32 OF CHAR;
BEGIN str[0] := 0X;
	exp := SYS.VAL(Win.Dword, x) DIV 800000H MOD 256 - 127;
	IF (exp >= -126) & (exp <= 127) THEN
		neg := x < 0.0; IF neg THEN x := -x; Strings.Append ('-', str) END;
		exp10 := 0; ch[1] := 0X;
		IF exp > 0 THEN
			WHILE x > 10.0 DO x := x / 10.0; INC (exp10) END
		ELSIF exp < 0 THEN
			WHILE x < 1.0 DO x := x * 10.0; DEC (exp10) END
		END;
		n := FLOOR (x); x := (x - FLT(n)) * 10.0; ch[0] := CHR(ORD('0') + n);
		Strings.Append (ch, str); Strings.Append ('.', str);
		frac := 0; i := 0; zeroNum := 0;
		WHILE (i < 6) & (x # 0.0) DO
			INC (i); n := FLOOR(x); x := (x - FLT(n)) * 10.0;
			IF (n # 0) OR (frac # 0) THEN frac := frac * 10 + n
			ELSE INC (zeroNum)
			END
		END;
		IF x # 0.0 THEN n := FLOOR(x); x := x - FLT(n);
			IF (x > 0.5) OR (x = 0.5) & ODD(n) THEN INC (n) END;
			IF n # 0 THEN frac := frac * 10 + n END
		END;
		WHILE (frac MOD 10) = 0 DO frac := frac DIV 10 END;
		WHILE zeroNum > 0 DO Strings.Append ('0', str); DEC (zeroNum) END;
		IF frac # 0 THEN IntToString (frac, str2);
			Strings.Append (str2, str)
		END;
		IF exp10 # 0 THEN Strings.Append ('e', str); IntToString (exp10, str2);
			Strings.Append (str2, str)
		END
	END
END RealToString;

PROCEDURE WriteReal* (r : REAL);
	VAR str : ARRAY 32 OF CHAR;
BEGIN
	RealToString (r, str); WriteString (str)
END WriteReal;

PROCEDURE Read* (VAR ch : CHAR);
	VAR nRead : Win.Dword; r : Win.Bool;
BEGIN
	r := Win.ReadConsoleW(
		stdin, SYS.ADR(ch), 1, SYS.ADR2(nRead), NIL
	);
	ASSERT (r # boolFalse); ASSERT (nRead = 1)
END Read;

PROCEDURE Init;
	VAR res : Win.Bool;
BEGIN
	res := Win.AllocConsole();
	stdin := Win.GetStdHandle(Win.STD_INPUT_HANDLE);
	stdout := Win.GetStdHandle(Win.STD_OUTPUT_HANDLE)
END Init;

BEGIN
	Init
END Console.