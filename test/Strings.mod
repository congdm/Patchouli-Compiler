MODULE Strings;

IMPORT
	SYSTEM;

CONST
	MaxInt = 7FFFFFFFFFFFFFFFH;
	MinInt = -MaxInt - 1;

PROCEDURE Length* (s: ARRAY OF CHAR) : INTEGER;
	VAR i : INTEGER;
BEGIN
	i := 0; WHILE (i < LEN(s)) & (s[i] # 0X) DO i := i + 1 END;
	RETURN i
END Length;

PROCEDURE InsertChar* (source: CHAR; pos: INTEGER; VAR dest: ARRAY OF CHAR);
	VAR dstlen, i: INTEGER;
BEGIN
	dstlen := Length(dest);
	IF (pos >= 0) & (pos <= dstlen) THEN
		IF dstlen < LEN(dest) - 1 THEN
			IF pos = dstlen THEN dest[pos+1] := 0X
			ELSIF pos < dstlen THEN i := dstlen;
				WHILE i >= pos DO dest[i+1] := dest[i]; DEC (i) END
			END;
			dest[pos] := source
		END
	END
END InsertChar;

PROCEDURE AppendChar* (source: CHAR; VAR dest: ARRAY OF CHAR);
BEGIN
	InsertChar (source, Length(dest), dest)
END AppendChar;

PROCEDURE Insert* (
	source: ARRAY OF CHAR;
	pos: INTEGER;
	VAR dest: ARRAY OF CHAR
);
	VAR srclen, dstlen, i, k : INTEGER;
BEGIN
	dstlen := Length(dest);
	IF (pos >= 0) & (pos <= dstlen) THEN
		IF pos = dstlen THEN
			i := 0;
			WHILE (pos + i < LEN(dest) - 1) & (source[i] # 0X) DO
				dest[pos + i] := source[i]; i := i + 1
			END;
			dest[pos + i] := 0X
		ELSE
			srclen := Length(source);
			IF srclen > 0 THEN
				k := dstlen + srclen - 1;
				IF k > LEN(dest) - 2 THEN k := LEN(dest) - 2 END;
				dest[k + 1] := 0X; i := k;
				WHILE i - srclen >= pos DO
					dest[i] := dest[i - srclen]; i := i - 1
				END;
				i := 0;
				WHILE (i < srclen) & (pos + i <= k) DO
					dest[pos + i] := source[i]; i := i + 1
				END
			END
		END
	END
END Insert;

PROCEDURE Append* (extra: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR);
BEGIN
	Insert (extra, Length(dest), dest)
END Append;

PROCEDURE Delete* (VAR s : ARRAY OF CHAR; pos, n : INTEGER);
	VAR i, len : INTEGER;
BEGIN
	len := Length(s);
	IF (pos >= 0) & (pos < len) THEN
		WHILE n < len - pos DO s[pos] := s[pos + n]; pos := pos + 1 END;
		s[pos] := 0X
	ELSIF pos = len THEN (* Do nothing *)
	END
END Delete;

PROCEDURE Replace* (source : ARRAY OF CHAR; pos : INTEGER;
		VAR dest : ARRAY OF CHAR);
BEGIN
	Delete (dest, pos, Length (source));
	Insert (source, pos, dest)
END Replace;

PROCEDURE Extract* (source : ARRAY OF CHAR; pos, n : INTEGER;
		VAR dest : ARRAY OF CHAR);
	VAR i, len : INTEGER;
BEGIN
	len := Length(source);
	IF (pos >= 0) & (pos < len) THEN
		i := 0; IF n > len - pos THEN n := len - pos END;
		WHILE (i < n) & (i < LEN(dest) - 1) DO
			dest[i] := source[pos + i]; i := i + 1
		END;
		dest[i] := 0X
	END
END Extract;

PROCEDURE Pos* (pattern, s : ARRAY OF CHAR; pos : INTEGER) : INTEGER;
	VAR i, slen, plen : INTEGER; notfound, differ : BOOLEAN;
BEGIN
	IF pos < 0 THEN pos := 0 END; slen := Length(s); plen := Length(pattern);
	IF (plen > 0) & (slen >= plen) THEN
		notfound := TRUE;
		WHILE notfound & (pos < slen) & (slen - pos >= plen) DO
			i := 0;
			REPEAT differ := s[pos + i] # pattern[i]; i := i + 1
			UNTIL differ OR (i = plen) OR (pos + i = slen);
			notfound := differ; pos := pos + 1
		END;
		IF notfound THEN pos := -1 ELSE pos := pos - 1 END
	ELSE pos := -1
	END
RETURN pos
END Pos;

PROCEDURE Cap* (VAR s : ARRAY OF CHAR);
	VAR i : INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(s)) & (s[i] # 0X) DO
		IF (s[i] >= 'a') & (s[i] <= 'z') THEN s[i] := CHR(ORD(s[i]) - 20H) END;
		i := i + 1
	END
END Cap;

PROCEDURE FormatInt* (x: INTEGER; VAR str: ARRAY OF CHAR);
	VAR neg: BOOLEAN; s: ARRAY 21 OF CHAR; i, j: INTEGER;
BEGIN
	IF x # MinInt THEN i := 0; j := 0;
		IF x < 0 THEN neg := TRUE; x := -x ELSE neg := FALSE END;
		REPEAT s[i] := CHR(x MOD 10 + ORD('0')); INC (i); x := x DIV 10
		UNTIL x = 0;		
		IF ~neg THEN
			WHILE (j < i) & (j < LEN(str)) DO
				str[j] := s[i-1-j]; INC (j)
			END;
			str[j] := 0X
		ELSE str[0] := '-';
			WHILE (j < i) & (j < LEN(str)-1) DO
				str[j+1] := s[i-1-j]; INC (j)
			END;
			str[j+1] := 0X
		END
	ELSE str[0] := 0X; Append ('-9223372036854775808', str)
	END
END FormatInt;

PROCEDURE FormatHex* (x: INTEGER; VAR str: ARRAY OF CHAR);
	VAR s: ARRAY 17 OF CHAR; i, j, n: INTEGER;
BEGIN i := 0;
	REPEAT n := x MOD 16; 
		IF n < 10 THEN s[i] := CHR(n + ORD('0'))
		ELSE s[i] := CHR(ORD('A') - 10 + n)
		END;
		INC (i); x := x DIV 16
	UNTIL (x = 0) OR (i = 16);
	j := 0; WHILE (j < i) & (j < LEN(str)) DO str[j] := s[i-1-j]; INC (j) END;
	str[j] := 0X
END FormatHex;

(* Simple but not accurate *)
PROCEDURE FormatReal* (x: REAL; VAR str: ARRAY OF CHAR);
	VAR i, n, exp, exp10, frac, zeroNum: INTEGER;
		str2: ARRAY 32 OF CHAR; x2: REAL;
BEGIN str[0] := 0X; x2 := ABS(x); UNPK (x2, exp);
	IF (exp >= -126) & (exp <= 127) THEN exp10 := 0;
		IF x < 0.0 THEN x := -x; AppendChar ('-', str) END;
		IF exp > 0 THEN
			WHILE x > 10.0 DO x := x / 10.0; INC (exp10) END
		ELSIF exp < 0 THEN
			WHILE x < 1.0 DO x := x * 10.0; DEC (exp10) END
		END;
		n := FLOOR(x); x := (x - FLT(n)) * 10.0;
		AppendChar (CHR(ORD('0') + n), str); AppendChar ('.', str);
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
		WHILE zeroNum > 0 DO AppendChar ('0', str); DEC (zeroNum) END;
		FormatInt (frac, str2); Append (str2, str); AppendChar ('e', str);
		FormatInt (exp10, str2); Append (str2, str)
	END
END FormatReal;

PROCEDURE ParseInt (str: ARRAY OF CHAR) : INTEGER;
BEGIN
END ParseInt;

END Strings.