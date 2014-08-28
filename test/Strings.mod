MODULE Strings;

PROCEDURE Length* (s : ARRAY OF CHAR) : INTEGER;
	VAR i : INTEGER;
BEGIN
	i := 0; WHILE (i < LEN(s)) & (s[i] # 0X) DO i := i + 1 END
RETURN i
END Length;

PROCEDURE Insert* (source : ARRAY OF CHAR; pos : INTEGER;
	VAR dest : ARRAY OF CHAR
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
	ELSE
	END
END Insert;

PROCEDURE Append* (extra : ARRAY OF CHAR; VAR dest : ARRAY OF CHAR);
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
	ELSIF pos = len THEN
	ELSE
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
BEGIN
END Extract;

END Strings.