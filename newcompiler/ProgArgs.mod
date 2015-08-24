MODULE ProgArgs;

IMPORT
	SYSTEM, WinApi;
	
TYPE
	CharArray = ARRAY 100000H OF CHAR;
	PCharArray = ADDRESS OF CharArray;
	
PROCEDURE Convert (VAR out: ARRAY OF CHAR; in: PCharArray);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE (i < LEN(out)) & (i < LEN(in^)) & (in[i] # 0X) DO
		out[i] := in[i]; INC (i)
	END;
	IF i < LEN(out) THEN out[i] := 0X END
END Convert;

PROCEDURE GetArg* (VAR out: ARRAY OF CHAR; VAR paramLen: INTEGER; n: INTEGER);
	VAR i, k: INTEGER; buf: PCharArray;
BEGIN buf := SYSTEM.VAL(PCharArray, WinApi.GetCommandLineW()); i := 0;
	WHILE n > 0 DO
		WHILE (buf[i] # ' ') & (buf[i] # 0X) DO INC (i) END;
		IF buf[i] = 0X THEN n := 0
		ELSIF buf[i] = ' ' THEN DEC (n);
			WHILE buf[i] = ' ' DO INC (i) END
		END
	END;
	k := 0; paramLen := 0;
	WHILE (buf[i] # ' ') & (buf[i] # 0X) DO
		IF k < LEN(out) THEN out[k] := buf[i] END;
		INC (k); INC (i); INC (paramLen)
	END;
	IF k < LEN(out) THEN out[k] := 0X END
END GetArg;

	
END ProgArgs.