MODULE Test2;
(*$CONSOLE*)

IMPORT
    Crypt, Console;
	
CONST
	msg = '12345678901234567890123456789012345678901234567890123456789012345678901234567890'@;
	
VAR
	hash: Crypt.MD5Hash;
	lo, hi, i: INTEGER; b: BYTE;

BEGIN
	Crypt.InitMD5Hash (hash);
	Crypt.MD5Compute (hash, msg, (LEN(msg)-1)*8);
	lo := Crypt.MD5GetLowResult(hash);
	hi := Crypt.MD5GetHighResult(hash);
	FOR i := 1 TO 8 DO
		b := lo; lo := lo DIV 256;
		Console.WriteHex (ASR(b, 4));
		Console.WriteHex (b MOD 16)
	END;
	FOR i := 1 TO 8 DO
		b := hi; hi := hi DIV 256;
		Console.WriteHex (ASR(b, 4));
		Console.WriteHex (b MOD 16)
	END
END Test2.