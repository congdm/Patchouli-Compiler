MODULE Test8;
(*$CONSOLE*)
IMPORT In := [Oberon07.In], Out := [Oberon07.Out];
VAR ch: CHAR;
BEGIN
	In.Char(ch); Out.String("The char you've just inputed is: "); Out.Char(ch)
END Test8.