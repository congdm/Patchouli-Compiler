MODULE Test8;
(*$CONSOLE*)
IMPORT In := [Oberon07.In], Out := [Oberon07.Out];
VAR ch: CHAR; i: INTEGER;
BEGIN
	Out.String('Input a character: '); In.Char(ch); In.Ln;
	Out.String("The char you've just inputed is: "); Out.Char(ch); Out.Ln;
	Out.String('Input an integer number: '); In.Int(i);
	Out.String("The number you've just inputed is: "); Out.Int(i, 0)
END Test8.