MODULE Test8;
(*$CONSOLE*)
IMPORT In := [Oberon07.In], Out := [Oberon07.Out];
VAR ch: CHAR; i, n: INTEGER;
BEGIN
	Out.String('Input a character: '); In.Char(ch); In.Ln;
	Out.String("The char you've just inputed is: "); Out.Char(ch); Out.Ln;
	Out.String('Input two integer numbers: '); In.Int(i); In.Int(n);
	Out.String("The numbers you've just inputed are: ");
	Out.Int(i, 0); Out.Char(' '); Out.Int(n, 0)
END Test8.