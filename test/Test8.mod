MODULE Test8;
(*$CONSOLE*)
IMPORT SYSTEM, In := [Oberon07.In], Out := [Oberon07.Out];
VAR ch: CHAR; i, n: INTEGER; f: REAL;
BEGIN
	Out.String('Input a character: '); In.Char(ch); In.Ln;
	Out.String("The char you've just inputed is: "); Out.Char(ch); Out.Ln;
	Out.String('Input two integer numbers: '); In.Int(i); In.Int(n);
	Out.String("The numbers you've just inputed are: ");
	Out.Int(i, 0); Out.Char(' '); Out.Int(n, 0); Out.Ln;
	
	f := 653.385;
	Out.String('A floating point number: '); Out.Real(f, 0);
	Out.String(' (hex: '); Out.Hex(SYSTEM.VAL(INTEGER, f), 0); Out.Char(')')
END Test8.