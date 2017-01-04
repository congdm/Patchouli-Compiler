MODULE Test4;
(*$CONSOLE*)
IMPORT SYSTEM, Rtl, Out;
VAR i, adr: INTEGER;
BEGIN
	Out.String('Number of modules: '); Out.Int(Rtl.nMod, 0); Out.Ln;
	FOR i := 0 TO Rtl.nMod-1 DO
		Out.String('Module '); Out.Int(i, 0); Out.String(' address (code): ');
		SYSTEM.GET(Rtl.modList+i*8, adr); Out.Hex(adr, 0); Out.Ln
	END
END Test4.