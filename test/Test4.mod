MODULE Test4;
(*$CONSOLE*)
IMPORT SYSTEM, Rtl, Out;
VAR i, adr: INTEGER; i1: BYTE; i2: SYSTEM.CARD16; i4: SYSTEM.CARD32;
BEGIN
	Out.String('Number of modules: '); Out.Int(Rtl.nMod, 0); Out.Ln;
	FOR i := 0 TO Rtl.nMod-1 DO
		Out.String('Module '); Out.Int(i, 0); Out.String(' address (code): ');
		SYSTEM.GET(Rtl.modList+i*8, adr); Out.Hex(adr, 0); Out.Ln
	END;
	Out.Ln; i := 8877665544332211H; i1 := i; i2 := i; i4 := i;
	Out.String('INTEGER: '); Out.Hex(i, 0); Out.Ln;
	Out.String('BYTE: '); Out.Hex(i1, 0); Out.Ln;
	Out.String('CARD16: '); Out.Hex(i2, 0); Out.Ln;
	Out.String('CARD32: '); Out.Hex(i4, 0); Out.Ln
END Test4.