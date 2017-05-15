MODULE Test4;
(*$CONSOLE*)
IMPORT SYSTEM, Rtl := [Oberon07.Rtl], Out := [Oberon07.Out];
TYPE List = POINTER TO RECORD x: INTEGER; next: List END;
VAR i, adr, off, ptrTable: INTEGER; p1, p2: List;
	i1: BYTE; i2: SYSTEM.CARD16; i4: SYSTEM.CARD32;
	p3, p4: List;
BEGIN
	Out.String('Number of modules: '); Out.Int(Rtl.nMod, 0); Out.Ln;
	FOR i := 0 TO Rtl.nMod-1 DO
		Out.String('Module '); Out.Int(i, 0); Out.String(' address (code): ');
		SYSTEM.GET(Rtl.modList+i*8, adr); Out.Hex(adr, 0); Out.Ln;
		Out.String('	Pointer table:'); Out.Ln; SYSTEM.GET(adr+112, ptrTable);
		REPEAT
			SYSTEM.GET(ptrTable, off); Out.Char('	');
			Out.Int(off, 0); INC(ptrTable, 8)
		UNTIL off = -1;
		Out.Ln
	END;
	Out.Ln; i := 8877665544332211H; i1 := i; i2 := i; i4 := i;
	Out.String('INTEGER: '); Out.Hex(i, 0); Out.Ln;
	Out.String('BYTE: '); Out.Hex(i1, 0); Out.Ln;
	Out.String('CARD16: '); Out.Hex(i2, 0); Out.Ln;
	Out.String('CARD32: '); Out.Hex(i4, 0); Out.Ln
END Test4.