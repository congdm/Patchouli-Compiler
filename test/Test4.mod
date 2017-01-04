MODULE Test4;
(*$CONSOLE*)
IMPORT SYSTEM, Rtl, Out;

VAR teb: INTEGER;
	CreateFileMappingW: PROCEDURE(
		hFile, lpAttributes, flProtect,
		dwMaximumSizeHigh, dwMaximumSizeLow, lpName: INTEGER
	): INTEGER;
	MapViewOfFile: PROCEDURE(
		hFileMappingObject,
		dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow,
		dwNumberOfBytesToMap: INTEGER
	): INTEGER;
	
PROCEDURE Main;
	VAR hFile, adr, x, n, i: INTEGER;
BEGIN teb := SYSTEM.NtCurrentTeb();
	Out.String('Thread Information Block address: '); Out.Hex(teb, 0); Out.Ln;
	Rtl.ImportExtProc(CreateFileMappingW, 'Kernel32.dll', 'CreateFileMappingW');
	Rtl.ImportExtProc(MapViewOfFile, 'Kernel32.dll', 'MapViewOfFile');
	hFile := CreateFileMappingW(-1, 0, 4, 0, 1000H, SYSTEM.ADR('Local\PatchouliOberon07'));
	adr := MapViewOfFile(hFile, 2, 0, 0, 0); ASSERT(adr # 0);
	SYSTEM.GET(adr, n); Out.String('Number of modules: ');
	Out.Int(n, 0); Out.Ln;
	FOR i := 1 TO n DO
		SYSTEM.GET(adr+i*8, x); Out.String('Module '); Out.Int(i, 0);
		Out.String(' address: '); Out.Hex(x, 0); Out.Ln
	END
END Main;

BEGIN Main
END Test4.