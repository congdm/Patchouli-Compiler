MODULE NppPlugin; (*$RTL-*)
IMPORT SYSTEM, Rtl := [Oberon07 Rtl], Out := [Oberon07 Out];

CONST
	NppMsg = 400H + 1000;
	NPPM_GETCURRENTBUFFERID = NppMsg + 60;
	NPPM_GETFULLPATHFROMBUFFERID = NppMsg + 58;

TYPE
	Card32 = SYSTEM.CARD32; Card16 = SYSTEM.CARD16;
	Int = Card32; UInt = Card32; Pointer = INTEGER; Handle = INTEGER;
	WParam = INTEGER; LParam = INTEGER; LResult = INTEGER;

	Shortcut = RECORD
		isCtrl, isAlt, isShift: BOOLEAN; key: BYTE
	END;

	FuncItem = RECORD
		itemName: ARRAY 64 OF CHAR;
		func: PROCEDURE;
		cmdId: Int;
		init2check: BOOLEAN;
		pShKey: POINTER [untraced] TO Shortcut
	END;

VAR
	nppHandle: Handle;
	funcsArray: ARRAY 2 OF FuncItem;
	SendMessageW: PROCEDURE(
		hWnd: Handle; Msg: UInt; wParam: WParam; lParam: LParam
	): LResult;
	
PROCEDURE isUnicode*(): BOOLEAN;
	RETURN TRUE
END isUnicode;

PROCEDURE setInfo*(nppData: Pointer);
BEGIN
	SYSTEM.GET(nppData, nppHandle)
END setInfo;
	
PROCEDURE getName*(): Pointer;
	RETURN SYSTEM.ADR('Patchouli Oberon-07 Compiler')
END getName;

PROCEDURE getFuncsArray*(VAR nFuncs: Int): Pointer;
BEGIN
	nFuncs := LEN(funcsArray);
	RETURN SYSTEM.ADR(funcsArray)
END getFuncsArray;

PROCEDURE beNotified*(notif: Pointer);
END beNotified;

PROCEDURE messageProc*(msg: UInt; wParam: WParam; lParam: LParam): LResult;
	RETURN 1
END messageProc;

PROCEDURE DisplayAbout;
BEGIN
	Rtl.MessageBox('About', 'Patchouli Oberon-07 Compiler')
END DisplayAbout;

PROCEDURE GetBufferId;
	VAR id, res: LResult; path: ARRAY 1024 OF CHAR;
BEGIN
	id := SendMessageW(nppHandle, NPPM_GETCURRENTBUFFERID, 0, 0);
	Out.String('Current active buffer: '); Out.Int(id, 0); Out.Ln;
	res := SendMessageW(nppHandle, NPPM_GETFULLPATHFROMBUFFERID, id, 0);
	IF res < LEN(path) THEN
		res := SendMessageW(
			nppHandle, NPPM_GETFULLPATHFROMBUFFERID, id, SYSTEM.ADR(path)
		);
		Out.String('   File path: '); Out.String(path); Out.Ln
	END
END GetBufferId;

PROCEDURE Init;
	CONST User32 = 'User32.dll';
BEGIN
	funcsArray[0].itemName := 'About';
	funcsArray[0].func := DisplayAbout;
	funcsArray[0].init2check := FALSE;
	
	funcsArray[1].itemName := 'Get Active Buffer Id';
	funcsArray[1].func := GetBufferId;
	funcsArray[1].init2check := FALSE;
	
	Rtl.Import(SendMessageW, User32, 'SendMessageW')
END Init;

BEGIN Init
END NppPlugin.