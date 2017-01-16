(* Must have Rtl.dll in Notepad++ dir or system path *)
(* Also crashs Notepad++ if exception happened (^_^) *)
MODULE NppPlugin; (*$RTL-*)
IMPORT SYSTEM, Rtl;

TYPE
	Card32 = SYSTEM.CARD32; Card16 = SYSTEM.CARD16;
	Int = Card32; UInt = Card32; Pointer = INTEGER;
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
	funcsArray: ARRAY 1 OF FuncItem;
	
PROCEDURE isUnicode*(): BOOLEAN;
	RETURN TRUE
END isUnicode;

PROCEDURE setInfo*(nppData: Pointer);
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

BEGIN
	funcsArray[0].itemName := 'About';
	funcsArray[0].func := DisplayAbout;
	funcsArray[0].init2check := FALSE
END NppPlugin.