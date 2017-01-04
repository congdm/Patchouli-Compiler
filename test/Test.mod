MODULE Test;
(*$MAIN*)
IMPORT SYSTEM, Test2;

TYPE
	Str = ARRAY 256 OF CHAR;

VAR
	i, user32: INTEGER;
	ansiStr: ARRAY 256 OF BYTE; str, str1, str2: Str;
	MessageBoxW: PROCEDURE(hWnd: INTEGER; lpText, lpCaption: Str; uType: INTEGER);

BEGIN
	i := -1; ansiStr[i] := 0;
	IF str1 = str2 THEN
		i := 1
	END;
	str := 'MessageBoxW'; i := 0;
	WHILE str[i] # 0X DO ansiStr[i] := ORD(str[i]); INC(i) END;
	SYSTEM.LoadLibraryW(user32, 'User32.dll');
	SYSTEM.GetProcAddress(MessageBoxW, user32, SYSTEM.ADR(ansiStr));
	MessageBoxW(0, 'Hello, world!', 'Test', 0)
END Test.