MODULE Test;
	CONST str = 'Hello';
	VAR x : ARRAY 512 OF BYTE;
		AllocConsole : PROCEDURE () : INTEGER;
		MessageBoxW : PROCEDURE (hwnd, lpText, lpCaption, uType : INTEGER);

	PROCEDURE Main (input : INTEGER);
		VAR i : INTEGER;
	BEGIN
		i := 2;
		input := input + 6;
		i := input;
		MessageBoxW (0, ADR('Oberon for Win64'), ADR('Hello, World!'), 0)
	END Main;
	
	PROCEDURE Init;
		VAR user32 : INTEGER;
	BEGIN
		LoadLibrary (user32, 'USER32.DLL');
		GetProcAddress (MessageBoxW, user32, 2050)
	END Init;
	
BEGIN
	Init;
	Main (0)
END Test.