MODULE Test;
(*$MAIN*)

IMPORT
    SYSTEM;	
CONST
	mess = 'Oberon for Win64';
	title = 'Hello, World!';
TYPE
    Handle = INTEGER;
    Uint = CARD32;
    PChar = ADDRESS OF CHAR;
VAR
    MessageBoxW: PROCEDURE (
		hwnd: Handle;
		lpText, lpCaption: PChar;
		uType: Uint
	);
	user32: Handle;

BEGIN
    SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	SYSTEM.GetProcAddress (MessageBoxW, user32, 'MessageBoxW'@);
	MessageBoxW (0, SYSTEM.STRADR(mess), SYSTEM.STRADR(title), 0)
END Test.