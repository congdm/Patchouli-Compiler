DEFINITION User32;

PROCEDURE MessageBoxW* (
	hwnd: INTEGER;
	lpText, lpCaption: ARRAY OF CHAR;
	uType: CARD32
);

END User32.