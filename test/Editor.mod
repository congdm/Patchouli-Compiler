MODULE Editor;
(*$MAIN*)

IMPORT
	SYSTEM, Kernel32, User32, Comdlg32;

VAR
	hInst: Kernel32.HANDLE;
	hwnd: User32.HANDLE;
	
PROCEDURE StdWindowProc(
	hwnd: User32.HANDLE; uMsg: CARD32; wParam, lParam: INTEGER
): INTEGER;
	VAR bRes: User32.BOOL; result: INTEGER;
BEGIN result := 0;
	IF uMsg = User32.WM_CLOSE THEN
		bRes := User32.DestroyWindow (hwnd)
	ELSIF uMsg = User32.WM_DESTROY THEN
		User32.PostQuitMessage (0)
	ELSE result := User32.DefWindowProcW (hwnd, uMsg, wParam, lParam)
	END;
	RETURN result
END StdWindowProc;

PROCEDURE CreateRichEdit;
	CONST MSFTEDIT_CLASS = 'RICHEDIT50W';
	VAR hEdit: User32.HANDLE; rect: User32.RECT;
		bRes: User32.BOOL;
BEGIN
	bRes := User32.GetClientRect (hwnd, rect);
	hEdit := User32.CreateWindowExW(
		0, MSFTEDIT_CLASS, 'Type here',
		ORD(User32.ES_MULTILINE + User32.WS_VISIBLE + User32.WS_CHILD
			+ User32.WS_BORDER + User32.WS_TABSTOP
		),
		0, 0, 790, 555,
		hwnd, 0, hInst, 0
	);
END CreateRichEdit;

PROCEDURE CreateMenu(): User32.HANDLE;
	VAR hMenu: User32.HANDLE;
		
	PROCEDURE CreateItem (hMenu: User32.HANDLE; text: ARRAY OF CHAR; pos: INTEGER);
		VAR mii: User32.MENUITEMINFO; bRes: User32.BOOL;
	BEGIN
		mii.cbSize := SYSTEM.SIZE(User32.MENUITEMINFO);
		mii.fMask := ORD(User32.MIIM_STRING + User32.MIIM_ID);
		mii.fType := 0;
		mii.fState := 0;
		mii.wID := pos;
		mii.hSubMenu := 0;
		mii.hbmpChecked := 0;
		mii.hbmpUnchecked := 0;
		mii.dwItemData := 0;
		mii.dwTypeData := SYSTEM.ADR(text);
		mii.cch := 0;
		mii.hbmpItem := 0;
		bRes := User32.InsertMenuItemW (hMenu, pos, 1, mii)
	END CreateItem;
		
BEGIN (* CreateMenu *)
	hMenu := User32.CreateMenu();
	CreateItem (hMenu, 'File', 0);
	CreateItem (hMenu, 'Edit', 1);
	CreateItem (hMenu, 'Font', 2);
	RETURN hMenu
END CreateMenu;
	
PROCEDURE Main;
	CONST
		className = 'MainWindow';
	VAR
		wclass: User32.WNDCLASSEXW; msg: User32.MSG;
		res16: CARD16; bRes: User32.BOOL;
		hDll, hMenu: Kernel32.HANDLE;
BEGIN
	SYSTEM.LoadLibraryW (hDll, 'Msftedit.dll'); ASSERT (hDll # 0);
	hInst := Kernel32.GetModuleHandleW(NIL);
	
	wclass.cbSize := SYSTEM.SIZE(User32.WNDCLASSEXW);
    wclass.style := 0;
    wclass.lpfnWndProc := StdWindowProc;
    wclass.cbClsExtra := 0;
    wclass.cbWndExtra := 0;
    wclass.hInstance := hInst;
    wclass.hIcon := 0;
    wclass.hCursor := 0;
    wclass.hbrBackground := User32.COLOR_WINDOW;
    wclass.lpszMenuName  := 0;
    wclass.lpszClassName := SYSTEM.ADR(className);
	wclass.hIconSm := 0;
	res16 := User32.RegisterClassExW (wclass); ASSERT (res16 # 0);
	
	hMenu := CreateMenu();
	hwnd := User32.CreateWindowExW(
		0, className, 'Editor',
		ORD(User32.WS_OVERLAPPEDWINDOW),
		0, 0, 800, 600,
		0, hMenu, hInst, 0
	);
	bRes := User32.ShowWindow (hwnd, 1);
	CreateRichEdit;
	
	WHILE User32.GetMessageW (msg, 0, 0, 0) > 0 DO
		bRes := User32.TranslateMessage (msg);
		bRes := User32.DispatchMessageW (msg)
	END
END Main;

BEGIN Main
END Editor.