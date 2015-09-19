DEFINITION User32;

CONST
	(* Window message *)
	WM_NULL* = 0;
	WM_CREATE* = 1;
	WM_DESTROY* = 2;
	WM_CLOSE* = 16;
	WM_KEYDOWN* = 256;
	WM_KEYUP* = 257;
	
	(* Window style *)
	WS_OVERLAPPED* = {};
	WS_TILED* = WS_OVERLAPPED;
	WS_BORDER* = {23};
	WS_DLGFRAME* = {22};
	WS_CAPTION* = WS_BORDER + WS_DLGFRAME;
	WS_CHILD* = {30};
	WS_SYSMENU* = {19};
	WS_TABSTOP* = {16};
	WS_THICKFRAME* = {18};
	WS_MINIMIZEBOX* = {17};
	WS_MAXIMIZEBOX* = {16};
	WS_VISIBLE* = {28};
	WS_OVERLAPPEDWINDOW* = WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU
		+ WS_THICKFRAME + WS_MINIMIZEBOX + WS_MAXIMIZEBOX;
	WS_TILEDWINDOW* = WS_OVERLAPPEDWINDOW;
	
	(* Extended Window Styles *)
	WS_EX_DLGMODALFRAME* = {0};
	WS_EX_NOPARENTNOTIFY* = {2};
	WS_EX_TOPMOST* = {3};
	WS_EX_ACCEPTFILES* = {4};
	WS_EX_TRANSPARENT* = {5};
	WS_EX_MDICHILD* = {6};
	WS_EX_TOOLWINDOW* = {7};
	WS_EX_WINDOWEDGE* = {8};
	WS_EX_CLIENTEDGE* = {9};
	WS_EX_CONTEXTHELP* = {10};
	WS_EX_RIGHT* = {12};
	WS_EX_LEFT* = {};
	WS_EX_RTLREADING* = {13};
	WS_EX_LTRREADING* = {};
	WS_EX_LEFTSCROLLBAR* = {14};
	WS_EX_RIGHTSCROLLBAR* = {};
	WS_EX_CONTROLPARENT* = {16};
	WS_EX_STATICEDGE* = {17};
	WS_EX_APPWINDOW* = {18};
	WS_EX_OVERLAPPEDWINDOW* = WS_EX_WINDOWEDGE + WS_EX_CLIENTEDGE;
	WS_EX_PALETTEWINDOW* = WS_EX_WINDOWEDGE + WS_EX_TOOLWINDOW + WS_EX_TOPMOST;
	
	(* Color Types *)
	CTLCOLOR_MSGBOX* = 0;
	CTLCOLOR_EDIT* = 1;
	CTLCOLOR_LISTBOX* = 2;
	CTLCOLOR_BTN* = 3;
	CTLCOLOR_DLG* = 4;
	CTLCOLOR_SCROLLBAR* = 5;
	CTLCOLOR_STATIC* = 6;
	CTLCOLOR_MAX* = 7;

	COLOR_SCROLLBAR* = 0;
	COLOR_BACKGROUND* = 1;
	COLOR_ACTIVECAPTION* = 2;
	COLOR_INACTIVECAPTION* = 3;
	COLOR_MENU* = 4;
	COLOR_WINDOW* = 5;
	COLOR_WINDOWFRAME* = 6;
	COLOR_MENUTEXT* = 7;
	COLOR_WINDOWTEXT* = 8;
	COLOR_CAPTIONTEXT* = 9;
	COLOR_ACTIVEBORDER* = 10;
	COLOR_INACTIVEBORDER* = 11;
	COLOR_APPWORKSPACE* = 12;
	COLOR_HIGHLIGHT* = 13;
	COLOR_HIGHLIGHTTEXT* = 14;
	COLOR_BTNFACE* = 15;
	COLOR_BTNSHADOW* = 16;
	COLOR_GRAYTEXT* = 17;
	COLOR_BTNTEXT* = 18;
	COLOR_INACTIVECAPTIONTEXT* = 19;
	COLOR_BTNHIGHLIGHT* = 20;

	COLOR_3DDKSHADOW* = 21;
	COLOR_3DLIGHT* = 22;
	COLOR_INFOTEXT* = 23;
	COLOR_INFOBK* = 24;

	COLOR_HOTLIGHT* = 26;
	COLOR_GRADIENTACTIVECAPTION* = 27;
	COLOR_GRADIENTINACTIVECAPTION* = 28;

	COLOR_MENUHILIGHT* = 29;
	COLOR_MENUBAR* = 30;
	
	COLOR_DESKTOP* = COLOR_BACKGROUND;
	COLOR_3DFACE* = COLOR_BTNFACE;
	COLOR_3DSHADOW* = COLOR_BTNSHADOW;
	COLOR_3DHIGHLIGHT* = COLOR_BTNHIGHLIGHT;
	COLOR_3DHILIGHT* = COLOR_BTNHIGHLIGHT;
	COLOR_BTNHILIGHT* = COLOR_BTNHIGHLIGHT;
		
	(* Edit Control Styles *)
	ES_LEFT* = {};
	ES_CENTER* = {0};
	ES_RIGHT* = {1};
	ES_MULTILINE* = {2};
	ES_UPPERCASE* = {3};
	ES_LOWERCASE* = {4};
	ES_PASSWORD* = {5};
	ES_AUTOVSCROLL* = {6};
	ES_AUTOHSCROLL* = {7};
	ES_NOHIDESEL* = {8};
	ES_OEMCONVERT* = {10};
	ES_READONLY* = {11};
	ES_WANTRETURN* = {12};
	ES_NUMBER* = {13};
	
	(* Virtual-Key Codes *)
	VK_SHIFT* = 10H;
	VK_CONTROL* = 11H;
	VK_LEFT* = 25H;
	VK_UP* = 26H;
	VK_RIGHT* = 27H;
	VK_DOWN* = 28H;
	
	(* MENUITEMINFO Masks *)
	MIIM_STATE* = {0};
	MIIM_ID* = {1};
	MIIM_SUBMENU* = {2};
	MIIM_CHECKMARKS* = {3};
	MIIM_TYPE* = {4};
	MIIM_DATA* = {5};
	MIIM_STRING* = {6};
	MIIM_BITMAP* = {7};
	MIIM_FTYPE* = {8};
	
	(* MENUITEMINFO Types *)
	MFT_STRING* = {};
	MFT_BITMAP* = {2};
	MFT_MENUBARBREAK* = {5};
	MFT_MENUBREAK* = {6};
	MFT_OWNERDRAW* = {8};
	MFT_RADIOCHECK* = {9};
	MFT_SEPARATOR* = {11};
	MFT_RIGHTORDER* = {13};
	MFT_RIGHTJUSTIFY* = {14};
	
	(* MENUITEMINFO States *)
	MFS_GRAYED* = {0, 1};
	MFS_DISABLED* =	MFS_GRAYED;
	MFS_CHECKED* = {3};
	MFS_HILITE* = {7};
	MFS_ENABLED* = {};
	MFS_UNCHECKED* = {};
	MFS_UNHILITE* = {};
	MFS_DEFAULT* = {12};

TYPE
	HANDLE* = INTEGER;
	BOOL* = CARD32;
	WSTR* = ARRAY OF CHAR;
	LPVOID* = INTEGER;

	WNDPROC* = PROCEDURE (
		hWnd: HANDLE;
		uMsg: CARD32;
		wParam: INTEGER;
		lParam: INTEGER
	): INTEGER;
	
	POINT* = RECORD
		x*, y*: INT32
	END;
	
	MSG* = RECORD
		hwnd*: HANDLE;
		message*: CARD32;
		wParam*: INTEGER;
		lParam*: INTEGER;
		time*: CARD32;
		pt*: POINT
	END;
	
	WNDCLASSW* = RECORD
		style*: CARD32;
		lpfnWndProc*: WNDPROC;
		cbClsExtra*, cbWndExtra*: INT32;
		hInstance*: HANDLE;
		hIcon*: HANDLE;
		hCursor*: HANDLE;
		hbrBackground*: HANDLE;
		lpszMenuName*, lpszClassName*: LPVOID
	END;
	
	WNDCLASSEXW* = RECORD
		cbSize*: CARD32;
		style*: CARD32;
		lpfnWndProc*: WNDPROC;
		cbClsExtra*, cbWndExtra*: INT32;
		hInstance*: HANDLE;
		hIcon*: HANDLE;
		hCursor*: HANDLE;
		hbrBackground*: HANDLE;
		lpszMenuName*, lpszClassName*: LPVOID;
		hIconSm*: HANDLE
	END;
	
	RECT* = RECORD
		left*, right*, top*, bottom*: INT32
	END;
	
	PAINTSTRUCT* = RECORD
		hdc*: HANDLE;
		fErase*: BOOL;
		rcPaint*: RECT;
		fRestore*, fIncUpdate*: BOOL;
		rgbReserved*: ARRAY 32 OF BYTE
	END;

	MENUITEMINFO* = RECORD
		cbSize*: CARD32;
		fMask*, fType*, fState*: CARD32;
		wID*: CARD32;
		hSubMenu*: HANDLE;
		hbmpChecked*, hbmpUnchecked*: HANDLE;
		dwItemData*, dwTypeData*: INTEGER;
		cch*: INT32;
		hbmpItem*: HANDLE
	END;

(* Window Class functions *)
PROCEDURE RegisterClassW* (lpWndClass: WNDCLASSW): CARD16;
PROCEDURE RegisterClassExW* (lpWndClass: WNDCLASSEXW): CARD16;

(* Window Functions *)
PROCEDURE CreateWindowExW*(
	dwExStyle: CARD32;
	lpClassName, lpWindowName: ARRAY OF CHAR;
	dwStyle: CARD32;
	x, y, nWidth, nHeight: INT32;
	hWndParent: HANDLE;
	hMenu: HANDLE;
	hInstance: HANDLE;
	lpParam: LPVOID
): HANDLE;

PROCEDURE CreateWindowExA*(
	dwExStyle: CARD32;
	lpClassName, lpWindowName: ARRAY OF CHAR8;
	dwStyle: CARD32;
	x, y, nWidth, nHeight: INT32;
	hWndParent: HANDLE;
	hMenu: HANDLE;
	hInstance: HANDLE;
	lpParam: LPVOID
): HANDLE;

PROCEDURE ShowWindow* (hWnd: HANDLE; nCmdShow: INT32): BOOL;
PROCEDURE DestroyWindow* (hWnd: HANDLE): BOOL;
PROCEDURE GetClientRect* (hWnd: HANDLE; VAR lpRect: RECT): BOOL;

(* Window Procedure functions *)
PROCEDURE DefWindowProcW*(
	hWnd: HANDLE;
	uMsg: CARD32;
	wParam: INTEGER;
	lParam: INTEGER
): INTEGER;

(* Message functions *)
PROCEDURE GetMessageW*(
	VAR lpMsg: MSG;
	hWnd: HANDLE;
	uMsgFilterMin, uMsgFilterMax: CARD32
): BOOL;

PROCEDURE TranslateMessage* (lpMsg: MSG): BOOL;
PROCEDURE DispatchMessageW* (lpMsg: MSG): BOOL;
PROCEDURE PostQuitMessage* (nExitCode: INT32);

(* Dialog Box functions *)
PROCEDURE MessageBoxW* (
	hwnd: HANDLE;
	lpText, lpCaption: ARRAY OF CHAR;
	uType: CARD32
): INT32;

(* Menu Functions *)
PROCEDURE CreateMenu*(): HANDLE;
PROCEDURE CreatePopupMenu*(): HANDLE;

PROCEDURE InsertMenuItemW*(
	hMenu: HANDLE;
	uItem: CARD32;
	fByPosition: BOOL;
	lpmii: MENUITEMINFO
): BOOL;

(* Painting and Drawing functions *)
PROCEDURE BeginPaint* (hwnd: HANDLE; VAR lpPaint: PAINTSTRUCT): HANDLE;

END User32.