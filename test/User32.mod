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
	WS_BORDER* = {22};
	WS_CAPTION* = WS_BORDER + {23};
	WS_SYSMENU* = {19};
	WS_THICKFRAME* = {18};
	WS_MINIMIZEBOX* = {17};
	WS_MAXIMIZEBOX* = {16};
	WS_OVERLAPPEDWINDOW* = WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU
		+ WS_THICKFRAME + WS_MINIMIZEBOX + WS_MAXIMIZEBOX;
	WS_TILEDWINDOW* = WS_OVERLAPPEDWINDOW;
	
	(* Virtual-Key Codes *)
	VK_SHIFT* = 10H;
	VK_CONTROL* = 11H;
	VK_LEFT* = 25H;
	VK_UP* = 26H;
	VK_RIGHT* = 27H;
	VK_DOWN* = 28H;

TYPE
	HANDLE* = INTEGER;
	BOOL* = CARD32;
	LPWSTR* = POINTER TO ARRAY OF CHAR;
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
		lpszMenuName*, lpszClassName*: LPWSTR
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

(* Window Class functions *)
PROCEDURE RegisterClassW* (lpWndClass: WNDCLASSW): CARD16;

(* Window functions *)
PROCEDURE CreateWindowExW*(
	dwExStyle: CARD32;
	lpClassName, lpWindowName: LPWSTR;
	dwStyle: CARD32;
	x, y, nWidth, nHeight: INT32;
	hWndParent: HANDLE;
	hMenu: HANDLE;
	hInstance: HANDLE;
	lpParam: LPVOID
): HANDLE;

PROCEDURE ShowWindow* (hWnd: HANDLE; nCmdShow: INT32): BOOL;
PROCEDURE DestroyWindow* (hWnd: HANDLE): BOOL;

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
	lpText, lpCaption: LPWSTR;
	uType: CARD32
): INT32;

(* Painting and Drawing functions *)
PROCEDURE BeginPaint* (hwnd: HANDLE; VAR lpPaint: PAINTSTRUCT): HANDLE;

END User32.