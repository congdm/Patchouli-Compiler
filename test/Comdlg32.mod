DEFINITION Comdlg32;

TYPE
	BOOL* = CARD32;
	
	LPCFHOOKPROC* = PROCEDURE(
		hdlg: INTEGER; uiMsg: CARD32; wParam: INTEGER; lParam: INTEGER
	): INTEGER;
	
	CHOOSEFONTW* = RECORD
		lStructSize*: CARD32;
		hwndOwner*, hDC*: INTEGER;
		lpLogFont*: INTEGER;
		iPointSize*: INT32;
		Flags*: CARD32;
		rgbColors*: CARD32;
		lCustData*: INTEGER;
		lpfnHook*: LPCFHOOKPROC;
		lpTemplateName*: INTEGER;
		hInstance*: INTEGER;
		lpszStyle*: INTEGER;
		nFontType*: CARD16;
		nSizeMin*, nSizeMax*: INT32
	END;

PROCEDURE ChooseFontW* (VAR lpcf: CHOOSEFONTW): BOOL;

END Comdlg32.