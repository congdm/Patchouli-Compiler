MODULE Test6;
(*$MAIN*)

IMPORT
	SYSTEM, WinApi;
	
VAR
	hNil : WinApi.Handle;
	nilAdr : WinApi.Address;
	
PROCEDURE FillBytes (VAR buf : ARRAY OF BYTE; val : BYTE);
	VAR i : INTEGER;
BEGIN
	i := 0; WHILE i < LEN(buf) DO buf[i] := val; i := i + 1 END
END FillBytes;

PROCEDURE WindowProc (
	hWnd : WinApi.Handle;
	uMsg : SYSTEM.DWORD;
	wParam, lParam : INTEGER
) : INTEGER;
	VAR funcRes : INTEGER; bRes : WinApi.Bool;
BEGIN
	funcRes := 0;
	IF uMsg = WinApi.WM_CLOSE THEN bRes := WinApi.DestroyWindow (hWnd)
	ELSIF uMsg = WinApi.WM_DESTROY THEN WinApi.PostQuitMessage (0)
	ELSE funcRes := WinApi.DefWindowProcW (hWnd, uMsg, wParam, lParam)
	END
	RETURN funcRes
END WindowProc;

PROCEDURE Main;
	VAR hInstance, hwnd, hbr : WinApi.Handle;
		className, adrOfMsg : WinApi.Address;
		bRes, bRes2 : WinApi.Bool; wRes : SYSTEM.WORD;
		winclass : WinApi.Wndclassw; msg : WinApi.Msg;
BEGIN
	hInstance := WinApi.GetModuleHandleW (nilAdr);
	FillBytes (winclass, 0);

	className := WinApi.Adr('MyClass');
	winclass.lpfnWndProc := WindowProc;
	winclass.hInstance := hInstance;
	winclass.lpszClassName := className;
	hbr[0] := 1; winclass.hbrBackground := hbr;
	
	wRes := WinApi.RegisterClassW (WinApi.Adr(winclass));
	ASSERT (wRes # 0);
	
	
	hwnd := WinApi.CreateWindowExW (
		0, className, WinApi.Adr('MyWindow'), ORD(WinApi.WS_TILEDWINDOW),
		0, 0, 640, 480,
		hNil, hNil, hInstance, nilAdr
	);
	ASSERT (hwnd[0] # hNil[0]);
	
	bRes := WinApi.ShowWindow (hwnd, 1);
	adrOfMsg := WinApi.Adr(msg);
	REPEAT
		bRes := WinApi.GetMessageW (adrOfMsg, hNil, 0, 0);
		IF bRes[0] > 0 THEN
			bRes2 := WinApi.TranslateMessage (adrOfMsg);
			bRes2 := WinApi.DispatchMessageW (adrOfMsg)
		ELSIF bRes[0] < 0 THEN
			ASSERT(FALSE)
		END
	UNTIL bRes[0] <= 0
END Main;

BEGIN
	hNil [0] := 0; nilAdr [0] := 0;
	Main
END Test6.
