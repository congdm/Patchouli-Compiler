MODULE Test6;
(*$MAIN*)

IMPORT
	SYSTEM, WinApi, Console;
	
PROCEDURE ZeroClearRecord (adr, size : INTEGER);
	VAR i : INTEGER;
BEGIN
	i := 0;
	WHILE i < size DO
		SYSTEM.PUT (adr + i, SYSTEM.VAL(BYTE, 0)); i := i + 1
	END
END ZeroClearRecord;

PROCEDURE WindowProc (hWnd, uMsg, wParam, lParam : INTEGER) : INTEGER;
	VAR funcRes, res : INTEGER;
BEGIN
	funcRes := 0;
	IF uMsg = WinApi.WM_CLOSE THEN res := WinApi.DestroyWindow (hWnd)
	ELSIF uMsg = WinApi.WM_DESTROY THEN WinApi.PostQuitMessage (0)
	ELSE funcRes := WinApi.DefWindowProcW (hWnd, uMsg, wParam, lParam)
	END
RETURN funcRes
END WindowProc;

PROCEDURE Main;
	VAR hInstance, className, res, res2, hWnd, adrOfMsg : INTEGER;
		resw : SYSTEM.WORD;
		winclass : WinApi.Wndclass;
		msg : WinApi.Msg;
BEGIN
	hInstance := WinApi.GetModuleHandleW(0);
	ZeroClearRecord (SYSTEM.ADR(winclass), SYSTEM.SIZE(WinApi.Wndclass));

	className := SYSTEM.ADR('MyClass');
	winclass.lpfnWndProc := WindowProc;
	winclass.hInstance := hInstance;
	winclass.lpszClassName := className;
	winclass.hbrBackground := 1;
	
	hWnd := 0;
	resw := WinApi.RegisterClassW (SYSTEM.ADR(winclass));
	IF resw # 0 THEN
		hWnd := WinApi.CreateWindowExW (
			0, className, SYSTEM.ADR('MyWindow'), ORD(WinApi.WS_TILEDWINDOW),
			0, 0, 640, 480, 0, 0, hInstance, 0
		)
	END;
	
	IF hWnd # 0 THEN
		res := WinApi.ShowWindow (hWnd, 1);
		adrOfMsg := SYSTEM.ADR(msg);
		REPEAT
			res := WinApi.GetMessageW (adrOfMsg, 0, 0, 0);
			IF res > 0 THEN
				res2 := WinApi.TranslateMessage (adrOfMsg);
				res2 := WinApi.DispatchMessageW (adrOfMsg)
			ELSIF res < 0 THEN
			END
		UNTIL res <= 0
	END
END Main;

BEGIN
	Main
END Test6.
