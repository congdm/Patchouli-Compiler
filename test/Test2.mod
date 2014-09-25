MODULE Test2;
(*$MAIN*)

IMPORT
	SYSTEM, WinApi;

PROCEDURE Main;
	CONST mess = 'Oberon for Win64'; title = 'Hello, World!';
	VAR intResult : INTEGER; nilHandle : WinApi.Handle;
BEGIN
	nilHandle[0] := 0;
	intResult :=
		WinApi.MessageBoxW (nilHandle, WinApi.Adr(mess), WinApi.Adr(title), 0)
END Main;
	
BEGIN
	Main
END Test2.