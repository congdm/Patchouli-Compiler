MODULE Test2;
(*$MAIN*)

IMPORT
	SYSTEM, WinApi;

PROCEDURE Main;
	CONST mess = 'Oberon for Win64'; title = 'Hello, World!';
BEGIN
	WinApi.MessageBoxW (0, SYSTEM.ADR(mess), SYSTEM.ADR(title), 0)
END Main;
	
BEGIN
	Main
END Test2.