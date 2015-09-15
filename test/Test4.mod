MODULE Test4;
(*$MAIN*)

IMPORT
    User32;

BEGIN
    User32.MessageBoxW (0, 'Oberon for Win64', 'Hello, World!', 0)
END Test4.