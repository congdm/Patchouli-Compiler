MODULE Test7;(*$CONSOLE*)
IMPORT Rtl, Files;
VAR file: Files.File;

BEGIN
	file := Files.Old('Test7.mod'); file := NIL; Rtl.Collect
END Test7.