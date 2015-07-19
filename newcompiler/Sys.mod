MODULE Sys;

IMPORT
	SYSTEM, Strings, WinApi, Console;
	
TYPE
	FileHandle* = RECORD
		f : WinApi.Handle
	END;
	
PROCEDURE File_existed* (filename : ARRAY OF CHAR) : BOOLEAN;
	VAR res : WinApi.Dword; ptr: WinApi.PChar;
BEGIN
	ptr := SYSTEM.STRADR(filename);
	res := WinApi.GetFileAttributesW(SYSTEM.STRADR(filename));
	RETURN res # ORD(WinApi.INVALID_FILE_ATTRIBUTES)
END File_existed;
	
PROCEDURE Open* (VAR file : FileHandle; filename : ARRAY OF CHAR);
BEGIN
	IF File_existed(filename) THEN
		file.f := WinApi.CreateFileW(
			SYSTEM.STRADR (filename),
			ORD(WinApi.GENERIC_READ + WinApi.GENERIC_WRITE), 0, NIL,
			WinApi.OPEN_EXISTING, 0, 0
		)
	ELSE Console.WriteString ('File not existed!'); Console.WriteLn
	END
END Open;
	
PROCEDURE Rewrite* (VAR file : FileHandle; filename : ARRAY OF CHAR);
BEGIN
	file.f := WinApi.CreateFileW(
		SYSTEM.STRADR (filename),
		ORD(WinApi.GENERIC_READ + WinApi.GENERIC_WRITE), 0, NIL,
		WinApi.OPEN_EXISTING, 0, 0
	)
END Rewrite;

PROCEDURE Close* (VAR file : FileHandle);
	VAR res: WinApi.Bool;
BEGIN
	IF file.f # 0 THEN
		res := WinApi.CloseHandle(file.f); file.f := 0
	END
END Close;

PROCEDURE Rename_file* (oldname, newname : ARRAY OF CHAR);
	VAR res: WinApi.Bool;
BEGIN
	res := WinApi.MoveFileW(SYSTEM.STRADR(oldname), SYSTEM.STRADR(newname))
END Rename_file;

PROCEDURE Delete_file* (filename : ARRAY OF CHAR);
	VAR res: WinApi.Bool;
BEGIN
	res := WinApi.DeleteFileW(SYSTEM.STRADR(filename))
END Delete_file;

(* -------------------------------------------------------------------------- *)

PROCEDURE Read_byte* (VAR file : FileHandle; VAR n : INTEGER);
	VAR res: WinApi.Bool; buf: BYTE; byteRead: WinApi.Dword;
BEGIN
	res := WinApi.ReadFile(
		file.f, SYSTEM.ADR(buf), 1, SYSTEM.ADR2(byteRead), NIL
	);
	IF (res = 0) OR (byteRead # 1) THEN n := -1 ELSE n := buf
	END
END Read_byte;
	
PROCEDURE Read_2bytes* (VAR file : FileHandle; VAR n : INTEGER);
	VAR res: WinApi.Bool; buf: WinApi.Word; byteRead: WinApi.Dword;
BEGIN
	res := WinApi.ReadFile(
		file.f, SYSTEM.ADR(buf), 2, SYSTEM.ADR2(byteRead), NIL
	);
	IF (res = 0) OR (byteRead # 2) THEN n := -1 ELSE n := buf
	END
END Read_2bytes;

PROCEDURE Read_string* (VAR file : FileHandle; VAR str : ARRAY OF CHAR);
	VAR i, n : INTEGER;
BEGIN
	i := -1; n := 0;
	REPEAT
		i := i + 1; Read_2bytes (file, n); str[i] := CHR(n)
	UNTIL str[i] = 0X
END Read_string;
	
PROCEDURE Read_4bytes* (VAR file : FileHandle; VAR n : INTEGER);
	VAR res: WinApi.Bool; buf, byteRead: WinApi.Dword;
BEGIN
	res := WinApi.ReadFile(
		file.f, SYSTEM.ADR(buf), 4, SYSTEM.ADR2(byteRead), NIL
	);
	IF (res = 0) OR (byteRead # 4) THEN n := -1 ELSE n := buf
	END
END Read_4bytes;
	
PROCEDURE Read_8bytes* (VAR file : FileHandle; VAR n : INTEGER);
	VAR res: WinApi.Bool; buf: INTEGER; byteRead: WinApi.Dword;
BEGIN
	res := WinApi.ReadFile(
		file.f, SYSTEM.ADR(buf), 8, SYSTEM.ADR2(byteRead), NIL
	);
	IF (res = 0) OR (byteRead # 8) THEN n := -1 ELSE n := buf
	END
END Read_8bytes;

(* -------------------------------------------------------------------------- *)
	
PROCEDURE Write_byte* (VAR file : FileHandle; n : INTEGER);
	VAR res: WinApi.Bool; buf: BYTE; byteWritten: WinApi.Dword;
BEGIN
	buf := n;
	res := WinApi.WriteFile(
		file.f, SYSTEM.ADR(buf), 1, SYSTEM.ADR2(byteWritten), NIL
	)
END Write_byte;

PROCEDURE Write_ansi_str* (VAR file : FileHandle; str : ARRAY OF CHAR);
	VAR i : INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(str)) & (str[i] # 0X) DO
		Write_byte (file, ORD(str[i])); INC (i)
	END;
	Write_byte (file, 0)
END Write_ansi_str;
	
PROCEDURE Write_2bytes* (VAR file : FileHandle; n : INTEGER);
	VAR res: WinApi.Bool; buf: WinApi.Word; byteWritten: WinApi.Dword;
BEGIN
	buf := n;
	res := WinApi.WriteFile(
		file.f, SYSTEM.ADR(buf), 2, SYSTEM.ADR2(byteWritten), NIL
	)
END Write_2bytes;

PROCEDURE Write_string* (VAR file : FileHandle; str : ARRAY OF CHAR);
	VAR i : INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(str)) & (str[i] # 0X) DO
		Write_2bytes (file, ORD(str[i])); i := i + 1
	END;
	Write_2bytes (file, 0)
END Write_string;
	
PROCEDURE Write_4bytes* (VAR file : FileHandle; n : INTEGER);
	VAR res: WinApi.Bool; buf, byteWritten: WinApi.Dword;
BEGIN
	buf := n;
	res := WinApi.WriteFile(
		file.f, SYSTEM.ADR(buf), 4, SYSTEM.ADR2(byteWritten), NIL
	)
END Write_4bytes;
	
PROCEDURE Write_8bytes* (VAR file : FileHandle; n : INTEGER);
	VAR res: WinApi.Bool; byteWritten: WinApi.Dword;
BEGIN
	res := WinApi.WriteFile(
		file.f, SYSTEM.ADR(n), 8, SYSTEM.ADR2(byteWritten), NIL
	)
END Write_8bytes;

PROCEDURE FilePos* (VAR file : FileHandle) : INTEGER;
	VAR res: WinApi.Bool; byteToMove, newPointer: WinApi.Large_integer;
BEGIN
	byteToMove.QuadPart := 0;
	res := WinApi.SetFilePointerEx(
		file.f, byteToMove, SYSTEM.ADR2(newPointer), WinApi.FILE_CURRENT
	);
	RETURN newPointer.QuadPart
END FilePos;

PROCEDURE Seek* (VAR file : FileHandle; pos : INTEGER);
	VAR res: WinApi.Bool; byteToMove, newPointer: WinApi.Large_integer;
BEGIN
	byteToMove.QuadPart := pos;
	res := WinApi.SetFilePointerEx(
		file.f, byteToMove, SYSTEM.ADR2(newPointer), WinApi.FILE_BEGIN
	)
END Seek;

PROCEDURE SeekRel* (VAR file : FileHandle; offset : INTEGER);
	VAR res: WinApi.Bool; byteToMove, newPointer: WinApi.Large_integer;
BEGIN
	byteToMove.QuadPart := offset;
	res := WinApi.SetFilePointerEx(
		file.f, byteToMove, SYSTEM.ADR2(newPointer), WinApi.FILE_CURRENT
	)
END SeekRel;

BEGIN
END Sys.