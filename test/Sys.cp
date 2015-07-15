MODULE Sys;

IMPORT
	WinApi;
	
CONST
	failed = FALSE; success = TRUE;
	MIN_INT = -9223372036854775807 - 1;
	
TYPE
	FileHandle* = RECORD
		f : WinApi.Handle
	END;
	
PROCEDURE Get_executable_path* (VAR out : ARRAY OF CHAR; VAR n : INTEGER);
	VAR
		asm : Reflect.Assembly;
		s : System.String;
		i : INTEGER;
BEGIN
	asm := Reflect.Assembly.GetEntryAssembly();
	s := asm.get_Location();
	i := s.LastIndexOf('\');
	s := s.Remove(i + 1);
	out := s; n := s.get_Length()
END Get_executable_path;
	
PROCEDURE Show_error* (msg : ARRAY OF CHAR);
BEGIN
	Console.WriteString ('ERROR: ');
	Console.WriteString (msg);
	Console.WriteLn;
END Show_error;

PROCEDURE File_existed* (filename : ARRAY OF CHAR) : BOOLEAN;
	VAR res : Dword;
BEGIN
	res := WinApi.GetFileAttributesW(filename);
	RETURN res # ORD(WinApi.INVALID_FILE_ATTRIBUTES)
END File_existed;
	
PROCEDURE Open* (VAR file : FileHandle; filename : ARRAY OF CHAR);
BEGIN
	IF File_existed(filename) THEN
		file.f := WinApi.CreateFileW(
			SYSTEM.STRADR (filename), ORD(WinApi.GENERIC_READ), 0, NIL,
			WinApi.OPEN_EXISTING, 0, 0
		)
	ELSE
		Show_error ('File not existed!')
	END;
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
	VAR res: Bool;
BEGIN
	IF file.f # 0 THEN
		res := WinApi.CloseHandle(file.f); file.f := 0
	END
END Close;

PROCEDURE Rename_file* (oldname, newname : ARRAY OF CHAR);
	VAR res: Bool;
BEGIN
	res := WinApi.MoveFileW(SYSTEM.STRADR(oldname), SYSTEM.STRADR(newname))
END Rename_file;

PROCEDURE Delete_file* (filename : ARRAY OF CHAR);
	VAR res: Bool;
BEGIN
	res := WinApi.DeleteFileW(SYSTEM.STRADR(filename))
END Delete_file;

PROCEDURE Calculate_MD5_hash* (VAR file : FileHandle; VAR buf : ARRAY OF UBYTE);
(*
	VAR result : POINTER TO ARRAY OF UBYTE; md5hasher : Crypt.MD5; i : INTEGER;
*)
BEGIN
(*
	md5hasher := Crypt.MD5.Create();
	result := md5hasher.ComputeHash (file.f);
	FOR i := 0 TO 15 DO buf[i] := result[i] END
*)
	(* stub *)
END Calculate_MD5_hash;

(* -------------------------------------------------------------------------- *)

PROCEDURE Read_ansi_char* (VAR file : FileHandle; VAR c : CHAR) : BOOLEAN;
	VAR
		i : INTEGER;
		result : BOOLEAN;
BEGIN
	i := file.r.Read ();
	IF i = -1 THEN result := failed
	ELSE c := System.Convert.ToChar(i); result := success END;
	RETURN result
END Read_ansi_char;

PROCEDURE Read_byte* (VAR file : FileHandle; VAR n : INTEGER);
BEGIN
	n := file.f.ReadByte()
END Read_byte;
	
PROCEDURE Read_2bytes* (VAR file : FileHandle; VAR n : INTEGER);
BEGIN
	n := file.f.ReadByte();
	n := n + file.f.ReadByte() * 256
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
BEGIN
	n := file.f.ReadByte();
	n := n + file.f.ReadByte() * 256;
	n := n + file.f.ReadByte() * 65536;
	n := n + ASH (file.f.ReadByte(), 24)
END Read_4bytes;
	
PROCEDURE Read_8bytes* (VAR file : FileHandle; VAR n : LONGINT);
	VAR lo, hi : INTEGER;
BEGIN
	lo := 0; hi := 0;
	Read_4bytes (file, lo); Read_4bytes (file, hi);
	n := hi; n := ASH(n, 32); n := n + lo
END Read_8bytes;

(* -------------------------------------------------------------------------- *)
	
PROCEDURE Write_byte* (VAR file : FileHandle; n : INTEGER);
	VAR b : UBYTE;
BEGIN
	b := USHORT(n); file.f.WriteByte (b)
END Write_byte;

PROCEDURE Write_ansi_str* (VAR file : FileHandle; str : ARRAY OF CHAR);
	VAR b : UBYTE; i : INTEGER;
BEGIN
	i := 0;
	WHILE i < LEN(str) DO
		b := USHORT (ORD (str[i])); file.f.WriteByte (b); INC (i)
	END
END Write_ansi_str;
	
PROCEDURE Write_2bytes* (VAR file : FileHandle; n : INTEGER);
	VAR b : UBYTE;
BEGIN
	b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b)
END Write_2bytes;

PROCEDURE Write_string* (VAR file : FileHandle; str : ARRAY OF CHAR);
	VAR i : INTEGER;
BEGIN
	i := 0;
	WHILE str[i] # 0X DO Write_2bytes (file, ORD (str[i])); i := i + 1 END;
	Write_2bytes (file, 0)
END Write_string;
	
PROCEDURE Write_4bytes* (VAR file : FileHandle; n : INTEGER);
	VAR b : UBYTE;
BEGIN
	b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b)
END Write_4bytes;
	
PROCEDURE Write_8bytes* (VAR file : FileHandle; n : LONGINT);
	VAR b : UBYTE;
BEGIN
	b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b);
	n := n DIV 256; b := USHORT(n MOD 256); file.f.WriteByte (b)
END Write_8bytes;

PROCEDURE FilePos* (VAR file : FileHandle) : LONGINT;
BEGIN
	RETURN file.f.get_Position()
END FilePos;

PROCEDURE Seek* (VAR file : FileHandle; pos : LONGINT);
	VAR res : LONGINT;
BEGIN
	res := file.f.Seek (pos, IO.SeekOrigin.Begin);
	ASSERT (res = pos)
END Seek;

PROCEDURE SeekRel* (VAR file : FileHandle; offset : LONGINT);
	VAR res : LONGINT;
BEGIN
	res := file.f.Seek (offset, IO.SeekOrigin.Current)
END SeekRel;

PROCEDURE Int_to_string* (x : LONGINT; VAR str : ARRAY OF CHAR);
	VAR negative : BOOLEAN; s : ARRAY 32 OF CHAR; i, j : INTEGER;
BEGIN
	IF x = MIN_INT THEN str := '-9223372036854775808'
	ELSE
		IF x < 0 THEN negative := TRUE; x := -x
		ELSE negative := FALSE
		END;
		
		i := 0;
		REPEAT
			s[i] := CHR(x MOD 10 + ORD('0'));
			INC (i); x := x DIV 10
		UNTIL x = 0;
		
		IF negative THEN
			str[0] := '-';
			FOR j := 0 TO i - 1 DO str[j + 1] := s[i - 1 - j] END;
			str[i + 1] := 0X
		ELSE
			FOR j := 0 TO i - 1 DO str[j] := s[i - 1 - j] END;
			str [i] := 0X
		END
	END
END Int_to_string;
	
PROCEDURE Console_WriteInt* (x : LONGINT);
	VAR
		a : ARRAY 21 OF CHAR;
BEGIN
	Int_to_string (x, a);
	Console.WriteString (a)
END Console_WriteInt;
	
PROCEDURE Console_WriteString* (s : ARRAY OF CHAR);
BEGIN
	Console.WriteString (s)
END Console_WriteString;

PROCEDURE Console_WriteLn*;
BEGIN
	Console.WriteLn
END Console_WriteLn;

BEGIN
END Sys.