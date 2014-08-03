MODULE Sys;

IMPORT
	System := "[mscorlib]System",
	Reflect := "[mscorlib]System.Reflection",
	IO := "[mscorlib]System.IO",
	Console;
	
CONST
	failed = FALSE; success = TRUE;
	MIN_INT = -2147483648;
	
TYPE
	FileHandle* = RECORD
		f : IO.FileStream;
		r : IO.StreamReader;
		w : IO.StreamWriter
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
	
PROCEDURE Open* (VAR file : FileHandle; filename : ARRAY OF CHAR);
BEGIN
	IF IO.File.Exists (MKSTR(filename)) THEN
		file.f := IO.File.OpenRead (MKSTR(filename));
		file.r := IO.StreamReader.init (file.f)
	ELSE
		Show_error ('File not existed!')
	END;
END Open;
	
PROCEDURE Rewrite* (VAR file : FileHandle; filename : ARRAY OF CHAR);
BEGIN
	file.f := IO.File.Create (MKSTR (filename));
	file.w := IO.StreamWriter.init (file.f)
END Rewrite;

PROCEDURE Close* (VAR file : FileHandle);
BEGIN
	IF file.f # NIL THEN
		IF file.w # NIL THEN file.w.Flush; END;
		file.f.Close; file.f := NIL; file.w := NIL; file.r := NIL
	END
END Close;

PROCEDURE Read_char* (VAR file : FileHandle; VAR c : CHAR) : BOOLEAN;
	VAR
		i : INTEGER;
		result : BOOLEAN;
BEGIN
	i := file.r.Read ();
	IF i = -1 THEN result := failed
	ELSE c := System.Convert.ToChar(i); result := success END;
	RETURN result
END Read_char;
	
PROCEDURE Write_string* (VAR file : FileHandle; str : ARRAY OF CHAR);
BEGIN
	file.w.Write (MKSTR(str))
END Write_string;
	
PROCEDURE Write_newline* (VAR file : FileHandle);
BEGIN
	file.w.WriteLine
END Write_newline;
	
PROCEDURE Int_to_string* (x : LONGINT; VAR str : ARRAY OF CHAR);
	VAR
		negative : BOOLEAN;
		s : ARRAY 32 OF CHAR;
		i, j : INTEGER;
BEGIN
	IF x = MIN_INT THEN
		str := '-2147483648'
	ELSE
		IF x < 0 THEN negative := TRUE; x := -x
		ELSE negative := FALSE END;
		
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
	
PROCEDURE Write_number* (VAR file : FileHandle; x : LONGINT);
	VAR
		s : ARRAY 22 OF CHAR;
BEGIN
	Int_to_string (x, s);
	file.w.Write (MKSTR(s))
END Write_number;
	
PROCEDURE Write_char* (VAR file : FileHandle; ch : CHAR);
BEGIN
	file.w.Write (ch)
END Write_char;
	
PROCEDURE Write_byte* (VAR file : FileHandle; n : INTEGER);
BEGIN
END Write_byte;
	
PROCEDURE Write_2bytes* (VAR file : FileHandle; n : INTEGER);
BEGIN
END Write_2bytes;
	
PROCEDURE Write_4bytes* (VAR file : FileHandle; n : INTEGER);
BEGIN
END Write_4bytes;
	
PROCEDURE Write_8bytes* (VAR file : FileHandle; n : LONGINT);
BEGIN
END Write_8bytes;
	
PROCEDURE Copy_file* (VAR dst, src : FileHandle; n : INTEGER);
	VAR
		buffer : POINTER TO ARRAY OF CHAR;
		res : INTEGER;
BEGIN
	NEW (buffer, 4096);
	WHILE n > 0 DO
		IF n <= LEN(buffer) THEN res := src.r.ReadBlock (buffer, 0, n)
		ELSE res := src.r.ReadBlock (buffer, 0, LEN(buffer)) END;
		dst.w.Write (buffer, 0, res);
		DEC (n, LEN(buffer))
	END
END Copy_file;
	
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