MODULE Poc;
(*$CONSOLE*)

IMPORT
	Rtl, Out,
	S := Scanner, B := Base, P := Parser;
	
VAR
	str, fname: ARRAY 1024 OF CHAR;
	i: INTEGER; buildMode: BOOLEAN;
	
PROCEDURE Compile(fname: ARRAY OF CHAR);
	VAR srcfile: Rtl.File; sym: INTEGER;
BEGIN
	Out.String('Compile '); Out.String(fname); Out.Ln;
	Rtl.Reset(srcfile, fname); sym := 0;
	S.Init(srcfile, 0); Rtl.Close(srcfile); S.Get(sym);
	IF sym = S.module THEN P.Module ELSE S.Mark('MODULE?') END
END Compile;

PROCEDURE ErrorNotFound(fname: ARRAY OF CHAR);
BEGIN
	Out.String('File '); Out.String(fname);
	Out.String(' not found'); Out.Ln
END ErrorNotFound;

PROCEDURE Build(fname: ARRAY OF CHAR);
	VAR build: Rtl.File; x, i: INTEGER; start, end: INTEGER;
		byteStr: ARRAY 1024 OF BYTE; srcfname: ARRAY 1024 OF CHAR;
BEGIN
	start := Rtl.Time();
	Rtl.Reset(build, fname); Rtl.Read1(build, x);
	WHILE x # -1 DO i := 0;
		WHILE (x > 32) DO byteStr[i] := x; Rtl.Read1(build, x); INC(i) END;
		WHILE (x >= 0) & (x <= 32) DO Rtl.Read1(build, x) END;
		IF i > 0 THEN
			byteStr[i] := 0; i := Rtl.Utf8ToUnicode(byteStr, srcfname);
			IF Rtl.ExistFile(srcfname) THEN Compile(srcfname)
			ELSE ErrorNotFound(srcfname)
			END;
			Out.Ln; i := 0
		END
	END;
	Rtl.Close(build); end := Rtl.Time();
	Out.String('Total build time: ');
	Out.Int(Rtl.TimeToMSecs(end-start), 0);
	Out.String(' miliseconds'); Out.Ln
END Build;
	
BEGIN
	fname[0] := 0X; i := 1; Rtl.GetArg(str, i);
	WHILE str[0] # 0X DO
		IF (str[0] # '/') & (fname[0] = 0X) THEN fname := str
		ELSIF str[0] = '/' THEN Rtl.LowerCase(str);
			IF str = '/h' THEN B.SetFlag('handle')
			ELSIF (str = '/b') THEN buildMode := TRUE
			END
		END;
		INC(i); Rtl.GetArg(str, i)
	END;
	IF fname[0] # 0X THEN
		IF Rtl.ExistFile(fname) THEN
			IF ~buildMode THEN Compile(fname) ELSE Build(fname) END
		ELSE ErrorNotFound(fname)
		END
	ELSE
		Out.String('Patchouli Oberon-07 Compiler v0.8d'); Out.Ln;
 		Out.String('Usage: Poc <inputfile>'); Out.Ln
	END
END Poc.