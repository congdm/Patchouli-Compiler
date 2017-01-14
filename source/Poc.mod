MODULE Poc;
(*$CONSOLE*)

IMPORT
	Rtl, Out, Files,
	S := Scanner, B := Base, P := Parser;
	
VAR
	arg, fname: ARRAY 1024 OF CHAR;
	argIdx: INTEGER; buildMode, errFlag: BOOLEAN;
	
PROCEDURE Compile(fname: ARRAY OF CHAR);
	VAR srcfile: Files.File; sym, startTime, endTime: INTEGER;
BEGIN
	Out.String('Compile '); Out.String(fname); Out.Ln; B.SetSrcPath(fname);
	srcfile := Files.Old(fname); S.Init(srcfile, 0); S.Get(sym);
	
	startTime := Rtl.Time();
	IF sym = S.module THEN P.Module ELSE S.Mark('MODULE?') END;
	IF S.errcnt = 0 THEN
		endTime := Rtl.Time(); Out.String('Compile time: ');
		Out.Int(Rtl.TimeToMSecs(endTime - startTime), 0);
		Out.String(' miliseconds'); Out.Ln
	END;
	Rtl.Collect
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

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Get;
BEGIN INC(argIdx); Rtl.GetArg(arg, argIdx)
END Get;

PROCEDURE Mark(msg: ARRAY OF CHAR);
BEGIN
	Out.String('arg '); Out.Int(argIdx, 0); Out.String(': ');
	Out.String(msg); Out.Ln; errFlag := TRUE
END Mark;

PROCEDURE Arguments;
	PROCEDURE Option;
	BEGIN Rtl.LowerCase(arg);
		IF arg = '/b' THEN buildMode := TRUE; Get; Arguments
		ELSIF arg = '/sym' THEN Get;
			IF arg[0] = '/' THEN Mark('path to symbols?'); Option
			ELSE B.SetSymPath(arg); Get; Arguments
			END
		ELSE (* unhandled *) Get; Arguments
		END
	END Option;
BEGIN (* Arguments *)
	IF arg = 0X THEN (* end parsing *)
	ELSIF arg[0] # '/' THEN
		IF fname[0] = 0X THEN fname := arg
		ELSE Mark('another filename?')
		END;
		Get; Arguments
	ELSIF arg[0] = '/' THEN Option
	END
END Arguments;
	
BEGIN
	Get; Arguments;
	IF fname[0] # 0X THEN
		IF Rtl.ExistFile(fname) THEN
			IF ~buildMode THEN Compile(fname) ELSE Build(fname) END
		ELSE ErrorNotFound(fname)
		END
	ELSE
		Out.String('Patchouli Oberon-07 Compiler v0.8f'); Out.Ln;
 		Out.String('Usage: Poc <inputfile>'); Out.Ln
	END
END Poc.