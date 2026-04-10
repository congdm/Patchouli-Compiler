MODULE Poc;
(*$CONSOLE*)

IMPORT
	SYSTEM,
	Rtl := [Oberon07.Rtl], Out := [Oberon07.Out], Files := [Oberon07.Files],
	Sys, Scn, Psr, PsrX8664, BaseX8664;
	
VAR
	arg, fname: ARRAY 1024 OF CHAR;
	buildfile: Files.File; argIdx: INTEGER;
	buildMode, errFlag: BOOLEAN;
	
PROCEDURE Compile(fname: ARRAY OF CHAR);
	VAR srcfile: Files.File;
		sym, startTime, endTime: INTEGER;
		scn: Scn.Scanner; psr: Psr.Parser; mod: BaseX8664.Module;
BEGIN
	Scn.SetInput(scn, fname, 0); Scn.Get(scn);
	IF scn.sym = scn.module THEN
		
	ELSE Scn.Mark(scn, 'MODULE keyword expected')
	END 
END Compile;

PROCEDURE ErrorNotFound(fname: ARRAY OF CHAR);
BEGIN
	Out.String('File '); Out.String(fname);
	Out.String(' not found'); Out.Ln
END ErrorNotFound;

PROCEDURE Build(fname: ARRAY OF CHAR);
	VAR r: Files.Rider; i: INTEGER; x: BYTE; start, end: INTEGER;
		byteStr: ARRAY 1024 OF BYTE; srcfname: ARRAY 1024 OF CHAR;
BEGIN
	start := Rtl.Time(); buildfile := Files.Old(fname);
	Files.Set(r, buildfile, 0); i := 0; Files.Read(r, x);
	WHILE ~r.eof DO
		WHILE (x <= 32) & ~r.eof DO Files.Read(r, x) END;
		WHILE (x > 32) & ~r.eof DO
			byteStr[i] := x; Files.Read(r, x); INC(i)
		END;
		IF i > 0 THEN
			byteStr[i] := 0; i := Rtl.Utf8ToUnicode(byteStr, srcfname);
			IF Files.Old(srcfname) # NIL THEN Compile(srcfname)
			ELSE ErrorNotFound(srcfname)
			END;
			Out.Ln; i := 0
		END
	END;
	end := Rtl.Time();
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
			(*
			IF arg[0] = '/' THEN Mark('path to symbols?'); Option
			ELSE B.SetSymPath(arg); Get; Arguments
			END
			*)
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

(*
PROCEDURE NotifyError(pos: INTEGER; msg: ARRAY OF CHAR);
BEGIN
	Out.String('file pos '); Out.Int(pos, 0);
	Out.String(': '); Out.String(msg); Out.Ln
END NotifyError;
*)
	
BEGIN
	(* S.InstallNotifyError(NotifyError); *) Get; Arguments;
	IF fname[0] # 0X THEN
		IF Files.Old(fname) # NIL THEN
			IF ~buildMode THEN Compile(fname) ELSE Build(fname) END
		ELSE ErrorNotFound(fname)
		END
	ELSE
		Out.String('Patchouli Oberon-07 Compiler v0.999999'); Out.Ln;
 		Out.String('Usage: Poc <inputfile>'); Out.Ln
	END
END Poc.