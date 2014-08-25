# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book.

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

Written in Component Pascal, compiled by Gardens Point Component Pascal, and require .NET 2.0 to run.

Binary is also supply with source.

## Minimal Hello World program on Windows (no runtime)

```oberon
MODULE Test;
(*$EXE*)

IMPORT
	SYSTEM;
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
VAR
	MessageBoxW* : PROCEDURE (hwnd, lpText, lpCaption, uType : INTEGER);
	
PROCEDURE Make_AsciiStr (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
	VAR n, i : INTEGER;
BEGIN
	n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
	i := 0; WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
END Make_AsciiStr;

PROCEDURE Init;
	VAR user32 : INTEGER;
		str : AsciiStr; 
BEGIN
	SYSTEM.LoadLibrary (user32, 'USER32.DLL');
	Make_AsciiStr (str, 'MessageBoxW');
	SYSTEM.GetProcAddress (MessageBoxW, user32, SYSTEM.ADR(str))
END Init;

PROCEDURE Main;
	CONST mess = 'Oberon for Win64'; title = 'Hello, World!';
BEGIN
	MessageBoxW (0, SYSTEM.ADR(mess), SYSTEM.ADR(title), 0)
END Main;
	
BEGIN
	Init;
	Main
END Test.
```


