# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

The old version is written in Component Pascal, compiled by Gardens Point Component Pascal, and require .NET 2.0 to run. The current version is written in Oberon-07 and self-compiled itself.

Binary is also supply with source.

## Why Niklaus Wirth was right after all, or What I had learned from this compiler

https://github.com/congdm/AyaCompiler/wiki/Niklaus-Wirth-was-right-after-all

## What I had learned from this compiler (part 2)

https://github.com/congdm/AyaCompiler/wiki/What-I-had-learned-from-this-compiler-(part-2)

## Minimal Hello World program on Windows (no runtime)

```oberon
MODULE Test;
(*$MAIN*)

IMPORT
	SYSTEM;
TYPE
	AsciiStr = ARRAY 64 OF BYTE;
	Handle = INTEGER;
	Uint = SYSTEM.DWORD;
	PChar = ADDRESS OF CHAR;
VAR
	MessageBoxW: PROCEDURE (hwnd: Handle; lpText, lpCaption: PChar; uType: Uint);
	
PROCEDURE Make_AsciiStr (VAR out : ARRAY OF BYTE; in : ARRAY OF CHAR);
	VAR n, i : INTEGER;
BEGIN
	n := LEN(out); IF n > LEN(in) THEN n := LEN(in) END;
	i := 0; WHILE i < n DO out[i] := ORD(in[i]); i := i + 1 END
END Make_AsciiStr;

PROCEDURE Init;
	VAR user32: Handle; str: AsciiStr; 
BEGIN
	SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	Make_AsciiStr (str, 'MessageBoxW');
	SYSTEM.GetProcAddress (MessageBoxW, user32, SYSTEM.ADR(str))
END Init;

PROCEDURE Main;
	CONST mess = 'Oberon for Win64'; title = 'Hello, World!';
BEGIN
	MessageBoxW (0, SYSTEM.STRADR(mess), SYSTEM.STRADR(title), 0)
END Main;
	
BEGIN
	Init;
	Main
END Test.
```


