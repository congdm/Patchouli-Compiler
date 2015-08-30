# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

The old version is written in Component Pascal, compiled by Gardens Point Component Pascal, and require .NET 2.0 to run. The current version is written in Oberon-07 and self-compiled itself.

Binary is also supply with source.

## Niklaus Wirth was right after all

https://github.com/congdm/AyaCompiler/wiki/Niklaus-Wirth-was-right-after-all

## What I had learned from this compiler (part 2)

https://github.com/congdm/AyaCompiler/wiki/What-I-had-learned-from-this-compiler-(part-2)

## Minimal Hello World program on Windows (no runtime)

```oberon
MODULE Test;
(*$MAIN*)

IMPORT
    SYSTEM;	
CONST
	mess = 'Oberon for Win64';
	title = 'Hello, World!';
TYPE
    Handle = INTEGER;
    Uint = CARD32;
    PChar = ADDRESS OF CHAR;
VAR
    MessageBoxW: PROCEDURE (
		hwnd: Handle;
		lpText, lpCaption: PChar;
		uType: Uint
	);
	user32: Handle;

BEGIN
    SYSTEM.LoadLibraryW (user32, 'USER32.DLL');
	SYSTEM.GetProcAddress (MessageBoxW, user32, 'MessageBoxW'@);
	MessageBoxW (0, SYSTEM.STRADR(mess), SYSTEM.STRADR(title), 0)
END Test.
```
