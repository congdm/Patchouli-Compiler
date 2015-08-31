# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

The old version is written in Component Pascal, compiled by Gardens Point Component Pascal, and require .NET 2.0 to run. The current version is written in Oberon-07 and self-compiled itself.

Binary is also supply with source.

## Documentation

This compiler follows the latest [Oberon-07 Report](http://www.inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf) faithfully (hope so), with the following extensions (more exactly, complications):

### INT8, INT16, INT32, CARD16, CARD32 types

**Reason for introducing:** Same as BYTE type, in order to economise the use of memory.

**Note:** There is no type inclusion. All the integer types: BYTE, INT8, CARD16, INT16, CARD32, INT32 and INTEGER are interchangable. All integer variables when loaded to register are signed extended to 64-bit INTEGER type. All integer calculation results are 64-bit INTEGER. That means BYTE + BYTE = INTEGER. Therefore, there is no speed benefit for using 
small integer types over INTEGER type. The programmer should use only INTEGER type for most of cases, which is good programming practice.

**Note:** There is no range checking in assignments between integer types!

**Note:** The 64-bit CARDINAL is not introduced, because 64-bit INTEGER is big enough, and also because of you cannot loselessly convert CARDINAL to INTEGER.

### ADDRESS types

### RECORD UNION

### Standard procedure DISPOSE

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
