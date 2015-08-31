# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

The old version is written in Component Pascal, compiled by Gardens Point Component Pascal, and require .NET 2.0 to run. The current version is written in Oberon-07 and self-compiled itself.

Binary is also supply with source.

## Personal ranting

* [Niklaus Wirth was right after all](https://github.com/congdm/AyaCompiler/wiki/Niklaus-Wirth-was-right-after-all)
* [What I had learned from this compiler](https://github.com/congdm/AyaCompiler/wiki/What-I-had-learned-from-this-compiler-(part-2))

## Documentation

This compiler follows the latest [Oberon-07 Report](http://www.inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf) faithfully (hope so), with the following extensions (more exactly, complications):

### INT8, INT16, INT32, CARD16, CARD32 types

**Reason for introducing:** Same as BYTE type, in order to economise the use of memory.

**Note:** There is no type inclusion. All the integer types: BYTE, INT8, CARD16, INT16, CARD32, INT32 and INTEGER are interchangable. All integer variables when loaded to register are signed extended to 64-bit INTEGER type. All integer calculation results are 64-bit INTEGER. That means BYTE + BYTE = INTEGER. Therefore, there is no speed benefit for using 
small integer types over INTEGER type. The programmer should use only INTEGER type for most of cases, which is good programming practice.

**Note:** There is no range checking in assignments between integer types!

**Note:** The 64-bit CARDINAL is not introduced, because 64-bit INTEGER is big enough, and also because of you cannot loselessly convert CARDINAL to INTEGER.

### ADDRESS types

**Reason for introducing:** In order to interface with Win32 API.

**Note:** The ADDRESS types is same as POINTER in Modula-2 and PASCAL. You can declare ADDRESS of any suitable base type:
```oberon
TYPE PINTEGER = ADDRESS OF INTEGER;
```
And use the SYSTEM.ADR2 function to get the address of any variable:
```oberon
adr := SYSTEM.ADR2(x) (* If x type is T, then type of adr must be ADDRESS OF T *)
```
Finally, there also exists SYSTEM.STRADR function, which accepts an ARRAY OF CHAR or string variable, and outputs the address of that variable as ADDRESS OF CHAR.

**Note:** In order to use ADDRESS keyword, you must import SYSTEM module.

### UNION in RECORD

**Reason for introducing:** In order to interface with Win32 API.

**Note:** Usage example:
```oberon
TYPE
	OVERLAPPED* = RECORD
		Internal*, InternalHigh*: ULONG_PTR;
		UNION
			Offset*, OffsetHigh*: DWORD |
			Pointer*: PVOID
		END;
		hEvent*: HANDLE
	END;
```

**Note:** In order to use UNION keyword, you must import SYSTEM module.

### Standard procedure DISPOSE

**Reason for introducing:** The native Win32 environment doesn't have Garbage Collection (GC).

**Note:** The progammer can introduce his own GC module later and refrain from using DISPOSE.

### Multiple CHAR types

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
