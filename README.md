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

**Note:** Type inclusion is not the same as in Oberon-2/Component Pascal. All the integer types: BYTE, INT8, CARD16, INT16, CARD32, INT32 and INTEGER are interchangable. All integer variables when loaded to register are signed extended to 64-bit INTEGER type. All integer calculation results are 64-bit INTEGER. That means BYTE + BYTE = INTEGER. Therefore, there is no speed benefit for using 
small integer types over INTEGER type. The programmer should use only INTEGER type for most of cases, which is good programming practice.

**Note:** There is no range checking in assignments between integer types!

**Note:** The 64-bit CARDINAL is not introduced, because 64-bit INTEGER is big enough, and also because of you cannot loselessly convert CARDINAL to INTEGER.

### EXTENSIBLE RECORD keyword.

**Reason for introducing:** For efficiency, because VAR param of non-extensible RECORD types doesn't need a hidden tag. Same as in Component Pascal.

### DEFINITION module for external DLL

**Reason for introducing:** In order to interface with Win32 API and external DLLs.

**Note:** For example usage, see Kernel32.mod file in source directory.

**Note:** You can declare CONSTs, TYPEs, VARs and PROCEDUREs in DEFINITION module. RECORD types declared in DEFINITION modules will be marked with unsafe flag, so their usage in normal Oberon modules will be restricted. POINTER types in DEFINITION module are not allowed. Open array formal parameter in DEFINITION module doesn't have hidden length tag (compatible with Win32 API).

**Note:** In DEFINITION module, RECORD can has UNION, for example:
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

### Casting INTEGER address into variable

**Reason for introducing:** To ease Win32 programming.

**Example:**
```oberon
TYPE
	ChArr = ARRAY OF CHAR;
VAR
	adr: INTEGER;
BEGIN
	adr := Kernel32.GetCommandLineW(); (* adr is INTEGER value *)
	adr{ChArr}[0] := 'A'; (* adr{ChArr} is the ChArr array pointed by adr *)
END;
```

**Note:** This is the equivalence of Cee pointer and even more dangerous. Although its usage is restricted in system modules, the programmer should not abuse this extension.

### Standard procedure DISPOSE

**Reason for introducing:** The native Win32 environment doesn't have Garbage Collection (GC).

**Note:** The progammer can introduce his own GC module later and refrain from using DISPOSE.

### Multiple CHAR types

**Reason for introducing:** There are many character encodings: ANSI, UTF-8, UCS-2, UTF-16, etc.

**Note:** The main CHAR type is 16-bit. The other char type is CHAR8, which is 8-bit character. CHAR and CHAR8 are compatible with each other, just like BYTE and INTEGER. There is no CHAR32 type, because UTF-32 encoding have little usage in real world.

**Note:** There are two types of string, the default one is 16-bit string and the other one is 8-bit:
```oberon
VAR
	str16: ARRAY 256 OF CHAR;
	str8: ARRAY 256 OF CHAR8;
BEGIN
	str16 := 'Hello, world!'; (* This is string const of type CHAR *)
	str8 := 'Hello, world!'@ (* This is string const of type CHAR8 *)
END Proc;
```

## Minimal Hello World program on Windows

```oberon
(* User32.mod file *)
DEFINITION User32;

	PROCEDURE MessageBoxW* (hwnd: INTEGER; lpText, lpCaption: ARRAY OF CHAR; uType: CARD32);

END User32.

(* Test.mod file *)
MODULE Test;
(*$MAIN*)

IMPORT
    SYSTEM, User32;

BEGIN
	User32.MessageBoxW (0, 'Hello, World!', 'Message Box', 0)
END Test.
```
