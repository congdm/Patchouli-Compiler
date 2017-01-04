# Patchouli Oberon-07 Compiler

## Description

This compiler is based on Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement a recursive descent compiler for Oberon-07 language, Intel 64 architecture.

Previously named AyaCompiler, which was a single pass compiler. However, since the current version (which employs a syntax tree) is considerably different from the old one, I decide to change its name.

Binary is also supply with source.

## Documentation

This compiler follows the latest [Oberon-07 Report](http://www.inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf) faithfully (hope so), with the following extensions (more exactly, complications):

### SYSTEM.LoadLibraryW and SYSTEM.GetProcAddress

**Reason for introducing:** For calling external procedures of Win32 API and other languages.

**Note: Their formal parameters are as follow:

```modula-2
PROCEDURE LoadLibraryW(VAR result: INTEGER; lpFileName: ARRAY OF CHAR);

PROCEDURE GetProcAddress(VAR result: INTEGER or any procedure type; hModule, lpProcName: INTEGER);
```

**Example usage:

```modula-2
PROCEDURE ImportProc;
VAR user32, i: INTEGER;
	ansiStr: ARRAY 256 OF BYTE;
	str: ARRAY 256 OF CHAR;
	MessageBoxW: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER);
BEGIN
	SYSTEM.LoadLibraryW(user32, "USER32.DLL");
	ASSERT(user32 # 0); str := "MessageBoxW"; i := 0;
	WHILE str[i] # 0X DO ansiStr[i] := ORD(str[i]); INC(i) END; ansiStr[i] := 0;
	SYSTEM.GetProcAddress(MessageBoxW, user32, SYSTEM.ADR(ansiStr));
	MessageBoxW(0, SYSTEM.ADR("Text"), SYSTEM.ADR("Caption"), 0)
END ImportProc;
```

### Pragma
Here is the list of compiler pragma:

| Pragma         | Meaning                                                                                                                                 | Example usage                                                                                                                                                                                    |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `(*$MAIN*)`    | Generate .EXE file instead of .DLL                                                                                                      |                                                                                                                                                                                                  |
| `(*$CONSOLE*)` | Same as `MAIN` but for console applications                                                                                             |                                                                                                                                                                                                  |
| `(*$RTL-*)`    | This module don't need Run Time Library (low level modules)                                                                             |                                                                                                                                                                                                  |

All pragma should stay at the beginning of module for easy visibility.

### Run Time Library (Rtl.dll)

The Run Time Library can be written in any language (not necessary Oberon). It must export two following procedures:

```modula-2
PROCEDURE New(VAR ptr: INTEGER; tdAdr: INTEGER); (* dynamic memory allocator *)
PROCEDURE Register(modAdr: INTEGER); (* Oberon modules will call this function during initialization *)
```

See module Rtl.mod for example implementation. Type desc structure is same as in Project Oberon, with each word be 64-bit, instead of 32-bit, and the max level of extension is 7, instead of 3.

### Debugging facility

In the tradition of most Oberon compilers, the code outputted by this compiler is accompanied by runtime error checking guard. There is no way to remove runtime error checking, so if you want to bypass them, use SYSTEM module or external assembly module. In my opinion, running the code without runtime guard is similar to driving car without seatbelt.

Additionally, there is SYSTEM.INT3 statement (yes, it is the x86 INT3 interrupt) in order to hook in your favourite assembly code debugger (in my case, Visual Studio). Sadly, there is no debugging information for external symbolic debugger. Not worth the effort and "debugger abusing is considered harmful" after all.

The image below is an example of Oberon array index guard in action:

![alt text](https://cloud.githubusercontent.com/assets/2053094/21479529/b58ede56-cb87-11e6-866d-3277be4c6361.png "Array guard")

The source pos is the byte position in source code, if you are using Notepad++, you can quickly identify the offending error location by Ctrl+G and find by offset. Combines with Console Write, that is how I develop the compiler without symbolic debugger (and thanks to the powerful ASSERT statement too). If you had used GPCP or BlackBox, then this feature would be familiar.

Another example of runtime guard, this time, is the ability to verify the interface of imported module:

![alt text](https://cloud.githubusercontent.com/assets/2053094/21479512/a0594792-cb87-11e6-9a13-e05a1abf0004.png "Module guard")

Arithmetic overflow is not checked, as same as in Project Oberon 2013.

### Data types

| Type    | Size (in bytes) | Note                                               |
|---------|-----------------|----------------------------------------------------|
| BOOLEAN |        1        |                                                    |
| CHAR    |        2        |                                                    |
| INTEGER |        8        |                                                    |
| REAL    |        8        | Windows x64 standard floating point type           |
| BYTE    |        1        | Same as in Project Oberon 2013, BYTE = SYSTEM.BYTE |
| SET     |        8        | With 64 elements, SET is powerful now              |
