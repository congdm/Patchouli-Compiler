# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book.

I intend to turn it into full-fledged Oberon-07 compiler for x86_64 architecture.

Written in Pascal, compiled by Free Pascal and output tested with FASM.

NOTE: At this early stage, this compiler produce bare assembly code, which cannot run directly on any
system (Windows, Linux, ...). In order to be run, the output code must be "glued" with runtime system
manually. For example, PE64DEMO.ASM is the output code of test.obr glued with Windows-specific code.
Anyway, I will use this compiler to write my OS, so the ability to run on Windows, Unixes,... is
not considered yet.

If there are any bugs, please post in Issues. I would greatly appreciate any helping.

## Features

Language specification: http://www.inf.ethz.ch/personal/wirth/Articles/Oberon/Oberon07.Report.pdf

### INT8, INT16 and INT32 types

I reintroduce those integer types for accessing and storing small data in memory (<8 bytes).
For numberic calculation purpose, 8 bytes INTEGER type is recommend.
When doing calculation with small integer types, all intermediate values are in 8 bytes register
so there is no code size benefit from using small integer types over standard INTEGER.
There are 3 standard functions for convert larger integer to smaller integer: TOINT8, TOINT16 and TOINT32.

### Dynamic array type

Syntax: TYPE x = ARRAY OF NonArrayType;

Dynamic array is just a pointer with an array descriptor attach to it. A normal pointer's size is 8 bytes
 but the size of a variable of dynamic array type can be 24 or 32 bytes. Dynamic array is restricted to
1-dimension only. Different from ARM Oberon-07, where dynamic array is allocated on stack, dynamic array
here is allocated on heap. Therefore, a dynamic array type can be contained in a record type,
or be an element type of an array type, or be passed to another procedure as an open array or a dynamic array.
However, array assignment will not work with dynamic array and assignment of dynamic array variables is treated
as pointer assignment.
