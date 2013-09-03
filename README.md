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

### Function procedure

Function procedure is not able to return structured (ARRAY, RECORD) value, because return value
is store in RAX register.

### Open array

At the present, only 1-dimension open arrays are supported.
