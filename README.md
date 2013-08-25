AyaCompiler
===========

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book.

I intend to turn it into full-fledged Oberon compiler for x86_64 architecture.

Write in Pascal, compile by Free Pascal and output tested with FASM.

NOTE: At this early stage, this compiler produce bare assembly code, which cannot run directly on any
system (Windows, Linux, ...). In order to be run, the output code must be "glued" with runtime system
manually. For example, PE64DEMO.ASM is the output code of test.obr glued with Windows-specific code.
Anyway, I will use this compiler to write my OS, so the ability to run on Windows, Unixes,... is
not considered yet.

If there is any bugs, please post in Issue. I would greatly appreciate your helping.
