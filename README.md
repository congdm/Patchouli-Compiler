# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book.

The goal is to implement the complete compiler for Oberon-07 language. Now under reconstruction.

The early versions of this compiler was written in Pascal (Delphi dialect), and the output was intend to run on bare machine without OS. But due to the lacking of runtime system, now I rebuilding the compiler from the scratch again, with the more practical goal that is to produce executables run on Windows x64 platform.

Beside that, I also switch from using Free Pascal to Gardens Point Component Pascal. From my exprience in this project, modern Pascal (Delphi dialect) is quite complex and error-prone than Oberon languages family. Despite having to rewrite many basic facilities that Free Pascal or Delphi already provided (like String handling, file I/O,...), it is still faster working with Component Pascal. Because a huge chunk of time was spend on debugging when using Pascal (mostly pointer-related bugs). And Gardens Point Component Pascal also has good runtime error/exception messages that total free me from using a debugger.

A downside of switching to Gardens Point Component Pascal is that the compiler now require .NET 2.0 to run.

Written in Component Pascal, compiled by Gardens Point Component Pascal, and the generated output of this compiler is FASM-syntax assembly code.

