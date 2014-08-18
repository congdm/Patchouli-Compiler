# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book.

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

Written in Component Pascal, compiled by Gardens Point Component Pascal, and the generated output of this compiler is FASM-syntax assembly code.

Binary is also supply with source. At current stage, the compiler can build some simple Win64 program, without import/export abilities and some language constructs is still missing.

**(\*UPDATE\*)**
I have changed the design again. The output this time will be native machine code. It isn't runable as the old version yet.

## Niklaus Wirth was right after all

After 1 year of "hacking" on this compiler, I feel a need to share my experience.

The task of writing compiler is really fighting with a dragon. Now I understand why they call the famous book about compiler construction as "Dragon Book".

When writing a compiler, one could be tempt with the idea of a "All Powerful Super Optimizing Compiler" (just like the Philosopher's Stone in Alchemy), but if you are a solo programmer and aren't a superman, then forget about it. What Niklaus Wirth have done was exactly like that, don't care about reaching the seeming-optimal point, just focus on the essentials and calculate the tradeoff carefully. Of course the final product would be not-so-optimized, but it is working nonetheless.

Most people feel the method of Niklaus Wirth is too extreme. I had the same thought too, but that was one year ago. Now, it is different. The more time I spent on writing this compiler, the more I appreciated his method. And every time I went against his principles, I created a mess and spend a lot time cleaning it up.

### Here are some of my mistakes:

+ **Using Pascal instead of Oberon/Component Pascal:** WHO WANT TO USE THE LANGUAGE THAT IS FULL OF ALLCAPS!!!??? It is not like that I am using Cee language or Cee-Plus-Plus anyway. Pascal is an awesome language! Long live Pascal lanaguage!
**The result:** A total mess of pointer faults. Cannot track the origin of global variables => Rewriting all the compiler in Compenent Pascal.

+ **Trying my best to generate x86 CISC code:** Optimize, optimize, MOARR OPTIMIZEEE!!
**The result:** Confusing logic, wrong code generation, insanity ensure => Treat the x86 ISA as RISC one, forget about its CISC instructions.

+ **Output to assembly code instead of native machine code:** I don't want to deal with the mess of x86 machine code, so it is best to use someone's assembler.
**The result:** Bloat the generator with string processing code. Almost turning the compiler into a word processor => Rewriting again.

So, Niklaus Wirth was right after all.
