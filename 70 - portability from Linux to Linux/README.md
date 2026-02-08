2025-11-05: TBD: finally, re-check this portability list on a fresh Vanilla OS installation (for example), the target system, with a **very different, but "modern" Linux kernel version** than the testing system (that is _6.14.0-37-generic #37~24.04.1-Ubuntu_ as of 2026-02-07)

<br/>

# Portability of programs

Here's a list of programming languages where standalone executables can be compiled by default that **may** run **without any extra installations** on another, arbitrary ("basic") Linux machine.

You may have a view into the potantial dependencies of a Linux executable with the [ldd](https://www.man7.org/linux/man-pages/man1/ldd.1.html) command to see what shared objects, that are shared libraries, including virtual ones, are required: _$ ldd \<executable file name\>_:

programming language | dependencies on shared objects (shared libraries)? | dynamic executable? (ldd command)
-- | -- | --
Ada | yes | yes
C | yes | yes
C++ | yes | yes
C3 | yes | yes
Chez Scheme (only the "speed part" tested) | yes | yes
Common Lisp (SBCL) | yes | yes
Crystal | yes | yes
Eiffel (Liberty) | yes | yes
Fortran | yes | yes
Gambit Scheme (only the "speed part" tested) | yes | yes
Go | no | "not a dynamic executable"
Inko | yes | yes
Nim | yes | yes
OCaml | yes | yes
Odin | yes | yes
Roc | no | "statically linked"
Rust | yes | yes
Standard ML (MLton) | yes | yes
V | yes | yes
Zig | no | "not a dynamic executable"

<br/>

As of 2026-02-07, I haven't taken extra efforts to potentially compile into statically linked executables where possible. So, above table is a "default compilation" table.

<br/>

[GraalVM based](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm):

programming language | dependencies on shared objects (shared libraries)? | dynamic executable? (ldd command)
-- | -- | --
Clojure (only the "speed part" tested) | yes | yes
Kotlin (only the "speed part" tested) | yes | yes
Scala (only the "speed part" tested) | yes | yes

<br/>

## This list does not contain

- program versions which are exceeding the [1 second execution time limit](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#languages-that-were-too-slow)
- program versions which depend on the Java Virtual Machine (JVM), like Clojure, Groovy, Kotlin and Scala, where portability between operating systems, including Windows, is anyway strived for
- program versions which depend on the .NET ecosystem, like [C#](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%23#installation-tips), where portability between operating systems, including Windows, is anyway strived for
- intepreted languages ("scripting languages") like Lua, Perl 5, PowerShell, Python, or Ruby which usually depend on some prior language related installation
- Julia, which in one form or another needs to bring its runtime along with the original source code or along with a precompiled program for anything more demanding than "Hello, World!"

## Other omissions from above list

- [Chapel](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#installation-tips), which needs at least one one shared library being installed
- [D](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/D#d), which needs at least one shared library (libgphobos.so.4) being installed
- Dart, which isn't so easily portable to another, "foreign" Linux system; see at [Standalone (or self-contained) executable (which may not be portable so easily)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dart#standalone-or-self-contained-executable-which-may-not-be-portable-so-easily)
- [FreeBASIC](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/FreeBASIC#installation-tips), which needs at least one shared library being installed for perfect execution (though the program probably will work)
- [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming#mercury): shared libraries: libmer_std.so, libmer_rt.so, libgc.so - and what else? - are needed on the target system. I think its easier to install Mercury on the target system to have all needed libraries available before copying them one by one from source system to target system
- [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#installation-tips), which needs at least one one shared library being installed
- [SWI Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#swi-prolog), which needs at least one one shared library being installed
- [Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/README.md#size-of-executables): the Bigloo and CHICKEN executables need shared libraries being installed; the Racket executable needs Racket being installed
- [Swift](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift#installation-tips), which needs at least one shared library being installed
- while a Clozure Common Lisp ([CCL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ccl)) executable is also portable, same like a Steel Bank Common Lisp [SBCL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#sbcl) executable, an Embeddable Common Lisp [ECL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ecl) executable is not, because it needs to have access to at least one shared library

<br/>

##_end
