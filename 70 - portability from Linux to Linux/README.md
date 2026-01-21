2025-11-05: TBD: finally, re-check this portability list on a fresh Oracle Linux 10 installation as a "basic Linux system", that is the "target system".

# Portability of programs

Here's a list of programming languages where standalone executables can be compiled that should run **without any extra installations** on another, arbitrary ("basic") Linux machine:

- Ada
- C
- C++
- C3
- Chez Scheme (only the "speed part" tested)
- Common Lisp (SBCL)
- Crystal
- Fortran
- Gambit Scheme (only the "speed part" tested)
- Go
- Inko
- Nim
- OCaml
- Odin
- Roc
- Rust
- Standard ML (MLton)
- V
- Zig

[GraalVM based](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm):
- Clojure
- Kotlin
- Scala

<br/>

## This list does not contain

- program versions which are exceeding the [1 second execution time limit](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#languages-that-were-too-slow)
- program versions which depend on the Java Virtual Machine (JVM), like Clojure, Scala and Kotlin, where portability between operating systems, including Windows, is anyway strived for
- program versions which depend on the .NET ecosystem, like [C#](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%23#installation-tips), where portability between operating systems, including Windows, is anyway strived for
- intepreted languages ("scripting languages") like Lua, Perl 5, PowerShell or Python, which usually depend on some prior language related installation
- Julia, which in one form or another needs to bring its runtime along with the original source code or a precompiled program for anything more demanding than "Hello, World!"

## Other omissions from above list

- [Chapel](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#installation-tips), which needs at least one one shared library being installed
- [D](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/D#d), which needs at least one shared library (libgphobos.so.4) being installed
- [FreeBASIC](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/FreeBASIC#installation-tips), which needs at least one shared library being installed for perfect execution (though the program probably will work)
- [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming#mercury): shared libraries: libmer_std.so, libmer_rt.so, libgc.so - and what else? - are needed on the target system. I think its easier to install Mercury on the target system to have all needed libraries available before copying them one by one from source system to target system
- [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#installation-tips), which needs at least one one shared library being installed
- [SWI Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#swi-prolog), which needs at least one one shared library being installed
- [Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/README.md#size-of-executables): the Bigloo and CHICKEN executables need shared libraries being installed; the Racket executable needs Racket being installed
- [Swift](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift#installation-tips), which needs at least one shared library being installed
- while a Clozure Common Lisp ([CCL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ccl)) executable is also portable, same like a [SBCL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#sbcl) executable, an Embeddable Common Lisp [ECL](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#ecl) executable is not, because it needs to have access to at least one shared library

<br/>

##_end
