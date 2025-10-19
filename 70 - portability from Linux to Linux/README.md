# Portability of programs

Here's a list of programming languages where standalone executables can be compiled that should run **without any extra installations** on another, arbitrary ("basic") Linux machine:

- Ada
- C
- C3
- Common Lisp
- Crystal
- Gambit Scheme
- Go
- Inko
- OCaml
- Roc
- Rust
- SML MLton
- V
- Zig

<br/>

This list does not contain:

- program versions which are exceeding the [1 second execution time limit](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#languages-that-were-too-slow)
- program versions which depend on the Java Virtual Machine ([JVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#ahead-of-time-aot-program-compilation-with-the-graalvm)), like Clojure, Scala and Kotlin, where portability between operating systems, including Windows, is anyway strived for
- program versions which depend on the .NET ecosystem, like [C#](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%23#installation-tips), where portability between operating systems, including Windows, is anyway strived for
- any intepreted language ("scripting language"), like Lua, Perl 5, PowerShell or Python, which usually depend on some prior, language related installation

Some other notable misses:

- my [Chapel](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#installation-tips) program may crash in another Linux system
- [FreeBASIC](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/FreeBASIC#installation-tips), which needs a shared library being installed for perfect execution (though the program probably will work)
- [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#installation-tips), which needs a shared library being installed
- [Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/README.md#size-of-executables): the Bigloo and CHICKEN programs need libraries being installed; the Racket program needs Racket being installed
- [Swift](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Swift#installation-tips), which needs a library being installed

<br/>

##_end
