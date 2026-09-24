2026-09-23: work in progress

- very high quality = cryptographic quality
- high quality
- medium quality
- low quality
- bad quality, like current system time with resolution in seconds only

<br/>

# Sources of a random seed

The solution in this Pascal implementation:

> The [ISO 7185 program version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_streams_for_perf_stats_iso7185.pp) cannot access (Linux) system resources, and thus not read a time value for example.

..with a [Random seed with leveraging the Address Space Layout Randomization (ASLR)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal#random-seed-with-leveraging-the-address-space-layout-randomization-aslr) 
got me thinking about the general quality of a random seed in the numerous language implementation of the pseudo-random number generator in question.
Leveraging this (sophisticated) idea unexpectedly provided a good source of randomness in a programming language which otherwise cannot access Linux system resources at all!

I was already aware of the fact that not all implementations feature a somehow decent source of randomness, and thus started another language list to get me an overview:

programming language | source of random seed | estimated quality of randomness | comment
--- | --- | --- | ---
Ada (GNAT) | package _Ada.Numerics.Discrete_Random_ | ? |
AssemblyScript | 
Awk (GNU) | 
Ballerina | 
C | 
C++ | 
C3 | 
C# | 
Chapel | 
Clojure | 
COBOL (GnuCOBOL) | 
CoffeeScript | 
Common Lisp | 
Crystal | 
Curry (KiCS2) | 
D | 
Dart | 
Dylan | 
Eiffel, Liberty | 
Factor | 
Forth (Gforth) | 
Fortran (GNU) | 
FreeBASIC | 
(Object) Free Pascal | 
Gleam | 
Go | 
Groovy | 
Haskell | 
Haxe | 
Hy | 
Inko | 
Java | 
Julia | 
Kotlin | 
Lua | 
Mercury | 
Modula-2 (GNU) | 
Modula-3 (CM3) | 
Mojo | 
Nim | 
Oberon (OBC) | 
OCaml | 
Odin | 
Perl 5 | 
PHP | 
Picat | 
Pike | 
PowerShell | 
Prolog, SWI | 
Python | 
Roc | 
Ruby | 
Rust | 
Scala | 
Scheme, Bigloo | 
Scheme, Racket | 
Smalltalk (GNU) | 
Standard ML (MLton) | 
Swift | 
Tcl | 
TypeScript | 
V | 
Zig | 

<br/>

##_end
