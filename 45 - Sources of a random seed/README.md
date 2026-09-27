2026-09-23: work in progress

- very high quality = cryptographic quality
- high quality, like current system timestamp with resolution of milliseconds or even nanoseconds
- low quality, like current system timeststamp with resolution of 1 second
- bad quality, which is worse than an entropy source like the current system timeststamp with resolution of 1 second

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
AssemblyScript | _The Math API is very much like JavaScript's, .._: [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | high(?) tbd, though it doesn't _provide cryptographically secure random numbers_
Awk (GNU) | the _srand()_ function probably uses the system clock with a resolution of 1 second | low | different implementations and versions of Awk and Mawk may feature implementations of _srand()_ and _rand()_
Ballerina | probably uses resources of Java version 21 as of August 2026 | high(?)
C | the _srand(time(NULL))_ function uses the current timestamp with a resolution of 1 second | low | 
C++ | the _srand(static_cast<unsigned int>(time(nullptr)))_ function uses the current timestamp with a resolution of 1 second | low | C++'s _random_ library to generate non-cryptographically secure pseudo-random numbers features a function to get a really random value as a seed for the random number engine
C3 | 
C# | 
Chapel | 
Clojure | 
COBOL (GnuCOBOL) | 
CoffeeScript | CoffeeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | high(?), though it doesn't _provide cryptographically secure random numbers_; tbd: check why this is high <==> same like with Java?
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
Java | class _ThreadLocalRandom_ uses the current timestamp with a resolution of milliseconds and the current timestamp with a resolution of  nanoseconds, and then XOR's them to finally get a random seed: [ThreadLocalRandom.java](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/concurrent/ThreadLocalRandom.java) | high | _ThreadLocalRandom_ is not cryptographically secure: [Class ThreadLocalRandom](https://docs.oracle.com/javase/8/docs//api/java/util/concurrent/ThreadLocalRandom.html)
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
PHP | PHP's _rand()_ function uses the locally supported C function: [Random Values In PHP](https://phpsecurity.readthedocs.io/en/latest/Insufficient-Entropy-For-Random-Values.html#random-values-in-php) | low
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
TypeScript | TypeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | high, though it doesn't _provide cryptographically secure random numbers_
V | 
Zig | 

<br/>

##_end
