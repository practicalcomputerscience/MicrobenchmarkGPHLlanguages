2026-09-23: work in progress

# Sources of a random seed

- CS = cryptographically secure
- PCG = permuted congruential generator: http://www.pcg-random.org
- PRNG = pseudo-random number generator
- RNG = random number generator

<br/>

- very high quality, like cryptographic quality
- high quality, like current system timestamp with resolution of milliseconds or even nanoseconds
- medium quality, like current system timestamp with resolution of hundredths of a second
- low quality, like current system timeststamp with resolution of 1 second
- bad quality, which is worse than an entropy source like the current system timestamp with a resolution of only 1 second

<br/>

The solution in this Pascal implementation:

> The [ISO 7185 program version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_streams_for_perf_stats_iso7185.pp) cannot access (Linux) system resources, and thus not read a time value for example.

..with a [Random seed with leveraging the Address Space Layout Randomization (ASLR)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal#random-seed-with-leveraging-the-address-space-layout-randomization-aslr) 
got me thinking about the general quality of a random seed in the numerous language implementations of the pseudo-random number generator in question.
Leveraging this (sophisticated) idea unexpectedly provided a good source of randomness in a programming language which otherwise cannot access Linux system resources at all!

Before I came to a Pascal implementation, I was already aware of the fact that not all implementations feature a somehow decent source of randomness, and thus started another language list to get me an overview.

> [!NOTE]
> The given sources of random seeds only mean the sources I've (implicitly) used, not that these are necessarily the only sources of entropy in a given language!

Usually, I just took the oldest and simplest method.

Nowadays, many languages, which are still actively maintained, offer cryptographically secure sources of entropy, and if it's only implicitly making an operating system call of [getrandom(2)](https://www.man7.org/linux/man-pages/man2/getrandom.2.html) in Linux for example, something which could often be done with user defined code in many programming languages. However, I don't have a need for it in this project.

<br/>

<br/>

programming language | used source of random seed | estimated quality of randomness | comment
--- | --- | --- | ---
Ada (GNAT) | package _Ada.Numerics.Discrete_Random_ | high | two manual program runs within 1 second will yield two different byte streams
AssemblyScript | using function _Math.random()_ from: _The Math API is very much like JavaScript's, .._ from [Math](https://www.assemblyscript.org/stdlib/math.html#math) | high | see below at TypeScript
Awk (GNU) | the _srand()_ function probably uses the system clock with a resolution of 1 second | low(?) | different implementations and versions of Awk and Mawk may feature different implementations of _srand()_ and _rand()_
Ballerina | [module-ballerina-random/ballerina/natives.bal](https://github.com/ballerina-platform/module-ballerina-random/blob/main/ballerina/natives.bal#L22) initially reads the current system time in milliseconds: _isolated decimal x0 = currentTimeInMilliSeconds();_ | high
C | the _srand(time(NULL))_ function uses the current timestamp with a resolution of 1 second (*) | low | [Random Numbers in C: rand, srand, and Generating a Number in a Range](https://coddy.tech/docs/c/random-numbers)
C++ | same like in C (*) | low |
C3 | ? | high | two manual program runs within 1 second will yield two different byte streams
C# | ? | high | two manual program runs within 1 second will yield two different byte streams
Chapel | ? | high | two manual program runs within 1 second will yield two different byte streams
Clojure | 
COBOL (GnuCOBOL) | _ACCEPT FROM TIME_ returns the current system time in format HHMMSSCC, where CC represents the hundredths of a second | medium | [Working with Dates and Time in COBOL](https://www.mainframemaster.com/tutorials/cobol/dates-time)
CoffeeScript | CoffeeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | probably high (nowadays) | see at TypeScript below
Common Lisp | 
Crystal | Linux system call of [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) in module [getrandom.cr](https://github.com/crystal-lang/crystal/blob/master/src/crystal/system/unix/getrandom.cr) | very high | Crystal implemented Melissa O'Neill's PCG Random Number Generation for C (2014): [pcg32.cr](https://github.com/crystal-lang/crystal/blob/master/src/random/pcg32.cr)
Curry (KiCS2) | 
D | Linux system call of [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [Function std.random.unpredictableSeed](https://dlang.org/library/std/random/unpredictable_seed.html)
Dart | 
Dylan | Dylan function [default-random-seed()](https://github.com/dylan-lang/opendylan/blob/master/sources/common-dylan/unix-common-extensions.dylan#L40) calls POSIX C function _time()_ to get the current system timestamp, that is the count of seconds elapsed since January 1, 1970 (00:00:00 UTC), and then takes the first 4 bytes and does some bitwise operations on them to generate an integer seed (*) | low |
Eiffel, Liberty | 
Factor | 
Forth (Gforth) | 
Fortran (GNU) | ? | high | two manual program runs within 1 second will yield two different byte streams
FreeBASIC | seeding is based on the return value of the [TIMER](https://www.freebasic-portal.de/befehlsreferenz/timer-295.html) function with a resolution in microseconds | high | the [RANDOMIZE instruction](https://www.freebasic-portal.de/befehlsreferenz/randomize-539.html) is used for seeding
(Object) Free Pascal |
Gleam | 
Go | 
Groovy | 
Haskell | 
Haxe | 
Hy | 
Inko | 
Java | class _ThreadLocalRandom_ uses the current timestamp with a resolution of milliseconds and the current timestamp with a resolution of nanoseconds, and then XOR's them to finally get a random seed: [ThreadLocalRandom.java](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/concurrent/ThreadLocalRandom.java) | high | _ThreadLocalRandom_ is not cryptographically secure: [Class ThreadLocalRandom](https://docs.oracle.com/javase/8/docs//api/java/util/concurrent/ThreadLocalRandom.html)
Julia | Julia's default RNG initially calls Julia function [uv_random](https://github.com/JuliaLang/julia/blob/master/base/libc.jl#L457), which in return calls function [uv_random](https://docs.libuv.org/en/stable/misc.html#c.uv_random) in C library _libuv_ for cross-platform asynchronous I/O, which in return makes a [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) Linux system call to _obtain a series of random bytes_ | very high | the [urandom(4)](https://linux.die.net/man/4/urandom) entropy source _gathers environmental noise from device drivers and other sources into an entropy pool_
Kotlin | 
Lua | the _math.randomseed(os.time())_ function most probably uses the system clock with a resolution of 1 second | low | https://www.luadocs.com/docs/functions/math/random
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
Pike | _[Class Random.System](https://pike.lysator.liu.se/generated/manual/modref/ex/predef_3A_3A/Random/System.html#System)_ "is the default implementation of the random functions. This is the Random.Interface combined with a system random source. ..on Unix systems it is /dev/urandom." | very high | 
PowerShell | ? | probably high | [Get-Random](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.6)
Prolog, SWI | 
Python | 
Roc | 
Ruby | 
Rust | Linux system call of [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [from_os_rng](https://docs.rs/rand/0.9.1/rand/trait.SeedableRng.html#method.from_os_rng), [getrandom: system’s random number generator](https://docs.rs/getrandom/latest/getrandom/#getrandom-systems-random-number-generator)
Scala | 
Scheme, Bigloo | 
Scheme, Racket | 
Smalltalk (GNU) | 
Standard ML (MLton) | 
Swift | Linux system call of [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [Platform Implementation of `SystemRandomNumberGenerator`](https://developer.apple.com/documentation/swift/systemrandomnumbergenerator#Platform-Implementation-of-SystemRandomNumberGenerator)
Tcl | _The seed of the generator is initialized from the internal clock of the machine..._ from: [rand](https://www.tcl-lang.org/man/tcl8.6/TclCmd/mathfunc.htm#M28) | high | two manual program runs within 1 second will yield two different byte streams
TypeScript | TypeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | probably high (nowadays) | a random seed probably (still) depends on the exact JavaScript engine being used: [Math.random() is not so random: The Illusion of Randomness in JavaScript](https://vinitshahdeo.substack.com/p/mathrandom-is-not-so-random-the-illusion), 2025; there's a chapter on [Better Alternatives for Randomness](https://vinitshahdeo.substack.com/i/167041440/the-quantum-question-randomness-in-the-future)
V | [rand](https://modules.vlang.io/rand.html#readme_rand): _All the generators are initialized with time-based seeds._ | high | two manual program runs within 1 second will yield two different byte streams
Zig | own implementation of a Linux system call of [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) with instruction _try std.posix.getrandom(std.mem.asBytes(&seed));_ | very high | 

<br/>

(*) the 1 second seeding resolution can be easily tested with two manual program runs within 1 second:

```
$ ./random_streams_for_perf_stats_clang; head -c 10 random_bitstring.byte

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
bf16f7c597$ ./random_streams_for_perf_stats_clang; head -c 10 random_bitstring.byte

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
bf16f7c597$
```

Here, the first 10 characters of the random byte stream are identical, indicating that the quality of randomness of a seed is rather low.

<br/>

##_end
