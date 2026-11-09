# Sources of a random seed

Glossary:

- ABI = [Application Binary Interface](https://en.wikipedia.org/wiki/Application_binary_interface)
- AIS = Anwendungshinweise und Interpretationen = Notes on Application and Interpretation of the German Federal Office for Information Security (BSI): [Notes on Application and Interpretation (AIS)](https://www.bsi.bund.de/EN/Themen/Unternehmen-und-Organisationen/Standards-und-Zertifizierung/Zertifizierung-und-Anerkennung/Zertifizierung-von-Produkten/Zertifizierung-nach-CC/Anwendungshinweise-und-Interpretationen/anwendungshinweise-und-interpretationen_node.html) (+)
- CS = cryptographically secure
- DRNG = deterministic random number generator
- Epoch = Unix time, which is defined as the number of non-leap seconds and microseconds from system function call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html), which have passed since 00:00:00 UTC on Thursday, 1 January 1970
- OS = operating system
- PCG = permuted congruential generator: http://www.pcg-random.org; https://en.wikipedia.org/wiki/Permuted_congruential_generator: "It has been shown that it is practically possible (with a large computation) to recover the seed of the pseudo-random generator given 512 consecutive output bytes."
- PRNG = pseudo-random number generator
- RNG = random number generator
- vm = virtual machine
- XOR = Exclusive Or operation; below only the bitwise XOR operation is meant; the same is true for other operations around a XOR operation, like a bitwise Or for example

---

Table of contents:

- [Quality of a random seed with every program run](#quality-of-a-random-seed-with-every-program-run)
- [German Federal Office for Information Security (BSI)](#german-federal-office-for-information-security-bsi)
- [Ranking by quality of the randomness of a seed](#ranking-by-quality-of-the-randomness-of-a-seed)
- [Motivation](#motivation)
- [Noteable things](#noteable-things)
- [The language list](#the-language-list)
- [Test of a 1 second seeding resolution](#test-of-a-1-second-seeding-resolution)
- [Variant 13 of David Stafford's 64-bit mix function](#variant-13-of-david-staffords-64-bit-mix-function)
- [Details on the seeding of selected languages](#details-on-the-seeding-of-selected-languages)
- [Ada's default seeding](#adas-default-seeding)
- [Bigloo Scheme's default seeding](#bigloo-schemes-default-seeding)
- [The PRNG's of GNU C Library glibc](#the-prngs-of-gnu-c-library-glibc)
- [The random seeding mechanism of the Dart vm](#the-random-seeding-mechanism-of-the-dart-vm)
- [The random seeding mechanism of Google's V8 JavaScript engine](#the-random-seeding-mechanism-of-googles-v8-javascript-engine)
- [Racket's REPL and original source of a random seed](#rackets-repl-and-original-source-of-a-random-seed)

<br/>

---

##  Quality of a random seed with every program run

### Wikipedia

> In Unix-like operating systems, [/dev/random and /dev/urandom](https://en.wikipedia.org/wiki//dev/random) are special files that provide random numbers from a cryptographically secure pseudorandom number generator (CSPRNG). The CSPRNG is seeded with entropy (a value that provides randomness) from environmental noise, collected from device drivers and other sources.

> The /dev/urandom source is itself a PRNG but it is frequently reseeded from the high entropy /dev/random resource which makes it impractical for an attacker to target.

from [Random Values In PHP](https://phpsecurity.readthedocs.io/en/latest/Insufficient-Entropy-For-Random-Values.html#random-values-in-php).

<br/>

### German Federal Office for Information Security (BSI)

Also see PDF document [Documentation and Analysis of the Linux Random Number Generator](https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/Studies/LinuxRNG/LinuxRNG_EN_V7_1.pdf), Version: 7.1 from 2026-07-01 (and potentially later) of the [BSI](https://www.bsi.bund.de/EN/Home/home_node.html).

I found at least these conclusions:

> The implementation of the ChaCha20 DRNG feeding /dev/random, /dev/urandom, the getrandom system call, and the in-kernel get_random_bytes API does not comply with the requirements of DRT.1. ... The non-compliance with DRT.1 rests in not meeting formal requirements. Yet, these non-compliances do not hint to cryptographic insecure behavior.

> The Linux-RNG does not comply with NTG.1 as the base requirement is not met: ...  The Linux-RNG is not permanently reseeded as required by NTG.1. Also, the DRNG shall be seeded with at least 240 bits of
entropy obtained individually from two different software-based entropy sources before the DRNG is considered fully seeded. ...

- DRT.1 = Deterministic Random number generator Tree in version 1, where a tree structure is assumed like this: there's a main "parent" engine which creates the initial secret codes and passes them down to branch off into smaller "child" engines across the computer system
- NTG.1 = Non-physical True RNGs in version 1

See also these documents of the BSI:

- AIS 20, Version 4 from 2025: I only found a German version at (+) above: "Functionality classes and evaluation methodology for deterministic random number generators"; older official English versions may exist
- AIS 31, Version 4 from 2025: I only found a German version at (+) above: "Functionality Classes and Evaluation Methodology for True (Physical) Random Number Generators"; older official English versions may exist

The BSI nowadays regularly evaluates Linux's entropy resource _/dev/random_ as stated here at [Random Number Generators](https://www.bsi.bund.de/EN/Themen/Unternehmen-und-Organisationen/Informationen-und-Empfehlungen/Kryptografie/Zufallszahlengenerator/zufallszahlengenerator_node.html):

> For several years now, the BSI continuously examined the random number generator (RNG for short) /dev/random for each newly released version of Linux. This enables the BSI to make security statements about this RNG, but also about cryptographic systems that use this RNG to generate key material. The main aspect of the investigation is to prove that the Linux RNG is compliant with a certain functionality class.

My wild guess is this: since a myriad of versions of software and hardware exist and environments are changing constantly, clear and simple statements on the quality of the randomness of computer resources are not made by this German authority officially.

<br/>

### Ranking by quality of the randomness of a seed

Here is my own and simple ranking just for orientation:

- very high quality, like cryptographic quality, for example based on hardware entropy
- high quality, like current (Linux) system timestamp with resolution of milliseconds or even nanoseconds
- medium quality, like current system timestamp with resolution of hundredths of a second
- low quality, like current system timeststamp with resolution of 1 second
- bad quality, which is worse than an entropy source like the current system timestamp with a resolution of 1 second. As of 2026-11-09, I have done only one (unofficial) implementation with no entropy sourcing within the program, and that is at the Miranda solution, where I provide a random seed value from the Bash shell (_echo $RANDOM_) as a [command line argument](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell/Miranda#mirandas-deficits).

<br/>

### Motivation

The solution in this Pascal implementation:

> The [ISO 7185 program version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_streams_for_perf_stats_iso7185.pp) cannot access (Linux) system resources, and thus not read a time value for example.

..with a [Random seed with leveraging the Address Space Layout Randomization (ASLR)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal#random-seed-with-leveraging-the-address-space-layout-randomization-aslr) got me thinking about the general quality of a random seed in the numerous language implementations of the pseudo-random number generator in question. Leveraging this (sophisticated) idea unexpectedly provided a good source of randomness in a programming language which otherwise cannot access Linux system resources at all!

Before I came to a Pascal implementation, I was already aware of the fact that not all program implementations feature a somehow decent source of randomness, and thus started the [language list](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/45%20-%20Sources%20of%20a%20random%20seed#the-language-list) below to get me an overview.

<br/>

### Noteable things

> [!NOTE]
> The given sources of random seeds as listed below only mean that these are the sources I've been (implicitly) using in my programs, not that these are necessarily the only sources of entropy in a given programming language!

<br/>

Nowadays, many languages offer an interface to a cryptographically secure source of entropy, for example with implicitly calling Linux system function [getrandom(2)](https://www.man7.org/linux/man-pages/man2/getrandom.2.html), something which could often be implemented **directly** with a user defined function in many programming languages.

However, often a (Linux) system **time based solution** is just good enough. My Mercury implementation for example features an individual but "high quality" solution from my point of view,
where everything needed is just packed into one user defined C function as part of the Mercury source code, see below.

With some languages, I couldn't figure out the seeding algorithm within a reasonable amount of time. I just marked them with "how is this seeded?".

<br/>

## The language list

programming language | used source of random seed | estimated quality of randomness | comment
--- | --- | --- | ---
Ada (GNAT) | Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html), which returns the number of microseconds since the Epoch, which is then used to initially feed a (modified) MT19937 Mersenne Twister. | high | For more details, see below at [Ada](#adas-default-seeding).
AssemblyScript | using function _Math.random()_ from: _The Math API is very much like JavaScript's, .._ from [Math](https://www.assemblyscript.org/stdlib/math.html#math) | very high | see also below at TypeScript
Awk (GNU) | the _srand()_ function probably uses the system clock with a resolution of 1 second: [9.1.3 Numeric Functions](https://www.gnu.org/software/gawk/manual/html_node/Numeric-Functions.html#Numeric-Functions-1) | low | different implementations and versions of Awk and Mawk may feature different implementations of _srand()_ and _rand()_; two program runs within 1 second may yield the **same** byte streams with GNU Awk 5.2.1 in Ubuntu 24
Ballerina | [module-ballerina-random/ballerina/natives.bal](https://github.com/ballerina-platform/module-ballerina-random/blob/main/ballerina/natives.bal#L22) initially reads the current system time in milliseconds: _isolated decimal x0 = currentTimeInMilliSeconds();_ | high | 
[C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) | In my own implementation, a Linux system call of [clock_gettime(3)](https://www.man7.org/linux/man-pages/man3/clock_gettime.3.html) reads the number of seconds and the number of any residual **nanoseconds** since the last system boot ("CLOCK_MONOTONIC"), which are then simply mixed as: _unsigned int seed = (unsigned int)((ts.tv_sec * 13) ^ ts.tv_nsec);_. The result is finally modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed. | high | I just applied the Oxford Oberon-2 Compiler's simple mixing function, see below, to not overdo it at the seeding, but to do better seeding than just doing ubiquitous function call _srand(time(NULL));_, which only has a resolution of 1 second.
C++ | same as with C | high | same as with C
C3 | C3's default RNG is [Sfc64Random](https://github.com/c3lang/c3c/blob/master/lib/std/math/random.c3#L63). Like the other RNG's, it's seeded with function [entropy()](https://github.com/c3lang/c3c/blob/master/lib/std/math/random/math.seeder.c3#L75) in a non-freestanding WebAssembly environment. There, an array of hashed character values is filled from a diverse set of pseudo-random values, like pointer address data and the number of nanoseconds since the last system boot ([posix::CLOCK_MONOTONIC](https://github.com/c3lang/c3c/blob/master/lib/std/time/os/time_posix.c3#L11)) for example. | high | 
C# | it looks to me that (nowadays) C# is also leveraging the Address Space Layout Randomization (ASLR), as it can be seen at instruction: _ulong* ptr = stackalloc ulong[4];_ in sources [Random.Xoshiro256StarStarImpl.cs](https://github.com/dotnet/dotnet/blob/main/src/runtime/src/libraries/System.Private.CoreLib/src/System/Random.Xoshiro256StarStarImpl.cs#L35) | high | Scrambled linear generator _Xoshiro256StarStar_ has been originally implemented in C sources [xoshiro256**](https://prng.di.unimi.it/xoshiro256starstar.c). See also at [Scrambled Linear PRNGs](https://arxiv.org/abs/1805.01407).
Chapel | Chapel's [Random](https://github.com/chapel-lang/chapel/blob/main/modules/standard/Random.chpl) module includes submodule [PCGRandomLib](https://github.com/chapel-lang/chapel/blob/main/modules/standard/Random.chpl#L1906) to implement a permuted congruential generator, which is seeded at procedure [randomishSeed()](https://github.com/chapel-lang/chapel/blob/main/modules/standard/Random.chpl#L84), which tries to read from /dev/urandom. | very high | Alternatively, seeding is done from the hashed value of Chapel's task identifier, xor'ed with the value of Linux system call [getpid(2)](https://man7.org/linux/man-pages/man2/getpid.2.html), xor'ed with the hashed number of seconds since the Epoch, finally xor'ed with the hashed value of the current locale, that is Chapel's predefined variable _here_.
Clojure | It starts with calling Clojure function [rand-int()](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj#L5177), which calls Clojure function [rand()](https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj#L5169), which calls Clojure function [random](https://github.com/clojure/clojure/blob/master/src/clj/clojure/math.clj#L267), which is a wrapper function of the [java.lang.Math.random()](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--) static method. Thus, the default seeding mechanism is the same as described below at Groovy with initializing an underlying [Random()](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/Random.java#L310) instance in Java. | high | 
COBOL (GnuCOBOL) | _ACCEPT FROM TIME_ returns the current system time in format HHMMSSCC, where CC represents the hundredths of a second | medium | [Dates and Time in COBOL](https://www.mainframemaster.com/tutorials/cobol/dates-time)
CoffeeScript | CoffeeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | very high | see at TypeScript below
Common Lisp | reading 256 bits from /dev/urandom with function [os-random-seed ()](https://github.com/sbcl/sbcl/blob/master/src/code/target-random.lisp#L113) | very high | if reading from /dev/urandom doesn't work, the fallback strategy is to read the current system time in microseconds at [get-internal-real-time ()](https://github.com/sbcl/sbcl/blob/master/src/code/unix.lisp#L1043) and the current process identification as basis for a random seed: [fallback-random-seed()](https://github.com/sbcl/sbcl/blob/master/src/code/target-random.lisp#L104)
Crystal | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) in module [getrandom.cr](https://github.com/crystal-lang/crystal/blob/master/src/crystal/system/unix/getrandom.cr) | very high | Crystal implemented Melissa O'Neill's PCG Random Number Generation for C (2014): [pcg32.cr](https://github.com/crystal-lang/crystal/blob/master/src/random/pcg32.cr)
Curry (KiCS2) | I took inbuilt function [getRandomSeed](https://cpm.curry-lang.org/DOC/random-3.0.0/System.Random_curry.html#getRandomSeed) and slightly modified it to implement an effective user defined function. It reads the clock time in seconds precision and cpu time in milliseconds precision and processes them into an integer number of range [1..65521-1] for a safe 16 bit integer random seed. | high | 
D | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [unpredictableSeed](https://dlang.org/library/std/random/unpredictable_seed.html)
Dart | If possible and with a complicated chain of function calls, the runtime of Dart's vm, implemented in C++, initially makes a raw (Linux) system call of [__NR_getrandom_](https://github.com/dart-lang/sdk/blob/main/runtime/bin/crypto_linux.cc#L48), instead of the user-space libc wrapper _getrandom()_. | (usually) very high | See at [Dart](#the-random-seeding-mechanism-of-the-dart-vm) below for more details.
Dylan | Dylan function [default-random-seed()](https://github.com/dylan-lang/opendylan/blob/master/sources/common-dylan/unix-common-extensions.dylan#L40) calls POSIX C function _time()_ to get the current system timestamp, that is the count of seconds that elapsed since the Epoch, and then takes the first 4 bytes and does some bitwise operations on them to generate an integer seed; see below at [Test of a 1 second seeding resolution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/45%20-%20Sources%20of%20a%20random%20seed#test-of-a-1-second-seeding-resolution) | low | two program runs within 1 second may yield the **same** byte streams
Eiffel, Liberty | Liberty Eiffel function [clock_based_random_seed](https://github.com/GitMirroring/liberty-eiffel/blob/master/src/lib/random/random_number_generator.e#L62) reads some _CLOCK_ value since its creation, so working like the ticks of a stopwatch, plus apparently the number of any residual microseconds since the last tick of a second, and finally processes both values until a positive result has been reached | high | examples with the _CLOCK_ type: [test_sz340.e](https://github.com/GitMirroring/liberty-eiffel/blob/master/test/language/unclassified/sz/test_sz340.e), [example2.e](https://github.com/GitMirroring/liberty-eiffel/blob/master/tutorial/time/example2.e)
Factor | Function [random](https://github.com/factor/factor/blob/master/basis/random/random.factor#L81) gets an integer random number from the default Mersenne Twister random number generator, which has being seeded with the value of [nano-count](https://github.com/factor/factor/blob/master/basis/random/mersenne-twister/mersenne-twister.factor#L71), which has been set to [bootstrap time](https://github.com/factor/factor/blob/master/basis/bootstrap/stage2.factor#L61) with a resolution of nanoseconds. | high | 
Forth (Gforth) | First reading the current system time in microseconds since the Epoch with function [utime](https://net2o.de/gforth/Keeping-track-of-Time.html#index-utime-_0028-_002d_002d-dtime-_0029-gforth_002d0_002e5). This raw value is then just modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed. | high |
Fortran (GNU) | GNU function [random_number](https://fortran-lang.org/learn/intrinsics/math/#random-number) maps into C functions _gfc_check_random_number_ and _gfc_resolve_random_number_ at [intrinsic.cc](https://github.com/gcc-mirror/gcc/blob/master/gcc/fortran/intrinsic.cc#L3991). GNU Fortran's default PRNG of the xoshiro256** type has been initially seeded from /dev/urandom at C function [getosrandom()](https://github.com/gcc-mirror/gcc/blob/master/libgfortran/intrinsics/random.c#L387). | very high | Alternatively, seeding is done from the current timestamp in microseconds resolution, plus the value of Linux system call [getpid(2)](https://man7.org/linux/man-pages/man2/getpid.2.html) to get the current process identification.
FreeBASIC | seeding is based on the return value of the [TIMER](https://www.freebasic-portal.de/befehlsreferenz/timer-295.html) function with a resolution of microseconds | high | the [RANDOMIZE instruction](https://www.freebasic-portal.de/befehlsreferenz/randomize-539.html) is used for seeding
(Object) Free Pascal | Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html) at function [Fptime()](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/linux/ossysc.inc?plain=1#L22), though this function only reads the number of seconds and **not** microseconds since the Epoch | low | [Procedure Randomize](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/linux/system.pp#L469); two program runs within 1 second may yield the **same** byte streams
Gleam | As to be expected, calling _int.random()_ in Gleam is, via a little function chain, calling a function in Erlang and that is [rand](https://www.erlang.org/doc/apps/stdlib/rand.html). The [default seed](https://github.com/erlang/otp/blob/master/lib/stdlib/src/rand.erl#L1008) is implemented like this: _SeedTerm = {self(), erlang:unique_integer(), erlang:system_time()}, <<A:43, B:43, C:42>> = erlang:md5(term_to_binary(SeedTerm)), {A, B, C}._, with _self()_ being the node name and _erlang:system_time()_ the count of nanoseconds that elapsed since the Epoch. | high | test like this: _$ erl_ -> _1> erlang:system_time()._ --> 1792697309533995913, that is Thursday, October 22, 2026 at 19:28:29.533995913 UTC for example
Go | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html): [getrandom.go](https://cs.opensource.google/go/go/+/master:src/internal/syscall/unix/getrandom.go) | very high | the exact (default) mechanism is complex, because Go also has fallback's implemented and cares about concurrency issues etc.; [getrandom_linux.go](https://cs.opensource.google/go/go/+/master:src/internal/syscall/unix/getrandom_linux.go)
Groovy | Groovy imports Java packages _java.util.*_ automatically, and thus package [java.util.Random](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/Random.java). This package maintains thread-safe seed [seedUniquifier](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/Random.java#L310-L330), which is XOR'ed with the current value of the running JVM's time source with a resolution of nanoseconds ([System.nanoTime()](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/System.java#L384)) to provide a unique seed for each instance of the _Random_ class. | high | 
Haskell | When using function [randomRIO](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Haskell/random_streams_for_perf_stats.hs#L38), the global PRNG 'System.Random.Stateful.globalStdGen' is at first seeded with: _theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen_ in [Internal.hs](https://github.com/haskell/random/blob/master/src/System/Random/Internal.hs#L834). | very high | The [random](https://github.com/haskellari/splitmix/tree/master/src/System/Random) package depends on the [splitmix](https://github.com/haskellari/splitmix/blob/master/src/System/Random/SplitMix.hs) (SM) package, which in Linux is finally calling C function _splitmix_init_ in source [init.c](https://github.com/haskellari/splitmix/blob/master/cbits-unix/init.c#L4). The [splitmix.cabal](https://github.com/haskellari/splitmix/blob/master/splitmix.cabal#L102) shows its dependence on _c-sources: cbits-unix/init.c_, where C function _splitmix_init_ finally makes the Linux system call of [getentropy(3)](https://www.man7.org/linux/man-pages/man3/getentropy.3.html), which is implemented with using [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html).
Haxe | When running Haxe source code on the HashLink vm and function [Std.random()](https://github.com/HaxeFoundation/haxe/blob/development/std/hl/_std/Std.hx#L48) is called, the default RNG (already) got initialized with instruction [rnd = rnd_sys();](https://github.com/HaxeFoundation/haxe/blob/development/std/hl/_std/Std.hx#L33), which is based on calling [rnd_init_system](https://github.com/HaxeFoundation/hashlink/blob/master/src/std/random.c#L118), which calls [*hl_rnd_init_system()](https://github.com/HaxeFoundation/hashlink/blob/master/src/std/random.c#L71), which finally reads the seconds and microseconds values from Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html), plus the value of Linux system call [getpid(2)](https://man7.org/linux/man-pages/man2/getpid.2.html) to get the current process identification. Then, the seed named _r_ is calculated like this: [hl_rnd_set_seed(r,time ^ (pid \\| (pid << 16)));](https://github.com/HaxeFoundation/hashlink/blob/master/src/std/random.c#L87), where the system time value in microseconds (Epoch) is XOR'ed with the OR'ed value of the current pid value and the (bitwise) 16-bit-left shifted pid value. | very high | the link between function _rnd_sys()_ and name _rnd_init_system_ is here at: [rnd_sys()](https://github.com/HaxeFoundation/haxe/blob/development/std/hl/_std/Std.hx#L36); also see [hl_random](https://github.com/HaxeFoundation/haxe/blob/development/std/hl/_std/Std.hx#L25)
Hy | My implementation is using Python's [random](https://github.com/python/cpython/blob/main/Lib/random.py) package. In Linux, this OS related wildcard Python import is being applied: [from posix import *](https://github.com/python/cpython/blob/main/Lib/os.py#L55), as an intermediate step. Python name [urandom](https://github.com/python/cpython/blob/main/Lib/random.py#L57) is then implemented as C function [os_urandom_impl()](https://github.com/python/cpython/blob/main/Modules/posixmodule.c#L15994), which is implemented at C function [_PyOS_URandom()](https://github.com/python/cpython/blob/main/Python/bootstrap_hash.c#L532), which calls function [pyurandom()](https://github.com/python/cpython/blob/main/Python/bootstrap_hash.c#L478), which first tries to call [py_getrandom()](https://github.com/python/cpython/blob/main/Python/bootstrap_hash.c#L95), which is doing Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html). | very high | _py_getrandom_ is preferred over _py_getentropy_ at the [pyurandom()](https://github.com/python/cpython/blob/main/Python/bootstrap_hash.c#L502) function: _#ifdef PY_GETRANDOM_. The initial check for availability of _getrandom_ happens at the configuration of cpython in the _configure.ac_ file at function [AC_CACHE_CHECK()](https://github.com/python/cpython/blob/main/configure.ac#L8018), written in the [GNU Autoconf](https://www.gnu.org/software/autoconf/manual/) language
Inko | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [sys.random_bytes()](https://github.com/inko-lang/inko/blob/main/std/src/std/rand.inko#L99), [sys.getrandom()](https://github.com/inko-lang/inko/blob/main/std/src/std/sys/linux/rand.inko#L4)
Java | When class _ThreadLocalRandom_ is being used (for a change in the Java implementation), initial seeding is managed at the [localInit()](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/concurrent/ThreadLocalRandom.java#L156) method. Loading that class, a [seeder](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/concurrent/ThreadLocalRandom.java#L383) instance is initialized by reading the current timestamp with a resolution of milliseconds and the current timestamp with a resolution of nanoseconds, which are then XOR'ed to calculate a random raw seed: _private static final AtomicLong seeder = new AtomicLong(RandomSupport.mixMurmur64(System.currentTimeMillis()) ^ RandomSupport.mixMurmur64(System.nanoTime()));_ | high | _ThreadLocalRandom_ is not cryptographically secure, see at [Class ThreadLocalRandom](https://docs.oracle.com/javase/8/docs//api/java/util/concurrent/ThreadLocalRandom.html). The timestamp values are first going through 64-bit mix method [mixMurmur64()](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/jdk/internal/util/random/RandomSupport.java#L793), and then also even the result of the XOR operation!
Julia | Julia's default RNG initially calls Julia function [uv_random](https://github.com/JuliaLang/julia/blob/master/base/libc.jl#L457), which in return calls function [uv_random](https://docs.libuv.org/en/stable/misc.html#c.uv_random) in C library _libuv_ for cross-platform asynchronous I/O, which in return makes a Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) to _obtain a series of random bytes_ | very high | the [urandom(4)](https://linux.die.net/man/4/urandom) entropy source _gathers environmental noise from device drivers and other sources into an entropy pool_
Kotlin | same as with Groovy and Scala, Kotlin's default random number generator is also using the _java.util.Random_ package, see at [FallbackThreadLocalRandom](https://github.com/JetBrains/kotlin/blob/master/libraries/stdlib/jvm/src/kotlin/random/PlatformRandom.kt#L47) | high | [package kotlin.random](https://github.com/JetBrains/kotlin/blob/2.4.20/libraries/stdlib/src/kotlin/random/Random.kt)
Lua | the _math.randomseed(os.time())_ function most probably uses the system clock with a resolution of 1 second | low | [math.random](https://www.luadocs.com/docs/functions/math/random); two program runs within 1 second may yield the **same** byte streams
[Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury/random_streams_for_perf_stats.m) | my own and direct implementation of Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html), which reads the number of seconds plus any residual microseconds since the Epoch. Both values are then fed into the 64-bit FNV-1a (Fowler/Noll/Vo, variant 1a) hashing algorithm. The result is finally modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed. | high | I have just chosen the [FNV-1a](https://mojoauth.com/security-guides/fnv-1a-in-c) hashing algorithm for its simplicity compared to the (superior) [David Stafford's 64-bit mix function](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/45%20-%20Sources%20of%20a%20random%20seed#variant-13-of-david-staffords-64-bit-mix-function), which would be too much of a good thing here. But the Oxford Oberon-2 Compiler's simple mixing function would have also done the job; see below.
Modula-2 (GNU) | GNU Modula-2's random number generation is initialized with the help of module [RandomNumber.mod](https://github.com/gcc-mirror/gcc/blob/master/gcc/m2/gm2-libs-iso/RandomNumber.mod), where procedure [RandomInit(usec) ;](https://github.com/gcc-mirror/gcc/blob/master/gcc/m2/gm2-libs-iso/RandomNumber.mod#L49) is called based on some microseconds value, I guess the residual number of microseconds which is reset with every tick of a second. Then, the common PRNG of GNU C is seeded with this value at [srand(seed)](https://github.com/gcc-mirror/gcc/blob/master/gcc/m2/gm2-libs-iso/RandomNumber.mod#L60). | high | I compile with switch _-fiso_ to turn on ISO standard features of Modula-2.
Modula-3 (CM3) | procedure [RandomSeed](https://github.com/modula3/cm3/blob/master/m3-libs/libm3/src/random/Common/Random.m3#L64) takes two timestamp values in different formats, both in microseconds resolution, and processes them together with an initial seed value of 1202056903 | high | 
Mojo | [seed()](https://github.com/modular/modular/blob/main/Mojo/stdlib/std/random/random.mojo#L39) initially reads the current system time in nanoseconds: _seed(perf_counter_ns())_ | high | [perf_counter_ns()](https://github.com/modular/modular/blob/main/Mojo/stdlib/std/time/time.mojo#L174), [_clock_gettime()](https://github.com/modular/modular/blob/main/Mojo/stdlib/std/time/time.mojo#L71)
Nim | procedure [randomize()](https://github.com/nim-lang/Nim/blob/devel/lib/pure/random.nim#L615) initially reads the current system time in nanoseconds: _randomize(now.toUnix * 1_000_000_000 + now.nanosecond)_ | high |
Oberon (OBC) | Oberon instruction _Random.Randomize;_ calls C function [GetSeed(void)](https://github.com/Spivoxity/obc-3/blob/master/lib/Random.m#L109), which makes Linux system call _(gettimeofday(&tv, NULL))_. _GetSeed()_ finally returns this mixed value from the seconds and residual microseconds values: _return 13 * tv.tv_sec + tv.tv_usec;_ | high | The underlying idea of mixing operation _13 * tv.tv_sec + tv.tv_usec_ is that the microseconds value _tv.tv_usec_ is automatically reset to 0 every time a new second ticks, which would make a simple addition more predictable. However, the hashing quality of this mixing operation is still weak because the microseconds part dominates the lower bits of the returned seed value. See below at [David Stafford's 64-bit mix function](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/45%20-%20Sources%20of%20a%20random%20seed#variant-13-of-david-staffords-64-bit-mix-function) for a high quality mixing alternative.
OCaml | [Random.self_init ()](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/random_streams_for_perf_stats_main.ml#L46), via a little function chain, first tries to make a Linux system call of [getentropy](https://github.com/ocaml/ocaml/blob/trunk/runtime/sys.c#L672), which is implemented with using [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high |
Odin | function [rand.int_max()](https://pkg.odin-lang.org/core/math/rand/#int_max) indirectly makes a Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) to obtain a series of random bytes: [_rand_bytes](https://github.com/odin-lang/Odin/blob/master/base/runtime/os_specific_linux.odin#L30) | very high |
Perl 5 | the [rand()](https://perldoc.perl.org/5.38.2/functions/rand) function initially calls the [srand](https://perldoc.perl.org/5.38.2/functions/srand) function, which first tries to make a Linux system call of [getentropy(3)](https://www.man7.org/linux/man-pages/man3/getentropy.3.html), which is implemented with using [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [U64 Perl_seed(pTHX)](https://github.com/Perl/perl5/blob/blead/util.c#L4738-L4739)
PHP | function [rand()](https://www.php.net/manual/en/function.rand.php) is defined in _random.c_ and _Returns a random number from Mersenne Twister_, which is initialized with C function _php_random_generate_fallback_seed_ex()_ in the same sources. It starts with C function _PHP_SHA1InitArgs()_ in _sha1.c_ with SHA1 initialization constants, to which the count of non-leap seconds and residual microseconds that elapsed since the Epoch from system function call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html) are added, plus any previous seed. | high | 
Picat | Calling function [random2()](https://github.com/picat-lang/Picat/blob/main/lib/picat_lib_aux.pi#L66) is doing seeding with values from a Picat's program's execution environment: _random2() = Rand => statistics(runtime,[A,B]), statistics(program,[C,D]), statistics(heap,[E,F]), Rand = random(10*A+B+10*C+13*D+11*E+101*F)._, with _Rand_ being an integer value. | high | In [arith.c](https://github.com/picat-lang/Picat/blob/main/emu/arith.c#L138), predicates _random/0_ and _random/1_ are mapped to C functions _ran0_ and _ran1_, respectively. "bp" refers to B-Prolog. _ran1_ then returns C function [bp_math_random1()](https://github.com/picat-lang/Picat/blob/main/emu/arith.c#L1380), which is calling C function [srand()](https://github.com/picat-lang/Picat/blob/main/emu/arith.c#L1396) as defined in _stdlib.h_, see below at [The PRNG's of GNU C Library glibc](#the-prngs-of-gnu-c-library-glibc).
Pike | _[Class Random.System](https://pike.lysator.liu.se/generated/manual/modref/ex/predef_3A_3A/Random/System.html#System)_ "is the default implementation of the random functions. This is the Random.Interface combined with a system random source. ..on Unix systems it is /dev/urandom." | very high | 
PowerShell | it's my wild guess that cmdlet [Get-Random](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.6) applies the same seeding mechanism (nowadays) as C# (as shown above) | probably high |
Prolog, SWI | initially, reading at least 16 bytes ([MIN_RAND_SEED_LEN](https://github.com/SWI-Prolog/swipl-devel/blob/master/src/pl-arith.c#L4351)) from /dev/urandom, or if not working from /dev/random, with C function [seed_from_dev()](https://github.com/SWI-Prolog/swipl-devel/blob/master/src/pl-arith.c#L4436) | very high | if reading from /dev/urandom or /dev/random doesn't work, the fallback strategy is to read the current system time in [microseconds resolution](https://github.com/SWI-Prolog/swipl-devel/blob/master/src/os/pl-os.c#L356); [u.t = WallTime();](https://github.com/SWI-Prolog/swipl-devel/blob/master/src/pl-arith.c#L4446)
Python | [class numpy.random.RandomState(seed=None)](https://numpy.org/doc/stable/reference/random/legacy.html#numpy.random.RandomState): _If seed is None, then the MT19937 BitGenerator is initialized by reading data from /dev/urandom..._ | very high | using "legacy random generation" here: [numpy.random.randint](https://numpy.org/doc/stable/reference/random/generated/numpy.random.randint.html); new code should use _np.random.default_rng()_ instead
Roc | First reading the current system time in milliseconds since the Epoch. This value is then fed into the 64-bit FNV-1a (Fowler/Noll/Vo, variant 1a) hashing algorithm. The result is finally modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed. | high | 
Ruby | Via [formatter.rb](https://github.com/ruby/ruby/blob/master/lib/random/formatter.rb), calling _SecureRandom.random_number()_ calls C function [rand_random_number](https://github.com/ruby/ruby/blob/master/random.c#L1847), which is also implemented in _random.c_ at [rand_random_number()](https://github.com/ruby/ruby/blob/master/random.c#L1559). In a Linux system, Ruby's PRNG, a modified Mersenne Twister, has been initially seeded from [/dev/urandom](https://github.com/ruby/ruby/blob/master/random.c#L469). | very high | [module SecureRandom](https://docs.ruby-lang.org/en/3.2/SecureRandom.html); alternatively, seeding is done from the current timestamp, in microseconds resolution at least, plus the value of Linux system call [getpid(2)](https://man7.org/linux/man-pages/man2/getpid.2.html) to get the current process identification
Rust | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [from_os_rng](https://docs.rs/rand/0.9.1/rand/trait.SeedableRng.html#method.from_os_rng), [getrandom](https://docs.rs/getrandom/latest/getrandom/#getrandom-systems-random-number-generator)
Scala | class [Random](https://github.com/scala/scala3/blob/main/library/src/scala/util/Random.scala#L24) is based on _java.util.Random_, see above at Groovy | high | 
Scheme, Bigloo | same as with C | high | I replaced Bigloo's calendar time based seeding, which only has a resolution of 1 second, with my own and simple seeding mechanism. See below at [Bigloo default seeding](#bigloo-schemes-default-seeding) for more details.
Scheme, Racket | Racket function [random](https://docs.racket-lang.org/reference/generic-numbers.html#(part._.Random_.Numbers)) is mapped on Chez Scheme function [random](https://github.com/racket/racket/blob/master/racket/src/cs/rumble/random.ss#L342). The current timestamp with a resolution of seconds, plus any residual nanoseconds, since the Epoch is being read as the [original seeding](https://github.com/racket/racket/blob/master/racket/src/ChezScheme/s/5_3.ss#L3177), and processed into a 32-bit value. | high | For more details, see below at [Racket's REPL](#rackets-repl-and-original-source-of-a-random-seed).
Smalltalk (GNU) | seeding at _rnd := Random new._ is done so that system time values in seconds, milliseconds and nanoseconds are read and mixed into one seed value: [setSeed](https://github.com/gnu-smalltalk/smalltalk/blob/master/kernel/Random.st#L162) | high | 
Standard ML ([MLton](https://github.com/MLton/mlton)) | my own and direct implementation with calling Standard ML function _Time.now()_ to read the current timestamp with a resolution of seconds and any residual microseconds. Both values are then fed into the 64-bit FNV-1a (Fowler/Noll/Vo, variant 1a) hashing algorithm. The result is finally modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed.  | high |
Swift | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [SystemRNG](https://developer.apple.com/documentation/swift/systemrandomnumbergenerator#Platform-Implementation-of-SystemRandomNumberGenerator)
Tcl | Initial seeding happens at _tclBasic.c_ with C instruction: _iPtr->randSeed = TclpGetClicks() + PTR2UINT(Tcl_GetCurrentThread()) * 4093U;_, so the number of microseconds since the Epoch with function _TclpGetClicks()_ from _tclUnixTime.c_, plus the ID value of the currently running thread, as implemented in _tclUnixThrd.c_, multiplied with 4093. | high |
TypeScript | TypeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://tc39.es/ecma262/#sec-math.random). In my testing environment with the V8 implementation of [node.js](https://github.com/nodejs/node/blob/main/node.cc#L1413), initially entropy source /dev/urandom of Linux is being read into a 64-bit integer value named [seed](https://github.com/nodejs/node/blob/main/deps/v8/src/base/utils/random-number-generator.cc#L70). | very high | See at [V8 JavaScript engine](#the-random-seeding-mechanism-of-googles-v8-javascript-engine) below for more details.
V | V's default PNG gets seeded with function call [seed_generator.time_seed_array(2)](https://github.com/vlang/v/blob/master/vlib/rand/rand.v#L37), which is calling [time.sys_mono_now()](https://github.com/vlang/v/blob/master/vlib/rand/seed/seed.v#L16), which makes a Linux system call at [C.clock_gettime(1, &ts)](https://github.com/vlang/v/blob/master/vlib/v/gen/x64/native_constant_test.v#L160) to read the number of seconds and the number of any residual nanoseconds since the last system boot. | high |
Zig | my own and direct implementation of Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) with Zig instruction: _try std.posix.getrandom(std.mem.asBytes(&seed));_ | very high | 

<br/>

### Test of a 1 second seeding resolution

A probable 1 second seeding resolution can easily be tested with two automatic program runs within 1 second, here in the Dylan implementation. Command _date +%s_ shows the current Linux system time as the number of seconds since the Epoch:

```
$ date +%s; ./_build/bin/random-streams-for-perf-stats; head -c 10 ./random_bitstring.byte; echo -e "\n"; date +%s; ./_build/bin/random-streams-for-perf-stats; head -c 10 ./random_bitstring.byte
1791747223

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
f86e60a2ec

1791747223

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
f86e60a2ec$
```

Here, the first 10 characters of the random byte stream are identical with _f86e60a2ec_, indicating that the quality of randomness of the seed is rather low.

Now, we make a sleep of 1 second between program runs:

```
$ date +%s; ./_build/bin/random-streams-for-perf-stats; head -c 10 ./random_bitstring.byte; echo -e "\n"; sleep 1s; date +%s; ./_build/bin/random-streams-for-perf-stats; head -c 10 ./random_bitstring.byte
1791747505

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
3ea0b56843

1791747506

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
429e8da19f$
```

..only then to get two different random byte streams.

<br/>

#### Variant 13 of David Stafford's 64-bit mix function

As seen from here for example: [MixFunctions.java](https://commons.apache.org/proper/commons-rng/commons-rng-simple/jacoco/org.apache.commons.rng.simple.internal/MixFunctions.java.html):

```
    static long stafford13(long x) {
        x = (x ^ (x >>> 30)) * 0xbf58476d1ce4e5b9L;
        x = (x ^ (x >>> 27)) * 0x94d049bb133111ebL;
        return x ^ (x >>> 31);
    }
```

Based on Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html),
a demo program in C to effectively mix the seconds value of the Linux system time with its residual microseconds value may look like this:

```
#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>

int main(void) {
    struct timeval tv;

    if (gettimeofday(&tv, NULL) != 0) {
        perror("gettimeofday");
        return 1;
    }

    uint64_t seconds = (uint64_t)tv.tv_sec;
    uint64_t microseconds = (uint64_t)tv.tv_usec;
    uint64_t milliseconds = seconds * 1000ULL + microseconds / 1000ULL;

    // 1. Pack both 32-bit values into a single 64-bit integer
    uint64_t random_seed_stafford13 = (seconds << 32) | microseconds;

    // 2. Apply a SplitMix64/MurmurHash3 avalanching mixer
    random_seed_stafford13 ^= random_seed_stafford13 >> 30;  // ^ is the bitwise XOR (Exclusive OR) operation
    random_seed_stafford13 *= 0xbf58476d1ce4e5b9ULL;  // multiply with an "avalancing" constant
    random_seed_stafford13 ^= random_seed_stafford13 >> 27;
    random_seed_stafford13 *= 0x94d049bb133111ebULL;  // multiply with an "avalancing" constant
    random_seed_stafford13 ^= random_seed_stafford13 >> 31;

    // weak hashing of the Oxford Oberon-2 Compiler: https://github.com/Spivoxity/obc-3
    //   Random.m: https://github.com/Spivoxity/obc-3/blob/master/lib/Random.m#L109
    uint64_t random_seed_oxford_oberon2 = 13ULL * seconds + microseconds;

    // less calculation heavy FNV-1a (Fowler/Noll/Vo, variant 1a) non-cryptographic hash as seen here:
    // FNV-1a Hash in C#: Algorithm and Implementation: https://mojoauth.com/security-guides/fnv-1a-in-c
    uint64_t random_seed_fnv_1a = 14695981039346656037ULL;  // FNV offset basis in 64 bit: 0xCBF29CE484222325
    uint64_t fnv_1a_prime       = 1099511628211ULL;         // FNV prime in 64 bit: 0x100000001B3
    random_seed_fnv_1a ^= seconds;  // ^ is the bitwise XOR (Exclusive OR) operation
    random_seed_fnv_1a *= fnv_1a_prime;
    random_seed_fnv_1a ^= microseconds;
    random_seed_fnv_1a *= fnv_1a_prime;

    printf("Linux system time in seconds:      %lu\n", seconds);
    printf("Linux system time in milliseconds: %lu\n", milliseconds);
    printf("residual microseconds:                       %lu\n", microseconds);
    printf("64 bit random seed using David Stafford's 64-bit mix function: %lu\n", random_seed_stafford13);
    printf("64 bit random seed using function: 13 * sec + microsec:        %lu\n", random_seed_oxford_oberon2);
    printf("64 bit random seed using the Fowler/Noll/Vo, variant 1a:       %lu\n", random_seed_fnv_1a);

    return 0;
}
```

Two automatic program runs within 1 second may then look like this:

```
$ ./gettimeofday_call_SplitMix64_hashing; echo -e "\n"; ./gettimeofday_call_SplitMix64_hashing
Linux system time in seconds:      1791747698
Linux system time in milliseconds: 1791747698683
residual microseconds:                       683522
64-bit random seed using David Stafford's 64-bit mix function: 5620633334799208262
64-bit random seed using function: 13 * sec + microsec:        23293403596
64-bit random seed using the Fowler/Noll/Vo, variant 1a:       8021942652032737621


Linux system time in seconds:      1791747698
Linux system time in milliseconds: 1791747698684
residual microseconds:                       684221
64-bit random seed using David Stafford's 64-bit mix function: 14102637944186594101
64-bit random seed using function: 13 * sec + microsec:        23293404295
64-bit random seed using the Fowler/Noll/Vo, variant 1a:       8014502256844633784
$
```

It's obvious that David Stafford's 64-bit mix function does a much better good job at generating a system time based random seed than just calculating 13 * sec + microsec, and to some extent also the 64-bit Fowler/Noll/Vo, variant 1a hashing algorithm.

<br/>

---

# Details on the seeding of selected languages

In alphabetic order of the (underlying) programming language.

<br/>

#### Ada's default seeding

Application instruction [Reset (G);](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb#L143), with no extra seed value being provided, initializes a [RNG](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.adb#L97) of the [Mersenne Twister MT19937](https://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/emt.html) type with [624](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.ads#L141) 32-bit integers at procedure [Init](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.adb#L713).

_Init_ has been called in the same package at procedure [Reset (Gen : Generator)](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.adb#L532), which is calling function [Random_Seed.Get_Seed](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-ransee.adb#L50), which is calling function [Clock](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/a-calend.adb#L396), which is calling function [System.OS_Primitives.Clock](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-osprim__posix.adb#L42), which is finally making a Linux system call of [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html) to return a timestamp in microseconds resolution.

The _Generator_ type is (originally) defined in specification file [s-rannum.ads](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.ads#L156).

After some origin shifting (the Ada Epoch apparently starts at [2000-01-01T00:00:00Z](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-ransee.adb#L39) and not at 1970-01-01T00:00:00Z like the Unix Epoch) this value, and nothing else, is taken to seed the state vector of the Mersenne Twister at procedure _Init_ in the _System.Random_Numbers_ package.

From my point of view, the difficulty with figuring out the seeding mechanism in GNAT Ada lies in the wrapping of generic package [Ada.Numerics.Discrete_Random](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/a-nudira.ads) around its underlying package [System.Random_Numbers](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.ads). Both define and implement the _Reset (Gen : Generator)_ procedure (next to their other versions of _Reset_). 

Re the source file naming convention, for example at sources [a-nudira.adb](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/a-nudira.adb): this file name could stand for _Ada Numerics Discrete Random_, here with the body of that package, that is its implementation part as seen from its file extension ~.adb.

<br/>

Question: can the generic wrapper package _Ada.Numerics.Discrete_Random_ be ignored and instead package _System.Random_Numbers_ be used directly?

Answer: yes, but its not recommended for good reasons.

One reason, when mimicing the style of my application _random_streams_for_perf_stats.adb_, is the fact that _Default_Min_ is hardcoded to 0 in [s-rannum.ads](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/s-rannum.ads#L83):

```
   generic
      type Result_Subtype is (<>);
      Default_Min : Result_Subtype := Result_Subtype'Val (0);
   function Random_Discrete
     (Gen : Generator;
      Min : Result_Subtype := Default_Min;
      Max : Result_Subtype := Result_Subtype'Last) return Result_Subtype;
   --  Returns pseudo-random numbers uniformly distributed on Min .. Max
```

The GNAT compiler compiles such Ada code:

```
   subtype Target_Range is Integer range 1 .. 100;
   function Random_Range_Fun is new System.Random_Numbers.Random_Discrete
     (Result_Subtype => Target_Range);
   Gen : System.Random_Numbers.Generator;
   Result : Target_Range;
```

...but later the compiled program is indeed crashing during runtime as already warned of during compilation time:

```
...: warning: "System.Random_Numbers" is an internal GNAT unit [-gnatwi]
...: warning: use of this unit is non-portable and version-dependent [-gnatwi]
```

Though, this modified definition works fine: _subtype Target_Range is Integer range 0 .. 100;_

However, the following code is the better solution, and is still keeping a minimum value of 1:

```
   ...
   function Random_Int is new System.Random_Numbers.Random_Discrete
     (Result_Subtype => Integer);
   Gen : System.Random_Numbers.Generator;
   Result : Integer;
begin
   System.Random_Numbers.Reset (Gen);
   Result := Random_Int (Gen, Min => 1, Max => 100);  -- call the user defined function from above
   Ada.Text_IO.Put_Line ("Drawn number:" & Integer'Image (Result));
   ...
```

<br/>

#### Bigloo Scheme's default seeding

Bigloo Scheme's default seeding happens in runtime sources [cmain.c](https://github.com/manuel-serrano/bigloo/blob/master/runtime/Clib/cmain.c#L215) with the usual C function _srand()_:

```
      /* random seed */
      {
	 time_t taux;
	 struct tm *tm;
	 time(&taux);
	 tm = gmtime(&taux);
	    
	 srand((24 * (60 * tm->tm_sec + tm->tm_min)) + tm->tm_hour);
     ... 
      }
```

Seeding term _(24 * (60 * tm->tm_sec + tm->tm_min)) + tm->tm_hour_ takes a minimum value of 0 and a maximum value of (24 * (60 * 59 + 59)) + 23 = 86399,
and features a contiguous range of integer numbers with no gaps and no duplicates! It changes with every tick of a second.

Here's my [replacement code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Bigloo/random_streams_for_perf_stats.scm#L38) for better seeding quality with the friendly help of Google AI, applying the same mechanism as in my improved [C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c#L60) and [C++](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B/random_streams_for_perf_stats.cpp#L83) implementations, based on the simple idea from the Oxford Oberon-2 Compiler for a seeding (that is _return 13 * tv.tv_sec + tv.tv_usec_):

```
(module random_streams_for_perf_stats
   (main main)
   (foreign (include "time.h"))
   (extern (type timespec (struct (tv_sec::long "tv_sec")
                                  (tv_nsec::long "tv_nsec")) "struct timespec"))
   (extern (macro CLOCK_MONOTONIC::int "CLOCK_MONOTONIC"))
   (extern (clock_gettime::int (int timespec*) "clock_gettime")))
...

(define seconds 0)
(define nanoseconds 0)
(let ((ts (make-timespec*)))
      (clock_gettime CLOCK_MONOTONIC ts)
      (let ((seconds (timespec*-tv_sec ts))
                (nanoseconds (timespec*-tv_nsec ts)))
             (set! old-seed (bit-xor (* 13 seconds) nanoseconds))))
...

(define (main args)
...
  (set! old-seed (+ (modulo old-seed (- m 2)) 1))  ; 2026-10-26: scale the raw seed appropriately to [1..m[
...
)
```

<br/>

#### The PRNG's of GNU C Library glibc

As used by [Picat](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Picat/random_streams_for_perf_stats.pi#L104).

You may clone and explore the glibc sources, with a cool 21,088 files as of 2026-11-03, like this for example:

```
$ git clone https://sourceware.org/git/glibc.git
$ cd glibc/stdlib
```

So, in case of Picat, called C function _srand(unsigned int x)_ in _random.c_ calls function __srandom (unsigned int x) in the same file, which calls __srandom_r (unsigned int seed, struct random_data *buf) in _random_r.c_. 

By default, _.rand_type = TYPE_3_ is chosen in _random.c_, so, under normal circumstances, a (modern) linear-feedback shift register PRNG is used with trinomial x^31 + x^3 + 1.

Then calling C function _rand()_ returns an integer number between 0 and 2^31-1 inclusive, computed from that type of PRNG.

In other words, from a program written in pure Picat you cannot change the type of PRNG being used. In C code (with the friendly help of Google AI), it would look like this for example to use a _TYPE_2_ PRNG with only 64 bytes of state information instead of _TYPE_3_ with 128 bytes:

```
#define _GNU_SOURCE  // instruct the GNU C Library to unlock non-standard features, like function initstate_r()
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
...
int main(void) {
    static char manipulated_global_table[64];
    initstate(1, manipulated_global_table, sizeof(manipulated_global_table));
    ...
}
```

<br/>

### The random seeding mechanism of the Dart vm

Raw system call ___NR_getrandom_ is called from function _Crypto::GetRandomBytes()_, here in the Linux OS variant: [crypto_linux.cc](https://github.com/dart-lang/sdk/blob/main/runtime/bin/crypto_linux.cc#L43), which is called by function [DartUtils::EntropySource()](https://github.com/dart-lang/sdk/blob/main/runtime/bin/dartutils.cc#L269), which is assigned to variable _entropy_source_:

```
init_params.entropy_source = DartUtils::EntropySource;
```

in [main_impl.cc](https://github.com/dart-lang/sdk/blob/main/runtime/bin/main_impl.cc#L1495).

The Dart vm initially hooks into its host environment with: _set_entropy_source_callback(params->entropy_source);_ in [dart.cc](https://github.com/dart-lang/sdk/blob/main/runtime/vm/dart.cc#L300).

Functions _set_entropy_source_callback()_ and _entropy_source_callback()_ are defined in [dart.h](https://github.com/dart-lang/sdk/blob/main/runtime/vm/dart.h#L113-L118):

```
  static void set_entropy_source_callback(Dart_EntropySource entropy_source) {
    entropy_source_callback_ = entropy_source;
  }
  static Dart_EntropySource entropy_source_callback() {
    return entropy_source_callback_;
  }
```

Beware that C++ identifier [FLAG_random_seed](https://github.com/dart-lang/sdk/blob/main/runtime/vm/random.cc#L13) is set to 0 when not in debug mode, which leads to a call of function [entropy_source_callback()](https://github.com/dart-lang/sdk/blob/main/runtime/vm/random.cc#L21) from a Dart application which for example initializes a DRNG with instruction: [Random rnd = Random();](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/05%20-%20web%20programming/05b%20-%20Dart%20on%20the%20Dart%20virtual%20machine/random_streams_for_perf_stats.dart#L52):

```
Dart_EntropySource callback = Dart::entropy_source_callback();
```

<br/>

All in all, the two chains of function calls roughly look like this as I see it:

**Dart vm**: _set_entropy_source_callback_ -> _entropy_source_callback_ -> _entropy_source_ -> _DartUtils::EntropySource_ -> _Crypto::GetRandomBytes()_ -> ___NR_getrandom_ (Linux raw system call)

**Dart app**: _import 'dart:math'; ... Random rnd = Random();_ -> _Dart::entropy_source_callback()_ to get a raw seed -> [Initialize(seed)](https://github.com/dart-lang/sdk/blob/main/runtime/vm/random.cc#L36) to take the fourth output value of Dart's DRNG for a seed which can then be used by the user's application

<br/>

If reading of _Crypto::GetRandomBytes()_ fails, Dart's vm falls back to reading the current OS timestamp with a resolution of microseconds at [OS::GetCurrentTimeMicros()](https://github.com/dart-lang/sdk/blob/main/runtime/vm/random.cc#L31).

<br/>

### The random seeding mechanism of Google's V8 JavaScript engine

See the bridge from JavaScript function [Math.random()](https://tc39.es/ecma262/#sec-math.random) to its [V8 Torque builtin](https://v8.dev/docs/torque-builtins):

> In V8, builtins can be seen as chunks of code that are executable by the VM at runtime.

_Math.random()_ is [transitioning javascript builtin MathRandom()](https://github.com/nodejs/node/blob/main/deps/v8/src/builtins/math.tq#L554), which is written in Google's Torque programming language (~.tq). There and initially, the extern macro [InitializeAndMaybeRefillMathRandom(context);](https://github.com/v8/v8/blob/main/src/builtins/math.tq#L563) is being run.

This macro is matched to C++ function [TNode<Smi> CodeStubAssembler::InitializeAndMaybeRefillMathRandom()](https://github.com/v8/v8/blob/main/src/codegen/code-stub-assembler.cc#L21012) in source _code-stub-assembler.cc_:

- CSA = CodeStubAssembler
- Smi = small integer, and represents integer Numbers that can be stored in 31 bits: [smi.h](https://github.com/v8/v8/blob/main/src/objects/smi.h)

Also see external reference _V(initialize_and_maybe_refill_math_random, "MathRandom::InitializeAndMaybeRefillCache")_ at [external-reference.h](https://github.com/v8/v8/blob/main/src/codegen/external-reference.h#L208).

C++ source _math-tq-csa.cc_ is **dynamically** generated when building node.js in a specific environment. I indeed built node.js (with latest version 26.12.1 as of 2026-10-21) from sources ([Building Node.js](https://github.com/nodejs/node/blob/main/BUILDING.md)) to check out this file, which is stored into output folder _./node-26.12.1/out/Release/obj/gen/torque-generated/src/builtins_:

```
...
  TNode<Smi> tmp6;
  if (block1.is_used()) {
    ca_.Bind(&block1);
    tmp6 = CodeStubAssembler(state_).RefillMathRandom(TNode<NativeContext>{parameter0});
    ca_.Goto(&block2, tmp6);
  }
...
```

So, function call _InitializeAndMaybeRefillMathRandom(context)_ is expanded into above code snippet.

Node.js handwritten source _code-stub-assembler.cc_ (of the same file name as the handwritten source of the V8 engine from above!) in folder _./deps/v8/src/codegen_ contains this code snippet. Pay attention to function _refill_math_random()_:

```
...
TNode<Smi> CodeStubAssembler::RefillMathRandom(
    TNode<NativeContext> native_context) {
  // Cache exhausted, populate the cache. Return value is the new index.
  const TNode<ExternalReference> refill_math_random =
      ExternalConstant(ExternalReference::refill_math_random());
  const TNode<ExternalReference> isolate_ptr =
      ExternalConstant(ExternalReference::isolate_address());
  MachineType type_tagged = MachineType::AnyTagged();
  MachineType type_ptr = MachineType::Pointer();

  return CAST(CallCFunction(refill_math_random, type_tagged,
                            std::make_pair(type_ptr, isolate_ptr),
                            std::make_pair(type_tagged, native_context)));
}
...
```

Node.js handwritten source _code-stub-assembler.cc_ in folder _./deps/v8/src/numbers_ contains this code snippet:

```
...
Address MathRandom::RefillCache(Isolate* isolate, Address raw_native_context) {
  ...
  // Initialize state if not yet initialized. If a fixed random seed was
  // requested, use it to reset our state the first time a script asks for
  // random numbers in this context. This ensures the script sees a consistent
  // sequence.
  if (state.s0 == 0 && state.s1 == 0) {
    uint64_t seed;
    if (v8_flags.random_seed != 0) {
      seed = v8_flags.random_seed;
    } else {
      isolate->random_number_generator()->NextBytes(&seed, sizeof(seed));
    }
    state.s0 = base::RandomNumberGenerator::MurmurHash3(seed);
    state.s1 = base::RandomNumberGenerator::MurmurHash3(~seed);
    CHECK(state.s0 != 0 || state.s1 != 0);
  }
  ...
}
...
```

The bridge from external reference _refill_math_random()_ and function _MathRandom::RefillCache()_ is made in file _external-reference.cc_ in folder __./deps/v8/src/codegen_:

```
...
FUNCTION_REFERENCE(refill_math_random, MathRandom::RefillCache)
...
```

> [!NOTE]
> Calling JavaScript function _Math.random()_ has actually and initially called compiled C++ function [MathRandom::RefillCache()](https://chromium.googlesource.com/v8/v8/+/60c0edc/src/math-random.cc#34).

Function _RandomNumberGenerator::NextBytes()_ in V8 engine source [random-number-generator.cc](https://github.com/v8/v8/blob/main/src/base/utils/random-number-generator.cc#L124) takes its data from the _RandomNumberGenerator::RandomNumberGenerator()_, which has been seeded - if all went well - from Linux entropy source [/dev/urandom](https://github.com/v8/v8/blob/main/src/base/utils/random-number-generator.cc#L70).

So, _isolate->random_number_generator()->NextBytes(&seed, sizeof(seed));_ in _math-random.cc_ is actually calling _RandomNumberGenerator::NextBytes()_ in _random-number-generator.cc_.

<br/>

Or as Google AI has stated it on prompt "Why does a call of JavaScript function Math.random() go such a long way in the Node.js runtime?":

> ... V8 enforces a strict separation between JIT compilation (speed) and the C++ Runtime (complex state & OS access)
> ...
> 1. Torque / CSA acts as a high-speed velvet rope, serving random numbers instantly out of cache without breaking a sweat.
> 2. The C++ Runtime / OS layer acts as the heavy-duty machinery that boots up quietly in the background only when that cache runs dry.

<br/>
 
#### Racket's REPL and original source of a random seed

In the Racket REPL, provide some erroneous arguments when calling the _random_ function to induce error messages and other replies:

```
$ racket
Welcome to Racket v9.0 [cs].
> random
#<procedure:random>
> (random 4294967087)
1954668196
> (random 4294967088)
random: contract violation
  expected: (or/c (integer-in 1 4294967087) pseudo-random-generator?)
  given: 4294967088
 [,bt for context]
> (random)
0.8138154631651977
> (random 20 10)
random: contract violation
  expected: (>/c 20)
  given: 10
  argument position: 2nd
 [,bt for context]
> (exit)
$
```

..and then look for (unique) parts of these replies in [Racket's GitHub repository](https://github.com/racket/racket/tree/master).
That's how I found _random_'s pre-processing in Racket's [pre-base.rkt](https://github.com/racket/racket/blob/master/racket/collects/racket/private/pre-base.rkt) source file.

There, Racket calling function _(random x y)_ is further processed, after all checks have passed successfully, as [(+ x (random d))](https://github.com/racket/racket/blob/master/racket/collects/racket/private/pre-base.rkt#L144), where _(random d)_, with d being calculated as y - x, is being mapped on Chez Scheme's [random](https://github.com/racket/racket/blob/master/racket/src/cs/rumble/random.ss#L342) function, which only takes one argument (_d_) in case _[(n) ... ]_.

Racket's Chez Scheme (CS) implementation is organized in layers, with the top one being called "Rumble", and implemented in [Chez Scheme](https://github.com/racket/racket/tree/master/racket/src/cs). [random](https://github.com/racket/racket/blob/master/racket/src/cs/rumble.sls#L499) is a Racket keyword in the Rumble dictionary, which is implemented in a ~.sls file for a Scheme Library Source. Chez Scheme source file [rumble/random.ss](https://github.com/racket/racket/blob/master/racket/src/cs/rumble.sls#L951) is also mentioned in this Scheme Library Source file.

Chez Scheme's _random_ function calls function [pseudo-random-generator-next!‎](https://github.com/racket/racket/blob/master/racket/src/cs/rumble/random.ss#L335) in the _rumble/random.ss_ source file.

> [!CAUTION]
> Be careful with source file _rumble/random.ss_, because everything up to line #297 is only comment!

The _current-pseudo-random-generator_ has been created with function call [(make-pseudo-random-generator)](https://github.com/racket/racket/blob/master/racket/src/cs/rumble/random.ss#L307) in its new Racket Scheme implementation (at line #307).

_make-pseudo-random-generator_ can then be found in Racket's Chez Scheme sources in file [5_3.ss](https://github.com/racket/racket/blob/master/racket/src/ChezScheme/s/5_3.ss#L3177) :

```
    (set! make-pseudo-random-generator
          (lambda ()
            (let ([s (create-pseudo-random-generator 0.0 0.0 0.0 0.0 0.0 0.0)]
                  [t (current-time 'time-utc)])
              (init! s (bitwise-and (+ (time-second t) (time-nanosecond t))
                                    #xFFFFFFFF))
              s)))
```

<br/>

##_end
