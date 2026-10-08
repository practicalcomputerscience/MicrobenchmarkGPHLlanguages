2026-09-23: work in progress

# Sources of a random seed

- CS = cryptographically secure
- Epoch = Unix time, which is defined as the number of non-leap seconds and microseconds from system function call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html), which have passed since 00:00:00 UTC on Thursday, 1 January 1970
- PCG = permuted congruential generator: http://www.pcg-random.org
- PRNG = pseudo-random number generator
- RNG = random number generator

<br/>

From Wikipedia:

> In Unix-like operating systems, [/dev/random and /dev/urandom](https://en.wikipedia.org/wiki//dev/random) are special files that provide random numbers from a cryptographically secure pseudorandom number generator (CSPRNG). The CSPRNG is seeded with entropy (a value that provides randomness) from environmental noise, collected from device drivers and other sources.

> The /dev/urandom source is itself a PRNG but it is frequently reseeded from the high entropy /dev/random resource which makes it impractical for an attacker to target.

from [Random Values In PHP](https://phpsecurity.readthedocs.io/en/latest/Insufficient-Entropy-For-Random-Values.html#random-values-in-php).

<br/>

Here is my simple quality ranking of the randomness of a seed:

- very high quality, like cryptographic quality
- high quality, like current (Linux) system timestamp with resolution of milliseconds or even nanoseconds
- medium quality, like current system timestamp with resolution of hundredths of a second
- low quality, like current system timeststamp with resolution of 1 second
- bad quality, which is worse than an entropy source like the current system timestamp with a resolution of only 1 second. Have I done an implementation with no entropy source at all? (tbd)

<br/>

The solution in this Pascal implementation:

> The [ISO 7185 program version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_streams_for_perf_stats_iso7185.pp) cannot access (Linux) system resources, and thus not read a time value for example.

..with a [Random seed with leveraging the Address Space Layout Randomization (ASLR)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal#random-seed-with-leveraging-the-address-space-layout-randomization-aslr) 
got me thinking about the general quality of a random seed in the numerous language implementations of the pseudo-random number generator in question.
Leveraging this (sophisticated) idea unexpectedly provided a good source of randomness in a programming language which otherwise cannot access Linux system resources at all!

Before I came to a Pascal implementation, I was already aware of the fact that not all program implementations feature a somehow decent source of randomness, and thus started the language list below to get me an overview.

> [!NOTE]
> The given sources of random seeds only mean the sources I've (implicitly) used, not that these are necessarily the only sources of entropy in a given programming language!

<br/>

Nowadays, many languages offer an interface to a cryptographically secure source of entropy, for example with implicitly calling Linux system function [getrandom(2)](https://www.man7.org/linux/man-pages/man2/getrandom.2.html), something which could often be implemented **directly** with a user defined function in many programming languages.

However, often a (Linux) system time based solution is just good enough. My Mercury implementation for example features an individual "high quality" solution from my point of view,
where everything needed is just packed into a user defined C function as part of the Mercury source code, see below.

<br/>

<br/>

programming language | used source of random seed | estimated quality of randomness | comment
--- | --- | --- | ---
Ada (GNAT) | package _Ada.Numerics.Discrete_Random_: [Standard library: Numerics](https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_numerics.html#standard-library-numerics): how is this seeded? | high | two manual program runs within 1 second will yield two different byte streams
AssemblyScript | using function _Math.random()_ from: _The Math API is very much like JavaScript's, .._ from [Math](https://www.assemblyscript.org/stdlib/math.html#math) | high | see below at TypeScript
Awk (GNU) | the _srand()_ function probably uses the system clock with a resolution of 1 second: [9.1.3 Numeric Functions](https://www.gnu.org/software/gawk/manual/html_node/Numeric-Functions.html#Numeric-Functions-1) | low(?) | different implementations and versions of Awk and Mawk may feature different implementations of _srand()_ and _rand()_
Ballerina | [module-ballerina-random/ballerina/natives.bal](https://github.com/ballerina-platform/module-ballerina-random/blob/main/ballerina/natives.bal#L22) initially reads the current system time in milliseconds: _isolated decimal x0 = currentTimeInMilliSeconds();_ | high
[C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) | a Linux system call of [clock_gettime(3)](https://www.man7.org/linux/man-pages/man3/clock_gettime.3.html) reads the number of seconds and the number of any residual, non-overlapping **nanoseconds** since the last system boot ("CLOCK_MONOTONIC"), which are then simply mixed as: _unsigned int seed = (unsigned int)((ts.tv_sec * 13) ^ ts.tv_nsec);_. The result is finally modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed. | high | Here, I just applied the Oxford Oberon-2 Compiler's simple mixing function, see below, to not overdo it at the seeding.
C++ | tbd | high | tbd: 2026-10-09
C3 | [random.c3](https://github.com/c3lang/c3c/blob/master/lib/std/math/random.c3): how is this seeded? | high | two manual program runs within 1 second will yield two different byte streams
C# | it looks to me that (nowadays) C# is also leveraging the Address Space Layout Randomization (ASLR), as it can be seen at instruction: _ulong* ptr = stackalloc ulong[4];_ in sources [Random.Xoshiro256StarStarImpl.cs](https://github.com/dotnet/dotnet/blob/1aed6a13a182cbe8d03d0f60f9a0817c875f0ea1/src/runtime/src/libraries/System.Private.CoreLib/src/System/Random.Xoshiro256StarStarImpl.cs#L35) | high |
Chapel | [Random](https://chapel-lang.org/docs/modules/standard/Random.html): _When not provided explicitly, a seed value will be generated in an implementation specific manner which is designed to minimize the chance that two distinct randomStream’s will have the same seed._: how exactly is this seeded? | high | two manual program runs within 1 second will yield two different byte streams
Clojure | 
COBOL (GnuCOBOL) | _ACCEPT FROM TIME_ returns the current system time in format HHMMSSCC, where CC represents the hundredths of a second | medium | [Working with Dates and Time in COBOL](https://www.mainframemaster.com/tutorials/cobol/dates-time)
CoffeeScript | CoffeeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | probably high (nowadays) | see at TypeScript below
Common Lisp | 
Crystal | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) in module [getrandom.cr](https://github.com/crystal-lang/crystal/blob/master/src/crystal/system/unix/getrandom.cr) | very high | Crystal implemented Melissa O'Neill's PCG Random Number Generation for C (2014): [pcg32.cr](https://github.com/crystal-lang/crystal/blob/master/src/random/pcg32.cr)
Curry (KiCS2) | 
D | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [Function std.random.unpredictableSeed](https://dlang.org/library/std/random/unpredictable_seed.html)
Dart | 
Dylan | Dylan function [default-random-seed()](https://github.com/dylan-lang/opendylan/blob/master/sources/common-dylan/unix-common-extensions.dylan#L40) calls POSIX C function _time()_ to get the current system timestamp, that is the count of seconds elapsed since the Epoch, and then takes the first 4 bytes and does some bitwise operations on them to generate an integer seed (*) | low |
Eiffel, Liberty | 
Factor | 
Forth (Gforth) | 
Fortran (GNU) | _call random_number(ini_random_number)_ from [random_number](https://fortran-lang.org/learn/intrinsics/math/#random-number): how is this seeded? | high | two manual program runs within 1 second will yield two different byte streams
FreeBASIC | seeding is based on the return value of the [TIMER](https://www.freebasic-portal.de/befehlsreferenz/timer-295.html) function with a resolution in microseconds | high | the [RANDOMIZE instruction](https://www.freebasic-portal.de/befehlsreferenz/randomize-539.html) is used for seeding
(Object) Free Pascal | Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html) at function [Fptime()](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/linux/ossysc.inc?plain=1#L29), though this function only reads the number of seconds and **not** microseconds since the Epoch | low | [Procedure Randomize](https://gitlab.com/freepascal.org/fpc/source/-/blob/main/rtl/linux/system.pp#L462)
Gleam | 
Go | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html): [getrandom.go](https://cs.opensource.google/go/go/+/master:src/internal/syscall/unix/getrandom.go) | very high | the exact (default) mechanism is complex, because Go also has fallback's implemented and cares about concurrency issues etc.; [getrandom_linux.go](https://cs.opensource.google/go/go/+/master:src/internal/syscall/unix/getrandom_linux.go)
Groovy | 
Haskell | 
Haxe | 
Hy | 
Inko | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [sys.random_bytes()](https://github.com/inko-lang/inko/blob/042424276c517110faf6fe0043febd42579ce663/std/src/std/rand.inko#L99), [sys.getrandom()](https://github.com/inko-lang/inko/blob/042424276c517110faf6fe0043febd42579ce663/std/src/std/sys/linux/rand.inko#L4)
Java | class _ThreadLocalRandom_ uses the current timestamp with a resolution of milliseconds and the current timestamp with a resolution of nanoseconds, and then XOR's them to finally get a random seed: [ThreadLocalRandom.java](https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/util/concurrent/ThreadLocalRandom.java) | high | _ThreadLocalRandom_ is not cryptographically secure: [Class ThreadLocalRandom](https://docs.oracle.com/javase/8/docs//api/java/util/concurrent/ThreadLocalRandom.html)
Julia | Julia's default RNG initially calls Julia function [uv_random](https://github.com/JuliaLang/julia/blob/master/base/libc.jl#L457), which in return calls function [uv_random](https://docs.libuv.org/en/stable/misc.html#c.uv_random) in C library _libuv_ for cross-platform asynchronous I/O, which in return makes a Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) to _obtain a series of random bytes_ | very high | the [urandom(4)](https://linux.die.net/man/4/urandom) entropy source _gathers environmental noise from device drivers and other sources into an entropy pool_
Kotlin | 
Lua | the _math.randomseed(os.time())_ function most probably uses the system clock with a resolution of 1 second | low | https://www.luadocs.com/docs/functions/math/random
[Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury/random_streams_for_perf_stats.m) | my own and direct implementation of Linux system call [gettimeofday(2)](https://www.man7.org/linux/man-pages/man2/gettimeofday.2.html), which reads the number of seconds plus any residual microseconds since the Epoch. Both values are then fed into the 64 bit FNV-1a (Fowler/Noll/Vo, variant 1a) hashing algorithm. The result is finally modulo-scaled to range [1..65521-1] for a safe 16 bit integer random seed. | high | I have just chosen the [FNV-1a](https://mojoauth.com/security-guides/fnv-1a-in-c) hashing algorithm for its simplicity compared to the (superior) [Variant 13 of David Stafford's 64-bit mix function](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/45%20-%20Sources%20of%20a%20random%20seed#variant-13-of-david-staffords-64-bit-mix-function), which would be too much of a good thing here. But the Oxford Oberon-2 Compiler's simple mixing function would have also done it; see below.
Modula-2 (GNU) | in module [RandomNumber](https://github.com/gcc-mirror/gcc/blob/master/gcc/m2/gm2-libs-iso/RandomNumber.mod), procedure _RandomInt_ calls procedure _RandomLongInt_, which calls procedure _RandomBytes_, which calls C function _rand()_ as defined in [libc.def](https://github.com/gcc-mirror/gcc/blob/master/gcc/m2/gm2-libs/libc.def). So, the GNU C compiler is then usually looking at the Linux system's C library implementation at source file [rand.c](https://github.com/bminor/glibc/blob/master/stdlib/rand.c). 
Modula-3 (CM3) | 
Mojo | [seed()](https://github.com/modular/modular/blob/85356f6562ed57bab8762fde38448ba5b50b69c9/Mojo/stdlib/std/random/random.mojo#L39) initially reads the current system time in nanoseconds: _seed(perf_counter_ns())_ | high | [perf_counter_ns()](https://github.com/modular/modular/blob/85356f6562ed57bab8762fde38448ba5b50b69c9/Mojo/stdlib/std/time/time.mojo#L174), [_clock_gettime()](https://github.com/modular/modular/blob/85356f6562ed57bab8762fde38448ba5b50b69c9/Mojo/stdlib/std/time/time.mojo#L71)
Nim | procedure [randomize()](https://github.com/nim-lang/Nim/blob/519ef706f77a6982ac3c57532a35aaf3b1de6c55/lib/pure/random.nim#L615) initially reads the current system time in nanoseconds: _randomize(now.toUnix * 1_000_000_000 + now.nanosecond)_ | high |
Oberon (OBC) | Oberon instruction _Random.Randomize;_ calls C function [GetSeed(void)](https://github.com/Spivoxity/obc-3/blob/1719f9fb328257b46dc7721267850bf929ebafd5/lib/Random.m#L109), which makes Linux system call _(gettimeofday(&tv, NULL))_. _GetSeed()_ finally returns this mixed value from the seconds and residual microseconds values: _return 13 * tv.tv_sec + tv.tv_usec;_ | high | Underlying idea of this operation: the microseconds value is automatically reset to 0 every time a new second ticks. However, the hashing quality of operation _13 * tv.tv_sec + tv.tv_usec_ is still weak because the microseconds part dominates the lower bits of the return value. See below at [Variant 13 of David Stafford's 64-bit mix function](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/45%20-%20Sources%20of%20a%20random%20seed#variant-13-of-david-staffords-64-bit-mix-function) for a high quality alternative.
OCaml | 
Odin | function [rand.int_max()](https://pkg.odin-lang.org/core/math/rand/#int_max) indirectly makes a Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) to obtain a series of random bytes: [_rand_bytes](https://github.com/odin-lang/Odin/blob/4d09219ff432b7abb28418ce5051c4088c664e42/base/runtime/os_specific_linux.odin#L30) | very high |
Perl 5 | the [rand()](https://perldoc.perl.org/5.38.2/functions/rand) function initially calls the [srand](https://perldoc.perl.org/5.38.2/functions/srand) function, which first tries to call [getentropy(3)](https://www.man7.org/linux/man-pages/man3/getentropy.3.html), which is implemented using [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [U64 Perl_seed(pTHX)](https://github.com/Perl/perl5/blob/157525abaa406f6d0737a3480cc625eb33e3ff8b/util.c#L4738-L4739)
PHP | function [rand()](https://www.php.net/manual/en/function.rand.php): _This function uses the global Mt19937 (“Mersenne Twister”) instance as the source of randomness and thus shares its state with all other functions using the global Mt19937._ | high | two manual program runs within 1 second will yield two different byte streams
Picat | 
Pike | _[Class Random.System](https://pike.lysator.liu.se/generated/manual/modref/ex/predef_3A_3A/Random/System.html#System)_ "is the default implementation of the random functions. This is the Random.Interface combined with a system random source. ..on Unix systems it is /dev/urandom." | very high | 
PowerShell | it's my wild guess that cmdlet [Get-Random](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.6) may apply the same seeding (nowadays) as C# (as shown above) | probably high |
Prolog, SWI | 
Python | [class numpy.random.RandomState(seed=None)](https://numpy.org/doc/stable/reference/random/legacy.html#numpy.random.RandomState): _If seed is None, then the MT19937 BitGenerator is initialized by reading data from /dev/urandom..._ | very high | using "legacy random generation" here: [numpy.random.randint](https://numpy.org/doc/stable/reference/random/generated/numpy.random.randint.html); new code should use _np.random.default_rng()_ instead
Roc | 
Ruby | probably /dev/urandom | very high | [module SecureRandom](https://docs.ruby-lang.org/en/3.2/SecureRandom.html)
Rust | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [from_os_rng](https://docs.rs/rand/0.9.1/rand/trait.SeedableRng.html#method.from_os_rng), [getrandom: system’s random number generator](https://docs.rs/getrandom/latest/getrandom/#getrandom-systems-random-number-generator)
Scala | 
Scheme, Bigloo | 
Scheme, Racket | 
Smalltalk (GNU) | 
Standard ML (MLton) | 
Swift | Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) | very high | [Platform Implementation of `SystemRandomNumberGenerator`](https://developer.apple.com/documentation/swift/systemrandomnumbergenerator#Platform-Implementation-of-SystemRandomNumberGenerator)
Tcl | _The seed of the generator is initialized from the internal clock of the machine..._ from: [rand](https://www.tcl-lang.org/man/tcl8.6/TclCmd/mathfunc.htm#M28) | high | two manual program runs within 1 second will yield two different byte streams
TypeScript | TypeScript uses JavaScript's resources, so here it's (again) method [Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) | probably high (nowadays) | a random seed probably (still) depends on the exact JavaScript engine being used: [Math.random() is not so random: The Illusion of Randomness in JavaScript](https://vinitshahdeo.substack.com/p/mathrandom-is-not-so-random-the-illusion), 2025; there's a chapter on [Better Alternatives for Randomness](https://vinitshahdeo.substack.com/i/167041440/the-quantum-question-randomness-in-the-future)
V | [rand](https://modules.vlang.io/rand.html#readme_rand): _All the generators are initialized with time-based seeds._ | high | two manual program runs within 1 second will yield two different byte streams
Zig | my own and direct implementation of Linux system call [getrandom(2)](https://man7.org/linux/man-pages/man2/getrandom.2.html) with Zig instruction: _try std.posix.getrandom(std.mem.asBytes(&seed));_ | very high | 

<br/>

(*) the 1 second seeding resolution can easily be tested with two manual program runs within 1 second, here in the Dylan implementation:

```
$ ./_build/bin/random-streams-for-perf-stats; head -c 10 ./random_bitstring.byte

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
995b31f810$ ./_build/bin/random-streams-for-perf-stats; head -c 10 ./random_bitstring.byte

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
995b31f810$
```

Here, the first 10 characters of the random byte stream are identical, indicating that the quality of randomness of a seed is rather low.

<br/>

## Variant 13 of David Stafford's 64-bit mix function

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

    // weak hashing at the Oxford Oberon-2 Compiler: https://github.com/Spivoxity/obc-3
    //   Random.m: https://github.com/Spivoxity/obc-3/blob/1719f9fb328257b46dc7721267850bf929ebafd5/lib/Random.m#L109
    uint64_t random_seed_oxford_oberon2 = 13ULL * seconds + microseconds;

    printf("Linux system time in seconds:      %lu\n", seconds);
    printf("Linux system time in milliseconds: %lu\n", milliseconds);
    printf("residual microseconds:                       %lu\n", microseconds);
    printf("64 bit random seed using David Stafford's 64-bit mix function: %lu\n", random_seed_stafford13);
    printf("64 bit random seed using function: 13 * sec + microsec:        %lu\n", random_seed_oxford_oberon2);

    return 0;
}
```

Two (compiled) program runs within 1 second may then look like this:

```
$ ./gettimeofday_call_SplitMix64_hashing
Linux system time in seconds:      1791473786
Linux system time in milliseconds: 1791473786018
residual microseconds:                       18065
64 bit random seed using David Stafford's 64-bit mix function: 6567806001172984750
64 bit random seed using function: 13 * sec + microsec:        23289177283
$ ./gettimeofday_call_SplitMix64_hashing
Linux system time in seconds:      1791473786
Linux system time in milliseconds: 1791473786344
residual microseconds:                       344726
64 bit random seed using David Stafford's 64-bit mix function: 11554284189388881105
64 bit random seed using function: 13 * sec + microsec:        23289503944
$ 
```

By the way: the output formatting of this program is not perfect as it can be seen at the first residual microseconds value which should be shown as 018065, and not just 18065.

However, it's obvious that David Stafford's 64-bit mix function makes a much better good job at generating a system time based random seed than just calculating 13 * sec + microsec.

<br/>

##_end
