2026-07-10: work in progress tbd

<br/>

# Modula-2

https://www.nongnu.org/gm2/about.html

ISO Libraries: https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gm2/M2-ISO-Libraries.html

ISO standard for Modula-2: https://www.iso.org/standard/18583.html

<br/>

Niklaus Wirth: Modula-2 ... An Improvement On Pascal And Modula

<br/>

> One of Modula-2’s most successful features is the provision for explicit interfaces between modules.

from: https://www.mcours.net/cours/pdf/hasclic3/hasssclic786.pdf

<br/>

Explicit object-orientation was only later introduced in [Modula-3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3#modula-3), though the goal of standard [ISO/IEC 10514-3:1998](https://www.iso.org/standard/20793.html) "..is to provide simple extensions to allow object oriented programming facilities to be added to the Base Language..". 

<br/>

## How to write fast Modula-2 programs

After implementing the "speed part" of the microbenchmark program in [Modula-3](tbd), without any user defined functions or other hacks, and with an execution time of about 78 milliseconds,
I got curious about how former Modula-2 would compare.

In short: very bad without user defined functions or with using dynamic strings for string building, at least when using the GNU gm2 compiler and ISO standard features.

At worst, the compiled program would not terminate within a reasonable amount of time.

However, I didn't test other combinations, for example using the PIM2, PIM3, PIM4 dialects, which are also supported by the gm2 compiler. (PIM2 = "Programming in Modula-2", 2nd Edition, Springer Verlag, 1982: https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gm2/What-is-GNU-Modula-2.html).
I also didn't test other open source Modula-2 compilers, which may still be around (tbd).

<br/>

This bad picture completely flipped when I refactored the program to use a mixture of my
[C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) and
[Ada](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb) implementations.
Then the gcc compiler could obviously apply a lot of optimizations and thus generate a very fast executable with an execution time of about only 6 milliseconds!

So, I also implemented the [complete microbenchmark program](tbd) in Modula-2 to put also this language on my "official" list of programming languages: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

<br/>

Consequently, it should not surprise that once Modula-2 was seen as a replacement language of C for system level development:

> Modula-2 is quite possibly the most suitable C-replacement for system level development. In any event, Modula-2 is structurally closer to C than any other language of the Pascal family.

from: https://objective.modula-2.net/faq.shtml

<br/>

##_end
