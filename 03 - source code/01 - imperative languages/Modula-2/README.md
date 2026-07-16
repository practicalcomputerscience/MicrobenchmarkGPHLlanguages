# Modula-2

Here using GNU Modula-2: https://www.nongnu.org/gm2/about.html

ISO Libraries: https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gm2/M2-ISO-Libraries.html

ISO standard for Modula-2: https://www.iso.org/standard/18583.html

---

Table of contents:

- [Idea of Modula-2](#idea-of-modula-2)
- [Installation tips](#installation-tips)
- [How to write fast Modula-2 programs](#how-to-write-fast-modula-2-programs)
- [Issues with the GNU Modula-2 GCC frontend](#issues-with-the-gnu-modula-2-gcc-frontend)
- [Mocka Modula-2 compiler](#mocka-modula-2-compiler)

<br/>

---

## Idea of Modula-2

Niklaus Wirth: Modula-2 ... An Improvement On Pascal And Modula

<br/>

> One of Modula-2’s most successful features is the provision for explicit interfaces between modules.

from: https://www.mcours.net/cours/pdf/hasclic3/hasssclic786.pdf

<br/>

Explicit object-orientation was only later introduced in [Modula-3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3#modula-3), though the goal of later standard [ISO/IEC 10514-3:1998](https://www.iso.org/standard/20793.html) "..is to provide simple extensions to allow object oriented programming facilities to be added to the Base Language..". 

<br/>

## Installation tips

In Ubuntu 24 LTS, I just used normal package installation to install the GNU Modula-2 compiler:

```
$ sudo apt install gm2
$ gm2 --version  # a simple version test
gm2 (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$
```

<br/>

## How to write fast Modula-2 programs

After implementing the "speed part" of the microbenchmark program in [Modula-3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3/random_streams_for_perf_stats_Main.m3),
without any user defined functions or other hacks, and with an execution time of about 78 milliseconds, I got curious about how former Modula-2 would compare.

In short: very bad without user defined functions or with using dynamic strings for string building, at least when using the GNU gm2 compiler with a GCC 13 frontend and activated ISO standard features.

At worst, the compiled program would not terminate within a reasonable amount of time.

However, I didn't test other combinations, for example using the PIM2, PIM3, PIM4 dialects, which are also supported by the gm2 compiler. For example, PIM2 refers to: ’Programming in Modula-2’, 2nd Edition, Springer Verlag, 1982, 1983 by Niklaus Wirth: https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gm2/What-is-GNU-Modula-2.html

<br/>

This bad picture completely flipped when I refactored the program to use a mixture of my
[C](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) and
[Ada](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb) implementations.
Then the gcc compiler could obviously apply a lot of optimizations and thus generate a very fast executable with an execution time of about only 6 milliseconds!

So, I also implemented the [complete microbenchmark program](./random_bitstring_and_flexible_password_generator.mod) in Modula-2.

<br/>

Consequently, it should not surprise that right from start Modula-2 was designed for systems programming:

> Modula-2 grew out of a practical need for a general, efficiently implementable systems programming language for minicomputers.

from PDF: [MODULA-2, Niklaus Wirth, ETH Zürich, March 1980](https://www.modula2.org/downloads/wirth-modula2/Wirth_Modula2.pdf)

<br/>

#### Regular Expressions in GNU Modula-2

Neither the ISO/IEC 10514 standard, nor the standard library, nor the ISO features of the gm2 compiler support regular expressions, or require them.

At least one third party library still exists: https://www.modula2.org/projects/RegExpr.php

Similar to [Regular expressions in Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#regular-expressions-in-mojo), also here I've decided against using this library.

<br/>

#### Issues with the GNU Modula-2 GCC frontend

See at: https://github.com/wgottwalt/gnu_modula2_playground/tree/main

<br/>

#### Mocka Modula-2 compiler

I have added an extra page on my successful experiments with the historic Mocka Modula-2 compiler, which is still usable in a 32-bit Linux system (and may work with extra expert work in a modern 64-bit Linux system):

[Mocka Modula-2 compiler for 32-bit Linux and X Window graphics](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2/Mocka%20Modula-2%20compiler#mocka-modula-2-compiler-for-32-bit-linux-and-x-window-graphics)

Originally, this compiler was basically an implementation of the PIM3 dialect of the Modula-2 language as defined in: ’Programming in Modula-2’, 3rd Corrected Edition, Springer Verlag, 1985 (PIM3) (https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gm2/What-is-GNU-Modula-2.html), "with a few minor extensions" (PDF): [GMD MODULA SYSTEM MOCKA User Manual](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2/Mocka%20Modula-2%20compiler/UserMan.pdf)

<br/>

##_end
