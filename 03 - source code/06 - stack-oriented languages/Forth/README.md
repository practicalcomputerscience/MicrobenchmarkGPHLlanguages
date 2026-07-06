# Forth

https://forth-standard.org/

<br/>

As of 2026-07-06: after the ["speed part"](./random_streams_for_perf_stats.fth) of the microbenchmark program in ccforth, I should also implement the full microbenchmark program (tbd). The ccforth solution is already listed in _programming_languages_exe_speeds.csv_!

<br/>

System level programming in postfix notation, also called Reverse Polish Notation, where operators follow their operands.

> Stack machines offer processor complexity that is much lower than that of CISC (Complex Instruction Set Computers) machines,
> and overall system complexity that is lower than that of either RISC (Reduced Instruction Set Computers) or CISC machines. They do this without requiring complicated compilers or cache control hardware for good performance.

from: "Stack Computers: the new wave", Philip Koopman, 1989: https://users.ece.cmu.edu/~koopman/stack_computers/sec1_1.html

<br/>

> Forth has been in use from 1972 on..

from: https://www.forth.com/starting-forth/0-starting-forth/

<br/>

[A Glossary of Forth Primitives ](https://users.ece.cmu.edu/~koopman/stack_computers/appb.html)

<br/>

## Installation tips for Gforth

https://gforth.org/

aus = address units (as seen in the Gforth documentation)

After some experimentation, I noticed that I need a working Gforth implementation to build the latest version of Gforth! So, I started like this:

```
$ sudo apt  install gforth
...
$ gforth --version
gforth 0.7.3
$
```

Do _$ make clean >/dev/null 2>&1 || true_, if you have messed up a build before.

That's good enough to build latest version 0.7.9 from sources in tarball file _gforth.tar.xz_ from here: https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/current/

After unzipping that file, I followed instructions as given in _./gforth/gforth-0.7.9_20260610/INSTALL.md_:

```
$ cd ./gforth/gforth-0.7.9_20260610
$ BUILD_FROM=tarball
$ source ./install-deps.sh
...
The following packages have unmet dependencies:
...
$ 
```

I was still missing some packages, which I installed like this, and further ignored above warnings (for those packages in their newest form anyway):

```
$ sudo apt install libtool libtool-bin swig
...
$
```

Then I noticed that I had to do more installations:

```
$ sudo ./install-swig.sh
...
Installation complete
$
```

Only then I could run the _configure_ command correctly, and start building with _make_:

```
$ ./configure 
...

*** Config summary: everything fine ***
$ make
...
*** Check successful ***
*** no performance problems ***
    in gforth-fast
*** no performance problems ***
    in gforth
*** no performance problems ***
    in libgforth-fast
*** no performance problems ***
    in libgforth
$ sudo make install
...
============= INSTALL SUCCEEDED =============
Bash users: type 'hash -r' to empty the cache
$ hash -r
$ gforth --version
gforth 0.7.9_20260610 amd64
$
```

Voilà! Gforth in its latest version! (which already looks improved at the REPL, see below, compared to version 0.7.3)

<br/>

## From Forth to Factor and back

I highly recommend to first have a look into the official [Forth Tutorial](https://net2o.de/gforth/Tutorial.html) before doing anything more meaningful than "Hello, world!" in Forth:

```
$ gforth
Gforth 0.7.9_20260610
Authors: Anton Ertl, Bernd Paysan, Jens Wilke et al., for more type `authors'
Copyright © 2025 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `help' for basic help
\  ok
\ user input starts now:  ok
cr ." Hello, world from Gforth!" cr
Hello, world from Gforth!
 ok
bye
$
```

<br/>

However, when I continued with the Tutorial, I got doubts: should I really go on with this rather low-level programming language?

Then I discovered much younger stack-oriented language [Factor](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Factor#factor), and decided to first continue with that language.

However, when I finished the whole microbenchmark program in [Factor](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Forth/random_streams_for_perf_stats.fs), I had a second look at Gforth.

At first, a complete transpilation of the ["speed part" of the microbenchmark program in Factor](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Factor/random_streams_for_perf_stats.factor) with the help of "Big AI" (again) didn't work at all. These two stack-oriented languages are just too different after all.

Consequently and piece by piece, I developed from the ground up a little Linear Congruential Generator (LCG) for only generating 20 random integer numbers in Gforth.

From that skeleton of a program on and with lots of help from "Big AI", I slowly got the final and very imperative [Forth solution](./random_streams_for_perf_stats.fs), which runs significantly faster with an execution time of about 26 milliseconds (as a Gforth image file, not standalone executable) versus the quite functional Factor program with about 59 milliseconds as a (dynamically linked) standalone executable.

<br/>

## Installation tips for ccforth

https://github.com/ncw/ccforth (*)

After I failed to make a truly standalone executable with Gforth, I have been searching for a possibility to make a standalone executable based on Forth source code, specifically with the background that Forth (originally) is a system programming language.

Then I discovered ccforth, which allows to emit transpiled C source code from ccforth complient source code. That C code can then be compiled and linked into a standalone executable:

> ccforth is a mostly Gforth compatible Forth 2012 compliant Forth-to-C compiler written in Go ... It interprets compile-time Forth (immediate words, meta-programming) and emits flattened C11 code that is compiled with gcc or clang to produce standalone executables.

However, just compiling the original [Gforth program](./random_streams_for_perf_stats.fs) with the ccforth compiler does, of course, not work!

For installation of ccforth I followed these official [Installation](https://github.com/ncw/ccforth#installation) instructions (*):

```
$ go install github.com/ncw/ccforth/cmd/ccforth@latest
go: downloading github.com/ncw/ccforth v0.3.2
go: downloading github.com/chzyer/readline v1.5.1
$ 
```

Then expand your _~/.bashrc_ configuration file with line: _export PATH="$HOME/go/bin:$PATH"_, and activate it with: _$ source ~/.bashrc_. Depending on you Go installation, it could also be for example: _export PATH="$HOME/gopath/bin:$PATH"_

```
$ ccforth -version
ccforth dev
$
```

However, even when refactoring [random_streams_for_perf_stats.fs](./random_streams_for_perf_stats.fs) to be more ccforth-compliant, but still working with Gforth, the produced executable crashed:

```
# ccforth needs more than default memory for transpilation:
$ ccforth -memsize 8000000 -c ./random_streams_for_perf_stats.fs > random_streams_for_perf_stats_ccforth.c
# also in C code, allocate more than default memory for compilation:
$ sed -i 's/^#define MEM_SIZE .*/#define MEM_SIZE 8388608/' ./random_streams_for_perf_stats_ccforth.c
$ gcc random_streams_for_perf_stats_ccforth.c -o random_streams_for_perf_stats_ccforth  # cautiously compiling without any optimizations
$ ./random_streams_for_perf_stats_ccforth

generating a random bit stream...
Segmentation fault (core dumped)
$
```

That was the end of this cross-compilation development road, and I again developed [random_streams_for_perf_stats.fth](./random_streams_for_perf_stats.fth) specifically for ccforth from the ground up:

```
$ c$ ccforth -c ./random_streams_for_perf_stats.fth > random_streams_for_perf_stats_ccforth.c
# also in C code, allocate more than default memory for compilation:
$ sed -i 's/^#define MEM_SIZE .*/#define MEM_SIZE 8388608/' ./random_streams_for_perf_stats_ccforth.c
# now safely compiling with optimizations on:
$ gcc -O3 random_streams_for_perf_stats_ccforth.c -o random_streams_for_perf_stats_ccforth
time ./random_streams_for_perf_stats_ccforth

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.005s
...
$ 
```

5 milliseconds is sharp! :flushed:

Above building procedure can be shortened with a [makefile](./makefile):

```
$ make
Generating C source from random_streams_for_perf_stats.fth with MEM_SIZE=8388608  ...
ccforth -c random_streams_for_perf_stats.fth \
| sed 's/^#define MEM_SIZE .*/#define MEM_SIZE 8388608  /' > random_streams_for_perf_stats_ccforth.c
Compiling random_streams_for_perf_stats_ccforth...
gcc -O3   -o random_streams_for_perf_stats_ccforth random_streams_for_perf_stats_ccforth.c
$ make run  # for building and additionally running the program
...
$
```

tbd

<br/>

##_end
