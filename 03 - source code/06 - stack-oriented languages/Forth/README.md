# Forth

TL;DR: only [GNU's Gforth](https://gforth.org/) is ready for full showtime as an open source Forth implementation for general purpose computer programming.

Gforth mirror on GitHub: https://github.com/forthy42/gforth

But you can't make a standalone executable for Linux with Gforth, because it's (nowadays) always based on its own virtual machine.

The Gforth ecosystem has also become confusing over the decades with numerous commands like: _gforth_, _gforthmi_, _gforth-fast_, _gforth-itc_, _gforth-ditc_, ...

In the very early days there was still the command _gforth-native_, which was soon given up (+), because it caused to much hassle to maintain it, and C compilers got better to make faster virtual machines.

<br/>

Otherwise, Forth programming is **System level programming** in postfix notation, also called Reverse Polish Notation, where operators follow their operands:

> Stack machines offer processor complexity that is much lower than that of CISC (Complex Instruction Set Computers) machines,
> and overall system complexity that is lower than that of either RISC (Reduced Instruction Set Computers) or CISC machines. They do this without requiring complicated compilers or cache control hardware for good performance.

from: "Stack Computers: the new wave", Philip Koopman, 1989: https://users.ece.cmu.edu/~koopman/stack_computers/sec1_1.html

<br/>

> Forth has been in use from 1972 on..

from: https://www.forth.com/starting-forth/0-starting-forth/

<br/>

---

Table of contents:

- [Installation tips for Gforth](#installation-tips-for-gforth)
- [Gforth is also the name of its virtual machine](#gforth-is-also-the-name-of-its-virtual-machine)
- [From Forth to Factor and back](#from-forth-to-factor-and-back)
- [Installation tips for ccforth](#installation-tips-for-ccforth)
- [Microbenchmark program in ccforth (only "speed part")](#microbenchmark-program-in-ccforth-only-speed-part)
- [Full microbenchmark program in Gforth](#full-microbenchmark-program-in-gforth)
- [Microbenchmark program in SwiftForth (only "speed part")](#microbenchmark-program-in-swiftforth-only-speed-part)

<br/>

---

## Installation tips for Gforth

aus = address units (as seen in the Gforth documentation: https://gforth.org/)

After some experimentation, I noticed that I need a working Gforth implementation to build the latest version of Gforth! So, I started like this:

```
$ sudo apt  install gforth
...
$ gforth --version
gforth 0.7.3
$
```

Do _$ make clean >/dev/null 2>&1 || true_, if you have messed up a build before.

This version is good enough to build latest version 0.7.9 from sources in tarball file _gforth.tar.xz_ from here: https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/current/

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

I was still missing some packages, which I installed like this, and further ignored above warnings ("The following packages have unmet dependencies:") for those packages installed in their newest form on my system anyway:

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

## Gforth is also the name of its virtual machine

Gforth is not only the name of an implementation of the Forth programming language, but also the name of its virtual machine, or "engine". It's available in four flavors:

- _gforth_ for development mode and which uses "hybrid direct/indirect threaded code": https://gforth.org/manual/Direct-or-Indirect-Threaded_003f.html (**)
- _gforth-fast_, which is _gforth_ for production mode, and which "uses dynamic superinstructions (native code that still uses the threaded code quite a bit)" from: "Inlining in Gforth: Early Experiences" by David Gregg, Trinity College Dublin, M. Anton Ertl, TU Wien, 1998 (+)
- _gforth-itc_, which is still available for backward compatibility, because "traditionally Forth has been implemented as indirect threaded code" (**)
- _gforth-ditc_, which uses a "double indirect threaded system": https://manpages.debian.org/buster/gforth/gforth-itc.1.en.html: it looks like that command _gforthmi_ for creating a Gforth image file is using this command.

DTC = Direct Threaded Code

ITC = Indirect Threaded Code

<br/>

Though, the fastest way to run _random_streams_for_perf_stats.fs_, according to my experiments, is first to create a Gforth image file, which is then being executed:

```
$ gforthmi random_streams_for_perf_stats random_streams_for_perf_stats.fs
$ time ./random_streams_for_perf_stats  # real	0m0.025s
```

The other options are slower:

```
$ time gforth      ./random_streams_for_perf_stats2.fs  # real	0m0.042s
$ time gforth-fast ./random_streams_for_perf_stats2.fs  # real	0m0.030s
$ time gforth-itc  ./random_streams_for_perf_stats2.fs  # real	0m0.051s
$ time gforth-ditc ./random_streams_for_perf_stats2.fs  # real	0m0.044s
```

These execution times are only indicative and can vary widely when not based on a Gforth image file.

_random_streams_for_perf_stats2.fs_ just needs a different program tail with:

```
main
bye ;
```

<br/>

Another issue with the Gforth documentation is this: how valid are academic papers written in the 90ies still for a modern version of _gforth_? They are still listed as part of the official documentation: https://gforth.org/manual/Engine.html#Engine

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

At first, a complete transpilation of the ["speed part" of the microbenchmark program in Factor](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Factor/random_streams_for_perf_stats.factor) with the help of "Big AI" (again) didn't work at all. These two stack-oriented languages are just too different.

Consequently, for a Gforth solution I developed from the ground up and with the help of "Big AI" a simple Linear Congruential Generator (LCG) that started with only generating 20 random integer numbers.

From that skeleton of a program on, I slowly got the final and very imperative [Forth solution](./random_streams_for_perf_stats.fs), which runs faster with an execution time of about 22 milliseconds (as a Gforth image file, not standalone executable) than the quite functional Factor program with about 59 milliseconds as a (dynamically linked) standalone executable.

<br/>

## Installation tips for ccforth

https://github.com/ncw/ccforth (*)

After I failed to make a truly standalone executable with Gforth, I have been searching for a possibility to make a standalone executable based on Forth source code, specifically with the background that Forth (originally) is a system programming language.

Then I discovered ccforth, which allows to emit transpiled C source code from ccforth complient source code. That C code can then be compiled and linked into a standalone executable:

> ccforth is a mostly Gforth compatible Forth 2012 compliant Forth-to-C compiler written in Go ... It interprets compile-time Forth (immediate words, meta-programming) and emits flattened C11 code that is compiled with gcc or clang to produce standalone executables.

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

<br/>

However, just compiling the original [Gforth program](./random_streams_for_perf_stats.fs) with the ccforth compiler does, of course, not work! Even when cautiously refactoring the Gforth-compliant source code to be more ccforth-compliant, while still working with Gforth, the ccforth-produced executable crashed:

```
# ccforth needs more than default memory for transpilation of this program:
$ ccforth -memsize 8000000 -c ./random_streams_for_perf_stats.fs > random_streams_for_perf_stats_ccforth.c
# also in C code, allocate more than default memory for compilation:
$ sed -i 's/^#define MEM_SIZE .*/#define MEM_SIZE 8388608/' ./random_streams_for_perf_stats_ccforth.c
# cautiously compiling without any optimizations:
$ gcc random_streams_for_perf_stats_ccforth.c -o random_streams_for_perf_stats_ccforth
$ ./random_streams_for_perf_stats_ccforth

generating a random bit stream...
Segmentation fault (core dumped)
$
```

<br/>

## Microbenchmark program in ccforth (only "speed part")

That was the end of cross-compiling from Gforth code to ccforth code, and thus I developed [random_streams_for_perf_stats.fth](./random_streams_for_perf_stats.fth) specifically for ccforth from the ground up again:

```
$ ccforth -c ./random_streams_for_perf_stats.fth > random_streams_for_perf_stats_ccforth.c
# also in C code, allocate more than default memory for compilation:
$ sed -i 's/^#define MEM_SIZE .*/#define MEM_SIZE 8388608/' ./random_streams_for_perf_stats_ccforth.c
# now safely compiling with optimizations on:
$ gcc -O3 random_streams_for_perf_stats_ccforth.c -o random_streams_for_perf_stats_ccforth
$ time ./random_streams_for_perf_stats_ccforth

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.005s
...
$ 
```

5 milliseconds is sharp!

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

<br/>

The difference in size of the two "executables" is stark: 

- Gforth: 8,552,994 bytes
- ccforth: 21,048 bytes

The ccforth based program has good portability:

```
$ ldd random_streams_for_perf_stats_ccforth 
	linux-vdso.so.1 (0x00007ffdbe7ab000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x000073510ae00000)
	/lib64/ld-linux-x86-64.so.2 (0x000073510bbd5000)
$
```

<br/>

The ccforth executable also passes the valgrind test with flying colors (well, it compiles first to C code: [Summaries table](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/15%20-%20memory%20leak%20detection%20with%20Valgrind#summaries-table)): 

```
$ valgrind ./random_streams_for_perf_stats_ccforth
...
==5329== HEAP SUMMARY:
==5329==     in use at exit: 0 bytes in 0 blocks
==5329==   total heap usage: 5 allocs, 5 frees, 10,160 bytes allocated
==5329== 
==5329== All heap blocks were freed -- no leaks are possible
==5329== 
==5329== For lists of detected and suppressed errors, rerun with: -s
==5329== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
$
```

<br/>

However, reading user input from the keyboard into a string on the console isn't working yet with ccforth in its current version despite elaborate experimentation with "Big AI". Thus, that was the end of my experiments with ccforth.

<br/>

## Full microbenchmark program in Gforth

Reading user input at the terminal is working in Gforth, as this factorial program shows at the word _read-int_. Run it like this: _$ gforth factorial.fs_

```
: factorial ( n -- n! )
  recursive
  dup 0= if
    drop 1
  else
    dup 1- factorial *
  then ;

: read-int ( -- n ok )
  PAD 64 ACCEPT          ( u )  \ ACCEPT: Get a string of up to n1 characters from the user input device and store it at c-addr.
                                \ n2 is the length of the received string. The user indicates the end by pressing RET.
                                \ Gforth supports all the editing functions available on the Forth command line
                                \ (including history and word completion)
                                \ PAD: c-addr is the address of a transient region that can be used as temporary data storage.
                                \      At least 84 characters of space is available.

  PAD SWAP s>number?     ( d flag )
  IF
    drop                 ( n )
    DPL @ -1 = IF
      true               ( n true )
    ELSE
      drop false         ( false )
    THEN
  ELSE
    2drop false          ( false )
  THEN ;

: main
  begin
    cr ." Enter an integer n >= 1: " flush
    read-int
    if
      dup 1 < over 20 > or if \ Checks if n < 1 OR n > 20
        drop cr ." Error: Please enter an integer between 1 and 20." cr
      else
        dup factorial
        cr ." factorial(" swap . ." ) = " . cr
        bye
      then
    else
      cr ." Error: Please enter an integer between 1 and 20." cr
    then
  again ;

main
```

Consequently, the full microbenchmark program is only implemented in Gforth ([random_bitstring_and_flexible_password_generator.fs](./random_bitstring_and_flexible_password_generator.fs)), and represents my official implementation in Forth.

<br/>

However, even word _read-int_, or here in its expanded form _read_int_or_y_, needed an upgrade (from Google AI) in program _random_bitstring_and_flexible_password_generator.fs_ to make it work safely:

```
CREATE input-buf 64 ALLOT  \ Protects data from terminal interpreter overwrite
\
: read_int_or_y ( -- n ok )
  input-buf 64 ACCEPT

  \ Guard: If the user typed absolutely nothing, exit as false
  dup 0= IF drop 0 false EXIT THEN

  \ Exact string match: check if the input is strictly a single lowercase 'y'
  input-buf over s" y" compare 0= IF
    drop                 \ Discard the input length 'u'
    n_char_default true  \ Push 12 and true to the stack
    EXIT                 \ Bypass number parsing entirely
  THEN

  \ integer number conversion logic:
  input-buf swap s>number?  ( d flag )
  IF
    drop
    DPL @ -1 = IF      \ Valid single-cell integer check
      true             ( n true )
    ELSE
      drop false
    THEN
  ELSE
    2drop false
  THEN ;
```  

<br/>

Otherwise, the only key to success for Google AI was to introduce **meticulous, manual memory management** throughout the full microbenchmark program, something which was not needed to this extent in program [random_streams_for_perf_stats.fs](./random_streams_for_perf_stats.fs). I also updated that source code with Google AI's changes for the full microbenchmark program to make it also as bullet-proof as it gets:

```
...
\ Create POINTER variables to hold our dynamic heap memory addresses
0 VALUE x
0 VALUE bits_x
0 VALUE bits_hex
0 VALUE bits_x_str_total
0 VALUE bits_hex_str_total

: allocate-large-buffers ( -- )
    END CELLS ALLOT               \ We keep 'x' in dictionary if small, or heap allocate:
    END CELLS            ALLOCATE THROW TO x
    END STR_LENGTH_BIN * ALLOCATE THROW TO bits_x
    END STR_LENGTH_HEX * ALLOCATE THROW TO bits_hex
    M1                   ALLOCATE THROW TO bits_x_str_total
    K250                 ALLOCATE THROW TO bits_hex_str_total ;
...
```

<br/>

### Microbenchmark program in SwiftForth (only "speed part")

With [SwiftForth](https://www.forth.com/swiftforth/), at least one commercial compiler still exists for Forth.

I'm using a pre-compiled evaluation copy (in version _SwiftForth x64-Linux 4.1.8 05-Jul-2026_), which let's you run SwiftForth-compliant source code, but not building standalone executables.

Install SwiftForth from the unzipped archive file like this:

```
$ sudo ./SwiftForth-linux-eval/SwiftForth/install.sh
```

Now, SwiftForth's compiler _sf64_ should be now located in a path for executables, and thus should allow executing an [adapted version of the "speed part"](./random_streams_for_perf_stats_swift.f) of the microbenchmark program (which still uncovered some more glitches in _random_streams_for_perf_stats.fs_ for Gforth!):

```
$ time sf64 ./random_streams_for_perf_stats_swift.f

generating a random bit stream...
Bit stream written to disk under name: random_bitstring.bin
Byte stream written to disk under name: random_bitstring.byte

real	0m0.016s
...
$
```

With only 16 milliseconds of execution time, this is fast if you thank that it probably also includes compilation time.

However, I noticed that the introduction of local variables _bits_x_str_ and _bits_x_str_ in the _main_ word had a substantial, negative effect on the program execution time. So, I defined _bits_x_str_ and _bits_x_str_, just like _seed_, as global variables in the SwiftForth solution:

```
...
VARIABLE seed
VARIABLE bits_x_str
VARIABLE bits_hex_str
...
: main ( -- )
    END 0 DO
        ...
        bits_x I STR_LENGTH_BIN * + bits_x_str !
        ...
        bits_hex I STR_LENGTH_HEX * + bits_hex_str !
        ...
    LOOP
...
```

That alone brought down execution time to around 10 milliseconds!

<br/>

Gforth, according to my tests, isn't so sensitive on local versus global at variables _bits_x_str_ and _bits_x_str_.
However, statistically a positive effect of about -4% was measurable, and so I also made _bits_x_str_ and _bits_x_str_ global:

```
...
VARIABLE bits_x_str
VARIABLE bits_hex_str
...
: main ( -- )
    ...
    END 0 DO
        ...
        bits_x I STR_LENGTH_BIN * + bits_x_str !
        ...
        bits_hex I STR_LENGTH_HEX * + bits_hex_str !
        ...
    LOOP
...
```

<br/>

_bits_x_str_ and _bits_x_str_ are also global in the [ccforth solution](./random_streams_for_perf_stats.fth):

```
...
0 VALUE bits_x_str
0 VALUE bits_hex_str
...
: main
  ...
  HERE to bits_x_str          16 allot
  HERE to bits_hex_str         4 allot
  ...
  END 0 do
    ...
    dup integer_to_bin_string        ( -- n src_addr len )
    over bits_x_str !
    ...
    dup integer_to_hex_string        ( -- n src_addr len )
    over bits_hex_str !
    ...
  loop
...
```

<br/>

##_end
