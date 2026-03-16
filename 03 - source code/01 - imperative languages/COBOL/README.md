2026-03-09: work in progress

<br/>

# COBOL

COBOL for: "Common Business Oriented Language": 

- https://gnucobol.sourceforge.io/

- https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/

- https://sourceforge.net/projects/gnucobol/files/gnucobol/nightly_snapshots/

- https://www.iso.org/standard/74527.html

<br/>

At first, I had no intention to implement the microbenchmark program in a language which is supposed to be on the way out. However, with the rise of AI coding:

2026-02-24: [IBM Sinks Most Since 2000 as Anthropic Touts Cobol Tool](https://finance.yahoo.com/news/ibm-sinks-most-since-2000-210436663.html)

..allegedly still widely used COBOL made it into the news again recently, and so I just got curious how this archaic, general purpose programming language (*1960: https://en.wikipedia.org/wiki/COBOL#History_and_specification) would do in this microbenchmark program.

<br/>

## Installation tips

Install and test gnuCOBOL in Ubuntu (24 LTS) like this:

```
$ sudo apt install gnucobol
...
$ cobc -V
cobc (GnuCOBOL) 3.1.2.0
Copyright (C) 2020 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Edward Hart
Built     Apr 14 2024 07:59:15
Packaged  Dec 23 2020 12:04:58 UTC
C version "13.2.0"
$
```

Version 3.1.2.0 is not the latest (stable) version, but the easiest to install successfully in Ubuntu 24 LTS.

<br/>

I also tried to build version 3.3-dev from sources (https://sourceforge.net/projects/gnucobol/files/gnucobol/nightly_snapshots/), only to fail at first.

But then "Big AI" helped me out with the installation procedure, indicating that some potentially missing tools may have to be installed first:

```
$ sudo apt install -y gcc make libncurses-dev libdb-dev bison flex
...
$ ./configure  # located in subdirectory ./gnucobol-3.3-dev for example
...
$ make
...
$ make check
...
1338 tests behaved as expected.
18 tests were skipped.
...
$ sudo make install
...
$ sudo ldconfig  # make sure the system can find the new GnuCOBOL libraries
...
$ cobc --version
cobc (GnuCOBOL) 3.3-dev.0
Copyright (C) 2024 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Edward Hart
$ 
```

However, I couldn't detect any improvement in terms of execution speed from an executable being built with _cobc (GnuCOBOL) 3.3-dev.0_.

Although, one advantage of a more modern version of GnuCOBOL would be to have more built-in functions available, including _HEX-OF_, see below.

<br/>

## How to find available built-in functions in GnuCOBOL?

Just like this:

```
$ HEX-OF

Intrinsic Function              Implemented     Parameters
ABS                             Yes             1
ACOS                            Yes             1
ANNUITY                         Yes             2
ASIN                            Yes             1
ATAN                            Yes             1
BOOLEAN-OF-INTEGER              No              2
BYTE-LENGTH                     Yes             1 - 2
...
TRIM                            Yes             1 - 2
UPPER-CASE                      Yes             1
VARIANCE                        Yes             Unlimited
WHEN-COMPILED                   Yes             0
YEAR-TO-YYYY                    Yes             1 - 3
$ 
```

<br/>

Here's the background for this issue: in my COBOL implementation of the microbenchmark program, I haven't implemented any **user defined functions** (UDF's), like in other languages which miss certain functionalities (at least in the older COBOL version I have used, see above). Instead, I implemented **used defined procedures**, namely _CONVERT-TO-BINARY_, _CONVERT-TO-HEX_ and _BINARY-STR-TO-UNSIGNED-INT_, which make use of program-wide variables. Of course, this concept is error prone.

However, I thought that even in COBOL, the microbenchmark program is still not too complex to justify the effort to write more COBOL code, something which is needed for UDF's compared to user defined procedures. See from here for example: [COBOL user-defined function definition structure](https://www.ibm.com/docs/en/cobol-zos/6.5.0?topic=structure-cobol-user-defined-function-definition)

<br/>

## Trials for improvement of execution speed

I played with compiler optimization switches, like _-O3_ for example, only to notice that at least with this microbenchmark program there's no advantage in execution speed, but in a significantly smaller size of the executable (a phenomenon which is subdued in GnuCOBOL version 3.3-dev for example). So, I'm not officially using them.

Instead, I also tried the alternative (and experimental) **gcobol** compiler:

- download the pre-compiled ~.deb package from here: https://gitlab.cobolworx.com/COBOLworx/gcc-cobol/-/packages/6

..and then install this Debian package (in Ubuntu) like this:

```
$ sudo dpkg -i gcobol-16_16.0.1.20260311-10a0db-ubu20_x86_64.deb
$ gcobol --version
gcobol (GCOBOL-16.0.1.20260311-10a0db-ubu20) 16.0.1 20260311 (experimental)
...
$ gcobol -W -O3 random_streams_for_perf_stats.cob -o random_streams_for_perf_stats_gcobol
$ time ./random_streams_for_perf_stats_gcobol
...
real	0m0.734s
...
$
```

So, at least with this microbenchmark program there's no advantage in execution speed when compiling with this (new and free to use) COBOL compiler compared to GnuCOBOL, which makes an executable that takes about 430 milliseconds to run (as of 2026-03-16).

I didn't try [IBM COBOL](https://www.ibm.com/products/cobol-compiler-linux-x86), which is said to compile to fast machine code.

The last two compilers skip the step of first transpiling COBOL source code into C source code, like GnuCOBOL does, and - with an intermediate step at gcobol at least - compile more or less directly into machine code.

<br/>

##_end
