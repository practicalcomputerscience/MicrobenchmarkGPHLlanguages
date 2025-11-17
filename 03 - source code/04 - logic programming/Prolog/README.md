2025-11-13: work in progress:

- TBD: do an ECLiPSe version of _random_streams_for_perf_stats.pl_ (don't use .pl --> Perl 5)
- TBD: full ECLiPSe program? --> _random_bitstring_and_flexible_password_generator.pl_ (don't use .pl --> Perl 5)

# Prolog

Originally, this page and its Prolog (= _**PRO**grammation en **LOG**ique_) programs have been started for one reason:

> How fast is a (compiled) program in the [Mercury language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) compared to a (compiled) version in its precursor language Prolog?

<br/>

#### The TL;DR execution speed diagram

![plot](./mean_stddev_err_whiskers%20--%20only%20Prolog%2C%20Germany%20map.png)

---

Table of contents:

- [The map coloring problem of Germany with 16 states and 4 colors](#the-map-coloring-problem-of-germany-with-16-states-and-4-colors)
- [GNU Prolog](#gnu-prolog)
- [SWI Prolog](#swi-prolog)
- [Ciao Prolog](#ciao-prolog)
- [YAP Prolog](#yap-prolog)
- [ECLiPSe - Constraint Logic Programming System](#eclipse---constraint-logic-programming-system)
- [XSB Prolog](#xsb-prolog)
- [Tau Prolog - Prolog for the web](#tau-prolog---prolog-for-the-web)
- [Trealla Prolog](#trealla-prolog)
- [Scryer Prolog](#scryer-prolog)
- [ErgoAI: knowledge representation and reasoning](#ergoai-knowledge-representation-and-reasoning)
- [Other Prolog systems](#other-prolog-systems)
- [ISO standard, comments, etc.](#iso-standard-comments-etc)
- [Speed in the Land of Prolog's](#speed-in-the-land-of-prologs)
- [And Mercury?](#and-mercury)
- [MiniZinc - constraint modelling language](#minizinc---constraint-modelling-language)

<br/>

---

## The map coloring problem of Germany with 16 states and 4 colors

Here I use a different microbenchmark program, albeit one, which I think is much more typical for problems of logic programming than my accidental microbenchmark program. See also the [Four color theorem](https://en.wikipedia.org/wiki/Four_color_theorem).

In 2017, I successfully tested this [Prolog program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2a.pl), which I implemented back then in GNU Prolog version 1.3.0. It's this source code from 2013 at chapter "The Graph Coloring Problem": [Try logic programming! A gentle introduction to Prolog](https://web.archive.org/web/20170106042155/https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/)

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/map_coloring_germany-636x310.png)

..and now I thought that this is a good program to compare the execution speeds of a Prolog program and its version in Mercury.

However, Prolog shows the same phenomenon than Scheme: what dialect to use?

> Even though the core of the Prolog programming language has been standardized by ISO since 1995, it remains difficult to write complex Prolog programs that can run unmodified on multiple
Prolog implementations. Indeed, implementations sometimes deviate from the ISO standard and the standard itself fails to cover many features that are essential in practice.

from: [Making ProB Compatible with SWI-Prolog](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/making-prob-compatible-with-swiprolog/4813E70D0F335F079CE013F2CD84D003), 2022

Here's a list of [Prolog implementations](https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations).

<br/>

## GNU Prolog

With GNU Prolog I immediately ran into a problem: 

- I was not able to compile and install a later (v.1.5.0) and the latest version (v.1.6.0) of GNU Prolog:

- http://gprolog.org/#download
- https://github.com/didoudiaz/gprolog

I tried with three different Linux versions (openSUSE 16 Leap, Oracle Linux 10, Ubuntu 24 LTS) in vain. Another curious phenomenon was that all distributions showed (slightly) different error messages.

Finally, I just installed an older GNU Prolog version with the [Ubuntu package manager](https://installati.one/install-gprolog-ubuntu-22-04/), something which worked:

```
$ sudo apt-get update
$ sudo apt-get -y install gprolog
$ gplc --version  # test the compiler
Prolog compiler (GNU Prolog) 1.4.5
...
$
```

Obviously, I'm not the only one with GNU Prolog problems: https://github.com/didoudiaz/gprolog/issues/82

Building a standalone executable is very easy in GNU Prolog: _$ gplc ./graph_4coloring_Germany2a.pl_

But before running program [graph_4coloring_Germany2a](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2a.pl), make sure to have enough space on the stack. The usual 32kB is too small for this program. So, as one way, you could add in your _.bashrc_ file this global environment variable: _export GLOBALSZ=524288_, where 524288 bytes is just my proposal, which works in my system. Activate: _$ source ./.bashrc_ and check the new environment variable:

```
$ printenv GLOBALSZ
524288
$
$ ./graph_4coloring_Germany2a
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
$
```

## SWI Prolog

I took GNU Prolog source code file named _graph_4coloring_Germany2a.pl_ and could run it without changes on SWI Prolog:

```
$ swipl graph_4coloring_Germany2a.pl
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
$
```

However, this didn't make a standalone executable program. Building one is more complicated than in GNU Prolog.

This web page gives a hint how to accomplish it: https://www.swi-prolog.org/FAQ/UnixExe.md (*)

I slightly changed the original program, now named [graph_4coloring_Germany2c_SWI.pl](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2c_SWI.pl), where I changed goal _main_ into _graph_4coloring_Germany2c_SWI_, the later with no more initialization:

```
/* :- initialization(graph_4coloring_Germany2c_SWI). */
...
graph_4coloring_Germany2c_SWI :- ...
```

Then I managed with this command:

```
$ swipl -o graph_4coloring_Germany2c_SWI -g graph_4coloring_Germany2c_SWI -c graph_4coloring_Germany2c_SWI.pl --stand_alone=true
Warning: /usr/lib/swi-prolog/library/ansi_term.pl:45: 
Warning:   library(uri): No such file
% Disabled autoloading (loaded 32 files)
% Disabled autoloading (loaded 0 files)
$
$ ./graph_4coloring_Germany2c_SWI 
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
$ 
```

However, program _graph_4coloring_Germany2c_SWI_ doesn't run on a foreign Linux system without needed shared libraries (*): 

> Otherwise, you must make the shared objects available and findable to make the program usable on another computer.

Here, I look at the **target system** to see what is missing (and hope that it's not too much):

```
> ldd ./graph_4coloring_Germany2c_SWI
        linux-vdso.so.1 (0x00007fefeb954000)
        libswipl.so.9 => not found
        libc.so.6 => /lib64/libc.so.6 (0x00007fefeb73a000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fefeb956000)
>
```

..and copy file _libswipl.so.9_ from the source system to the target system, into a directory where it can be found automatically, like this for example: _sudo cp libswipl.so.9 /lib64_

This should work now:

```
> ./graph_4coloring_Germany2c_SWI
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
>
```

Both dialects, GNU and SWI, came to the same number of different solutions, that is 191808, and also show the same 1st solution and same last solution.

<br/>

By the way: first, I had an older version of SWI Prolog installed (version 9.0.4) in Ubuntu 24 LTS, which gets installed by this command: _$ sudo apt install swi-prolog-core_

This version may miss extra libraries. To install a later version with (some) extra libraries do this:

```
$ sudo apt remove swi-prolog-core  # deinstall this version
$ sudo apt install swi-prolog  # install a later version with more "batteries"
$ swipl --version  # check that version
SWI-Prolog version 9.3.34 for x86_64-linux
$
```

<br/>

## Ciao Prolog

I also tested Ciao Prolog: https://ciao-lang.org/

Again, I had to slightly change the original source code to make the program working in this [dialect](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2b_Ciao.pl).

What's unkown in Ciao, is predicate _nth0()_, which I changed to _nth()_, which I made available with clause: _:- use_module(library(lists))._ See from here at [core/lib/lists.pl](https://github.com/ciao-lang/ciao/blob/fdff410cf2b7f2b85baff97485a2db5522d785f3/core/lib/lists.pl)

Building a standalone executable is easy in Ciao Prolog:

```
$ ciao comp -S ./graph_4coloring_Germany2b_Ciao.pl  # -S switch for building a standalone executable
$ ./graph_4coloring_Germany2b_Ciao
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,green,yellow,red
$
```

While the 1st solution is the same as with GNU and SWI, the last solution is different. Ciao Prolog obviously computes things a little bit differently.

## YAP Prolog

I also tested YAP Prolog: https://www.dcc.fc.up.pt/~vsc/yap/index.html

Here's a quick installation guide for Ubuntu 24 LTS: make a zip file from related GitHub repository: https://github.com/vscosta/yap, and unzip it to (default) directory: _./yap-master_

Then change into this directory and do this:

```
$ mkdir Build
$ cd Build
$ sudo apt install libreadline-dev  # for the readline() library: this is essential to have!
$ cmake ../
$ make
$ ./yap  # this is a preliminary test
YAP 8.0.1-GITDIR-N (compiled  2025-11-08T00:11:56@...)
database loaded from ~/scripts/Prolog/YAP Prolog/yap-master/Build/startup.yss

?- halt.  # type halt. to stop the ommand-line interface
YAP execution halted.
$ sudo make install
```

**~** is the current user's home directory.

Now run a little test program, here I take David Warren's [original quicksort benchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/quicksort_benchmark.pl), which gives two warnings in modern Prolog versions, but runs OK otherwise:

```
$ time yap -L ./quicksort_benchmark.pl
~/scripts/Prolog/YAP Prolog/quicksort_benchmark.pl:20:17: warning, singleton variable I in user:main/0.

~/scripts/Prolog/YAP Prolog/quicksort_benchmark.pl:25:11: warning, singleton variable H in user:range/3.

[27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18,92,40,53,59,8]
[0,2,4,6,7,8,10,11,11,17,18,18,21,27,27,28,28,28,29,31,32,33,37,39,40,46,47,51,53,53,55,59,61,63,65,66,74,74,75,81,82,83,85,85,90,92,94,95,99,99]

real	0m0,147s
user	0m0,144s
sys	    0m0,003s
$ 
```

This microbenchmark program runs more than double as fast in YAP Prolog as in SWI Prolog: ~150 milliseconds versus ~360 milliseconds wall clock time!

The Ciao version of the program of interest, that is _graph_4coloring_Germany2b_Ciao.pl_, runs in YAP Prolog without any problems, however this time slower than with SWI Prolog:

```
$ time yap -L ./graph_4coloring_Germany2b_Ciao.pl 
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,green,yellow,red

real	0m0,946s
user	0m0,913s
sys	    0m0,033s
$
```

The last solution is the same as with Ciao Prolog.

Building a standalone executable is apparently not supported in YAP Prolog.

<br/>

It may be no coincidence that **SWI Prolog** runs relatively faster at a problem with a higher number of elements (or iterations) than a lower number of elements (or iterations). I'm apparently not the only one to make this observation:

> While testing SWI-Prolog’s performance, I noticed something interesting. When repeatedly running qsort, the speed isn’t particularly high for about 1,000 iterations, but at 10,000 iterations, it reaches 20 MLIPS. Why is this?

from: [Chasing the Speed of SWI-Prolog: Exploring Optimizations and Hidden Performance Tricks](https://medium.com/@kenichisasagawa/chasing-the-speed-of-swi-prolog-exploring-optimizations-and-hidden-performance-tricks-152f91fb30cb), Kenichi Sasagawa, 2025-02-22

SWI's speed maybe due to its [Just-in-time clause indexing](https://www.swi-prolog.org/pldoc/man?section=jitindex):

> YAP provides full JIT indexing, including indexing arguments of compound terms. YAP's indexing has been the inspiration for enhancing SWI-Prolog's indexing capabilities.

<br/>

## ECLiPSe - Constraint Logic Programming System

I also tested ECLiPSe (https://eclipseclp.org/index.html), whose "aim is to serve as a platform for integrating various Logic Programming extension" (see from User Manual at: https://eclipseclp.org/doc/).

Here's a quick installation guide for Ubuntu 24 LTS: I downloaded latest _eclipse_basic.tgz_ (as of 2025-11-08) from: https://eclipseclp.org/Distribution/Dev/7.2_4/x86_64_linux/, renamed it for some versioning to _eclipse_basic_7.2_4.tgz_ and unzipped it to directory: _./eclipse_basic_7.2_4_

Now change into that directory and do this:

```
$ sudo ./RUNME  # here you are guided interactively through the installation process;
# you can skip TCL/TK and Java installations for basic functionality; as root seems to be essential in a "standard" Ubuntu installation
...
$
```

Add to your PATH in the _.bashrc_ file for example: _export PATH="$PATH:~/scripts/Prolog/ECLiPSe/eclipse_basic_7.2_4/bin/x86_64_linux"_ and activate it: _$ source ~/.bashrc_

You may test ECLiPSe with starting its command-line interface: _$ eclipse_

The _RUNME_ script can be run repeatedly, if things went wrong.

For using ECLiPSe's Java interface, the path to your JRE (Java Runtime Environment) directory can be asked like this:

```
$ java -XshowSettings:properties -version 2>&1 > /dev/null | grep 'java.home'
    java.home = /usr/lib/jvm/java-21-openjdk-amd64
$ 
```

The TCL/TK GUI can be installed and started like this, but first better make sure that _wish_ is visible, and not shadowed:

```
$ whereis wish
wish: /usr/bin/wish /usr/share/man/man1/wish.1.gz
$ sudo apt install wish  # you may do this if wish cannot be found; wish is essential for ECLiPSe's Tcl scripts
$ sudo apt install tcl  # test with: $ tclsh
$ sudo apt install tk
$ tkeclipse
```

Hopefully, you get this window popping up now:

![plot](./tkeclipse.png)

The [Quicksort_benchmark script](./quicksort_benchmark_eclipse.pl) runs OK:

```
$ time eclipse -f ./quicksort_benchmark_eclipse.pl
File quicksort_benchmark_eclipse.pl, line 26: Singleton variable I
File quicksort_benchmark_eclipse.pl, line 31: Singleton variable H
[27, 74, 17, 33, 94, 18, 46, 83, 65, 2, 32, 53, 28, 85, 99, 47, 28, 82, 6, ...]
[0, 2, 4, 6, 7, 8, 10, 11, 11, 17, 18, 18, 21, 27, 27, 28, 28, 28, 29, ...]

real	0m0,175s
user	0m0,152s
sys	0m0,022s
$
```

..but again, not without changes, like _local_ initialization and a _halt._ without an argument.

Adaption of the coloring of Germany program, including a custom predicate for _nth()_ or _nth0()_, got the [ECLiPSe program version](./graph_4coloring_Germany2d_ECLiPSe.pl) also running:

```
$ eclipse -f ./graph_4coloring_Germany2d_ECLiPSe.pl 
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red, blue, blue, red, green, blue, green, red, green, red, blue, red, green, red, red, yellow
...
Last solution = yellow, green, green, yellow, blue, green, blue, yellow, blue, yellow, green, yellow, blue, green, yellow, red
$
```

It's 1st and last solutions are the same as Ciao's 1st and last solutions for example, if you take away the space characters from ECLiPSe's output.

Building a standalone executable is apparently not supported in ECLiPSe.

And, ECLiPSe runs pretty fast, beating SWI Prolog!

<br/>

## XSB Prolog

Installation tips: to build and install XSB Prolog correctly, even in a basic version (I didn't do more), is a little bit tricky from my point of view.

I used the "Download Snapshot" button from here: https://sourceforge.net/p/xsb/code/ci/git-origin/tree/, though also these current sources only got me version 5.0.0 from 2022. 

Then I shortened the resulting super-long file name to _xsb-code.zip_, unzipped it, and changed to directory: _$ cd ./xsb-code/XSB/build_

Then I ran: _$ ./configure_

Like hopefully given as the last order, start the compilation with: _$ ./makexsb_

Add the path to _xsb_ in your _.bashrc_ file (_export PATH="$PATH:~/scripts/Prolog/XSB_Prolog/xsb-code/XSB/bin"_) and activate it. The version can be tested with command: _$ xsb -v_

However, I didn't figure out how to smarter provide the path to a source code file than providing its _absolute_ path: _$ xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl_

Something like this usual command isn't working in my system:

```
$ xsb ./graph_4coloring_Germany2e_XSB.pl
...
++Error[XSB/Runtime/P]: [Existence (No file for module ./graph_4coloring_Germany2e_XSB.pl exists)] []
...
$
```

When you use the xsb REPL, wrap it before using it: _$ rlwrap xsb_

Building a standalone executable is apparently not supported in XSB Prolog. But when the source code file hasn't changed, _xsb_ is using a formerly generated byte code file, here named _graph_4coloring_Germany2e_XSB.xwam_, at the next execution: _$ xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl_

By the way: here, I measured the execution time with command: _$ ./exe_times_statistics_for_one_test_case_in_cwd2 "xsb ~/scripts/Prolog/XSB_Prolog/graph_4coloring_Germany2e_XSB.pl --quietload"_

<br/>

## Tau Prolog - Prolog for the web

While Tau Prolog, [a Prolog Interpreter for the Web](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/tau-prolog-a-prolog-interpreter-for-the-web/4D99CAEBCF06B072AF4D028FA7B06CC5), seems like an interesting idea, I didn't get bigger things accomplished with it (in up-to-date version 0.3.4 beta), including the benchmarking program of the map coloring problem of Germany.

I made a subpage with more details: [Tau Prolog: Prolog embedded in JavaScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/Tau%20Prolog#tau-prolog-prolog-embedded-in-javascript)

However, my results should not discourage anybody from experimenting with Prolog code inside JavaScript code, be it for web pages or for **node.js**.

<br/>

## Trealla Prolog

https://github.com/trealla-prolog/trealla/tree/main

I took file _tpl-linux-x64.zip_ from here: https://github.com/trealla-prolog/trealla/releases/tag/v2.84.17, unzipped it to default directory _./tpl-linux-x64_, expanded my _$PATH_ environment variable to that directory and tested the version like this: _$ tpl -v_

Answer was: _realla Prolog (c) Infradig 2020, v2.84.16_ So, this a very simple Prolog version to install. 

I measured the microbenchmark's execution time like this, which means that the GNU version of this program works here without a change:

```
$ ./exe_times_statistics_for_one_test_case_in_cwd2 "tpl -f ./graph_4coloring_Germany2a.pl"
...
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
=======================
SUM = 44810 [milliseconds]
mean = 2224 [milliseconds]
$
```

This result is identical to the one of GNU or SWI Prolog. Building a standalone executable is apparently not supported in Trealla Prolog.

<br/>

## Scryer Prolog

https://github.com/mthom/scryer-prolog

I downloaded the file linked at the "Download" button at _Linux (Ubuntu 22.04, 64 bits)_ at the project's home page: https://www.scryer.pl/, unzipped it to its default directory, expanded my _$PATH_ environment variable to:

```
export PATH="$PATH:~/scripts/Prolog/Scryer_Prolog/scryer-prolog_ubuntu-22.04_x86_64-unknown-linux-gnu/release"
```

..and tested the version like this: _$ scryer-prolog -v_

Answer was: _e7ac3ae_ So, also this is a very simple Prolog version to install.

The source code for Ciao Prolog is almost suitable for Scryer Prolog, but the _nth/3_ predicate is not understood, so I made another extra [source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2f_Scryer.pl):

```
$ scryer-prolog ./graph_4coloring_Germany2f_Scryer.pl 
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = red,blue,blue,red,green,blue,green,red,green,red,blue,red,green,red,red,yellow
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,yellow,yellow,red
$
```

Also this result is identical to the one of GNU or SWI Prolog. Building a standalone executable is apparently not supported in Scryer Prolog.

<br/>

## ErgoAI: knowledge representation and reasoning

TL;DR: after much tinkering, I got serious doubts that this programming language is the right one to solve problems like the map coloring problem in a handy way.

ErgoAI (https://github.com/ErgoAI), once proprietary software, is based also on [XSB Prolog](#xsb-prolog), given their common origins at Stony Brook University, State University of New York.

Actually, ErgoAI is much more than a Prolog system: 

> The Coherent ERGO reasoner is a sophisticated object-based knowledge representation and reasoning platform.
> It is based on decades of research into logic rules systems and it presents a unified language of F-logic [10], HiLog [5], Transaction Logic [3, 2], and defeasible reasoning [16].
> ERGO is based on the open-source Flora-2 inference system,1 but is much more scalable and extends Flora-2 in numerous ways that are crucial for enterprise use. 

from: "ErgoAI Reasoner User’s Manual, Version 3.0 (Philo), May 2023": https://drive.google.com/file/d/1UzI2bV7DwSOWvmZBKZY-bhyEvbZVmCt-/view?usp=share_link

And this fact represents a challenge to me, because just transpiling a Prolog program into a program with _native_ Ergo syntax, which is the aim here and not just calling a Prolog program from ErgoAI, isn't so easy from my point of view. Even after hours, I was not able to implement the necessary constraints in Ergo compliant syntax.

<br/>

I also played with its precursor language, that is **Flora-2** (https://flora.sourceforge.net/) in the false hope that that system isn't yet so sophisticated as ErgoAI and thus allowing me to find a working solution.

Flora = F-Logic tRAnslator

KR = knowledge representation

KRR = knowledge representation and reasoning

![plot](./Flora-2.png)

from the first part of the presentation from 2017: https://flora.sourceforge.net/tutorial/part1-foundations.ppt

Flora-2 "uses HiLog terms to represent objects, while Prolog uses Prolog terms terms ... the internal representation of HiLog and Prolog terms is different"; from the "ERGOLite (a.k.a. Flora-2 ): User’s Manual, Version 2.1 (Punica granatum), December 26, 2020": https://flora.sourceforge.net/docs/floraManual.pdf.

> HiLog is a logical formalism that provides higher-order and meta-programming features in a computationally tractable first-order setting.

from: https://flora.sourceforge.net/, see under "About HiLog".

<br/>

By the way: Flora-2 and ErgoAI are not Prolog system according to this test (in my understanding): [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury/README.md#mercury)

This works literally in the underlying XSB Prolog (after a related import):

```
| ?- import append/3 from lists.
...
1: ?- append([1,2],[3,4],X).
      append([1,2],[3,4],X).
X = [1,2,3,4]
yes
1: ?- 
```

..but not in Flora-2 and ErgoAI.

This is the official example for _append()_ in both Flora-2 and ErgoAI manuals, and it works literally like this in both systems:

```
flora2 ?- \list[append(["[a,b]"^^\list,[c,d],"[e,f]"^^\list])->?R]@\btp.
          \list[append(["[a,b]"^^\list,[c,d],"[e,f]"^^\list])->?R]@\btp.
?R = [a, b, c, d, e, f]
1 solution(s) in 0.001 seconds; elapsed time = 0.001
Yes
flora2 ?- 
```

This is also working:

```
flora2 ?- \list[append(["[a,b]"^^\list,[],"[c,d]"^^\list])->?R]@\btp.
          \list[append(["[a,b]"^^\list,[],"[c,d]"^^\list])->?R]@\btp.
?R = [a, b, c, d]
1 solution(s) in 0.000 seconds; elapsed time = 0.001
Yes
flora2 ?- \list[append(["[a,b]"^^\list,[],[c,d]])->?R]@\btp.  // also working
...
?R = [a, b, c, d]
...
flora2 ?- \list[append(["[a,b]"^^\list,[],[]])->?R]@\btp.  // also working
...
?R = [a, b]
...
flora2 ?-
```

..but not this (my Flora-2 and ErgoAI systems have to be stopped with [CTRL]-C):

```
flora2 ?- \list[append(["[a,b]"^^\list,"[c,d]"^^\list,[]"^^\list])->?R]@\btp.
...
flora2 ?-  // after pressing [CTRL]-C
```

_@\btp_ stands for _@\basetype_ and is a type annotation. _\basetype_ is a standard module. _\list_ is a primitive type in Flora-2 and ErgoAI, which is the usual Prolog list type.

So, this query returns a _No_:

```
flora2 ?- \list[append(["[a,b]"^^\list,[c,d],"[e,f]"^^\list])->?R].
...
No
flora2 ?-
```

<br/>

## Other Prolog systems

I also had look at these Prolog systems:

- **SICStus Prolog**, a rather well-known, commercial Prolog system: https://sicstus.sics.se/: it can only be used after an evaluation license has been obtained, something I didn't do. I think that I have already tested more than enough different Prolog systems.
- **Strawberry Prolog**, which is not natively supported in Linux: https://dobrev.com/

<br/>

### ISO standard, comments, etc.

GNU, SWI and Ciao Prolog claim to follow the [ISO standard of Prolog](https://www.iso.org/standard/21413.html), albeit I think that the potential possibility to port the source code from one dialect to the other without changes is the bigger benefit.

After having tested several Prolog dialects, again, nothing more than scratching at their surfaces, choosing the right dialect seems to be more difficult than with [Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme). While SWI Prolog nowadays seems to look like the first dialect to go for, the also ambitious, fast and very actively maintained ECLiPSe Constraint Logic Programming System, which is for sure lesser known, looks like another interesting choice; the only system I tested which actively markets a graphical user interface by the way.

On commenting in Prolog source code: generally, % should work as a remaining line comment; /* ... */ should work as a block comment, potentially comprising more than one line. The official examples at the [Ciao playground](https://ciao-lang.org/playground/) _may_ give the impression that only character % works, but this is false, since both comment symbols have already been around the 1980ies, as a view into old documents can reveal: [A Prolog Benchmark Suite for Aquarius](https://apps.dtic.mil/sti/tr/pdf/ADA211444.pdf)

It's maybe worth to note that Prolog was not the first logic programming languages, but Absys, which first appeared in 1967: https://en.wikipedia.org/wiki/Absys, [Absys: the first logic programming language —A retrospective and a commentary](https://www.sciencedirect.com/science/article/pii/0743106690900309?via%3Dihub)

<br/>

In Prolog, I also tumbled over the question: **Are rules and clauses the same thing in Prolog?**

Apparently not always. A rule in Prolog looks like this or similarly to this and denotes a _conditional truth_:

_nearby(X,Y):-connected(X,Z,L),connected(Z,Y,L)._ (I got this example from this classical book from 1994: https://github.com/simply-logical/simply-logical/releases/tag/v1.0 (*))

Further: _A program is a set of clauses, each of them terminated by a period._(*). So, a clause is a higher level concept, and a rule is just one type of a clause in my understanding.

<br/>

## Speed in the Land of Prolog's

Let the benchmarking game begin with usual command _$ sudo perf stat -r 20 ./graph_4coloring_Germany2..._, again with the best run out of 3:

Prolog dialect | best run out of 3
--- | ---
GNU | mean = 1597 [milliseconds]
SWI | 0,69330 +- 0,00175 seconds time elapsed  ( +-  0,25% )
Ciao | 0,75810 +- 0,00150 seconds time elapsed  ( +-  0,20% )
YAP | 0,93109 +- 0,00254 seconds time elapsed  ( +-  0,27% )
ECLiPSe | mean = 574 [milliseconds]
XSB | mean = 612 [milliseconds]
Trealla | mean = 2224 [milliseconds]
Scryer | mean = 1538 [milliseconds]

So, about 570 milliseconds is the benchmark time a logically equivalent Mercury program must beat!

See this table as a diagram at the top of this page: [The TL;DR execution speed diagram](#the-tldr-execution-speed-diagram)

<br/>

There was a problem with the GNU Prolog program again, since environment variable _GLOBALSZ_ is apparently not recognized in the context of _perf stat_. Instead, I took the [shell script](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/exe_times_statistics_for_one_test_case_in_cwd2) again: _$ ./exe_times_statistics_for_one_test_case_in_cwd2 ./graph_4coloring_Germany2a_. I also used it to time measure the ECLiPSe, XSB. Trealla and Scryer Prolog programs.

<br/>

By the way: is 570 milliseconds generally a fast execution time for the map coloring problem of Germany?

To answer this question I let basically MS Bing AI develop a [C/C++ program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/MapColoring_Germany.cpp), which is employing a (simple) backtracking algorithm.

Result of _$ sudo perf stat -r 20 ./MapColoring_Germany_cpp: 0,033907 +- 0,000422 seconds time elapsed  ( +-  1,24% )_ (so, not even best out of 3 runs). This is about 1 / (34 milliseconds / 574 milliseconds) = ~17 times faster than the best Prolog program, which is the ECLiPSe program.

By the way: here's an article which is listing techniques and algorithms, which are superior to the (original) backtracking algorithm: [Constraint Satisfaction Problems: A Comprehensive Guide to Map Coloring](https://medium.com/@payalsingh567951/constraint-satisfaction-problems-a-comprehensive-guide-to-map-coloring-d10955e261c7).

<br/>

## And Mercury?

The map coloring problem of Germany is a Constraint Satisfaction Problem (CSP), but:

> The original design of Mercury did not support constraint logic programming (CLP).

from: [Adding constraint solving to Mercury](https://mercurylang.org/documentation/papers/padl_solver.pdf) by Ralph Becket, Maria Garcia de la Banda, Kim Marriott, Zoltan Somogyi, Peter J. Stuckey & Mark Wallace, 2005

..and further:

> The easiest way to add constraint solving capability to a Mercury program is to provide an interface to an existing solver such as CPLEX [2] written in a foreign language.

That's it for Mercury in this benchmark. The farest I could get with Mercury is [The first solution of a map coloring problem](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#the-first-solution-of-a-map-coloring-problem).

Which actually means that Mercury is (still) not a competing language to Prolog for solving Constraint Satisfaction Problems.

<br/>

## MiniZinc - constraint modelling language

While looking around for potential Prolog systems and solutions for constraint programming (CP), I tumbled over this system: [MiniZinc](https://www.minizinc.org/) + https://github.com/minizinc

I gave it a try to find out how a dedicated language and implementation can do compared to Prolog at the map coloring problem of Germany.

I downloaded file _MiniZincIDE-2.9.4-bundle-linux-x86_64.tgz_ from here: https://www.minizinc.org/downloads/, unzipped this file and expanded _PATH_ to: _export PATH="$PATH:~/scripts/MiniZinc/MiniZincIDE-2.9.4-bundle-linux-x86_64/bin"_

I only quickly implemented a skeleton of the basic Prolog program in MiniZinc, so its functionality isn't exactly the same, as this would require much more of my time.

However, my concept is also correctly computing the total number of solutions, where the later could also be displayed individually (but not screened for first and last solutions only so easily).

Here's the [whole program](./MapColoring_Germany.mzn), with this source code, which makes it an easy to modify "low-code" solution:

```
int: nc = 4;

var 1..nc: SH;
var 1..nc: MV;
var 1..nc: HH;
var 1..nc: HB;
var 1..nc: NI;
var 1..nc: ST;
var 1..nc: BE;
var 1..nc: BB;
var 1..nc: SN;
var 1..nc: NW;
var 1..nc: HE;
var 1..nc: TH;
var 1..nc: RP;
var 1..nc: SL;
var 1..nc: BW;
var 1..nc: BY;

constraint SH != NI;
constraint SH != HH;
constraint SH != MV;
constraint HH != NI;
constraint MV != NI;
constraint MV != BB;
constraint NI != HB;
constraint NI != BB;
constraint NI != ST;
constraint NI != TH;
constraint NI != HE;
constraint NI != NW;
constraint ST != BB;
constraint ST != SN;
constraint ST != TH;
constraint BB != BE;
constraint BB != SN;
constraint NW != HE;
constraint NW != RP;
constraint SN != TH;
constraint SN != BY;
constraint RP != SL;
constraint RP != HE;
constraint RP != BW;
constraint HE != BW;
constraint HE != TH;
constraint HE != BY;
constraint TH != BY;
constraint BW != BY;

solve satisfy;
```

I ran this program like this to count the total number of solutions, which is 191808:

```
$ time minizinc ./MapColoring_Germany.mzn --all-solutions | grep "^-" | wc
 191808  191808 2109888

real	0m3,292s
user	0m4,672s
sys	0m0,555s
$
```

By the way: here is the MiniZinc playground: https://play.minizinc.dev/

So, this isn't a fast solution compared to let's say a very common solution in SWI Prolog for this specific problem. Above, MiniZinc has been using its default solver, which is constraint solver _gecode_, that is currently **Gecode** 6.3.0: https://www.gecode.dev/ + https://en.wikipedia.org/wiki/Gecode

Then, I tried the other working one for this problem, which is Chuffed CP solver 0.13.2 (https://github.com/chuffed/chuffed), with lazy clause generation for search reduction, which is a little bit slower at this specific problem:

```
$ time minizinc ./MapColoring_Germany.mzn --all-solutions --solver chuffed | grep "^-" | wc
 191808  191808 2109888

real	0m3,373s
user	0m4,471s
sys	0m0,591s
$
```

I also tried the other solvers from the list provided with command _$ minizinc --solvers_, only to find out that those solvers are for other problems or need a license as commercial solutions.

<br/>

##_end
