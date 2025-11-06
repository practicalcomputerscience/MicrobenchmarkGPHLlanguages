2025-11-06: work in progress: check remaining TBD's

# Prolog

This page and its Prolog (= _**PRO**grammation en **LOG**ique_) programs exist for one reason:

> How fast is a (compiled) program in the [Mercury language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) compared to a (compiled) version in its precursor language Prolog?

<br/>

---

Table of contents:

- [The map coloring problem of Germany with 16 states and 4 colors](#the-map-coloring-problem-of-germany-with-16-states-and-4-colors)
- [GNU Prolog](#gnu-prolog)
- [SWI Prolog](#swi-prolog)
- [Ciao Prolog](#ciao-prolog)
- [Speed in the Land of Prolog's](#speed-in-the-land-of-prologs)
- [The Mercury benchmark program](#the-mercury-benchmark-program)

<br/>

---

## The map coloring problem of Germany with 16 states and 4 colors

Here I use a different microbenchmark program, albeit one, which I think is much more typical for problems of logic programming than my accidental microbenchmark program.

In 2017, I successfully tested this [Prolog program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2a.pl), which I implemented back then in GNU Prolog version 1.3.0. It's this source code from 2013 at chapter "The Graph Coloring Problem": [Try logic programming! A gentle introduction to Prolog](https://web.archive.org/web/20170106042155/https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/)

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/map_coloring_germany-636x310.png)

..and now I thought that this is a good program to compare the execution speeds of a Prolog program and its version in Mercury.

However, Prolog shows the same phenomenon than Scheme: what dialect to use?

After some survey of modern [Prolog implementations](https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations), I came down to this short list of well maintained Prolog dialects:

- GNU Prolog: http://gprolog.org/
- SWI Prolog: https://www.swi-prolog.org/
- Ciao Prolog: https://ciao-lang.org/

<br/>

## GNU Prolog

However, with GNU Prolog I immediately ran into a problem: 

TBD

Building a standalone executable is very easy in GNU Prolog and a big advantage from my point of view: _$ gplc ./graph_4coloring_Germany2a.pl_

But before running program [graph_4coloring_Germany2a](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2a.pl), make sure to have enough space on the stack. The usual 32kB is too small for this program.

So, as one way, you could add in your _.bashrc_ file this global environment variable: _export GLOBALSZ=524288_

..where 524288 bytes is just my proposal, which works in my system. Activate: _$ source ./.bashrc_ and check the new environment variable:

```
$ printenv GLOBALSZ
524288
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
1st solution = 
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


## Ciao Prolog

To have a third opinion, I tested Ciao Prolog: https://ciao-lang.org/

Again, I had to slightly change the original source code to make the program working in this [dialect](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2b_Ciao.pl).

What's unkown in Ciao, is predicate _nth0()_, which I changed to _nth()_, which I made available with clause: _:- use_module(library(lists))._ See from here at [core/lib/lists.pl](https://github.com/ciao-lang/ciao/blob/fdff410cf2b7f2b85baff97485a2db5522d785f3/core/lib/lists.pl)

As one may have noticed, the default character for comments is _%_ here, not _/* ... */_. Apparently, comment blocks in Ciao Prolog are not possible.

Building a standalone executable is easy in Ciao Prolog:

```
$ ciao comp -S ./graph_4coloring_Germany2b_Ciao.pl  # -S switch for building a standalone executable
$ ./graph_4coloring_Germany2b_Ciao
number N of different solutions = 191808

               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY
1st solution = 
...
Last solution = yellow,green,green,yellow,blue,green,blue,yellow,blue,yellow,green,yellow,blue,green,yellow,red
$
```

While the 1st solution is the same as with GNU and SWI, the last solution is different. Ciao Prolog obviously computes things a little bit differently.

<br/>

All three dialects claim to follow the [ISO standard of Prolog](https://www.iso.org/standard/21413.html), including Ciao ("supporting the ISO-Prolog standard"), albeit I think that the potential possibility to port the source code from one dialect to the other without changes is the bigger benefit.

I don't have a clear favorite dialect; all three have their cons, but also their pros. Choosing the right Prolog dialect seems to be more difficult than with [Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme).

<br/>

## Speed in the Land of Prolog's

Now that true portability has been checked for:

- _graph_4coloring_Germany2a_GNU_
- _graph_4coloring_Germany2b_Ciao_
- _graph_4coloring_Germany2c_SWI_ + _libswipl.so.9_

..let the benchmarking game begin with usual command _$ sudo perf stat -r 20 ./graph_4coloring_Germany2..._, again with the best run out of 3:

Prolog dialect | best run out of 3
--- | ---
Ciao | 0,75810 +- 0,00150 seconds time elapsed  ( +-  0,20% )
SWI | 0,69330 +- 0,00175 seconds time elapsed  ( +-  0,25% )

However, there was a problem with the GNU Prolog program again, since environment variable _GLOBALSZ_ is apparently not recognized in the context of _perf stat_. This can be checked by running [shell script](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/exe_times_statistics_for_one_test_case_in_cwd2): _$ ./exe_times_statistics_for_one_test_case_in_cwd2 ./graph_4coloring_Germany2a_, which works also fine here:

Prolog dialect | best run out of 3
--- | ---
GNU | mean = 1597 [milliseconds]

So, about 690 milliseconds is the benchmark time a logically equivalent Mercury program must beat!

## The Mercury benchmark program

TBD

<br/>

##_end
