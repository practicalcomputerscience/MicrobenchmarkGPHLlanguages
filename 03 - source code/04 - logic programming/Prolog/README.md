2025-11-06: work in progress

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
- [Portability of executables](#portability-of-executables)

<br/>

---

## The map coloring problem of Germany with 16 states and 4 colors

Here I use a different microbenchmark program, albeit one, which I think is much more typical for problems of logic programming than my accidental microbenchmark program.

In 2017, I successfully tested this [Prolog program](), which I implemented back then in GNU Prolog version 1.3.0. It's this source code from 2013 at chapter "The Graph Coloring Problem":

[Try logic programming! A gentle introduction to Prolog](https://web.archive.org/web/20170106042155/https://bernardopires.com/2013/10/try-logic-programming-a-gentle-introduction-to-prolog/)

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

But before running program _graph_4coloring_Germany2a_, make sure to have enough space on the stack. The usual 32kB is too small for this program.

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

However, this didn't make a standalone executable program. Building one isn't so easy in SWI Prolog.

TBD

## Ciao Prolog

To have a third opinion, I tested Ciao Prolog: https://ciao-lang.org/

TBD

As one may have noticed, the default character for comments is _%_ here, not _/* ... */_. Apparently, comment blocks in Ciao Prolog are not possible.

Building a standalone executable is easy in Ciao Prolog: _$ciao comp -S ./graph_4coloring_Germany2b_Ciao.pl_, with the _-S_ switch for building a standalone executable.

<br/>

All three dialects claim to follow the [ISO standard of Prolog](https://www.iso.org/standard/21413.html), including Ciao ("supporting the ISO-Prolog standard"), albeit I think that the potential possibility to port the source code from one dialect to the other without changes is the bigger benefit.

I don't have a clear favorite Prolog dialect; all three have their cons, but also their pros. Choosing the right dialect seems to be more difficult than with [Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme).

<br/>

## Portability of executables

Right next to the portability of source code sits the challenge of portability of standalone executables.

TBD

<br/>

##_end
