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

## SWI Prolog

TBD

## Ciao Prolog

To have a third opinion, I tested Ciao Prolog: https://ciao-lang.org/

TBD

pros:

- xxx
- xxx

cons:

- xxx
- xxx

(TBD)

<br/>

So, at the moment, for Linux I can only recommend **SWI Prolog**. It's fast, easy to install and its syntax looks like "mainstream Prolog", whatever this may be.

SWI Prolog was already recommended in this super-old list: http://www.fraber.de/university/prolog/comparison.html#needs

All three dialects claim to follow the [ISO standard of Prolog](https://www.iso.org/standard/21413.html), including Ciao ("supporting the ISO-Prolog standard"), albeit I think that the potential possibility to port the source code from one dialect to the other without changes the bigger benefit is.

<br/>

## Portability of executables

Right next to the portability of source code sits the challenge of portability of executable programs.

TBD

<br/>

##_end
