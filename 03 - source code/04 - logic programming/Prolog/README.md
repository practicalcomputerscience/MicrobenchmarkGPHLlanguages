2025-11-06: work in progress

# Prolog

This page and its Prolog (= _**PRO**grammation en **LOG**ique_) programs only exist for one reason:

> How fast is a (compiled) program in the [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) language compared to a (compiled) version in its precursor language Prolog?

<br/>

---

Table of contents:

- [The map coloring problem of Germany with 16 states and 4 colors](#)
- [GNU Proglog](#)
- [SWI Proglog](#)
- [Ciao Proglog](#)
- [Portability of executables](#)

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

- GNU Prolog: (TBD)
- SWI Prolog: (TBD)
- Ciao Prolog: (TBD)

<br/>

## GNU Proglog

However, with GNU Prolog I immediately ran into a problem: 

TBD

## SWI Prolog

TBD

## Ciao Prolog

To have a third opinion, I tested Ciao Prolog: 

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

All three dialects claim to follow the ISO standard of Prolog (TBD), including Ciao ("supporting the ISO-Prolog standard"), albeit I think that the potential possibility to
port the source code from one dialect to the other without changes is a bigger benefit. 

<br/>

## Portability of executables

Right next to the portability of source code sits the challenge of portability of executable programs.

<br/>

##_end
