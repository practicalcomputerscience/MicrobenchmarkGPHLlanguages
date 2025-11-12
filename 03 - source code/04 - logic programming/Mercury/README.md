2025-11-05: work in progress

# Mercury

https://mercurylang.org/index.html

In my opinion:

> The idea of the Mercury programming language to put functional programming on top of logic programming is a real hit and should be way more popular!
> 
> Mercury feels like "Prolog on speed". (*)

<br/>

Although technically speaking, Mercury is not a Prolog system; see at page 782 from [Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD), because it does not follow Prolog's definition of _append()_:

```
append([], L, L).
append([X | L], M, [X | N]) :- append(L, M, N).
```

TBD: personally check this claim with a speed comparison: Prolog <--> Mercury of a much simpler program: 4 colors for 16 German states problem

<br/>

> Mercury is based on the paradigm of **purely declarative programming**...
> After stripping away the declarations of a Mercury program, the syntax of the remaining part of the program is mostly compatible with **Prolog** syntax.

My emphasis in bold; sources:

- https://mercurylang.org/information/doc-latest/mercury_reference_manual/Introduction.html#Introduction
- https://mercurylang.org/about.html

---

Table of contents:
- [Concepts of Mercury](#concepts-of-mercury)
- [Difference between logic programming and declarative programming](#difference-between-logic-programming-and-declarative-programming)
- [Installation tips](#installation-tips)
- [How I discovered Mercury](#how-i-discovered-mercury)
- [Selected features of and tips for Mercury](#selected-features-of-and-tips-for-mercury)

<br/>

---

## Concepts of Mercury

Even though not broadly known, I have the positive impression that the Mercury programming language,
started in [1995](https://mercurylang.org/information/doc-latest/mercury_reference_manual/index.html#SEC_Contents),
is in a very mature state now. Although, it's development is still ongoing as its GitHub repository show: https://github.com/Mercury-Language/mercury

This "The Mercury Programming Language" [presentation](https://paul.bone.id.au/pub/pbone-2015-mercury/) from 2015 says:

> Mercury is a purely declarative logic/functional programming language.
> 
> Mercury looks like Prolog, but it feels like strict Haskell or pure OCaml
> 
> Purely declarative programs have no side effects. If a predicate has an effect, it has to be reflected in its argument list.
> 
> Mercury has a strong, static type system similar to Haskell's.

The **OCaml** reference is true in my opinion. Knowing some [OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) helps with understanding and developing Mercury programs.

Also helpful was my [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#clojure) experience, specifically with its "pure" style, since Clojure, in contrast to Ocaml, also adheres to **strictly immutable data structures**.

<br/>

## Difference between logic programming and declarative programming

MS Bing Copilot gave me this answer, as part of a bigger answer (on 2025-11-03):

> Logic programming is a specific kind of declarative programming that expresses programs as logical facts and rules and computes by logical inference.
> 
> Declarative programming is a broader paradigm that emphasizes describing what the program should achieve rather than how to do it;
> logic programming, functional programming, and SQL-style query languages are all examples of declarative styles.

<br/>

## Installation tips

When installing Mercury for the first time (bootstrapping) and these programs are not installed yet, install them now before installing Mercury:

- gcc
- flex
- make
- bison

Also read at least this document to gain more background knowledge for installing Mercury: [README.bootstrap](https://github.com/Mercury-Language/mercury/blob/499d1935801e855cc0c824c2cb5635802542d729/README.bootstrap#L4)

I started with file _mercury-srcdist-rotd-2025-11-01.tar.gz_ from here: https://dl.mercurylang.org/index.html

..which I extracted to installation directory: _./mercury-srcdist-rotd-2025-11-01/_

..where I did these things:

```
$ ./configure --enable-minimal-install
$ make      # this will take some time
```

Alternatively, for speeding up compilation with 4 concurrent jobs:
```
$ make -j4
```

```
$ sudo make install               # this will also take some time
```

Alternatively, for speeding up installation with 4 concurrent jobs:
```
$ sudo make PARALLEL=-j4 install
```

Add to your _PATH_ in the _.bashrc_ file: _PATH=$PATH:/usr/local/mercury-rotd-2025-11-01/bin_

..and activate it:

```
$ source ~/.bashrc
$ mmc --version  # compiler test
Mercury Compiler, version rotd-2025-11-01
Copyright (C) 1993-2012 The University of Melbourne
Copyright (C) 2013-2025 The Mercury team
$
```

<br/>

## How I discovered Mercury

This is how I tumbled over the Mercury programming language accidentally:

While working on my [Functional languages overview](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#functional-languages), I read about the **Actor model** for concurrent computing: PDF from 2023: [A Formal specification For Half a Century of Actor Systems](https://soft.vub.ac.be/Publications/2024/vub-tr-soft-24-01.pdf).

> The first implementation of the actor model was in a Planner-like programming language that was modeled on actors [15], originally called Planner-73, but later renamed to PLASMA.

I wondered what has happened to the PLASMA (with capital letters) programming language: _A recursive implementation of factorial written in PLASMA is given in Listing 1._

```
(factorial ≡
  (≡> [=n]
    (rules n
      (≡> 1
           1)
      (≡> (> 1)
           (n * (factorial <= (n - 1)))))))
```

..and found this webpage: https://plasmalang.org/roadmap.html

However, that functional language, named _Plasma_, apparently has nothing do (directly) with the old PLASMA system.

So, I put that language, obviously under heavy construction, aside for while, only to read now more about it. There I read that the compiler for Plasma has been written in a language called Mercury:

> Plasma is written in Mercury (at least until we get to a self hosting stage) which means if you want to compile Plasma (to contribute to it) you may need to build Mercury from source...

from: https://plasmalang.org/docs/dev_mercury_grades.html, Updated: March 2020

And then I got interested in Mercury, specifically for its background as a "Prolog on speed" programming language.

<br/>

## Selected features of and tips for Mercury

Mercury has a dedicated **string builder**: https://github.com/Mercury-Language/mercury/blob/fca4505501852e5feda0734ff6c5ec6ac02bc638/library/string.builder.m

..which makes the [speed part](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/random_streams_for_perf_stats.m) of this program competitively fast with about 77 milliseconds execution time!

See also these official "Raw benchmark times" in milliseconds (*):

- https://www.mercurylang.org/about/bench/times.html
- https://mercurylang.org/about/benchmarks.html

Apparently, the execution speed of a (compiled, binary) Mercury program can be an order of magnitude higher than the execution speed of a [SWI Prolog](https://www.swi-prolog.org/) program.

<br/>

Mercury source code can be compiled to targets:

- high-level C, the default target with: _$ mmc < filename >.m_
- Java
- C#

See from: https://github.com/Mercury-Language/mercury/blob/fca4505501852e5feda0734ff6c5ec6ac02bc638/compiler/ml_backend.m

<br/>

A note on some older Mercury code or documentation:

> [!NOTE]
> ( Gc -> Gt ; Ge ) is an alternative, albeit old-fashioned, syntax for ( if Gc then Gt else Ge ).

- Gc = Goal condition (?)
- Gt = Goal true (?)
- Ge = Goal error (?)

from: [Tutorial on programming in Mercury](https://mercurylang.org/documentation/learning.html) from 2020. Albeit not ready, this document was a main help for me to develop the microbenchmark program in Mercury.

<br/>

Last but not least, I quote my old, personal Prolog tip from 2017, something which is also true for planning Mercury programs:

> [!IMPORTANT]
> DO NOT HAVE source code files with INITIAL Capital letter ("Hello_World.pl") => INITIAL Capital letters are ONLY FOR VARIABLES!

So, name your variables maybe in _CamelCase_, but your function, predicate ("statement"), program names etc in small letters only (like _hello_world.pl_ in Prolog).

<br/>

##_end
