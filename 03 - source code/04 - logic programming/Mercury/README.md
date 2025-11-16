2025-11-05: work in progress: TBD: update slides, benchmark diagram etc.

# Mercury

https://mercurylang.org/index.html

The Mercury programming language puts functional programming on top of logic programming.

However, Mercury is not the right language for solving **Constraint Satisfaction Problems (CSP's)**, like the map coloring problem, for what [Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#prolog) seems to be made for. At the moment, Mercury only features a [simple solver type supporting equality and disequality constraints](https://github.com/Mercury-Language/mercury/tree/master/samples/solver_types), which you can use to find **one** solution of a [map coloring problem](#the-first-solution-of-a-map-coloring-problem).

---

Table of contents:
- [Concepts of Mercury](#concepts-of-mercury)
- [Difference between logic programming and declarative programming](#difference-between-logic-programming-and-declarative-programming)
- [Installation tips](#installation-tips)
- [Installing the eqneq solver](#installing-the-eqneq-solver)
- [How to install extra programs](#how-to-install-extra-programs)
- [The first solution of a map coloring problem](#the-first-solution-of-a-map-coloring-problem)
- [How I discovered Mercury](#how-i-discovered-mercury)
- [Selected features of and tips for Mercury](#selected-features-of-and-tips-for-mercury)

<br/>

---

## Concepts of Mercury

Mercury is not a Prolog system; see at page 782 from [Fifty Years of Prolog and Beyond](https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/fifty-years-of-prolog-and-beyond/3A5329B6E3639879301A6D44346FD1DD), because it does not follow Prolog's definition of _append()_:

```
append([], L, L).  // appending L to empty list [] returns list L
append([X | L], M, [X | N]) :- append(L, M, N).  // appending lists L and M to new list N
```

Beside this point, there's a big difference between Mercury and Prolog from my point of view:

> [!NOTE]
> Mercury uses an expressive, statically checked type system similar to that of ML and Haskell, while key characteristics of Prolog-based systems are a dynamic type system, ...

ML for [Standard ML](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML#standard-ml-sml).

Sources:

- [Tutorial on programming in Mercury](https://mercurylang.org/documentation/papers/book.pdf)
- [Adding constraint solving to Mercury](https://mercurylang.org/documentation/papers/padl_solver.pdf)

<br/>

> Mercury is based on the paradigm of **purely declarative programming**...
> After stripping away the declarations of a Mercury program, the syntax of the remaining part of the program is mostly compatible with **Prolog** syntax.

My emphasis in bold; sources:

- https://mercurylang.org/information/doc-latest/mercury_reference_manual/Introduction.html#Introduction
- https://mercurylang.org/about.html

Although the development of the Mercury programming language started in [1995](https://mercurylang.org/information/doc-latest/mercury_reference_manual/index.html#SEC_Contents),
development is still ongoing as its GitHub repository activity shows: https://github.com/Mercury-Language/mercury

Furthermore, this "The Mercury Programming Language" [presentation](https://paul.bone.id.au/pub/pbone-2015-mercury/) from 2015 says:

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
$ cd  # go to home directory
$ mkdir Mercury  # create an extra target directory for the later language installation
# change back into installation directory: ./mercury-srcdist-rotd-2025-11-01/
$ ./configure --enable-minimal-install --prefix=$HOME/Mercury --enable-additional-libgrades=asm_fast.gc.tr
# do not install as usual into directory /usr/local/... as a root user,
# as this directory is a root directory and would make future installations of "extras" as a normal user hard;
# instead, use a subdirectory of the current, normal user: --prefix=$HOME/Mercury
# here, we will also install one more library grade, that is asm_fast.gc.tr, on top of very basic one asm_fast.gc,
# to later be able to compile the sudoku solver, which makes use of solver type "eqneq", and which needs library grade asm_fast.gc.tr.
...
using /home/booser/Mercury as the install prefix

the set of library grades to install will be
   asm_fast.gc
   asm_fast.gc.tr

Configuring to install 2 grades.
This will likely take 20 to 60 minutes.
...
$
```

The _tr_ in _asm_fast.gc.tr_ stands for _trailing_, that is to compile in a grade that supports trailing.

Now start the compilation with:

```
$ make  # this will take some time
```

However, for speeding up compilation with 4 concurrent jobs, this command could be used, something I highly recommend:

```
$ make -j4
...
$ make install  # this will also take some time; installation is done here not as root user!
```

Alternatively, for speeding up installation with 4 concurrent jobs, this command could be used, something I highly recommend:

```
$ make PARALLEL=-j4 install
...
-- Installation complete.  # good to know -;
...
$
```

There should be these three new directories now:

```
$ ls ~/Mercury
bin  lib  share
$
```

Add to your _PATH_ in the _.bashrc_ file: _export PATH="$PATH:$HOME/Mercury/bin"_, and activate the modified _.bashrc_ file. Then test the Mercury compiler:

```
$ source ~/.bashrc
$ mmc --version  # compiler test
Mercury Compiler, version rotd-2025-11-01
Copyright (C) 1993-2012 The University of Melbourne
Copyright (C) 2013-2025 The Mercury team
$
```

By the way: the Mercury compiler shows a huge help page with: _$ mmc --help_

If you want to unstall this installation, go back to the original installation directory _./mercury-srcdist-rotd-2025-11-01/_ and do:

```
$ make uninstall
```

In other words:

> [!TIP]
> Keep the original installation directory! Also for later potentially installing "extra programs" (under _./extras_).

### Installing the eqneq solver

Now comes the real test, that is using the _eqneq_ solver (equality/disequality) from here: https://github.com/Mercury-Language/mercury/tree/bdd2a574a86e5dfc37e7cbff7e5108313e796bc0/samples/solver_types

I did this:

```
$ cd ./mercury-srcdist-rotd-2025-11-01/samples/solver_types
$ make
...
$
```

The _make_ command has created two links among other things in this directory:

```
$ ls -l
...
sudoku -> Mercury/asm_fast.gc.tr/x86_64-pc-linux-gnu/Mercury/bin/sudoku
...
test_eqneq -> Mercury/asm_fast.gc.tr/x86_64-pc-linux-gnu/Mercury/bin/test_eqneq
...
$ 
```

Let's do this test and the game:

```
$ ./test_eqneq
[1, 2, 3, 1, 2, 3, 1, 2, 3]
$ ./sudoku ./sudoku_puzzle.easy
6 4 5  1 9 8  2 7 3  
1 2 7  3 4 6  5 9 8  
8 9 3  2 7 5  4 6 1  

5 1 4  6 2 7  3 8 9  
2 3 8  4 1 9  6 5 7  
7 6 9  8 5 3  1 4 2  

4 5 1  7 8 2  9 3 6  
9 8 6  5 3 1  7 2 4  
3 7 2  9 6 4  8 1 5  
$
```

Voilà!

<br/>

Installing Mercury in the right configurations is not the easiest language installation job. It took me three runs to come to this point. The key to success is to start correctly at the initial configuration:

```
$ ./mercury-srcdist-rotd-2025-11-01/configure --enable-minimal-install --prefix=$HOME/Mercury --enable-additional-libgrades=asm_fast.gc.tr
```

### How to install extra programs

This chapter relates to the [Extra programs in the Mercury implementation](https://github.com/Mercury-Language/mercury/tree/master/extras#extra-programs-in-the-mercury-implementation). My goal is to additionally install the [solver_types library](https://github.com/Mercury-Language/mercury/tree/master/extras/solver_types/library). I followed the given instructions:

> Most of these can be built by running the commands _mmake depend_ and then _mmake_ in the relevant subdirectory, and many can be installed by running _mmake install_.

So, I did this:

```
$ cd ./mercury-srcdist-rotd-2025-11-01/extras/solver_types/library
$ mmake depend
...
$ mmake
...
$ mmake install
...
$
```

Now, there should be a new directory called _extras_:

```
$ ls ~/Mercury
bin  extras  lib  share
$ ls ~/Mercury/extras/lib/mercury/
inc  ints  lib  modules
$
```

By the way: installing extra programs does not depend on ininitially installing the extra library grade _asm_fast.gc.tr_ according to my experience. Starting with only _--enable-minimal-install_, that is library grade _asm_fast.gc_, would be enough here.

<br/>

## The first solution of a map coloring problem

Here I worked with the GNU make tool, like used for official example source code files _test_eqneq.m_ and _sudoku.m_ from here: https://github.com/Mercury-Language/mercury/tree/master/samples/solver_types

I also prepared related make files _Makefile_ and _Mercury.options_ (as linked at the very top of this page), and put them together with the source code file _**graph_4coloring_Australia.m**_ (for coloring the Australian map and not the much bigger problem of the Germany map) into its own project directory, here named _./Mercury/graph_4coloring_Australia_

So, all in all there are these files in this directory:

```
$ ls -1
eqneq.m
graph_4coloring_Australia.m
Makefile
Mercury.options
$ 
```

Then I compiled the program with the _make_ command and executed the generated link to run it:

```
$ make
mmc --make graph_4coloring_Australia
...
Making Mercury/asm_fast.gc.tr/x86_64-pc-linux-gnu/Mercury/bin/graph_4coloring_Australia
$ ./graph_4coloring_Australia
NT, QL, NSW, VIC, SA, WA, TAS
["red", "green", "red", "green", "blue", "green", "red"]
bye.
$ 
```

This result represents only one solution, but not the 576 ones as computed with this [Python program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury/MapColoring_Australia.py), which is actually a 1:1 copy of this elegant, object-oriented Python program: https://github.com/parisasl/MapColoring/blob/main/MapColoring.py, which is employing **backtracking**.

Backtracking is a standard algorithm for solving Constraint Satisfaction Problems: https://en.wikipedia.org/wiki/Backtracking

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
