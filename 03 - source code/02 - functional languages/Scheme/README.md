2025-07-27: heavy work in progress

---

A home page for Scheme: https://www.scheme.org/schemers/

---

<br/>

> Some say there are more implementations than applications.

from: https://scheme.fail/manual/loko.html

<br/>

## What Scheme dialects are still maintained?

After I implemented the "speed part" program in 4 different Scheme dialects, I made an overview table with the state of certain Scheme dialects:

![plot](./Scheme%20dialects%20-%20big%2C%20actively%20maintained.png)

..which is based on the list as found here: _Scheme Containers - Available implementations - Big, actively maintained_ at: https://containers.scheme.org/ (*)

However, this list is now outdated, which can be seen at best from my point of view with **Kawa**, a Scheme dialect for the Java Virtual Machine (JVM), see at ![Kawa](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM)).

My defintion of "maintained" was straightforward: has there been some update in the last 12 months?

Sources for 10 out of these 16 Scheme dialects - not all of them are "big" - have been updated in the last 12 months roughly according to my counting and 15 within the last three years - with the exception of Kawa.

Maintaining a computer programming language is important from my point of view (same source from above (*)):

> Many (if not most) implementations keep working for years after active maintenance has ended, requiring few if any patches.

This can be true or not, I've made both experiences:
- OCaml for the Java Virtual Machine (JVM) from 2015 for example still runs fine: (TBD)
- the last update of Kawa Scheme (for the JVM) is much younger and still I wasn't able to run the _make_ process without errors, see at ![Kawa](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM)), which has then become my showstopper with Kawa

<br/>

## My 5 best practices with Scheme dialects

1. before coding in a Scheme dialect make sure that you have understood its limitations, but also look at the **SRFI**'s (Scheme Requests for Implementation: https://srfi.schemers.org/), and how these SRFI's are available, to help you overcome these limitations potentionally!
2. for **good execution speeds** with longer "sequences of data items" avoid lists as much as possible and use **vectors** instead; I'm sure that this applies to all Scheme dialects. See also below at [Vectors in Scheme](#vectors-in-scheme).
3. in a targeted Scheme dialect get familiar with how to install **libraries**, mostly the Scheme SRFI's. There's a great chance that a library procedure, which is not included in an already installed (standard) library, can provide a (partly) solution to your problem. I learned that specifically documentation for installing libraries often sucks greatly in the Land of Scheme's!
4. **Racket** Scheme (https://racket-lang.org/) has the most "batteries already included" and is in average not the slowest Scheme dialect (though in average it's not the speediest dialect). This makes Racket the best Scheme dialect to check out first in my opinion. I think that for most hobby users Racket is just good enough, but may also need elaborate experimentation for satisfactory results
5. be carefull, with the exception of Racket Scheme, to use the usual Linux distribution installations (_$ sudo apt install ..._). Better download directly from GitHub ("<> Code" ---> "Download ZIP") and compile and install according to the given instructions. Otherwise you may end up with an installation with "no
batteries included". This happend to me with Gambit Scheme! And often the Github versions are newer.

<br/>

## Features of the Scheme programming language

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Features%20of%20the%20Scheme%20programming%20language.png)

The sources at the bottom of this presentation slide are linked here:

- _An Introduction to Scheme and its Implementation_, Paul R. Wilson, 1996: https://doc.lagout.org/programmation/Lisp/Scheme/An%20Introduction%20to%20Scheme%20and%20its%20Implementation.pdf
- _Revised7 Report on the Algorithmic Language Scheme_, 2013: https://small.r7rs.org/attachment/r7rs.pdf
- _Common Lisp & Scheme, a comparison_, Pascal Costanza, 2006: https://www.p-cos.net/lisp/guide.html
- _AN INTRODUCTION TO FUNCTIONAL PROGRAMMING THROUGH LAMBDA CALCULUS_, Greg Michaelson, YYYY?: https://www.cs.rochester.edu/~brown/173/readings/LCBook.pdf

<br/>

## What they don't tell you in the Land of Scheme's at first

After coding in a few Scheme dialects, I made these experiences:

- even with displaying "Hello, World!" on the console, there's a certain chance that you have to change the source code file when moving from one Scheme dialect to another!
- with the "Hello, World!" program for example due to needed but different imports and potentially (global) declarations
- having source code for the compiler or having commands ("expressions") for the interpreter (REPL) of a Scheme dialect may be a different thing in the lands of Lisp's and Scheme's. This may give you a false sense of security
- sometimes the differences may only emerge later during production, because a Scheme compiler (and build script) of one dialect may also produce a standalone, binary executable without any complaints from source(s) that have been "battle tested" in another Scheme dialect, only to cause a runtime crash during production! So, having sufficient tests is essential when moving from one Scheme dialect to another for a production ready program, specifically for rarely used, user defined functions
- [Racket](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket) is no exception here according to my experience: it may be very easy to take source code meant for [Chez Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket#chez-scheme-cs) for example without any changes and compile it (with: _$ raco exe < program name >.ss_ with file name extension _.ss_ for a Chez source code file) successfully into a Racket based standalone program - only to find out later that it runs differently - but without any crashes or error messages for example! This is potentially a worse scenario than a visible crash during runtime

This is not only my experience:

> A program written for one implementation is likely not to work with another one.

from 2018: http://fmnt.info/blog/20181029_scheme.html.

From https://wingolog.org/archives/2013/01/07/an-opinionated-guide-to-scheme-implementations:

> So don't fear implementation lock-in...

But this is exactly the situation with the fragmented Scheme ecosystem from my point of view, with maybe only these two exceptions:

- the new alliance of Chez Scheme and Racket: https://blog.lambdaclass.com/rebuilding-the-racket-compiler-with-chez-scheme/, and
- Bigloo, because this dialect has the support of the Inria (Institut national de recherche en sciences et technologies du numérique: https://www.inria.fr/fr)

This (informal) Chez Scheme-Racket alliance is a very clever move from my point of view, because it increases the chances of survival for both dialects for years to come in a fragmented Scheme ecosystem.

<br/>

#### Scheme Surveys

These pages detail various differences between Scheme implementations: https://docs.scheme.org/surveys/

Also see _How do you import SRFI-1 (the list library) in scheme?_ at: https://rain-1.github.io/scheme-srfi-1.html

<br/>

## System limitations

Regardless of the Scheme dialect you choose, make sure you have noticed its limitations before using it; for example here with **Gambit** Scheme:

> The maximum number of arguments that can be passed to a procedure by the apply procedure is **8192**.

my bold emphasis; from: https://gambitscheme.org/latest/manual/#System-limitations (the link to this manual is broken as of 2025-08-15; however, this limitation can be also seen from the source code: [https://github.com/gambit/gambit/blob/6b898fc90c0a2842093d9c92cd6c30be329c4cea/lib/mem.h](https://github.com/gambit/gambit/blob/6b898fc90c0a2842093d9c92cd6c30be329c4cea/lib/mem.h#L64): #define ___MAX_NB_ARGS          8192)

Of course, finding the system limitations may easily become a cumbersome task in a specific Scheme dialect. Above limit was my showstopper in this dialect, because I need exactly - like in the working Chuz and Racket versions - 62500 arguments and not only 8192.

At first, because then I made a curious discovery.

With only 8192 arguments the (real) time ("wall clock") of the program to run was only **0.112 seconds**!?! So, theoretically having a program with (62500 arguments) / (8192 arguments) in 8 batches would then only take about 0.9 seconds?

Did you notice that 62500 multiplied with 16 is 1,000,000? I want exactly 1,000,000 (simple) characters in my string and write this string into one file in one take without further changes to this file.

Next I changed my Racket program and artificially limited the number of _apply_ arguments also to 8192. This program then took only **0.200** seconds to run! Slower than the Gambit version, but a tremendous jump into the right direction. So, there can be acceptable execution speed in Scheme land!

And: Racket allows you to have _define_'s inside another function, here _main()_, which makes refactoring (https://en.wikipedia.org/wiki/Code_refactoring) easier for me than doing it directly in Gambit Scheme. (Though after testing I changed this to local _let*_ expressions to be more functional and support translations
between Scheme dialects.)

I found another source of speed improvement: change everything possible to mutable vectors with _**make-vector**_: https://docs.racket-lang.org/reference/vectors.html#%28def._%28%28quote._~23~25kernel%29._make-vector%29%29: _This function takes time proportional to size._

This measure also helped a lot to improve execution speed. So, it hasn't been my string handling alone that prevented good execution speeds.

### Vectors in Scheme

This refers to item #2 of [My 5 best practices with Scheme dialects](#my-5-best-practices-with-scheme-dialects) from above:

> ... Also like most Lisp dialects, Scheme has a built-in _vector_ datatype. Whereas a list is built out of out of interlinked but still separate objects, a vector is a single object. ... The reason for a separate vector datatype is efficiency. ... Hence vector lookup is a constant-time operation, whereas list lookup is O(n). ...

from: https://docs.scheme.org/guide/arrays/

So, I ended up with 8 global (and dynamic) vectors for (62500 arguments) / (8192 arguments) in 8 batches - a very imperative programming style - in one of the first versions of my Gambit Scheme program: [source code old](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Gambit/random_streams_for_perf_stats%20-%20OK%2C%20superfast%208%20batch%20new%20solution.scm), but not in my final version: [source code new](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Gambit/random_streams_for_perf_stats.scm)

### Scheme's on Speed

(TBD)


<br/>

## Procedures or functions? (Procedures!)

After some literature studies I noticed a pattern in the Land of Scheme's:

- the constant talk about _procedures_

So, why not _functions_ like in many other places of _functional_ programming?

I think this has just to do with early, official definitions of Scheme. I take the "The REVISED REPORT ON SCHEME..." from 1978 (https://standards.scheme.org/early), on Page 3:

> Lambda-expressions evaluate to procedures. Unlike most LISP systems, SCHEME does not consider lambda-expressions ... to be a procedure. A lambda-expression only evaluates to a procedure. ... A lambda-expression must be "closed" (associated with an environment) to produce a procedure object. Evaluation of a lambda-expression performs such a closure operation.

**Closures** in my simple definition are function objects: https://en.wikipedia.org/wiki/Closure_(computer_programming) and above definition supports this idea in my opinion.

Specifically the Scheme report from 1978 contains a couple of references to **ALGOL**. For example in this ALGOL paper from 1971 (http://algol60.org/docs/Introduction%20to%20ALGOL%2060%20for%20those%20who%20have%20used%20other%20language.pdf) on page 5 under "1.5 Procedure" a procedure is just defined as:

> A procedure is a piece of program represented by a name. Execution of a procedure may require some parameters.

So, also there not a word on potential return value(s) from function calls.

#### Presentation of the history of Scheme from 2006

Ironically, this presentation by **Guy Steele**, one of the Scheme designers (https://en.wikipedia.org/wiki/Guy_L._Steele_Jr.):

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/The%20History%20of%20Scheme%2C%20Guy%20Steele%2C%20Sun%20Microsystems%20Laboratories%2C%202006.jpg)

from: http://jean.paul.roy.free.fr/JAOO-SchemeHistory-2006public.pdf

..does not mention term "procedure" once (or "procedural"), but eight time the term "function" and also eight time "functions".

<br/>

## The Larceny Benchmarks

#### 2024 benchmarks

(TBD)

#### The original Larceny Benchmarks

(TBD)

<br/>

### Scheme for the Java Virtual Machine (JVM)? (it's not looking good)

See from here: [Scheme dialects on the Java Virtual Machine (JVM)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM))

<br/>

### Brackets in Scheme dialects

While porting my Scheme program to [Gambit Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Gambit), I noticed that an expression like this:

```
(let ([...]
      [...])
     ...)
```

...to define local variables ("binding constructs") can only use round brackets ("parentheses") in Gambit:

```
(let ((...)
      (...))
     ...)
```

However, in the "Revised6 Report on the Algorithmic Language Scheme" from 2007 (http://r6rs.org/final/r6rs.pdf) under chapter _4.3.2. Pairs and lists_ square brackets have been officially introduced:

> List and pair data, representing pairs and lists of values (see section 11.9) are represented using parentheses or brackets. Matching pairs of brackets that occur in the rules of 〈list〉are equivalent to matching pairs of parentheses.

Even with latest version 4.9.6 Gambit won't let you compile _([...])_ due to "Ill-formed binding".

<br/>

### Two more Scheme dialects

Here I bring the attention to two more Scheme dialects, which I didn't test though, but may provide interesting features:

- [IronScheme](https://github.com/IronScheme/IronScheme) which "aims to be a R6RS conforming Scheme-like implementation for all .NET implementations and platforms", and
- [LIPS Scheme](https://lips.js.org/), which allows to "mix Scheme and JavaScript. You can access all JavaScript objects. You can also call any functions from Scheme."

<br/>

### Common Lisp & Scheme, a comparison

Here's an older but concise presentation of it: https://soft.vub.ac.be/~pcostanz/documents/scheme%20vs%20common%20lisp.pdf

CL = Common Lisp

##_end
