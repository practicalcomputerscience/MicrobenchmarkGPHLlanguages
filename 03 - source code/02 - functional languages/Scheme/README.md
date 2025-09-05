Table of contents:

- [What Scheme dialects are still maintained?](#what-scheme-dialects-are-still-maintained)
- [My 5 best practices with Scheme dialects](#my-5-best-practices-with-scheme-dialects)
- [Features of the Scheme programming language](#features-of-the-scheme-programming-language)
- [What they don't tell you in the Land of Scheme's at first](#what-they-dont-tell-you-in-the-land-of-schemes-at-first)
- [Scheme Surveys](#scheme-surveys)
- [System limitations](#system-limitations)
- [Vectors in Scheme](#vectors-in-scheme)
- [Scheme's on Speed](#schemes-on-speed)
- [Re-installation of Gambit Scheme](#re-installation-of-gambit-scheme)
- [Bigloo Scheme](#bigloo-scheme)
- [Why do I not publish my Chez Scheme results?](#why-do-i-not-publish-my-chez-scheme-results-no-easy-standalone-excutable-program)
- [Size of executables](#size-of-executables)
- [Functional error handling](#functional-error-handling)
- [Procedures or functions?](#procedures-or-functions-procedures)
- [The Larceny Benchmarks](#the-larceny-benchmarks)
- [2024 benchmarks](#2024-benchmarks)
- [FIB -- A classic benchmark, computes fib(n) inefficiently](#fib----a-classic-benchmark-computes fibn-inefficiently)
- [Scheme for the Java Virtual Machine (JVM)?](#scheme-for-the-java-virtual-machine-jvm-its-not-looking-good)
- [Brackets in Scheme dialects](#brackets-in-scheme-dialects)
- [Two more Scheme dialects](#two-more-scheme-dialects)
- [Common Lisp & Scheme, a comparison](#common-lisp--scheme-a-comparison)

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

### Scheme Surveys

These pages detail various differences between Scheme implementations: https://docs.scheme.org/surveys/

Also see _How do you import SRFI-1 (the list library) in scheme?_ at: https://rain-1.github.io/scheme-srfi-1.html

<br/>

## System limitations

Regardless of the Scheme dialect you choose, make sure you have noticed its limitations before using it; for example here with **Gambit** Scheme:

> The maximum number of arguments that can be passed to a procedure by the apply procedure is **8192**.

my bold emphasis; from: https://gambitscheme.org/latest/manual/#System-limitations This limitation can be also seen from the [source code](https://github.com/gambit/gambit/blob/6b898fc90c0a2842093d9c92cd6c30be329c4cea/lib/mem.h#L64):

``` 
#define ___MAX_NB_ARGS          8192
```

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

When doing it right Scheme programs can be competitively fast. Here's a list of program execution times:

- **Bigloo** Scheme in 1 batch: 0.029 seconds for the whole program with version bigloo v.4.6a and compiling with activated switches _-call/cc_ for procedure _call-with-current-continuation_ and _-O6_ for best opimization
- **Gambit** Scheme in 1 batch: 0.034 seconds with version gsc v.4.9.6
- **Gambit** Scheme in 8 batches: 0.040 seconds with version gsc v.4.9.6
- **CHICKEN** Scheme (https://www.call-cc.org/) in 1 batch: 0.058 seconds with version csc v.5.4.0 and compiling with optimization level switch _-O5_
- **Racket** Scheme in 1 batch: 0.103 seconds with version Racket v8.17

All times have been measured with command: _$ sudo perf stat -r 20 < program name >_ and thus represent the averages of 20 runs for each program.

The **Gambit** program in its first version with the 8-batch algorithm is applying a _string-append_ for each batch and where each batch has the maximum number of 8192 _apply_ arguments:

```
... (apply string-append (vector->list <my_vector>)) ...
```

The **Racket** solution is using one expression for one batch:

```
... (string-join (vector->list <my_vector>) "") ...
```

...but all (other) lists have been refactored to vectors when feasible (actually, there was only one left at last).

In its first version, the **Gambit** source code file (for an older Gambit version) could be taken by the **CHICKEN** compiler almost unchanged; I only had to add two standard library imports (the Gambit program is OK with its default environment without any extra imports) and change the creation of a first random integer number,
including some random seeding.

The Gambit dialect is a bit curious since it's the only one of the four dialects which doesn't need any _import_ or other (global) declarations. However, its binary, standalone executable is a fat one as shown below at [Size of executables](#size-of-executables)

However, I think that this 8-batch solution, though helping me to see that there can be real speed in the Land of Scheme's, is not a good one, because it breaks with the usual algorithm as implemented in all other programming languages and dialects so far and also now with my list-free Racket solution.

So I explored the possibilities of **CHICKEN** Scheme to tap into non-standard library procedures to replace this notorious expression:

```
... (apply string-append (vector->list <my_vector>)) ...
```

And I found it with _string-concatenate_ from **SRFI 152**: http://api.call-cc.org/5/doc/srfi-152:

```
(import (chicken random) ; for pseudo-random-integer
(chicken file) ; for file-exists?
(srfi-152)) ; for string-concatenate
... (string-concatenate (vector->list <my_vector>)) ...
```

The harder part was to install SRFI 152 into my (standard) CHICKEN build:

```
$ cd ./CHICKEN_Scheme/chicken-5.4.0 # my local installation dir
$ sudo ./chicken-install srfi-152 # wait! this command may take some time
```

This 1-batch-CHICKEN solution is only a bit slower (0.069 seconds for a 20 run average) than its 8-batch variant (0.055 seconds), but can't beat Gambit with 0.040 seconds in its old 8-batch variant.

This begged the question: what about a speedy 1-batch solution with Gambit Scheme?

I noticed that the _string-concatenate_ procedure wasn't working in the Gambit REPL (start this REPL with: _$ gsi_):

```
(define s "abc")
(string-concatenate (list s s s))
```

However, this is an official example from [string-concatenate](https://gambitscheme.org/latest/manual/#index-string_002dconcatenate) and should evaluate to "abcabcabc" without any special imports!

Only then I slowly got the idea that my Gambit installation is missing libraries that should be automatically included with a proper Gambit installation: https://practical-scheme.net/wiliki/schemexref.cgi?Gambit

### Re-installation of Gambit Scheme

So, I deinstalled my old Gambit installation (that is reverting an installation with _$ sudo apt install gambc_ with command _$ sudo apt-get remove gambc_), zipped the official repository: https://github.com/gambit/gambit, unzipped it on my Ubuntu machine and installed it like this:

```
$ ./configure --enable-single-host  # only this option worked OK with my system
$ make
$ make check
$ make modules  # optional, but I did this too
$ sudo make install
```

The installation documents emphasize the importance of the _--enable-single-host_ switch for speedy Gambit programs. However, at least one of the other switches caused problems with my installation and so I started successfully over with above sequence of commands.

Finally, in my _.bashrc_ file I added the two paths to both Gambit programs, gsi (REPL) and gsc (compiler), for convenience. And suddenly, also expressions _(define s "abc"); (string-concatenate (list s s s))_ worked as they should in the Gambit REPL!

By the way: this is also an installation procedure similar to my:

- CHICKEN installation: https://code.call-cc.org/ + https://code.call-cc.org/githtml/chicken-core/chicken-5/files/README.html and my
- Bigloo installation: https://www-sop.inria.fr/mimosa/fp/Bigloo/download.html

<br/>

> **Lesson learned**:
it can matter a lot what Scheme version has been built exactly how and how it has been installed specifically, and to some extent how source code files are compiled

<br/>

Now with the usual Gambit "batteries" included I tried my luck with the _string-concatenate_ procedure:

```
... (string-concatenate (vector->list <my_vector>)) ...
```

Bingo! This program version is now my second fastest in the Land of Scheme's! But why only second fastest? Well, because of:

### Bigloo Scheme

At last I ported my program to Bigloo Scheme (https://www-sop.inria.fr/indes/fp/Bigloo/). And there, notorious expression:

```
(apply string-append (vector->list <my_vector>))
```

..works like a charm!

<br/>

### Why do I not publish my Chez Scheme results? (no easy standalone excutable program!)

Racket, Gambit, CHICKEN and Bigloo Scheme all have two nice features in common:

- they are more or less easy to install (specifically Racket) or easy to build with the common triple jump of: _$ ./configure; make; sudo make install_
- but what counts more: these dialects, though they are not the only ones, actively support compiling to a standalone executable program, a thing Chez Scheme obviously tries to avoid (I tried two 3rd party script solutions from GitHub to no avail). It's not an essential feature, but all other programs have it, including the Common Lisp program

<br/>

### Size of executables

CHICKEN and Bigloo Scheme's compile to very small binary executables:

Scheme dialect | size of executable program in bytes | compilation command
--- | --- | ---
Gambit | 10,410,664 | _$ gsc -exe ./< program name >.scm_
Racket | 2,140,291 | _$ raco exe ./< program name >.rkt_
CHICKEN | 39,984 | _$ csc -O5 ./< program name >.scm_
Bigloo Scheme | 24,120 | _$ bigloo -call/cc -O6 ./<program name>.scm -o < program name >_

<br/>

## Functional error handling

The Racket program ([source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/eb7b71b5bc9a718db959ed74ab29dacb6f191ee7/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket/random_streams_for_perf_stats.rkt#L75)) has a nice procedure to write a final string into a file, including a functional approach to
error handling:

```
(define (write_to_file filename content)
  (with-handlers ([exn:fail? (lambda _ #f)]) ; Return #f to indicate failure
    (call-with-output-file filename
      (lambda (out)
        (display content out)) ; Write the string to the file
      #:exists 'can-update) ; Replace the file if it already exists, but not when permission is "Read-Only"
    #t))  ; Return #t to indicate success
```

By the way: these are little programming tasks where "prompt engineering" can help, that is in my case just MS Bing because there's enough Racket source code out there. However, my first solutions for the Gambit and CHICKEN dialects looked like the code below, because above Racket solution doesn't work with these dialects:

```
(define (write_to_file filename content)
  ; does file exist? --> if yes, delete it first, otherwise call-with-output-file will throw an exception!
  (if (file-exists? filename)
    (begin
      (delete-file filename)))
  (call-with-output-file filename
    (lambda (out)
      (display content out)))
  ; final test and return value true or false as a very simple test
  (file-exists? filename))
```

With _file-exists?_ I have at least some primitive return value for the calling function for "quality checking". At least I was happy to have a little bit more than nothing for the error handling after switching from Racket. This can happen, at least at the beginning, when a dialect or language comes "without many batteries included". However, I thought it would be a good thing to have something similar to the Racket solution for the Gambit and CHICKEN versions too:

- when there's a problem to write _content_ to _filename_, procedure _write_to_file_ should return _#f_ (the false value), and
- when there was success with writing _content_ to _filename_, procedure _write_to_file_ should return _#t_ (the true value)

My more functional Gambit solution now looks like this ([source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/eb7b71b5bc9a718db959ed74ab29dacb6f191ee7/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Gambit/random_streams_for_perf_stats.scm#L79)):

```
(define (write_to_file filename content)
  (call-with-current-continuation
    (lambda (exit)
      (with-exception-handler
        (lambda _ (exit #f))
        (lambda ()
          (begin
            (call-with-output-file filename
              (lambda (out)
                (display content out)))
           #t))))))
```

Be a little bit careful with procedure _call-with-current-continuation_. If not done correctly, your computer may end up in an endless loop where only pressing the reset button may stop it! (This happened to me.)

**Exception handling** is probably one of those fields where Scheme dialects tend to have their own, **non-portable solutions** and consequently above solution is not working with CHICKEN Scheme (and vice versa), however this solution, which doesn't need any extra imports (~), works with CHICKEN Scheme ([source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/eb7b71b5bc9a718db959ed74ab29dacb6f191ee7/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/CHICKEN/random_streams_for_perf_stats.scm#L80C1-L92C15)):

```
(define (write_to_file filename content)
  (call-with-current-continuation  ; equal to call/cc
    (lambda (return)
      (handle-exceptions
        ; ...
        _
        (return #f)
        (call-with-output-file filename
          (lambda (out)
            (display content out)))
        #t))))
```

Here are two older documents for CHICKEN's exception handling, sources with some examples on which my solution above is also based on:

- https://api.call-cc.org/5/doc/srfi-34: _Chicken implements SFRI-12, a withdrawn SRFI that is nonetheless a more featureful exception system than the one implemented in SRFI-34. If you don't need SFRI-34 for portablility to some other scheme, there is no reason to use this egg over the SFRI-12 functionality chicken provides out of the box._ (_SFRI-12_ should be named SRFI-12 apparently...)
- https://api.call-cc.org/5/doc/chicken/exceptions#def:handle-exceptions:

For **Bigloo** Scheme I could copy procedure _write_to_file_ in the Gambit version without any changes, but you have to consider the _-call/cc_ compilation switch; see here from the PDF manual (search in this manual): https://www-sop.inria.fr/mimosa/fp/Bigloo/doc/bigloo.pdf

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

..does not mention term "procedure" once (or "procedural"), but eight times the term "function" and also eight times term "functions".

<br/>

## The Larceny Benchmarks

The Larceny Benchmarks in their latest and original versions from 2017 for portable R6RS and R7RS top-level programs:

- latest R7RS version: https://www.larcenists.org/benchmarksAboutR7.html + https://github.com/larcenists/larceny/tree/master/test/Benchmarking/R7RS
- (older) R6RS version: https://www.larcenists.org/benchmarksAboutR6.html + https://github.com/larcenists/larceny/tree/master/test/Benchmarking/R6RS

From here: https://larcenists.org/download.html you can download for example file _larceny-1.3-src.tar.gz_ which includes the basic, original benchmark source code files for the R6RS benchmarks inside directory: _**.../larceny-1.3-src/test/Benchmarking/R6RS/src/**_

**Larceny** itself is (or was?) a Scheme dialect since the 90ies: https://larcenists.org/

I think that these benchmark source code files - even when most probably not running 1:1 for a specific Scheme dialect in a modern version - could provide great inspiration for your own Scheme coding efforts. And if it's only for looking up key words inside them. I even found an example for procedure _call-with-current-continuation_ (alternatively named _call/cc_) in file _fibc.scm (...FIB using first-class continuations...)_.

### 2024 benchmarks

Here are the latest, official benchmark results from 2024, which provided some orientation for my own Scheme dialect selections:

- https://ecraven.github.io/r7rs-benchmarks/
- https://github.com/ecraven/r7rs-benchmarks

A cut from there (see the linked web page for the complete benchmark table):

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/The_Larceny_Benchmarks_2024.png)

Notice the cells in magenta color. These are benchmarks that didn't succeed for various reasons, a phenomenon already observed by the original benchmark creators: https://www.larcenists.org/benchmarksAboutR7.html

> The R7RS (small) standard does not require implementations to provide all of the R7RS standard libraries, and some implementations are unable to run some of the benchmarks for
other reasons as well; furthermore, our benchmarking script gives up on any benchmark that takes more than an hour to run.

Or as stated in 2024: https://ecraven.github.io/r7rs-benchmarks/

> Many of these implementations do not fully implement R7RS, but instead there is a bit of "shim" code. You can find this by looking at _src/< Name >-prelude.scm_ and _src/< Name >-postlude.scm_, to see which changes are necessary. Some changes are also made by the _bench_ script, especially relating to _imports_.

Beware that in order to be able to run these tests, at least for CHICKEN and Racket dialects, extra libraries or packages have to be installed: https://github.com/ecraven/r7rs-benchmarks/tree/8ed2d74acc8828f91c5cb12afb41f6b8fbd403ce#notes-for-specific-implementations

For **CHICKEN** you may also need to install with administrator rights: _$ sudo ./chicken-install vector-lib r7rs_

For **Racket** I had to change into the right Racket directory before installing with administrator rights: _$ sudo ./Racket/racket/bin/raco pkg install --scope installation r7rs_

Download the whole GitHub repository ("<> Code" ---> "Download ZIP") to your Linux machine and unzip it in a test directory. Make sure that the related Scheme compiler can be found at your (Bash) shell - or provide the paths to the Scheme compilers like described here (if you didn't use _$ make install_ for example): https://github.com/ecraven/r7rs-benchmarks/tree/8ed2d74acc8828f91c5cb12afb41f6b8fbd403ce#path-to-executables

Running benchmark program _fib.scm_ for **Racket** Scheme looks like this in my system:

```
$ ./bench "racket" "fib"

Testing fib under Racket
Including prelude ... /Scheme_2024_benchmarks/r7rs-benchmarks_2024/src/Racket-prelude.scm
Compiling...
racket_comp /tmp/r7rs-benchmarks/Racket/fib.scm /tmp/r7rs-benchmarks/Racket/fib.scm
raco make /tmp/r7rs-benchmarks/Racket/fib.scm
Running...
time racket /tmp/r7rs-benchmarks/Racket/fib.scm
Running fib:40:5
Elapsed time: 2.276 seconds (2.276) for fib:40:5
+!CSVLINE!+racket-8.17/r7rs+chez,fib:40:5,2.276

real 0m2,498s
user 0m2,455s
sys 0m0,042s
$
```

For **CHICKEN** Scheme I had to modify the _bench_ script to (don't forget to do: _$ chmod 755 bench_new_ for example):

```
CHICKEN_CSC=${CHICKEN_CSC:-"csc"}
CHICKEN_CSI=${CHICKEN_CSI:-"csi"}
```

...to run it like this:

```
$ ./bench_new "chicken" "fib"
...
real 0m19,943s
...
```

For **Bigloo** there were two warnings, though an error has not been indicated with _(display "ERROR: returned incorrect result: ")_ as seen in: _/tmp/r7rs-benchmarks/Bigloo/fib.scm_:

```
$ ./bench_new "bigloo" "fib"
...
*** WARNING: define
...
*** WARNING: top-level
...
real 0m1,773s
...
```

For **GambitC** I had to tinker again with my _bench_new_ script:

```
GAMBITC=${GAMBITC:-"gsc"}
```

...to run it like this:

```
$ ./bench_new "gambitc" "fib"
...
real 0m1,899s
...
```

I start to notice a pattern here: the Bigloo version is the fastest again.

But maybe this benchmark is specifically unfair to **CHICKEN** Scheme, because with my string-heavy microbenchmark program in hand-optimized versions, the CHICKEN version only needs 56% of the Racket version's execution time.

### FIB -- A classic benchmark, computes fib(n) inefficiently

As a comparison to my microbenchmark, here the official 2024 benchmark ranking for the _fib_ program with "01" being the the fastest rank:

- 01 -- Bigloo (in version 4.5b)
- 02 -- GambitC (4.9.5)
- 05 -- Racket (8.13)
- 15 -- CHICKEN (5.3.0)

My own benchmarks confirm this ranking:

- 01 -- Bigloo (4.6a)
- 02 -- Gambit (4.9.6)
- 03 -- Racket (8.17)
- 04 -- CHICKEN (5.4.0)

In 2015 the original creators published a little rant about benchmarks in general: https://www.larcenists.org/bmcrock.temp.html

> ...Many of our benchmarks test only a few aspects of performance. ... Such benchmarks are not so good if your goal is to predict how well an implementation will perform on "typical" Scheme programs. ... The performance of a benchmark, even if it is derived from a real program, may not help to predict the performance of similar programs that have different hot spots. ...

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

<br/>

##_end
