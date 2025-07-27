https://www.scheme.org/schemers/

---

<br/>

> Some say there are more implementations than applications.

from: https://scheme.fail/manual/loko.html

<br/>

## What Scheme dialects are still maintained?

After I implemented the "speed part" program in 4 different Scheme dialects, I made an overview table with the state of certain Scheme dialects:

![plot](./Scheme%20dialects%20-%20big%2C%20actively%20maintained.png)

..which is based on this listing as found under _Scheme Containers - Available implementations - Big, actively maintained_ at: https://containers.scheme.org/ (*)

However, this list is now outdated, which can be seen at best from my point of view with **Kawa**, a Scheme dialect for the Java Virtual Machine (JVM), see at ![Kawa](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM))

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
2. for **good execution speeds** with longer "sequences of data items" avoid lists as much as possible and use **vectors** instead; I'm sure that this applies to all Scheme dialects
3. in a targeted Scheme dialect get familiar with how to install **libraries**, mostly the Scheme SRFI's. There's a great chance that a library procedure, which is not included in an already installed (standard) library, can provide a (partly) solution to your problem. I learned that specifically documentation for installing libraries often sucks greatly in the Land of Scheme's!
4. **Racket** Scheme (https://racket-lang.org/) has the most "batteries already included" and is in average not the slowest Scheme dialect (though in average it's not the speediest dialect). This makes Racket the best Scheme dialect to check out first in my opinion. I think that for most hobby users Racket is just good enough, but may also need elaborate experimentation for satisfactory results
5. be carefull, with the exception of Racket Scheme, to use the usual Linux distribution installations (_$ sudo apt install ..._). Better download directly from GitHub ("<> Code" ---> "Download ZIP") and compile and install according to the given instructions. Otherwise you may end up with an installation with "no
batteries included". This happend to me with Gambit Scheme! And often the Github versions are newer.

These best pratices are mostly based on my experience with ![Racket](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket).

<br/>

## Features of the Scheme programming language

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Features%20of%20the%20Scheme%20programming%20language.png)

Here are these sources in text form:

- _An Introduction to Scheme and its Implementation_, Paul R. Wilson, 1996: https://doc.lagout.org/programmation/Lisp/Scheme/An%20Introduction%20to%20Scheme%20and%20its%20Implementation.pdf
- _Revised7 Report on the Algorithmic Language Scheme_, 2013: https://small.r7rs.org/attachment/r7rs.pdf
- _Common Lisp & Scheme, a comparison_, Pascal Costanza, 2006: https://www.p-cos.net/lisp/guide.html
- _AN INTRODUCTION TO FUNCTIONAL PROGRAMMING THROUGH LAMBDA CALCULUS_, Greg Michaelson, YYYY?: https://www.cs.rochester.edu/~brown/173/readings/LCBook.pdf

(TBD)

<br/>

## The Larceny Benchmarks

#### 2024 benchmarks

(TBD)

#### The original Larceny Benchmarks

(TBD)

<br/>

## Procedures or functions? (Procedures!)

(TBD)

## What they don't tell you in the Land of Scheme's at first

(TBD)

#### Scheme Surveys

(TBD)

## System limitations

(TBD)

### Brackets in Scheme dialects

(TBD)

### Two more Scheme dialects

(TBD)

##_end
