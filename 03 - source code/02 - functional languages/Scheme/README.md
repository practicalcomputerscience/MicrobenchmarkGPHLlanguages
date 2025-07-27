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

However, this list is now outdated, which can be seen at best from my point of view with **Kawa**, a Scheme dialect for the Java Virtual Machine (JVM), see the related chapter below: [Kawa](#kawa-scheme)

My defintion of "maintained" was straightforward: has there been some update in the last 12 months?

Sources for 10 out of these 16 Scheme dialects - not all of them are "big" - have been updated in the last 12 months roughly according to my counting and 15 within the last three years - with the exception of Kawa.

Maintaining a computer programming language is important from my point of view (same source from above (*)):

> Many (if not most) implementations keep working for years after active maintenance has ended, requiring few if any patches.

This can be true or not, I've made both experiences:
- OCaml for the Java Virtual Machine (JVM) from 2015 for example still runs fine: (TBD)
- the last update of Kawa Scheme (for the JVM) is much younger and still I wasn't able to run the _make_ process without errors, see below (TBD), which has then has become my showstopper with Kawa

## My best practices with Scheme dialects

(TBD)

These best pratices are based on my experience with ![Racket](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket)

## Features of the Scheme programming language

(TBD)

## The Larceny Benchmarks

(TBD)

## Kawa Scheme

https://www.gnu.org/software/kawa/index.html

(TBD)

## Scheme dialects on the Java Virtual Machine: JScheme and the JSchemePlus hack

(TBD)

##_end
