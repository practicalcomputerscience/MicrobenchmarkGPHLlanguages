# The world as processes and events

## What could have been the driver of the relative revival of Functional Programming (FP) in the last 20 years?

The word "revival" maybe way too strong when taking the current TIOBE Index as of June 2025 (https://www.tiobe.com/tiobe-index/) as a benchmark:

- the top native FP languages are **Lisp** at position #27 and **Haskell** at position #29

Further FP languages in the Top 50: **ML** (Meta Language, #45), **Elixir** (Erlang family, #46) and **Erlang** (#50).

Clojure, F# (Microsoft) and OCaml are only listed alphabetically at positions #51 to #100 (with more FP languages listed there).

And multi-paradigm language **Scala** (with FP on top of OOP, Object-Oriented Programming), apparently too often misused with its bells and whistles of FP, is now sitting only at #31.

Here are some other popularity charts: https://trends.stackoverflow.co/?tags=clojure%2Cocaml%2Cf%23

<br/>

Coming back to the question above, I think the answer lies in the desire, and then the requirement, to have a reliable and not too complicated implementation of **concurrency** within one
programming language. Here explained for Clojure:

**Concurrent Programming**: https://clojure.org/about/concurrent_programming (*)

> Today’s systems have to deal with many simultaneous tasks ... Doing so with threads can be very difficult due to the complexities of synchronization. Clojure simplifies multi-
threaded programming in several ways.

Maybe a minor factor lies in the functional approach to [Error handling, exception handling and "Quality control"](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main?tab=readme-ov-file#error-handling-exception-handling-and-quality-control), which may have led to a generally renewed interest in FP.

<br/>

## Object-Process Methodology (OPM)

In 1995 the Object-process Analysis (OPA) concept was published: "Maintaining the Balance Between System Structure and Behaviour": https://academic.oup.com/logcom/article-abstract/5/2/227/1008599

> The underlying observation of the OPA paradigm is that every thing in the universe of interest is either an object or a process, and that a process is not necessarily a method of a single
object class.

OPA later became the **Object-Process Methodology (OPM)** and is now established in ISO Standard 19450: https://www.iso.org/standard/84612.html

So, apparently, the role of objects, including their individual behaviors, has become less important in the last 20 years or so and the role of processes, and events, became more important in
average. And a computer system's answer to processes and events is to facilitate (safe) concurrency.

But why is then the overwhelming majority of the Top 20 languages still sitting in the OOP camp, also often being confronted with concurrency requirements?

Two factors are relevant here from my point of view:

1/ it's somehow evident that for the big majority of human coders (implicitly) applying OOP instead of FP is just easier and faster to get a working computer program done

2/ old (OOP) languages now also support concurrency, for example as Clojure writes (*):

> ... Clojure does not replace the Java thread system, rather it works with it. Clojure functions are java.util.concurrent.Callable, therefore they work with the Executor framework etc.

The _java.util.concurrent_ package was introduced with JDK (Java Development Kit) version 5, introduced in 2004: https://en.wikipedia.org/wiki/Java_version_history#Java_SE_5

<br/>

##_end
