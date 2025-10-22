# Functional languages

<br/>

![plot](./Functional%20programming%20genealogy.png)

<br/>

Table of contents:

- [Two branches of Functional Programming (FP): pure and impure](#two-branches-of-functional-programming-fp-pure-and-impure)
- [Logic for Computable Functions (LCF)](#logic-for-computable-functions-lcf)
- [If You See What I Mean (ISWIM) and Pedagogic Algorithmic Language (PAL)](#if-you-see-what-i-mean-iswim-and-pedagogic-algorithmic-language-pal)
- [What functional programming language introduced the idea of "immutuable variables"?](#what-functional-programming-language-introduced-the-idea-of-immutuable-variables)
- [Functional languages on the Java Virtual Machine (JVM)](#functional-languages-on-the-java-virtual-machine-jvm)
- [So, who is mostly using functional programming?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#so-who-is-mostly-using-functional-programming)

---

# Two branches of Functional Programming (FP): pure and impure

I came to the conclusion that there must be two major branches of Functional Programming languages. The older one, going back to **Lisp** and including **Scheme** and its related dialects. And then there must be a different one that led to languages like **OCaml** and **Haskell** for example.

OCaml (since 1996) goes back to Caml (Categorical Abstract Machine Language), which has been implemented in 1984 (https://caml.inria.fr/resources/doc/faq/general.en.html), which means that Caml is about 6
years older than Haskell, which has been first published in 1990: https://dl.acm.org/doi/10.1145/1238844.1238856

And Caml goes back to ML (**Meta Language**), which was developed from 1973 to 1978: https://smlfamily.github.io/history/SML-history.pdf (https://smlfamily.github.io/history/)

<br/>

## Logic for Computable Functions (LCF)

Meta Language itself has its roots in (Stanford) LCF, a proof-checking program developed at Stanford University by **Robin Milner** in 1972, who later moved to Edinburgh University, where ML has been created: https://www.cl.cam.ac.uk/archive/mjcg/papers/HolHistory.pdf:

- ML has been built with Lisp: "(5) Edinburgh LCF, including the ML interpreter, was implemented in Lisp."
- "The original LCF team at Stanford consisted of Robin Milner, assisted by Whitfield Diffie (from whom Milner learnt Lisp)." Whitfield Diffie is one of the creators of public-key cryptography.

<br/>

## If You See What I Mean (ISWIM) and Pedagogic Algorithmic Language (PAL)

Another root of ML was ISWIM, which has never been implemented directly ("part programming language and part program for research" from "The next 700 programming languages": https://dl.acm.org/doi/10.1145/365230.365257 (1)), but later as PAL at the MIT in 1967 and was first implemented in Lisp (2) and then later in BCPL ("Basic Combined Programming Language") (2), a precursor of B, which then became **C** (https://en.wikipedia.org/wiki/BCPL):

- "THE HISTORY OF STANDARD ML -- IDEAS, PRINCIPLES, CULTURE": https://smlfamily.github.io/history/ML2015-talk.pdf (3)
- https://en.wikipedia.org/wiki/PAL_(programming_language)
- https://www.softwarepreservation.org/projects/PAL/Pal-ref-man.pdf/view (2)
- "The mechanical evaluation of expressions" - 1964 by Peter J. Landin; Thierry Vilmart, Emil Karlén, 2008: https://pdfs.semanticscholar.org/4969/5d3cbf0d03e8ab28a738108271993b5ac05e.pdf

ISWIM was conceived by **Peter Landin**, UK (1) and presented in August 1965. He was one of the first to state that "some forms of expression used in current programming languages can be modelled in Church's lambda-notation" as published in 1964: https://academic.oup.com/comjnl/article-abstract/6/4/308/375725 (4)

ISWIM was a language that broke with some traditions of the ALGOL family of programming languages:

![Some History of Functional Programming Languages](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Some%20History%20of%20Functional%20Programming.png)

From: "Some History of Functional Programming Languages", **David Turner**, 2012 (https://en.wikipedia.org/wiki/David_Turner_(computer_scientist)): https://www.cs.kent.ac.uk/people/staff/dat/tfp12/tfp12.pdf (5)

And not only ALGOL, but also Lisp: _...Lisp has some dark corners, especially outside "pure LISP,"..._ (1). But ISWIM was not the programming language which introduced the concept of "immutuable variables" (and also not PAL).

This leaves this question to me: 

## What functional programming language introduced the idea of "immutuable variables"?

I think it was **SASL** (St Andrews Static Language) from 1972/1973 (5) by David Turner:

> So over the Easter vacation in 1973 I wrote a compiler from SASL to SECD machine code and an interpreter for the latter, all in BCPL. The code of the first version was just over 300 lines — SASL was not a large language.

I'm also thinking now that SASL was the first **pure** functional programming language, first in teaching (and years later at Burroughs Corporation in Austin, Texas, at least for research purposes (5)), and innovated over Lisp and also PAL and is based on Landin's SECD (Stack, Environment, Control, Dump) state transition machine (4).

SASL only had a _let_ definition, but not _letref_ like in LCF/ML (3).

I shortly checked on other emerging programming languages in the UK in the 60ies and early 70ies which have been influenced by ISWIM, like **POP-2** (https://en.wikipedia.org/wiki/POP-2) and precursors. But it seems to me that these are not functional programming languages (at least POP-2 is not), though it should be mentioned that POP-2 played a role in the creation of **logic programming**: https://en.wikipedia.org/wiki/Planner_(programming_language)

I just remembered a reference expression somewhere in my program versions so far and found it in the OCaml program, which defines two variables, an integer counter and a growing string, with _let ... ref ... in_ expressions. This brings me to this idea: the younger branch of functional programming languages seems to have developed into two branches:

- a pure one, like Haskell, and
- a non-pure (impure) one, like OCaml for example

<br/>

## Functional languages on the Java Virtual Machine (JVM)

- ![Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure)
- ![Scheme dialects on the JVM](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Scheme%20dialects%20on%20the%20Java%20Virtual%20Machine%20(JVM))
- [Common Lisp on the JVM with Armed Bear Common Lisp (ABCL)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp/README.md#common-lisp-on-the-java-virtual-machine-jvm-with-armed-bear-common-lisp-abcl)

<br/>

## So, who is mostly using functional programming?

Logically, an industry where a lot of functions can be deployed. Functions which often can stay "pure", that is without "annoying" **side effects** (https://en.wikipedia.org/wiki/Side_effect_(computer_science)) like throwing exceptions or input/output operations (I/O's). Or in other words:

> [!NOTE]
> A mathemtical function has no side effects.

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/The%20functional%20programming%20pattern.png)

from: https://www.toptal.com/android/functional-reactive-programming-part-1

<br/>

Using pure functions has benefits like:

- _Pure functions are easy to reason about_
- _Pure functions are easy to refactor_
- _Laziness: only evaluate when I need the output_
- _Cacheable results: same answer every time; "memoization"_ (to speed up computer programs)
- _No order dependencies: I can evaluate them in any order I like_
- _Parallelizable: "embarrassingly parallel"_

from: Functional Design Patterns, Scott Wlaschin, 2014: https://docs.huihoo.com/programming-language/functional-design-patterns.pdf -- 249 slides!

The historical background of Clojure gives a hint, that is when Nubank from Brazil acquired **Clojure** inventor Rich Hickey's software company Cognitect in 2020: https://building.nubank.com/clojures-journey-at-nubank-a-look-into-the-future/

Apparently, it's the financial industry which can profit the most from functional programming because of a relativ absence of side effects:

2019, _**Functional programming reaches for stardom in finance**_

https://www.risk.net/risk-management/6395366/functional-programming-reaches-for-stardom-in-finance

<br/>

In the technical space I still have to see the industrial corner which is prominently using functional programming.

You may also have a look at [Array-oriented languages](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/03%20-%20array-oriented%20languages#array-oriented-languages).

<br/>

##_end
