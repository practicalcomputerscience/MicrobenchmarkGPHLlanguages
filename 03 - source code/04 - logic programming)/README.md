2025-11-05: work in progress

# Mercury

https://mercurylang.org/index.html

Mercury is a real hit in my opinion and should be way more popular!

> Mercury is based on the paradigm of **purely declarative programming**...
> After stripping away the declarations of a Mercury program, the syntax of the remaining part of the program is mostly compatible with **Prolog** syntax.

from:

- https://mercurylang.org/information/doc-latest/mercury_reference_manual/Introduction.html#Introduction
- https://mercurylang.org/about.html

---

Table of contents:
- [Concepts of Mercury](#concepts-of-mercury)
- [Difference between logic programming and declarative programming](#difference-between-logic-programming-and-declarative-programming)
- [How I found Mercury](#how-i-found-mercury)
- [Installation tips](#installation-tips)
- [Selected features of Mercury](#selected-features-of-mercury)

<br/>

---

## Concepts of Mercury

Even though not broadly known, I have the positive impression that the Mercury programming language,
started in [1995](https://mercurylang.org/information/doc-latest/mercury_reference_manual/index.html#SEC_Contents),
is in a very mature state now. Although, it's development is still ongoing as its GitHub repository show: https://github.com/Mercury-Language/mercury

This "The Mercury Programming Language" [presentation](https://paul.bone.id.au/pub/pbone-2015-mercury/) from 2015 says this:

> Mercury is a purely declarative logic/functional programming language.
> 
> Mercury looks like Prolog, but it feels like strict Haskell or pure OCaml
> 
> Purely declarative programs have no side effects. If a predicate has an effect, it has to be reflected in its argument list.
> 
> Mercury has a strong, static type system similar to Haskell's.

The **OCaml** reference is true in my opinion. Knowing some [OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) helps with understanding and developing Mercury programs.

Also helpful was my [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#clojure) program,
specifically with the "pure" style, since Clojure, in contrast to Ocaml, also adheres to **strictly immutable data structures**.

<br/>

## Difference between logic programming and declarative programming

MS Bing Copilot gave me this answer, as part of a bigger answer (on 2025-11-03):

> Logic programming is a specific kind of declarative programming that expresses programs as logical facts and rules and computes by logical inference.
> 
> Declarative programming is a broader paradigm that emphasizes describing what the program should achieve rather than how to do it;
> logic programming, functional programming, and SQL-style query languages are all examples of declarative styles.

<br/>

## Installation tips

(TBD)

<br/>

## How I found Mercury

Here's how I tumbled over the Mercury programming language accidentally:

(TBD)

<br/>

## Selected features of Mercury

Mercury has a dedicated **string builder**: https://github.com/Mercury-Language/mercury/blob/fca4505501852e5feda0734ff6c5ec6ac02bc638/library/string.builder.m

..which makes the [speed part](TBD) of this program competitively fast with about 77 milliseconds execution time!

See also these official "Raw benchmark times" in milliseconds:

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

##_end
