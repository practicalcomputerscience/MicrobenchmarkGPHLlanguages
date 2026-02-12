# Lines Of source Code (LOC): verbosity

LOC is here understood as Source Lines Of Code or SLOC.

Table of contents:

- [Idea of this page](#idea-of-this-page)
- [LOC ranking list](#loc-ranking-list)
- [Number of user defined functions](#number-of-user-defined-functions)
- [Ranking popular programming languages by density](#ranking-popular-programming-languages-by-density)

<br/>

## Idea of this page

This ranking list is arguably even more controversial than the ranking based on [execution times](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages?tab=readme-ov-file#the-1-second-execution-time-limit).

However, even with my limited knowledge I tried to make _idiomatic_ source code in every programming language. I think that naivety can even be a help here.

On the other side, there _are_ programs with more **lines of code** (LOC) than actually needed, with my Ada version being the most prominent example from my point of view:

> The true reason why my Ada program was so helpful was because of my incompetence! Originally, I wanted to use the Strings Edit library ..., but I was not able to figure out within an acceptable amount of time how to use such a third party library for Ada. Consequently, my Ada program has the second most lines of source code as of 2026-02-11...

from: [AI experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/README.md#ai-experiments)

<br/>

## LOC ranking list

The results of this list are [script-based](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/10%20-%20Lines%20Of%20source%20Code%20(LOC)%3A%20verbosity/lines_of_source_code_count.pl): _$ perl lines_of_source_code_count.pl random_bitstring_and_flexible_password_generator.< ... >_

programming language | LOC | comments | date
--- | --- | --- | ---
Inko              | 232 | compiled, very young language: lots of user-defined functions; also a construct is used to avoid repeated, individual access to array elements _x.get(i).or_panic_ and _x.get(i - 1).or_panic_ for exe speed reasons | 2026-01-24
Ada               | 229 | compiled, very mature language: lots of declarations and type definitions; still keeping the low level character copying from little strings into the big strings for performance reasons | 2026-01-26
Eiffel (Liberty)  | 222 | compiled, very mature language: needs some user-defined functions; since programs are collections of classes, this is causing some formal overhead | 2026-01-24
Standard ML (MLton) | 216 | compiled, very mature language: lots of user-defined functions
Mercury           | 194 | compiled, mature language: lots of user-defined functions
Roc               | 191 | compiled, very young, pure functional programming language: needs some user-defined functions
V                 | 179 | compiled, very young programming language: needs some user-defined functions | 2026-01-24
Fortran           | 164 | compiled, very mature language: some user-defined functions may be needed when not using the inofficial standard library
Gleam | 162 | interpreted, very young language (together with Erlang and JavaScript ecosystem):  lots of user-defined functions
C                 | 147 | compiled, very mature language | 2026-01-11
C3                | 144 | compiled, very young language: DStrings (dynamic strings) are (still) missing some functions that are available for Strings (fixed length) => some shuffling between DStrings and Strings | 2026-01-26
Picat (old B-Prolog) | 142 | interpreted, mature language: lots of user-defined functions 
SWI Prolog        | 133 | compiled, very mature language: lots of user-defined functions, even though SWI Prolog is a Prolog system with "batteries included"
OCaml             | 131 | compiled, mature language
C++               | 131 | compiled, very mature language; memory-safe source code can be very well more concise and compiled to a bit faster program compared to C | 2026-01-15
Lua               | 130 | compiled, very mature language
Perl 5            | 130 | interpreted, very mature language: verbosity with if-then-else form of error handling when writing to files | 2026-01-24
Zig               | 129 | compiled, young language still going through major changes
Odin              | 128 | compiled, young language, which is more effective in its efforts to modernize C than C3 in terms of verbosity
Go                | 127 | compiled, mature language
Rust              | 126 | compiled, mature language
Bigloo Scheme     | 124 | compiled, mature language
FreeBASIC         | 122 | compiled, mature language
Chapel            | 121 | compiled, mature language
PHP               | 118 | usually interpreted, very mature web programming language | 2026-02-11
D                 | 115 | compiled, very mature language; memory-safe source code can be easily more concise than an equivalent program in C++ | 2026-01-21
Swift             | 114 | compiled, mature language | 2026-01-26
Common Lisp       | 114 | compiled, very mature language
TypeScript        | 113 | interpreted, JIT (Just-In-Time) compiled, or AOT (Ahead-Of-Time) compiled, very mature language
Crystal           | 111 | compiled, young language
Racket Scheme     | 110 | compiled, mature language
Kotlin            | 109 | interpreted, young language (together with Java ecosystem)
C#                | 109 | interpreted, mature language (together with .NET ecosystem)
Julia             | 106 | just-in-time compiled, mature language
Dart              | 103 | just-in-time or ahead-of-time compiled, mature web programming language
Clojure           | 102 | interpreted, mature language (together with Java ecosystem)
Groovy            |  99 | interpreted, very mature language | 2026-02-05
Ruby              |  99 | interpreted, very mature language | 2026-01-18
Scala             |  98 | interpreted, mature language (together with Java ecosystem)
Nim               |  93 | compiled, young language, which is very effective in its efforts to modernize C in terms of verbosity | 2026-01-13 
Mojo              |  91 | compiled, very young language | 2026-01-10
PowerShell        |  90 | interpreted, mature language (together with .Net ecosystem): profiting from concise (and fast) _System.Text.StringBuilder_ source code
Python            |  87 | interpreted, very mature language

<br/>

## Number of user defined functions

..because I didn't find anything useful or reliably working in the (official) language libraries.

The results of this list have been manually counted:

programming language | number of user defined functions
--- | ---
Ada                | 2
C                  | 0
C++                | 0
C3                 | 0
C#                 | 0
Chapel             | 1
Clojure            | 3
Common Lisp        | 2
Crystal            | 0
D                  | 0
Dart               | 0
Eiffel (Liberty)   | 4
Fortran            | 3
FreeBASIC          | 1
Gleam              | 8
Go                 | 0
Groovy             | 0
Inko               | 4
Julia              | 0
Kotlin             | 1
Lua                | 2
Mercury            | 9
Mojo               | 0
Nim                | 0
OCaml              | 5
Odin               | 0
Perl 5             | 0
PHP                | 1 
Picat (old B-Prolog) | 8
Prolog, SWI        | 11
PowerShell         | 0
Python             | 1
Roc                | 9
Ruby               | 1
Rust               | 0
Scala              | 0
Scheme, Bigloo     | 7
Scheme, Racket     | 5
Standard ML (MLton) | 8
Swift              | 1
TypeScript         | 0
V                  | 3
Zig                | 0

<br/>

### Ranking popular programming languages by density

A recent article from February 2026: Boilerplate Tax - Ranking popular programming languages by density, 2026/02/03 (1799 words): https://boyter.org/posts/boilerplate-tax-ranking-popular-languages-by-density/

With an "dryness" index which is defined in percentage as: DRYness = ULOC / SLOC, where:

- ULOC = Unique Lines Of Code
- SLOC = Source Lines Of code

> Interpreting Dryness,
> 
> - 75% (High Density): Very terse, expressive code. Every line counts. (Example: Clojure, Haskell)
> - 60% - 70% (Standard): A healthy balance of logic and structural ceremony. (Example: Java, Python)
> - < 55% (High Boilerplate): High repetition. Likely due to mandatory error handling, auto-generated code, or verbose configuration. (Example: C#, CSS)

<br/>

I'm not suprised that Clojure is a winner here in terms of expressiveness ("Almost every line is an expression of business logic."), though this comes with a price (and for Haskell too):

> ..because from my point of view Clojure is not the easiest functional programming language to learn.

from: [On complexity in Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure#on-complexity-in-clojure)

Though, a Clojure expert could probably argue that my Clojure implementation also features some boilerplate code.

Boilerplate code in Wikipedia: https://en.wikipedia.org/wiki/Boilerplate_code

<br/>

##_end
