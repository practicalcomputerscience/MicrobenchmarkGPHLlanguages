2026-05-19: tbd: finally, calculate the median value of SLOC over all languages as listed below

<br/>

# Lines Of source Code (LOC): verbosity

TL;DR: get to me the [LOC ranking list](#loc-ranking-list) below!

<br/>

LOC is here understood as Source Lines Of Code (SLOC).

Table of contents:

- [Idea of this page](#idea-of-this-page)
- [The cloc tool](#the-cloc-tool)
- [LOC ranking list](#loc-ranking-list)
- [Number of user-defined functions](#number-of-user-defined-functions)
- [Ranking popular programming languages by density](#ranking-popular-programming-languages-by-density)

<br/>

## Idea of this page

This ranking list is arguably even more controversial than the ranking based on [execution times](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages?tab=readme-ov-file#the-1-second-execution-time-limit).

However, even with my limited knowledge I tried to make _idiomatic_ source code in every programming language. I think that naivety can even be a help here.

On the other side, there _are_ programs with more **lines of code** (LOC) than actually needed, with my Ada version being the most prominent example from my point of view:

> The true reason why my Ada program was so helpful was because of my incompetence! Originally, I wanted to use the Strings Edit library ..., but I was not able to figure out within an acceptable amount of time how to use such a third party library for Ada. Consequently, my Ada program has the second most lines of source code as of 2026-02-11...

from: [AI experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/README.md#ai-experiments)

<br/>

#### The cloc tool

After my Perl script [lines_of_source_code_count.pl](./lines_of_source_code_count.pl) miscalculated the SLOC number of the Smalltalk program initially, I gave the [CLOC tool](https://github.com/AlDanial/cloc) a try:

```
$ sudo apt install cloc
...
$ cloc ./random_bitstring_and_flexible_password_generator.st
       1 text file.
       1 unique file.                              
       0 files ignored.

github.com/AlDanial/cloc v 1.98  T=0.00 s (250.2 files/s, 57044.9 lines/s)
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Smalltalk                        1             43             56            122
-------------------------------------------------------------------------------
$ 
```

At Smalltalk program [random_bitstring_and_flexible_password_generator.st](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01a%20-%20object-oriented%20languages/Smalltalk/random_bitstring_and_flexible_password_generator.st) with its now simplified commenting, my updated Perl script arrived at the same number of source lines of code as the cloc tool!

However, with counting the SLOC's of Standard ML program [random_bitstring_and_flexible_password_generator.sml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_bitstring_and_flexible_password_generator.sml) I found an error with the cloc tool:

```
$ cloc random_bitstring_and_flexible_password_generator.sml
       1 text file.
       1 unique file.                              
       0 files ignored.

github.com/AlDanial/cloc v 1.98  T=0.00 s (266.1 files/s, 99788.4 lines/s)
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Standard ML                      1             72             89            214
-------------------------------------------------------------------------------
$
```

The actual SLOC number is 199 and not 214. My Perl script does the counting now correctly after fixing the block comments in the source code file (and in the [OCaml source code file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/random_bitstring_and_flexible_password_generator_main.ml)):

```
...
!!! new block comment rules for better counting of SLOC:
    - put markers to start and end a block comment only into extra solo lines
    - don't put nested comments with a marker at the end of a line inside block comments !!!
...
```

```
$ perl lines_of_source_code_count.pl random_bitstring_and_flexible_password_generator.sml
language = sml
  bracket_star_detected
  star_bracket_detected
  ...
  bracket_star_detected
  star_bracket_detected

total number of lines = 375
number of lines of source code (estimated) = 199

number of empty lines = 64
number of lines with ___// = 0
number of lines with ___# = 0
number of lines with ___-- = 0
number of lines with ___(*..*)___ = 31
number of lines with ___;(;) = 0
...
number of lines in block comment: /* ... */ = 0
number of lines in block comment: """ ... """ = 0
number of lines in block comment: (* ... *) = 81
number of lines in block comment: <# ... #> = 0
...
$
```

For [Racket Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket#racket-scheme), the cloc tool also calculates a wrong number of SLOC's (while my script works OK), same with Eiffel (it doesn't recognize that language correctly). There are probably more languages where cloc's counting is not correct.

<br/>

## LOC ranking list

programming language | LOC | comments | date
--- | --- | --- | ---
COBOL (GnuCOBOL)  | 357 | compiled, very mature language: lots of definitions needed and overhead in place, though the later could be brought down with the help of a compilation switch (no warnings about lack of scope terminators _END-XXX_, which make up 81 lines; so, without out them the LOC would be just 276); lack of control characters in strings (using _DISPLAY " "_ instead of _\n_ for example, which make up another 6 lines) | 2026-03-29
Inko              | 232 | compiled, very young language: lots of user-defined functions needed; also a construct is used to avoid repeated, individual access to array elements _x.get(i).or_panic_ and _x.get(i - 1).or_panic_ for exe speed reasons | 2026-01-24
Modula-2 (GNU)    | 231 | compiled, very mature language: some user-defined functions needed | 2026-07-12
Eiffel (Liberty)  | 220 | compiled, very mature language: some user-defined functions needed; since programs are collections of classes, this is causing some formal overhead | 2026-06-18
Ada (GNAT)        | 215 | compiled, very mature language: lots of declarations and type definitions; still keeping the low level character copying from little strings into the big strings for performance reasons | 2026-06-08
Forth (Gforth)    | 214 | interpreted, very mature language: lots of user-defined functions needed | 2026-07-09
Standard ML (MLton) | 197 | compiled, very mature language: numerous user-defined functions needed | 2026-06-18
Mercury           | 194 | compiled, mature language: lots of user-defined functions needed | 2026-06-17
Roc               | 191 | compiled, very young, pure functional programming language: numerous user-defined functions needed | 2026-05-31
V                 | 175 | compiled, very young programming language: some user-defined functions needed | 2026-06-19
Fortran (GNU)     | 161 | compiled, very mature language: some user-defined functions may be needed when not using the inofficial standard library | 2026-06-19
Gleam             | 151 | interpreted, very young language, together with Erlang and JavaScript ecosystem: lots of user-defined functions needed | 2026-06-18
C                 | 149 | compiled, very mature language | 2026-06-19
C3                | 144 | compiled, very young language: DStrings (dynamic strings) are (still) missing some functions that are available for Strings (fixed length) => some shuffling between DStrings and Strings | 2026-01-26
Factor            | 143 | interpreted, mature language: numerous user-defined functions needed | 2026-07-05
Picat (old B-Prolog) | 142 | interpreted, mature language: lots of user-defined functions needed | 2026-06-17
AssemblyScript    | 140 | interpreted, young language, together with WebAssembly ecosystem: lots of user-defined functions needed | 2026-05-24
Curry (KiCS2)     | 132 | compiled, very mature language | 2026-06-23
C++               | 129 | compiled, very mature language; memory-safe source code can be very well more concise and be compiled to a bit faster program than with C | 2026-05-25
Zig               | 129 | compiled, young language still going through major changes | 2026-06-17
OCaml             | 128 | compiled, mature language; some user-defined functions needed | 2026-06-18
Odin              | 127 | compiled, young language | 2026-06-18
SWI Prolog        | 123 | compiled, very mature language: lots of user-defined functions needed, even though SWI Prolog is a Prolog system with "batteries included" | 2026-06-16
FreeBASIC         | 122 | compiled, mature language | 2026-06-14
Perl 5            | 122 | interpreted, very mature language: verbosity with if-then-else form of error handling when writing to files | 2026-05-25
Rust              | 122 | compiled, mature language | 2026-06-18
Smalltalk (GNU)   | 122 | interpreted, very mature language, though at least GNU Smalltalk needs some user-defined functions | 2026-05-24
Go                | 121 | compiled, mature language | 2026-05-28
Ballerina         | 116 | interpreted, young language, together with Java ecosystem | 2026-06-18
Chapel            | 115 | compiled, mature language | 2026-06-15
D                 | 115 | compiled, very mature language; memory-safe source code can be easily more concise than an equivalent program in C++ | 2026-01-21
Lua               | 115 | interpreted, very mature language | 2026-06-11
Bigloo Scheme     | 114 | compiled, mature language | 2026-06-18
Common Lisp       | 114 | compiled, very mature language | 2026-06-18
Dylan (Open Dylan) | 113 | compiled, very mature language | 2026-06-28
Haxe              | 113 | interpreted, mature language: some user-defined functions needed | 2026-05-27
Kotlin            | 108 | interpreted, young language, together with Java ecosystem | 2026-05-26
Racket Scheme     | 107 | interpreted, mature language | 2026-06-18
PHP               | 105 | usually interpreted, very mature web programming language | 2026-06-18
Swift             | 105 | compiled, mature language | 2026-06-10
TypeScript        | 105 | interpreted, JIT (Just-In-Time) compiled, or AOT (Ahead-Of-Time) compiled, very mature language | 2026-06-18
C#                | 104 | interpreted, mature language, together with .NET ecosystem | 2026-06-18
Crystal           | 103 | compiled, young language | 2026-06-18
Pike              | 103 | interpreted, mature language | 2026-06-18
Java              | 101 | interpreted, very mature language, together with Java ecosystem | 2026-05-26
Scala             | 101 | interpreted, mature language, together with Java ecosystem | 2026-05-25
Julia             |  99 | just-in-time compiled, mature language | 2026-06-18
Clojure           |  96 | interpreted, mature language, together with Java ecosystem | 2026-06-18
Ruby              |  96 | interpreted, very mature language | 2026-06-18
Groovy            |  95 | interpreted, very mature language, together with Java ecosystem | 2026-05-27
Tcl               |  95 | interpreted, very mature language | 2026-06-19
Dart              |  90 | just-in-time or ahead-of-time compiled, mature web programming language | 2026-06-18
Mojo              |  90 | compiled, very young language | 2026-05-28
Nim               |  90 | compiled, young language, which is very effective in its efforts to modernize C in terms of verbosity | 2026-05-28
PowerShell        |  88 | interpreted, mature language, together with .NET ecosystem: profiting from concise (and fast) _System.Text.StringBuilder_ source code | 2026-06-18
CoffeeScript      |  84 | compiled to JavaScript, and then interpreted, JIT (Just-In-Time) compiled, or AOT (Ahead-Of-Time) compiled, very mature language | 2026-06-17
Python            |  84 | interpreted, very mature language | 2026-06-18
Hy                |  83 | interpreted, mature language, together with Python ecosystem | 2026-07-01

<br/>

2026-07-01: there's a new leader in succinctness: [Hy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Hy#hy), that is "a Lisp dialect that's embedded in Python"

<br/>

## Number of user-defined functions

..because I didn't find anything useful or reliably working in the (official) language libraries.

The results of this list have been manually counted:

programming language | number of user-defined functions
--- | ---
Ada (GNAT)         | 2
AssemblyScript     | 5
Ballerina          | 2
C                  | 0
C++                | 0
C3                 | 0
C#                 | 0
Chapel             | 1
Clojure            | 3
COBOL (GnuCOBOL)   | 3 (user-defined procedures)
CoffeeScript       | 0
Common Lisp        | 3
Crystal            | 0
Curry (KiCS2)      | 9
D                  | 0
Dart               | 0
Dylan (Open Dylan) | 1
Eiffel (Liberty)   | 4
Factor             | 6
Forth (Gforth)     | 14
Fortran (GNU)      | 3
FreeBASIC          | 1
Gleam              | 8
Go                 | 0
Groovy             | 0
Haxe               | 2
Hy                 | 2
Inko               | 4
Java               | 0
Julia              | 0
Kotlin             | 1
Lua                | 1
Mercury            | 9
Modula-2 (GNU)     | 4
Mojo               | 0
Nim                | 0
OCaml              | 4
Odin               | 0
Perl 5             | 0
PHP                | 1 
Picat (former B-Prolog) | 8
Pike               | 1
Prolog, SWI        | 11
PowerShell         | 0
Python             | 1
Roc                | 9
Ruby               | 1
Rust               | 0
Scala              | 0
Scheme, Bigloo     | 5
Scheme, Racket     | 6
Smalltalk (GNU)    | 2
Standard ML (MLton) | 7
Swift              | 1
Tcl                | 0
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
