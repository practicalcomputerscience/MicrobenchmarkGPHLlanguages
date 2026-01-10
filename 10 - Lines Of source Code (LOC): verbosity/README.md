# Lines Of source Code (LOC): verbosity

This ranking list is arguably even more controversial than the ranking based on [execution times](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages?tab=readme-ov-file#the-1-second-execution-time-limit).

However, even with my limited knowledge I tried to make _idiomatic_ source code in every programming language. I think that naivety can even be a help here.

On the other side, there _are_ programs with more **lines of code** (LOC) than actually needed, with my Ada version being the most prominent example from my point of view:

> The true reason why my Ada program was so helpful was because of my incompetence! Originally, I wanted to use the Strings Edit library ..., but I was not able to figure out within an acceptable amount of time how to use such a third party library for Ada. Consequently, my Ada program has the most lines of source code with 231, the highest so far...

from: [AI experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/README.md#ai-experiments)

<br/>

## LOC ranking list

The results of this list are [script-based](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/10%20-%20Lines%20Of%20source%20Code%20(LOC)%3A%20verbosity/lines_of_source_code_count.pl): _$ perl lines_of_source_code_count.pl random_bitstring_and_flexible_password_generator.< ... >_

programming language | LOC | comments
--- | --- | ---
Ada               | 231 | compiled, very mature language: lots of declarations and type definitions
Inko              | 224 | compiled, very young language: lots of user-defined functions
Standard ML (MLton) | 216 | compiled, very mature language: lots of user-defined functions
Mercury           | 194 | compiled, mature language: lots of user-defined functions
Roc               | 191 | compiled, very young, pure functional programming language: needs some user-defined functions
V                 | 179 | compiled, very young programming language: needs some user-defined functions
Fortran           | 164 | compiled, very mature language: some user-defined functions may be needed when not using the inofficial standard library
C3                | 162 | compiled, very young language: DStrings (dynamic strings) are (still) missing some functions that are available for Strings (fixed length) => some shuffling between DStrings and Strings
Gleam | 162 | interpreted, very young language (together with Erlang and JavaScript ecosystem):  lots of user-defined functions
C                 | 148 | compiled, very mature language
Picat (old B-Prolog) | 142 | interpreted, mature language: lots of user-defined functions 
SWI Prolog        | 133 | compiled, very mature language: lots of user-defined functions, even though SWI Prolog is a Prolog system with "batteries included"
OCaml             | 131 | compiled, mature language
Lua               | 130 | compiled, very mature language
Perl 5            | 130 | interpreted, very mature language: verbosity with if-then-else form of error handling when writing to files
Zig               | 129 | compiled, young language still going through major changes
Odin              | 128 | compiled, young language, which is more effective in its efforts to modernize C than C3 in terms of verbosity
Go                | 127 | compiled, mature language
Rust              | 126 | compiled, mature language
Bigloo Scheme     | 124 | compiled, mature language
FreeBASIC         | 122 | compiled, mature language
Chapel            | 121 | compiled, mature language
Swift             | 114 | compiled, mature language
Common Lisp       | 114 | compiled, very mature language
Crystal           | 111 | compiled, young language
Racket Scheme     | 110 | compiled, mature language
Kotlin            | 109 | interpreted, young language (together with Java ecosystem)
C#                | 109 | interpreted, mature language (together with .NET ecosystem)
Julia             | 106 | just-in-time compiled, mature language
Clojure           | 102 | interpreted, mature language (together with Java ecosystem)
Scala             |  98 | interpreted, mature language (together with Java ecosystem)
Mojo              |  91 | compiled, very young language
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
C3                 | 0
C#                 | 0
Chapel             | 1
Clojure            | 3
Common Lisp        | 2
Crystal            | 0
Fortran            | 3
FreeBASIC          | 1
Gleam              | 8
Go                 | 0
Inko               | 4
Julia              | 0
Kotlin             | 1
Lua                | 2
Mercury            | 9
Mojo               | 0
OCaml              | 5
Odin               | 0
Perl 5             | 0
Picat (old B-Prolog) | 8
Prolog, SWI        | 11
PowerShell         | 0
Python             | 0
Roc                | 9
Rust               | 0
Scala              | 0
Scheme, Bigloo     | 7
Scheme, Racket     | 5
Standard ML (MLton) | 8
Swift              | 1
V                  | 3
Zig                | 0

---

#### Re-testing script lines_of_source_code_count.pl

version of 2026-01-03a, LOC:

.adb: 231 -- OK

.bas: 122 -- OK

.c: 148 -- OK

.c3: 163 -- OK

.chpl: 121 -- OK

.clj: 102 -- OK

.cr: 111 -- OK (Crystal, fixed on 2025-01-03)

.cs: 109 -- OK (C#, fixed on 2025-01-03)

.gleam: 162 -- OK

.go: 127 -- OK

.inko: 224 -- OK

.jl: 106 -- OK

.kt: 109 -- OK

.lisp: 114 -- OK

.lua: 130 -- OK

.m: 194 -- OK

.ml: 131 -- OK (OCaml)

.mojo: 93 -- OK

.P: 133 -- OK (SWI Prolog)

.pi: 142 -- OK

.pl: 130 -- OK

.ps: 90 -- OK

.py: 87 -- OK

.rkt: 110 -- OK

.roc: 191 -- OK

.rs: 126 -- OK

.scala: 98 -- OK

.scm: 124  -- OK (Bigloo Scheme)

.sml: 216 -- OK

.swift: 114 -- OK

.v: 179 -- OK

.zig: 129 -- OK

<br/>

##_end
