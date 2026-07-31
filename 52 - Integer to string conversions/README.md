2026-07-31: work in progress

<br/>

# Integer to string conversions

Not all general purpose, high-level programming languages, including their officially maintained libraries, provide inbuilt functions to convert an unsigned 32-bit integer number into its 
representations as a:

- binary string, including padding to 16 characters with leading "0"'s if needed, and
- hexadecimal string, including padding to 4 characters with leading "0"'s if needed, and only using lowercase letters "a" to "f".

In no corner of my microbenchmark program I employed more **user defined functions** than for these two functionalities (which are already multi-functionalities).

Some languages, including young ones, provide inbuilt functions to do all jobs on a integer input number, some languages, including very old ones, don't provide any official solutions at all.

Some languages only partly provide the required functionalities, often with lacking the padding with leading zeros.

In order to provide some overview of this messy situation, I created another language list, see below.

<br/>

By the way: a clever implementation of above functionalities can have a surprisingly high impact; see for example at [String padding](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/README.md#string-padding):

> Integrated string padding on the left hand side of mandatory string variables _bits_x_str_ to 16 characters of '0' and '1' and _bits_hex_str_ (or similarly named) to 4 characters of '0' to 'f' can also have a major effect on execution speed at some implementations.
>
> For example, in the Haskell implementation this measure alone brought down program execution time by over 40%!

<br/>

programming language | integer to binary string | integer to hexadecimal string | integrated padding? | comment
--- | --- | --- | --- | ---
Ada (GNAT) | |  | | 
AssemblyScript | | ||   
Awk (GNU) |  |  | | 
Ballerina |  |  | | 
C |  |  | | 
C++ |  |  | | 
C3 |  |  | | 
C# |  |  | | 
Chapel | |  ||   
Clojure | |  | | 
COBOL (GnuCOBOL) |  |  | | 
CoffeeScript |  |  | | 
Common Lisp |  |  | | 
Crystal |  |  | | 
Curry (KiCS2) |  |  | | 
D |  |  | | 
Dart |  |  ||  
Dylan |  |  | | 
Eiffel, Liberty |  |  | | 
Factor |  |  | | 
Forth (Gforth) |  |  | | 
Fortran (GNU) |  |  | | 
FreeBASIC |  |  | | 
(Object) Free Pascal |  |  | | 
Gleam |  |  | | 
Go |  |  | | 
Groovy |  | ||   
Haskell |  |  ||  
Haxe |  |  | | 
Hy |  |  | | 
Inko |  |  | | 
Java |  |  | | 
Julia |  |  | | 
Kotlin |  |  | | 
Lua |  |  | | 
Mercury | |  ||   
Modula-2 (GNU)  | |  | | 
Modula-3 (CM3) |  |  | | 
Mojo |  |  | | 
Nim |  |  | | 
Oberon (OBC) ||   |  | 
OCaml |  |  | | 
Odin |  |  | | 
Perl 5 |  |  | | 
PHP |  |  | | 
Picat |  |  | | 
Pike |  |  | | 
PowerShell |  ||   | 
Prolog, SWI |  | | |  
Python |  |  | | 
Roc |  |  | | 
Ruby |  |  | | 
Rust |  |  | | 
Scala |  |  | | 
Scheme, Bigloo |  |  | | 
Scheme, Racket |  |  | | 
Smalltalk (GNU) |  |  | | 
Standard ML (MLton) | |  ||   
Swift |  |  | | 
Tcl |  |  | | 
TypeScript | ||    | 
V |  |  | | 
Zig |  |  | | 

<br/>

tbd

<br/>

##_end
