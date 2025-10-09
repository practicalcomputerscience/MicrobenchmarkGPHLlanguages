# Standard ML (SML)

> ‘ML’ stands for meta language; this is the term logicians use for a language in which other (formal or informal) languages are discussed and analysed.

from: https://direct.mit.edu/books/monograph/2094/The-Definition-of-Standard-ML,  see Book Chapter: Preface 

SMLNJ or SML/NJ = Standard ML of New Jersey: https://www.smlnj.org/, which may still be the premier Standard ML dialect.

---

Table of contents:

(TBD)

---

### MLton compiler

Here I'm talking about Standard ML since 1997: https://smlfamily.github.io and specifically with the help of the MLton compiler:

> MLton is a whole-program optimizing compiler for the Standard ML programming language.
- from: http://mlton.org/
- https://github.com/MLton/mlton

..in order to conveniently make standalone, binary executables for Linux: _$ mlton < your program >.sml_

..while in SML/NJ - again - some tinkering with "heap images" is needed: https://www.smlnj.org/doc/heap2exec/index.html

<br/>

Spoiler alert: the chances that a (basically same) program in SML will be faster than its variant in [OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) is slim (OCaml is full of "bells and whistles"), though you can make decently fast programs with SML too, that is programs which can compete with Common Lisp for example in terms of [execution speed](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments).

However, MLton doesn't have a REPL, so I still have a SML/NJ installation for quick tests (of "standard" Standard ML features). Start the SML/NJ REPL in Linux with: _$ rlwrap sml_

### Installation tips

I didn't manage to build my MLton implementation from sources, but downloaded file _mlton-20241230.x86_64-linux-gnu.tar.gz_ (for Ubuntu 24 LTS) from here: https://github.com/MLton/mlton/releases/tag/on-20241230-release, unzipped it and just expanded my Bash _$PATH_ environment variable to: _.../StandardML/mlton-20241230.x86_64-linux-gnu/mlton-on-20241230-release.x86_64-linux-gnu/bin/_

Some years ago, MLton published this benchmark page with five Standard ML dialects: http://www.mlton.org/Performance

I also tapped into **MLKit**, only to see a needed _structure_ listed in its basis, for example here: https://github.com/melsman/mlkit/blob/1733d3d90fc3ebd6157e1c34bcd68de51ab0d722/basis/Random.sml, which I didn't get working (I also wasn't able to build an implementation from MLKit sources). I wanted this _structure_ working to have equivalent functionality to these partly exclusive expressions in
MLton, see function _MLton.Random.seed_ for example:

```
val m = 65521 (* = 2^16 - 15 *)
val m_word = 0wx0000fff1 (* same number as hex word *)
val seed_ = MLton.Random.seed () (* this is a new random number with every program start! *)
val seed_word = valOf seed_ (* type conversion from word option to word *)

(* this can easily overflow if not taken care of => modulus operation: *)
val seed_word_mod = seed_word mod m_word
val start_seed = Word.toInt seed_word_mod
```






<br/>

### String building with Standard ML

The speed bottleneck of my initial and slow SML program was not my string handling, but having **a local list of integers** in the masterloop **to which I append one integer number in each iteration**!

I changed this to an (imperative) global array of integers, initially declared in its final size, and the execution time dropped from 12 seconds to 1.6 seconds! Obviously there is generally a speed issue with (very "functional") _lists_ in functional programming languages, see also best practice #2 from here: [My 5 best practices with Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#my-5-best-practices-with-scheme-dialects)

However, in a new functional programming I would always start with ("easy") _lists_ to get the functional aspects like recursions right first and only then try using other (imperative) data types like arrays or vectors, if available, to make a competitively performing program.

<br/>

##_end
