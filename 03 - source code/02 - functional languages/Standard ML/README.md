# Standard ML (SML)

> ‘ML’ stands for meta language; this is the term logicians use for a language in which other (formal or informal) languages are discussed and analysed.

from: https://direct.mit.edu/books/monograph/2094/The-Definition-of-Standard-ML,  see Book Chapter: Preface 

..with Meta Language being a pre-precursor of **OCaml**: https://dev.realworldocaml.org/prologue.html

SMLNJ, SML/NJ = Standard ML of New Jersey: https://www.smlnj.org/, which may still be the premier Standard ML dialect.

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

### MLton installation tips

I didn't manage to build my MLton implementation from sources, but downloaded file _mlton-20241230.x86_64-linux-gnu.tar.gz_ (for Ubuntu 24 LTS) from here: https://github.com/MLton/mlton/releases/tag/on-20241230-release, unzipped it and just expanded my Bash _$PATH_ environment variable to: _.../StandardML/mlton-20241230.x86_64-linux-gnu/mlton-on-20241230-release.x86_64-linux-gnu/bin/_

Some years ago, MLton published this benchmark page with five Standard ML dialects: http://www.mlton.org/Performance

I also tapped into **MLKit**, only to see a needed _structure_ listed in its basis, for example here: https://github.com/melsman/mlkit/blob/1733d3d90fc3ebd6157e1c34bcd68de51ab0d722/basis/Random.sml, which I didn't get working (I also wasn't able to build an implementation from MLKit sources). I wanted this _structure_ working to have equivalent functionality to these partly exclusive expressions in MLton, see function _MLton.Random.seed_ for example:

```
val m = 65521 (* = 2^16 - 15 *)
val m_word = 0wx0000fff1 (* same number as hex word *)
val seed_ = MLton.Random.seed () (* this is a new random number with every program start! *)
val seed_word = valOf seed_ (* type conversion from word option to word *)

(* this can easily overflow if not taken care of => modulus operation: *)
val seed_word_mod = seed_word mod m_word
val start_seed = Word.toInt seed_word_mod
```

Apart from this (I took poor man's [Time.now()](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_streams_for_perf_stats3.sml) in milliseconds as basis for a seed to get ahead with the MLKit program) there was no need to change my MLton program to make it through the MLKit compiler. Otherwise both programs show the same execution speed.

<br/>

### Other Standard ML  dialects

The old **Moscow ML** precompiled binaries (https://mosml.org/) from 2014 don't work in my machine.

I also shortly tapped into **Poly/ML** (https://polyml.org/) only to find out that this dialect seems to be the archaic version of Standard ML and thus would require refactoring many details of my MLton program.

<br/>

### Semicolons in Standard ML and OCaml

I had another motivation to test Standard ML: what can I learn from it for my OCaml program?

This: in my Standard ML program, right from start of development, I used as much _val_-statements for my printing expressions, expressions where I don't expect a return value or don't care of any respectively:

```
val _ = print ("\n....")
```

While in my OCaml program right from start I did it like this:

```
Printf.printf "\n...";
```

Instead I should have written this OCaml expression similarly like this:

```
let _ = Printf.printf "\n..." in
...
```

Spraying your OCaml source code with semicolons is seen as bad style nowadays:

> In CAML Light, the predecessor of OCaml, double semicolons were mandatory. For this reason they are quite common in old code originally written in CAML Light or written in the early days of OCaml. These days they are considered a bad style.

from: https://baturin.org/docs/ocaml-faq/

> But such a semicolon should **never** appear in your code. _Ullman’s book is full of unnecessary semicolons, and you must learn to ignore him._ Emulate the style in Ramsey’s
book, which has no unnecessary semicolons. Use a semicolon only to sequence effects in imperative code.

from: https://www.cs.tufts.edu/comp/105-2019s/readings/ml.html#definitions-ii-semicolons

In both languages semicolons are just **separators**; in Standard ML separators of expressions, declarations, signatures and programs: https://people.mpi-sws.org/~rossberg/sml.html

### Semicolon example: bad style versus good style

However, my itch for the imperative coding style still shows through here and there as this [example](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_bitstring_and_flexible_password_generator.sml) of a function declaration in Standard ML shows:

```
fun answer_yes_or_no () =
  (print ("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "); (*;!!!*)
   let
     val answer_str_ = valOf (TextIO.inputLine TextIO.stdIn)  (* returns a string option; \n is part of answer_str_ *)
     val answer_str  = String.substring (answer_str_, 0, (String.size answer_str_) - 1)
   in
     if answer_str = "y" then
       true
     else
       false
   end)
```

Instead, this function should have been written like this:

```
fun answer_yes_or_no () =
  let
    val _ = print ("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
    val answer_str_ = valOf (TextIO.inputLine TextIO.stdIn) (* returns a string option; \n is part of answer_str_ *)
    val answer_str = String.substring (answer_str_, 0, (String.size answer_str_) - 1)
  in
    if answer_str = "y" then
      true
    else
      false
  end
```

So, what I still underestimated when starting with the Standard ML program was the fact that many "statements" in Standard ML and languages that followed it, including OCaml, are **expressions** and should properly be treated like that:

> Functional programs are written by composing expressions...

from: https://kar.kent.ac.uk/24064/1/FuncOlaf.pdf ("Functional Programming", Olaf Chitil, University of Kent, United Kingdom, 2009)

<br/>

TBD

<br/>

### String building with Standard ML

The speed bottleneck of my initial and slow SML program was not my string handling, but having **a local list of integers** in the masterloop **to which I append one integer number in each iteration**!

I changed this to an (imperative) global array of integers, initially declared in its final size, and the execution time dropped from 12 seconds to 1.6 seconds! Obviously there is generally a speed issue with (very "functional") _lists_ in functional programming languages, see also best practice #2 from here: [My 5 best practices with Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#my-5-best-practices-with-scheme-dialects)

However, in a new functional programming I would always start with ("easy") _lists_ to get the functional aspects like recursions right first and only then try using other (imperative) data types like arrays or vectors, if available, to make a competitively performing program.

<br/>

##_end
