# Standard ML (SML)

> ‘ML’ stands for meta language; this is the term logicians use for a language in which other (formal or informal) languages are discussed and analysed.

from: https://direct.mit.edu/books/monograph/2094/The-Definition-of-Standard-ML,  see Book Chapter: Preface 

..with Meta Language being a pre-precursor of **OCaml**: https://dev.realworldocaml.org/prologue.html

SMLNJ, SML/NJ = Standard ML of New Jersey: https://www.smlnj.org/, which may still be the premier Standard ML dialect.

---

Table of contents:

TBD
- [Transpiling from Standard ML to Lua and JavaScript with LunarML](#transpiling-from-standard-ml-to-lua-and-javascript-with-lunarml)

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

### Using Standard ML of New Jersey (SML/NJ) libraries from MLton

This should apply nowadays:

> MLton includes a port of the SML/NJ Library synchronized with SML/NJ version...

from: http://mlton.org/SMLNJLibrary

..and this: 

> The SML/NJ Library is distributed as part of both the SML/NJ and MLton SML Compiler systems.

from: http://www.smlnj.org/doc/smlnj-lib/index.html

However, not everything what is available in SML/NJ is also readily available in MLton:

- for example the "utility library of the SML/NJ Library": https://github.com/smlnj/legacy/tree/c1a9b36470234153a46ec3f08ae732d1522c596a/smlnj-lib/Util
- ..and thus for example also not this "interface to stateful pseudo-random number generators": https://github.com/smlnj/legacy/blob/c1a9b36470234153a46ec3f08ae732d1522c596a/smlnj-lib/Util/random-sig.sml

Therefore this question naturally comes up:

- can we use functions from the utility library of the SML/NJ Library to replace the MLton specific source code as shown above at [MLton installation tips](#mlton-installation-tips)?

Spoiler alert: yes, we can!

### Some random number generation in Standard ML of New Jersey

But first let's have a little exercise in the SML/NJ REPL with: _$ rlwrap sml_

We can follow another utility to see how to start a random number generator in SML/NJ: https://github.com/smlnj/legacy/blob/c1a9b36470234153a46ec3f08ae732d1522c596a/smlnj-lib/UUID/gen-uuid.sml and enter these expressions - each terminated with a semicolon - in the REPL:

```
val m = 65521;
fun getTime () = IntInf.divMod (Time.toMicroseconds(Time.now()), 1000000);
val maxInt = IntInf.fromInt (valOf Int.maxInt) + 1;
val (secs, usecs) = getTime ();
val r = Random.rand (Int.fromLarge(secs mod maxInt), Int.fromLarge usecs);
val seed1 = (Random.randInt r);
val start_seed = seed1 mod m;
val _ = print("start_seed = " ^ Int.toString start_seed);
val _ = print("\n");
```
By the way: _seed1_ can take really big integer numbers, positive or negative, but the modulus operator mod will not only make them much smaller (absolutely), but also only positive, which is practical.

Since everything works, we now put above expressions - **without the semicolons** - into a source code file named _my_program.sml_.

#### Tapping into libraries of Standard ML of New Jersey with ML Basis

Now we only have to find a way how to use a function inside a library of SML/NJ which isn't automatically available in MLton. By the way: these SML/NJ libraries are
automatically installed, or unzipped respectively, along a MLton installation.

MLton has a _**ML Basis**_ system which "extends Standard ML to support programming-in-the-very-large, namespace management at the module level, separate delivery of library sources, and more": http://mlton.org/MLBasis

Which means that we also have to make a little _~.mlb_ file like this: http://mlton.org/MLBasisExamples, here just named _my_program.mlb_:

```
(* import libraries *)
$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb
$(SML_LIB)/basis/basis.mlb
(* program files *)
./my_program.sml
```

If something went wrong with variable _$(SML_LIB)_ for example, alternatively absolute path names can be given like this for example (or wheresoever directory MLton has been unzipped):

_.../StandardML/mlton-20241230.x86_64-linux-gnu/mlton-on-20241230-release.x86_64-linux-gnu/lib/mlton/sml/smlnj-lib/Util/smlnj-lib.mlb_

It's important to also consider library _basis.mlb_ which includes all basic Standard ML types. If not, MLton doesn't even know what an _int_ type is for example.

Now we can build standalone executable _my_program_ like this:

```
$ mlton ./my_program.mlb
```
..and run it like this:

```
$ ./my_program
unhandled exception: Overflow
$
```

### yyyyyyyyyyyyyyyyyyyyyyy

So, _my_program_ is working fine in the SML/NJ REPL, but not when being compiled with MLton!?!

Slowly, we could get an idea why MLton has its own implementation of seeding a random number generator with:

```
val seed_ = MLton.Random.seed ()
```

Now I do what I mostly do in cases like these before searching for the debugger manual: I paste some print expressions around the lines of source code in question and see after what expression the program goes into exception. It's this expression:

```
val seed1 = (Random.randInt r)
```

..which apparently cannot be fixed within a MLton program.

So, I went searching in the (legacy) Github repository of SML/NJ to see what else could be used instead. Here: https://github.com/smlnj/legacy/blob/c1a9b36470234153a46ec3f08ae732d1522c596a/smlnj-lib/Util/real-order-stats.sml I found function _Random.randRange_!

Actually, I only exchange the expression in question and leave the others untouched, except _val start_seed = seed1 mod m_, which isn't needed anymore:

```
val m = 65521
fun getTime () = IntInf.divMod (Time.toMicroseconds(Time.now()), 1000000)
val maxInt = IntInf.fromInt (valOf Int.maxInt) + 1
val (secs, usecs) = getTime ()
val r = Random.rand (Int.fromLarge(secs mod maxInt), Int.fromLarge usecs)

val start_seed = Random.randRange (1, m) r (* the new expression *)

val _ = print("start_seed = " ^ Int.toString start_seed)
val _ = print("\n")
```

```
$ ./my_program
start_seed = 16370
$ 
```

<br/>

Now my compiled microbenchmark program in [Standard ML](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_streams_for_perf_stats3.sml) works fine and also shines in another category, see below at [Transpiling from Standard ML to Lua and JavaScript with LunarML](#transpiling-from-standard-ml-to-lua-and-javascript-with-lunarml).

<br/>

### String building with Standard ML

The speed bottleneck of my initial and slow SML program was not my string handling, but having **a local list of integers** in the masterloop **to which I append one integer number in each iteration**!

I changed this to an (imperative) global array of integers, initially declared in its final size, and the execution time dropped from 12 seconds to 1.6 seconds! Obviously there is generally a speed issue with (very "functional") _lists_ in functional programming languages, see also best practice #2 from here: [My 5 best practices with Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#my-5-best-practices-with-scheme-dialects)

However, in a new functional programming I would always start with ("easy") _lists_ to get the functional aspects like recursions right first and only then try using other (imperative) data types like arrays or vectors, if available, to make a competitively performing program.

<br/>

### Transpiling from Standard ML to Lua and JavaScript with LunarML

While having a look at [Standard ML dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML#other-standard-ml--dialects), I noticed the actively maintained **LunarML** transpiler: https://lunarml.readthedocs.io/en/latest/intro.html and gave it a try.

For its full scope make sure that modern versions of [Lua](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Lua#lua), LuaJIT (https://luajit.org/) and node.js (https://nodejs.org/en/) are also installed.

I built LunarML without problems from sources, including all its tests (which may run for a while): https://lunarml.readthedocs.io/en/latest/intro.html#installation

However, I noticed that my original Standard ML program for compilation with ............


<br/>

##_end
