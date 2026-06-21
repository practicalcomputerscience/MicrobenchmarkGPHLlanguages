2026-06-19: work in progress tbd

<br/>

# Curry

https://www.curry-lang.org (*)

Up-to-date tutorial from 2025: https://curry-language.org/docs/tutorial/tutorial.pdf

Look at Curry packages from here: https://cpm.curry-lang.org/

CPM = Curry Package Manager

<br/>

> [!TIP]
> Use the PAKCS (→ Prolog) and KiCS2 (→ Haskell) implementations of Curry, but not the limited Curry2Go (→ Go) implementation!

<br/>

So, two programming language names have been attributed to [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), American mathematician and logician (1900-1982).

---

Table of contents:

- [Idea of Curry: integrating logic and functional programming in a purely declarative style](#idea-of-curry-integrating-logic-and-functional-programming-in-a-purely-declarative-style)
- [Documentation of Curry](#)
- [Maps of Australia and Germany for KiCS2 Curry](#maps-of-australia-and-germany-for-kics2-curry)
- [The Curry Package Manager (CPM)](#)

<br/>

---

## Idea of Curry: integrating logic and functional programming in a purely declarative style

After my sobering experiences with [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) and [Oz](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Oz#oz), I have been searching for another (and still maintained) language to combine the "natural fits" logic and functional programming, and found Curry (*):

> Curry is a declarative multi-paradigm programming language which combines in a seamless way features from functional programming ... and logic programming ... .

> Curry is called a declarative language, because computed results are independent of the time and order of evaluation, which simplifies reasoning on programs. Side effects can be modeled as “IO” operations, i.e., a declarative description of what to do. Operations are constructed by expressions only, there are no statements or instructions, and every binding to a variable is immutable.

Again, we see the phenomenon of multiple implementations: PAKCS, KiCS2, Curry2Go and MCC: https://www.curry-lang.org/implementations/overview/, with MCC being dead already: http://danae.uni-muenster.de/curry/

And again, same like [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury), also Curry is a "purely declarative" programming language (*).

Fun fact: both languages, Curry and Mercury, first appeared in 1995.

<br/>

#### Documentation of Curry

At least the official documentation of Curry isn't the best from my point of view. For example, I noticed that it usually steps over explaining the ubiquitous -> and <- operators:

- while _let_ is obviously for deterministic local bindings, <- seems to deal with non-determinism,

..which leaves me the -> operator to look for something similar in Haskell:

..where -> is for:

- Function type-mapping operator (a Monad: https://www.euclideanspace.com/software/language/functional/haskell/operators/index.htm; https://fwoelffel.me/posts/monads/)
- Lambda definition operator
- Separator in case construction, see at function _convertToBase_ below

..while <- in Haskell is for

- List comprehension generator
- Single assignment operator in do-blocks, which is also used like that in Curry

For Haskell, see from here: https://www.imada.sdu.dk/u/rolf/Edu/DM22/F06/haskell-operatorer.pdf

<br/>

#### Curry2Go: transpiling Curry source code into Go source code for a Linux executable

https://www.curry-lang.org/curry2go/

https://github.com/curry-language/curry2go

After some reading, I gave Curry2Go a try to compile Curry source code into a [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go) based executable: https://www.curry-lang.org/curry2go/

> [!IMPORTANT]
> However, making the choice of Curry2Go has a big drawback. The "usual" code examples and code snippets from the "usual" tutorials do not work here!

For example, this tutorial is meant for the PAKCS implementation of Curry, though the first exercise works also in curry2go: https://www.curry-lang.org/docs/tutorial/html/

Also these examples are only working in PAKS: https://www.curry-lang.org/pakcs/examples/

#### Installation tips for Curry2Go

For the curry2go compiler to work, a Go compiler needs to be installed first: https://go.dev/

Then, I followed the official installation instructions with: _$ curl -sSL https://www.curry-lang.org/curry2go/download.sh | sh_

..added _export PATH="$HOME/scripts/Curry/Curry2Go/Curry2Go/bin:$PATH"_ to my _.bashrc_ file, and finally activated it with: _$ source ~/.bashrc_

The Curry2Go REPL (Read-Eval-Print Loop) can now be started like this hopefully:

```
$ curry2go
Installing '~/.curry2gorc'...
--------------------------------------------------------------
Curry2Go Interactive Environment (Version 1.6.0 of 2025-10-30)
--------------------------------------------------------------

Type ":h" for help  (contact: info@curry-lang.org)
Compiling Prelude...
Prelude> :help
Basic commands (commands can be abbreviated to a prefix if unique):

<expr>             - evaluate expression <expr>
...
Prelude> :quit
$
```

#### A first Curry2Go program

However, working in the REPL was not my goal, and with the help of "Big AI" I tinkered together a first, little Curry2Go program to be compiled into a standalone, native executable for Linux, here named _factorial.curry_:

```
import System.Environment (getArgs)

-- define function factorial:
factorial :: Int -> Int
factorial n | n == 0    = 1
            | n > 0     = n * factorial (n - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
  -- x:_ is Head:Tail, with Tail being ignored
    (x:_) -> print (factorial (read x))
    _     -> putStrLn "Usage: program <n>"
```

Compile and run it like this:

```
$ curry2goc factorial  # no file extension needed here
...
$ ./factorial 5
120
$
```

Voilà!

See also this related and official Curry2Go example: https://github.com/curry-language/curry2go/blob/master/examples/Fac.curry

#### Map of Australia for Curry2Go

I coded the problem of the Australian map for Curry2Go ([graph_4coloring_Australia_Curry2Go.curry](./graph_4coloring_Australia_Curry2Go.curry)), but that program is not working completely like in this ALS Prolog implementation for example: [graph_4coloring_Australia_ALS.pro](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Australia_ALS.pro)

With Curry2Go, I didn't figure out how to count the total number of solutions, and showing the first and last solutions like in my Prolog programs:

```
$ curry2goc graph_4coloring_Australia  # compiling
...
$ ./graph_4coloring_Australia 
[Red, Yellow, Red, Yellow, Blue, Yellow, Red]
...
[Blue, Green, Blue, Green, Yellow, Green, Blue]
$ 
```

The challenge was always that _correct_:

```
...
-- correct coloring where non-deterministic generators are provided
-- 7 Australian States:
correct NT QL NSW VIC SA WA TAS =
  cond (diff WA  NT  &&
        diff WA  SA  &&
        diff NT  SA  &&
        diff NT  QL  &&
        diff SA  QL  &&
        diff SA  NSW &&
        diff SA  VIC &&
        diff QL  NSW &&
        diff VIC NSW &&
        diff TAS VIC)
       [NT, QL, NSW, VIC, SA, WA, TAS]
...
```

..is a **non-deterministic generator**, which was causing a _panic: NondetError "Non-determinism in I/O actions occurred!"_ when trying to count the number of solutions.

After these struggles I made the decision to switch to the **KiCS2** implementation of Curry, which first compiles to **Haskell** source code, because that should produce a faster executable than the Prolog based Curry implementation with: PAKCS: The Portland Aachen Kiel Curry System: https://www.curry-lang.org/pakcs/

<br/>

## Maps of Australia and Germany for KiCS2 Curry

https://www.curry-lang.org/kics2/download.html

https://github.com/curry-language/kics2

I took the latest _kics2-3.5.0-x86_64-linux.tar.gz (built on Ubuntu 24.04)_ sources (as of 2026-06-20) and built KiCS2 Curry like this:

```
$ sudo apt-get install haskell-stack
...
$ tar xvzf kics2-3.5.0-x86_64-linux.tar.gz
...
$ cd kics2-3.5.0-x86_64-linux
$ make  # be patient here! This may take some time.
...
==> Successfully bootstrapped KiCS2!
==> The executables are located in ~/scripts/Curry/KiCS2/kics2-3.5.0-x86_64-linux/bin
$ 
```

Finally, I added the above given path to the KiCS2 executables in my _~/.bashrc_ file, activated it and checked the KiCS2 version with wrapper command _curry_:

```
$ curry -V
 _  _  ____  ___  ___  ___
( )/ )(_  _)/ __)/ __)(__ \ 
 )  (  _)(_( (__ \__ \ / _/ 
(_)\_)(____)\___)(___/(____)

Version 3.5.0-b2 of 2025-12-15 (installed at Mon Dec 15 22:46:06 CET 2025)
$
```

At first, I tried to compile the original [graph_4coloring_Australia_Curry2Go.curry](./graph_4coloring_Australia_Curry2Go.curry) program, and executed it:

```
$ curry :load graph_4coloring_Australia_Curry2Go.curry :save :quit  # building an executable
...
Executable saved in 'graph_4coloring_Australia_Curry2Go'
$ ./graph_4coloring_Australia_Curry2Go | wc -l
576
$ 
```

576 solutions sounds fine!

So, the next step was to expand the original _graph_4coloring_Australia_Curry2Go.curry_ source code to provide more informative output on the terminal, like the Prolog programs.

These are the changed lines of source code for the [KiCS2 version of the program](./graph_4coloring_Australia_KiCS2.curry):

```
import Control.Search.SearchTree  -- getSearchTree function
...
-- main = correct aColor aColor aColor aColor aColor aColor aColor  -- original code: OK
...
solution = correct aColor aColor aColor aColor aColor aColor aColor

main :: IO ()
main = do
  t <- getSearchTree solution
  let sols = allValuesDFS t
  putStrLn ("number N of different solutions = " ++ show (length sols))
  putStrLn ("\n               [NT,QL,NSW,VIC,SA,WA,TAS]")
  putStrLn ("1st solution = " ++ show (head sols))
  putStrLn ("...")
  putStrLn ("Last solution = " ++ show (head (reverse sols)))
```

Compile it and run it:

```
$ curry :load graph_4coloring_Australia_KiCS2.curry :save :quit  # building an executable
$ ./graph_4coloring_Australia_KiCS2
number N of different solutions = 576

               [NT,QL,NSW,VIC,SA,WA,TAS]
1st solution = [Green,Red,Green,Red,Blue,Red,Green]
...
Last solution = [Blue,Yellow,Blue,Yellow,Green,Yellow,Blue]
$
```

<br/>

Microsoft Copilot gave me basically this answer, why library module file _SearchTree.curry_ (for _import Control.Search.SearchTree_) cannot be used with Curry2Go, but with KiCS2 (and PAKCS): 

> ..module Control.Search.SearchTree cannot be used with Curry2Go, and it’s structural. someSearchTree external, emptyVS external, addVS external, failVS external, vsToList external are external primitives. They are implemented in KiCS2 (→ Haskell) and in PAKCS (→ Prolog), but not in Curry2Go. Curry2Go has no runtime support for: encapsulated search, search trees, set functions, external data types, external operations.

<br/>

Then I made a [KiCS2 Curry program](./graph_4coloring_Germany_KiCS2.curry) for the much bigger map coloring problem of Germany to compare it with my implementations in various Prolog systems in terms of execution speed:

```
$ time ./graph_4coloring_Germany_KiCS2
number N of different solutions = 191808

               [SH,MV,HH,HB,NI,ST,BE,BB,SN,NW,HE,TH,RP,SL,BW,BY]
1st solution = [Red,Blue,Blue,Red,Green,Blue,Green,Red,Green,Red,Blue,Red,Green,Red,Red,Yellow]
...
Last solution = [Yellow,Green,Green,Yellow,Blue,Green,Blue,Yellow,Blue,Yellow,Green,Yellow,Blue,Yellow,Yellow,Red]

real	0m29.179s
...
$ 
```

..which comes to the same 1st and last solution than the [ALS Prolog version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Germany2g_ALS.pro).

But oh Boy! With an execution time of about 29 seconds for one program run, the KiCS2 Curry program is more than 11 times slower than the slowest Prolog system in this benchmark,
and that is ALS Prolog with about 2.63 seconds: [The TL;DR execution speed diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#the-tldr-execution-speed-diagram)

<br/>

## The Curry Package Manager (CPM)

For the speed part of the microbenchmark program, the CPM with command _cypm_ is being used, because it conveniently allows you to tap into some 160 Curry libraries from your Curry program:

> CPM is already part of recent distributions of the Curry systems PAKCS (Version 1.15.0 or higher), KiCS2 (Version 0.6.0 or higher), and Curry2Go.

from: https://www.curry-language.org/tools/cpm/

Do it like this: 

```
$ cypm new random_streams_for_perf_stats  # create your project
$ cd random_streams_for_perf_stats
$ cypm add random  # add this package
$ cypm add printf  # add this package
```

You may do a very basic test on a Curry package like this, here package random:

```
$ cypm info random
random-3.0.0
------------
Version      3.0.0
Author       Sergio Antoy <antoy@cs.pdx.edu>
Maintainer   Michael Hanus <mh@informatik.uni-kiel.de>
Synopsis     Library for pseudo-random number generation
Category     Programming Numeric
Dependencies
    base >= 3.0.0,  < 4.0.0
    time >= 3.0.0,  < 4.0.0
$
```

However, I recommend to scroll Curry packages from here: https://cpm.curry-lang.org/  (there are still other, "old school" ways to do that.)

Next problem: I noticed that not all available functions are exported, here function _convertToBase_ in the printf package: https://cpm.curry-lang.org/pkgs/printf-3.0.0-src.html. This function is located in library source code file _Format.curry_. So, I just copied and adapted that function for my own program: :wink:

```
convertToBase :: Int -> Int -> String
convertToBase b n = 
    if (n == 0) then "0"
      else cTB "" b n
  where
    cTB :: String -> Int -> Int -> String
    cTB acc base m = if (m == 0) then acc else
      let dr = ((div m base),(mod m base))
          d  = (fst dr)
          r  = (snd dr)
          st = if (r < 10) then (show r) else
            case r of
              10 -> "a"
              11 -> "b"
              12 -> "c"
              13 -> "d"
              14 -> "e"
              15 -> "f"
      in cTB (st ++ acc) b d
```






tbd

<br/>

##_end
