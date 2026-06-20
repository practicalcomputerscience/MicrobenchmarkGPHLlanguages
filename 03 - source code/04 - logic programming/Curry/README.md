2026-06-19: work in progress tbd

<br/>

# Curry

https://www.curry-lang.org (*)

<br/>

> [!TIP]
> Use the PAKCS (→ Prolog) and KiCS2 (→ Haskell) implementations of Curry, but not the limited Curry2Go (→ Go) implementation!

<br/>

So, two programming language names have been attributed to [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), American mathematician and logician (1900-1982).

---

Table of contents:

- [Idea of Curry: integrating logic and functional programming in a purely declarative style]()
- [Maps of Australia and Germany for KiCS2](#maps-of-australia-and-germany-for-kics2)

<br/>

---

## Idea of Curry: integrating logic and functional programming in a purely declarative style

After my sobering experiences with [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) and [Oz](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Oz#oz), I have been searching for another (and still maintained) language to combine the "natural fits" logic and functional programming, and found Curry (*):

> Curry is a declarative multi-paradigm programming language which combines in a seamless way features from functional programming ... and logic programming ... .

> Curry is called a declarative language, because computed results are independent of the time and order of evaluation, which simplifies reasoning on programs. Side effects can be modeled as “IO” operations, i.e., a declarative description of what to do. Operations are constructed by expressions only, there are no statements or instructions, and every binding to a variable is immutable.

Again, we see the phenomenon of multiple implementations (with one being already dead): PAKCS, KiCS2, Curry2Go, MCC: https://www.curry-lang.org/implementations/overview/

And again, same like [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury), also Curry is a "purely declarative" programming language (*).

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

## Maps of Australia and Germany for KiCS2

https://www.curry-lang.org/kics2/download.html

https://github.com/curry-language/kics2

I took the latest _kics2-3.5.0-x86_64-linux.tar.gz (built on Ubuntu 24.04)_ sources (as of 2026-06-20).

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

Finally, I added the given path to the KiCS2 executable, activated my _~/.bashrc_ file and checked the KiCS2 version with wrapper command _curry_:

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

MS Copilot gave me basically this answer, why library module file _SearchTree.curry_ (for _import Control.Search.SearchTree_) cannot be used with Curry2Go, but with KiCS2 (and PAKCS): 

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



tbd

<br/>

##_end
