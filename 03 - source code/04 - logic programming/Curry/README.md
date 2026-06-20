2026-06-19: work in progress tbd

- tbd: Table of contents:

<br/>

# Curry

https://www.curry-lang.org (*)

<br/>

So, two programming language names have been attributed to [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), American mathematician and logician (1900-1982).

<br/>

## Idea of Curry: integrating logic and functional programming in a purely declarative style

After my sobering experiences with [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) and [Oz](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Oz#oz), I have been searching for another (and still maintained) language to combine the "natural fits" logic and functional programming, and found Curry (*):

> Curry is a declarative multi-paradigm programming language which combines in a seamless way features from functional programming ... and logic programming ... .

> Curry is called a declarative language, because computed results are independent of the time and order of evaluation, which simplifies reasoning on programs. Side effects can be modeled as “IO” operations, i.e., a declarative description of what to do. Operations are constructed by expressions only, there are no statements or instructions, and every binding to a variable is immutable.

Again, we see the phenomenon of multiple implementations (with one being already dead): PAKCS, KiCS2, Curry2Go, MCC: https://www.curry-lang.org/implementations/overview/

And again, same like [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury), also Curry is a "purely declarative" programming language (*).

<br/>

## Curry2Go: transpiling Curry source code into Go source code for a Linux executable

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

I coded the problem of the Australian map for Curry2Go ([graph_4coloring_Australia_Curry2Go.curry](./graph_4coloring_Australia_Curry2Go.curry)), but that program is not working completely like in this ALS Prolog implementation: [graph_4coloring_Australia_ALS.pro](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog/graph_4coloring_Australia_ALS.pro)

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

## Map of Germany for KiCS2

https://www.curry-lang.org/kics2/download.html

I took the latest _kics2-3.5.0-x86_64-linux.tar.gz (built on Ubuntu 24.04)_ sources (as of 2026-06-20).

```
$ sudo apt-get install haskell-stack
...
$ tar xvzf kics2-3.5.0-x86_64-linux.tar.gz
...
$ cd kics2-3.5.0-x86_64-linux
$ make  # be patient here! This may take some time.
...
$ 
```

So, I made a KiCS2 compliant Curry program for the much bigger map coloring problem of Germany to compare it with my Prolog implementations in terms of execution speed: [The TL;DR execution speed diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#the-tldr-execution-speed-diagram):








tbd

<br/>

##_end
