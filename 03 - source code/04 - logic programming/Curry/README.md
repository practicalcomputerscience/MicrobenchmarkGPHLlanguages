2026-06-19: work in progress tbd

- tbd: coloring map of Australia for benchmarking (the one that couldn't be done with Mercury or Oz):
<br/>

# Curry

https://www.curry-lang.org (*)

https://www.curry-lang.org/curry2go/

https://github.com/curry-language/curry2go

<br/>

So, two programming language names have been attributed to [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), American mathematician and logician (1900-1982).

<br/>

## Idea of Curry: integrating logic and functional programming

After my sobering experiences with [Mercury](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Mercury#mercury) and [Oz](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Oz#oz), I have been searching for another (and still maintained) language to combine the "natural fits" logic and functional programming, and found Curry (*):

> Curry is a declarative multi-paradigm programming language which combines in a seamless way features from functional programming ... and logic programming ... .

> Curry is called a declarative language, because computed results are independent of the time and order of evaluation, which simplifies reasoning on programs. Side effects can be modeled as “IO” operations, i.e., a declarative description of what to do. Operations are constructed by expressions only, there are no statements or instructions, and every binding to a variable is immutable.

Again, we see the phenomenon of multiple implementations (with one already dead): PAKCS, KiCS2, Curry2Go, MCC: https://www.curry-lang.org/implementations/overview/

<br/>

## Curry2Go: transpiling Curry source code into Go source code for a Linux executable

After some reading, I gave Curry2Go a try to compile Curry source code into a [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go#go) based executable: https://www.curry-lang.org/curry2go/

> [!IMPORTANT]
> However, making the choice of Curry2Go has a big drawback. The "usual" code examples and code snippets from the "usual" tutorials do not work here!

For example, this tutorial is meant for the PAKCS implementation, though the first exercise works in curry2go: https://www.curry-lang.org/docs/tutorial/html/

Also these examples are only working in PAKS: https://www.curry-lang.org/pakcs/examples/

<br/>

## Installation tips

For the Curry2Go compiler to work, a Go compiler needs to be installed first: https://go.dev/

Then, I followed the official installation instructions with: _$ curl -sSL https://www.curry-lang.org/curry2go/download.sh | sh_

..added _export PATH="$HOME/scripts/Curry/Curry2Go/bin:$PATH"_ to my _.bashrc_ file, and finally activated that with: _$ source ~/.bashrc_

The Curry2Go REPL (Read-Eval-Print Loop) can now be started like this:

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

<br/>

## A first Curry2Go program

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

tbd

<br/>

##_end
