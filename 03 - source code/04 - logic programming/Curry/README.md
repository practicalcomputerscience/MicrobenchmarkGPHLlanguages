2026-06-19: work in progress tbd

- tbd: coloring map of Australia for benchmarking (the one that couldn't be done with Mercury or Oz)
- xxx
- xxx

<br/>

# Curry

After my sobering experiences with [Mercury](tbd) and [Oz](tbd), I have been searching for another (and still maintained) language to combine the "natural fits" logic and functional programming,
and then found: [Curry](https://www.curry-lang.org)

> Curry is a declarative multi-paradigm programming language which combines in a seamless way features from functional programming ... and logic programming ... .

> Curry is called a declarative language, because computed results are independent of the time and order of evaluation, which simplifies reasoning on programs. Side effects can be modeled as “IO” operations, i.e., a declarative description of what to do. Operations are constructed by expressions only, there are no statements or instructions, and every binding to a variable is immutable.

<br/>

Again, we see the phenomenon of multiple implementations (with one already dead): PAKCS, KiCS2, Curry2Go, MCC: https://www.curry-lang.org/implementations/overview/

After some reading, I gave **Curry2Go** a try to compile Curry source code into a [Go](tbd) based executable: https://www.curry-lang.org/curry2go/

<br/>

## Installation tips

.bashrc etc.

tbd

<br/>

## A first Curry program

This was my first Curry program, with a little help from Google AI, named _factorial.curry_:

```
import System.Environment (getArgs)

factorial :: Int -> Int
factorial n | n == 0    = 1
            | n > 0     = n * factorial (n - 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:_) -> print (factorial (read x))
    _     -> putStrLn "Usage: program <n>"
```

Compile and run it like this:

```
$ curry2goc factorial  # no file extension needed here
$ ./factorial 5
120
$
```

Voilà!

tbd

<br/>

##_end
