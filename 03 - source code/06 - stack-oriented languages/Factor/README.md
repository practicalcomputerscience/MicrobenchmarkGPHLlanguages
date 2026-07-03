2026-07-01: work in progress

<br/>
  
# Factor

https://factorcode.org/

https://github.com/factor/factor/

https://concatenative.org/wiki/view/Factor/Features/The%20language

<br/>

I picked up more high-level Factor as an alternative to more low-level [Forth](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Forth#from-forth-to-factor).

This language even has Haskell-style monads implemented: [monads vocabulary](https://docs.factorcode.org/content/vocab-monads.html)

<br/>

> [!NOTE]
> Getting exception handling right in this language, even though being a "high-level" stack-oriented language, may take patience! Look for examples with _recover_ in its GitHub repository!
 
<br/>

## Installation tips

I took latest (as of 2027-07-02) pre-compiled binaries _factor-linux-x86-64-2026-02-11-19-38.tar.gz_ from here: https://builds.factorcode.org/package?os=linux&cpu=x86.64,
unzipped it and added the following line to my _~/.bashrc_ configuration line, which I then activated with usual command: _source ~/.bashrc_

```
export PATH="$HOME/scripts/Factor/factor-linux-x86-64-2026-02-11-19-38/factor:$PATH"
```

This is very important, because there's another Linux application called _factor_ for something very different! Here, we want language interpreter _factor_ to be the first program to be executed under this name.

Then, at least for Ubuntu 24 LTS, a library still has to be installed, something which is also described in _./Factor/factor-linux-x86-64-2026-02-11-19-38/factor/README.md_:

> The development branch of Factor has switched from GTK2 to GTK3 for the GUI backend.

```
$ sudo apt install libgtk-3-dev
...
$
```

When command _$ factor_ is entered, the Factor Listener window should show up like this for example, being ready to receive the first functions that push themselves on the stack:

![plot](./Factor_Listener.png)

..here the ubiquitous _"Hello, world!" print_ function in Reverse Polish Notation (RPN).

<br/>

## Tutorial

I highly recommend to first have a look into the official [Guided tour of Factor](https://docs.factorcode.org/content/article-tour.html) before doing anything more meaningful than "Hello, world!". You may directly jump into chapter [Playing with the stack](https://docs.factorcode.org/content/article-tour-stack.html).

from [Combinators](https://docs.factorcode.org/content/article-tour-combinators.html):

> [!TIP]
> ..you should write code that does as little stack shuffling as possible.

Most important stack shuffling functions are: _dup_, _drop_ and _swap_

> [!NOTE]
> ..Factor words tend to be rather shallow, using one level of nesting for each higher-order function, unlike Lisps or more generally languages based on the lambda calculus, which use one level of nesting for each function..

from same source.

<br/>

### Inefficient (recursive) Fibonacci number calculation

Mostly with the help of the chapter [Learning the Tools](https://docs.factorcode.org/content/article-tour-tools.html) and hints of the Factor interpretor, I was able to get this [Factor script](./fibonacci.factor) done:

```
USING: io kernel math prettyprint ;  ! USING: declares external vocabularies to borrow tools from

IN: fibonacci  ! defines the current vocabulary (home namespace) where all subsequent words created will live

DEFER: fib-rec  ! DEFER: to define two mutually recursive words:
: fib ( n -- f(n) ) dup 2 < [ ] [ fib-rec ] if ;
: fib-rec ( n -- f(n) ) [ 1 - fib ] [ 2 - fib ] bi + ;

MAIN: [ 47 fib . ]  ! MAIN: declares the entry point
```

..to calculate the 47th Fibonacci number inefficiently:

```
$ time factor fibonacci.factor 
2971215073
real	0m35.218s
...
$
```

With an execution time of about 35 seconds, this **interpreted** script is in the range of YJIT compiled Ruby: [Execution speeds table](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02a%20-%20benchmarking%20with%20inefficient%20Fibonacci%20number%20calculations#execution-speeds-table).

<br/>

> [!IMPORTANT]
> The _( n -- f(n) )_ in _: fib ( n -- f(n) ) dup 2 < [ ] [ fib-rec ] if ;_ has operational meaning! So craft these stack effects carefully or leave them away! (See below at _readln_).

> Stack effects are how you document the inputs from the stack and outputs to the stack for your word. You can use any identifier to name the stack elements, here we use n. Factor will perform a consistency check that the number of inputs and outputs you specify agrees with what the body does.

from [Defining our first word](https://docs.factorcode.org/content/article-tour-first-word.html).

_word_ = function in other languages.

<br/>

### Program factorial.factor for terminal input and output

Analogously to this exercise in [Hy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Hy#program-factorialhy-for-terminal-input-and-output), the next step is to master input and output operations on the terminal, often a critical thing in a niche programming language.

How to basically implement a factorial calculation is already explained in chapter [Defining our first word](https://docs.factorcode.org/content/article-tour-first-word.html), "only" things to add are user input, type conversions, type checks, mastering the stack, control flow ("if-then-else") and concatenated terminal output:

```
USING: io kernel math math.parser prettyprint ranges sequences ;  ! USING: declares external vocabularies to borrow tools from

IN: factorial_with_user_input  ! defines the current vocabulary (home namespace) where all subsequent words created will live

: prod ( {x1,...,xn} -- x1*...*xn ) 1 [ * ] reduce ;  ! (..) just documents the stack effect
: fact ( n -- n! ) [1..b] prod ;

! mostly Google AI:
: factorial_with_user_input ( -- )
    "Enter an integer n >= 1: " write flush  ! USER INPUT FROM THE TERMINAL
    readln [                                 ! Reads line from stdin and pushes string to stack
        string>number                        ! Try to convert string to number
        dup integer? [                       ! Check if integer
            dup 1 >=                         ! Check if the integer is >= 1
        ] [ f ] if [
            "factorial(" write               ! If true: calculate factorial and print
            dup number>string write
            ") = " write
            fact number>string print         ! Ends the line with print
        ] [
            drop                             ! If false: drop the invalid number/f
            "Wrong input!\n" print
            factorial_with_user_input       ! Loop: call itself to ask again
        ] if
    ] [ "No input received (EOF)." print
        factorial_with_user_input           ! Loop: call itself if user just pressed Enter
    ] if* ;

MAIN: factorial_with_user_input ! MAIN: declares the entry point
```

<br/>

## Microbenchmark program in Factor: exception handling and balancing the stack

I think that doing _very_ stack-oriented programming is _really_ hard for man and machine. And I think that my implementation of the microbenchmark program,
with big help from Google AI in lots of iterations, just shows it. There's a lot of imperative and functional tinkering going on from my point of view with for example:

- heavily using the _locals_ vocabulary with _::_, _let_ and _:>_ for new lexical variables,
- an extra word _masterloop-rec_ as the recursive part of the initializing _masterloop_

<br/>

However, the hardest part was implementing **exception handling** when writing (a string) to a file.

This is not so complicated in low-level [Forth](tbd), because "balancing the stack" ("The input quotations to 'recover' do not all leave the stack at the same height")
between the success path, that is quotation #1, and the error path, that is quotation #2, is easier (for men and machines):

```
tbd Forth word solution
```

tbd

Here the case of failure when writing the first big string to file, success when writing the second:

```
$ factor random_streams_for_perf_stats.factor

generating a random bit stream...
could not write to file: random_bitstring.bin ! -- Unix system call 'open' failed:

Permission denied (13)

It was called with the following arguments:

"~/scripts/Factor/random_bitstring.bin"
577
438

Byte stream has been written to disk under name: random_bitstring.byte
$
```



tbd


<br/>

## Making a standalone executable

tbd

<br/>

##_end
