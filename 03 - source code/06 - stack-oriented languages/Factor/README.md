# Factor

https://factorcode.org/

https://github.com/factor/factor/

https://concatenative.org/wiki/view/Factor/Features/The%20language

<br/>

I picked up more high-level Factor as an alternative to more low-level [Forth](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Forth#from-forth-to-factor) to implement my microbenchmark program.

This language even has Haskell-style monads implemented: [monads vocabulary](https://docs.factorcode.org/content/vocab-monads.html)

<br/>

> [!NOTE]
> Getting exception handling right in this language, even though being a "high-level" stack-oriented language, may take patience! Look for examples for _recover_ in the GitHub repository!

<br/>

Even in the year 2026, the stack-oriented programming paradigm is a tough one for "Big AI". Pattern matching is just not enough, but "balancing the stack" is the key to success here. And this means that you have to keep track with an evolving stack throughout the program, something "Big AI" apparently doesn't care about.

So what happens at the LLM's when the Forth or Factor compiler emits an error message like this: "The input quotations to 'if*' do not all leave the stack at the same height" ?

Literally, "digging deeper" into the stack with more _drop's_ and words like this, only to push their proposed solutions further away from a real solution.

By the way: _words_ are functions in other languages (basically).
 
<br/>

Otherwise, there could be more informative and non-trivial **source code examples** in the [official documentation](https://docs.factorcode.org/content/article-handbook.html). Looking into the repository with the source code of a (demanding) programming language has its limits.

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

How to basically implement a factorial calculation is already explained in chapter [Defining our first word](https://docs.factorcode.org/content/article-tour-first-word.html), "only" things to add are:

- user input,
- type conversions,
- type checks,
- mastering the stack,
- control flow ("if-then-else")
- and concatenated terminal output:

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

### Control flow in Factor

Specifically getting the control flow with branching ("if-then-else") right in Factor takes time and experience, because exact "balancing the stack" is needed.

I noticed that also "Big AI" often suggests using _when_, when only the success path is needed. Here's a typical example for checking two conditions with using two nested [quotations](https://docs.factorcode.org/content/article-quotations.html) ([...]):

```
            char0_1 first char_pool member? [
                i length < [
                    char0_1 pw_accum push
                    i 1 + i!
                ] when
            ] when

```

Quotations are essential in Factor and are anonymous functions (values denoting a snippet of code) which can be used as values.

<br/>

## Microbenchmark program in Factor: exception handling and balancing the stack

I think that doing _very_ stack-oriented programming is _really_ hard for man and machine.

And I think that my implementation of the microbenchmark program, with big help from Google AI in lots of iterations, just shows it.
There's a lot of imperative (remember the _!_ in [OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml): _i := !i + 1;_ ?) and functional tinkering going on from my point of view with for example heavily using the _locals_ vocabulary with _::_, _let_ and _:>_ for new lexical variables.

<br/>

However, the hardest part was implementing **exception handling** when writing (a string) to a file.

Finding a solution in low-level [Forth](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Forth#forth) is not so hard, because source code for "balancing the stack" between the error path and the succes path is apparently amply available on the Internet:

```
\ Helper word that performs ALL risky file actions
: safe-file-operations ( c-addr u filename-addr filename-u -- )
  \ 1. Try to open/create the file
  w/o create-file throw               ( c-addr u fileid )
  \ 2. Try to write the string data to it
  >r                                  ( c-addr u ) ( R: fileid )
  r@ write-file throw
  \ 3. Try to close the file
  r> close-file throw ;

: main ( c-addr u filename-addr filename-u -- )
  ['] safe-file-operations catch ?dup if
    \ Error Path: Something failed inside safe-file-operations.
    ." File Operation Failed! Reason: "
    dup .error cr         \ Prints "Permission denied" instead of "-525"
    drop                  \ Drops the duplicate error code copy
    2drop 2drop           \ Clean up the initial string and filename parameters from stack
  else
    \ Success Path
    ." File written successfully" cr
  then ;
```

However, to implement such a solution in Factor turned out to be too tough (still) for Google AI and Microsoft Copilot. Often I ran into this compilation error:

```
The input quotations to 'recover' do not all leave the stack at the same height
```

Only when I searched [Factor's GitHub repository](https://github.com/factor/factor) intensively for constructs with _recover_, I found the key to success at: https://github.com/factor/factor/blob/main/basis/ftp/server/server.factor:

```
M: ftp-get handle-passive-command
    [  ! --- success path ---
        path>>
        [ transfer-outgoing-file ]
        [ binary <file-reader> swap stream-copy ] bi
        finish-file-transfer
    ] [  ! --- error path ---
        3drop "File transfer failed" ftp-error
    ] recover ;
```

Word _3drop_ drops 3 items from the top of the datastack in the error path to balance the stack between success path and error path. Based on this idea, Google AI got the **Stack layout** right in helper word _write-to-file_ after some iterations:

```
: write-to-file ( path string -- )
    [
        ! --- success path ---
        swap  ! swap path string
        utf8 <file-writer>              ! open file for writing
        [ write ] with-output-stream    ! write the string
        "Success: wrote to file." print
    ]
    [
        ! --- error path ---
        !  Stack layout here: string path error-obj
        "Error: could not write to file. Reason: " write
        ! 1. Duplicate the error object on top and print it cleanly
        dup error.                      ! Consumes the duplicate copy
        ! 2. Drop the remaining 3 items (string path error-obj) to clear the stack
        3drop         
    ] recover ;

```

<br/>

It wasn't then too difficult to get from above source code to my final solution:

```
:: write_to_file ( path string file_type -- )
    [let
        path :> p
        string :> s
        file_type :> ft
        [
            ! --- success path ---
            s p utf8 <file-writer>          ! open file for writing
            [ write ] with-output-stream    ! write the string
            ft write " stream has been written to disk under name: " write p print
        ]
        [
            ! --- error path ---
            ! Stack layout here: error-obj (locals p and s are handled by let)
            "could not write to file: " write p write " ! -- " write

            ! Extract the failing system word slot (e.g., 'open')
            dup word>> name>> "Unix system call '" write write "' failed and " write

            ! Extract the message and errno slots (e.g., 'Permission denied' (13))
            dup message>> write " (" write
            dup errno>> number>string write ")" print

            drop  ! drop the error object to balance stack
        ] recover
    ] ;
```
  
Here's the polished terminal output of a failure case when trying to write the first big string to a file, success case when writing the second big string. The most important thing here is anyway that in case of an exception the program doesn't terminate:

```
$ factor random_streams_for_perf_stats.factor

generating a random bit stream...
could not write to file: random_bitstring.bin ! -- Unix system call 'open' failed and Permission denied (13)
Byte stream has been written to disk under name: random_bitstring.byte
$
```

<br/>

## Making a standalone executable

Making a standalone executable in Factor (with default configuration) on the console takes some steps. Do it like this:

```
$ mkdir -p ./random_streams_for_perf_stats  # -p: no error if existing, make parent directories as needed
$ cp ./random_streams_for_perf_stats.factor ./random_streams_for_perf_stats  # this source code copy is actually needed!
$ factor -e='"extra" "~/scripts/Factor" add-vocab-root "random_streams_for_perf_stats" deploy'  # be patient; will take some time!
...
Saving final image

--- Data stack:
"extra"
$
```

The compiled Factor image is hopefully now located here and ready for execution:

```
$ time ./factor-linux-x86-64-2026-02-11-19-38/factor/random_streams_for_perf_stats/random_streams_for_perf_stats.out

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.065s
...
$
```

It runs about 60% faster than invoking the Factor interpreter directly (_$ factor \<Factor program\>_).

It seems that the default configuration is already trying to generate an optimized program in terms of execution speed: https://github.com/factor/factor/blob/master/basis/tools/deploy/deploy-docs.factor

<br/>

## Regular expressions in Factor

While interpreting the [full microbenchmark program](./random_bitstring_and_flexible_password_generator.factor), which uses regular expressions, just worked fine, the compiled version had a problem:

```
$ ./factor-linux-x86-64-2026-02-11-19-38/factor/random_bitstring_and_flexible_password_generator/random_bitstring_and_flexible_password_generator.out

generating a random bit stream...
Bit stream has been written to disk under name: random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

Password of 12 printable chars OK? 'y' or another integer number >= 8: y

Do you want me to use special characters like .;,+*... ? 'y' or 'n': n
You have triggered a bug in Factor. Please report.
critical_error: The die word was called by the library.: 0
Starting low level debugger...
Basic commands:
  q ^D             -- quit Factor
  c                -- continue executing Factor - NOT SAFE
  t                -- throw exception in Factor - NOT SAFE
  .s .r .c         -- print data, retain, call stacks
  help             -- full help, including advanced commands

> q
$
```

This was the code with regular expressions:

```
        ...
        with_special_chars [ [
            "^[!-~]$"         ! true branch
        ] [
            "^[A-Za-z0-9]$"   ! false branch
        ] if ] :> pattern
        pattern call :> pattern_str  ! essential: evaluates block and pops string into 'pattern_str': Google AI

        n_char x pattern_str pw_generator :> pw_chars_  ! pw_chars_ is still an array of chars
        ...

:: pw_generator ( length random_nbrs char_pool_str -- pw_array )
    [let
        length <vector> :> pw_accum
        0               :> i!  ! mutable char counter for the password
        0               :> j!  ! mutable counter for x
        char_pool_str <regexp> :> pattern_regex  ! this takes time to evaluate!

        [ i length < ] [  ! do as long as i < length
            j random_nbrs nth 2 >base :> bin0_
            bin0_ 16 CHAR: 0 pad-head :> bin0

            bin0 8 head :> bin0_0
            bin0 8 tail :> bin0_1

            bin0_0 2 base> 1string :> char0_0
            bin0_1 2 base> 1string :> char0_1

            char0_0 pattern_regex matches? [
                char0_0 pw_accum push
                i 1 + i!  ! Increment your password character counter
            ] when

            char0_1 pattern_regex matches? [
            ! If the regex matches, check if we still need more characters
                i length < [
                    char0_1 pw_accum push
                    i 1 + i!  ! Increment your password character counter
                ] when
            ] when

            j 1 + j!
        ] while

        pw_accum   >array
    ] ;
```

<br/>

Therefore, I decided to only feature a simple and robust string-based solution as my official solution:

```
        ...
        with_special_chars [
            33 126 [a..b] >string  ! MS Bing AI: this produces the full printable ASCII range from ! to ~
        ] [
            65 90 [a..b]  >string         ! uppercase A–Z
            97 122 [a..b] >string append  ! lowercase a–z
            48 57 [a..b]  >string append  ! digits 0–9
        ] if :> char_set
        ! char_set .  ! for testing

        n_char x char_set pw_generator :> pw_chars_  ! pw_chars_ is still an array of chars
        ...

:: pw_generator ( length random_nbrs char_pool -- pw_array )
    [let
        length <vector> :> pw_accum
        0               :> i!  ! mutable char counter for the password
        0               :> j!  ! mutable counter for x

        [ i length < ] [  ! do as long as i < length
            j random_nbrs nth 2 >base :> bin0_
            bin0_ 16 CHAR: 0 pad-head :> bin0

            bin0 8 head :> bin0_0
            bin0 8 tail :> bin0_1

            bin0_0 2 base> 1string :> char0_0
            bin0_1 2 base> 1string :> char0_1

            char0_0 first char_pool member? [
                char0_0 pw_accum push
                i 1 + i!
            ] when

            char0_1 first char_pool member? [
                i length < [
                    char0_1 pw_accum push
                    i 1 + i!
                ] when
            ] when

            j 1 + j!
        ] while

        pw_accum   >array
    ] ;
```

This Factor code can be safely compiled into a standalone executable which doesn't crash (on Factor version _Factor 0.102 x86.64 ... Feb 11 2026 ..._) according to my tests.

<br/>

##_end
