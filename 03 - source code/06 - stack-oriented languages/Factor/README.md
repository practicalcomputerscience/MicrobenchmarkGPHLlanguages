2026-07-01: work in progress

<br/>
  
# Factor

https://factorcode.org/

https://github.com/factor/factor/

https://concatenative.org/wiki/view/Factor/Features/The%20language

<br/>

I picked up more high-level Factor as an alternative to more low-level [Forth](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Forth#from-forth-to-factor).
 
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

Mostly with the help of the chapter [Learning the Tools](https://docs.factorcode.org/content/article-tour-tools.html), I was able to get this complete [Factor script](./fibonacci.factor) done:

```
USING: io kernel math prettyprint ;  ! USING: declares external vocabularies to borrow tools from

IN: fibonacci  ! defines the current vocabulary (home namespace) where all subsequent words created will live

DEFER: fib-rec  ! DEFER: to define two mutually recursive words:
: fib ( n -- f(n) ) dup 2 < [ ] [ fib-rec ] if ;
: fib-rec ( n -- f(n) ) [ 1 - fib ] [ 2 - fib ] bi + ;

MAIN: [ 47 fib . ]  ! MAIN: declares the entry point
```

..done to calculate the 47th Fibonacci number inefficiently:

```
$ time factor fibonacci.factor 
2971215073
real	0m35.218s
...
$
```

With an execution time of about 35 seconds, this script is in the range of YJIT compiled Ruby: [Execution speeds table](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02a%20-%20benchmarking%20with%20inefficient%20Fibonacci%20number%20calculations#execution-speeds-table), so basically "middle-of-the-road" efficient.

<br/>

### Program factorial.factor for terminal input and output

Analogously to this exercise in [Hy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Hy#program-factorialhy-for-terminal-input-and-output), the next step is to master input and output operations on the terminal, often a critical thing in a niche programming language.

How to implement a factorial calculation is already explained in chapter [Defining our first word](https://docs.factorcode.org/content/article-tour-first-word.html):

```
: prod ( {x1,...,xn} -- x1*...*xn ) 1 [ * ] reduce ;  ! (..) just documents the stack effect
: fact ( n -- n! ) [1..b] prod ;
```



tbd

<br/>

tbd


<br/>

tbd


<br/>

tbd



<br/>

##_end
