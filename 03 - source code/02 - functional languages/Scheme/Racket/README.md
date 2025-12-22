# Racket Scheme

https://racket-lang.org/

<br/>

Table of contents:

- [Installation tips](#installation-tips)
- [Execution speed](#execution-speed)
- [FFI's (Foreign Function Interfaces)](#ffis-foreign-function-interfaces)
- [Chez Scheme (CS)](#chez-scheme-cs)
- [Stack and heap usage](#stack-and-heap-usage)

---

## Installation tips

Download installation script _racket-9.0-x86_64-linux-buster-cs.sh_ (as of 2025-12-22) from here: https://download.racket-lang.org/, and run it like this (in Ubuntu 24 LTS): _sudo sh ./racket-9.0-x86_64-linux-buster-cs.sh_, where my answers were: "yes" (for a Unix-style distribution) - "1" (/usr/... as installation base) - "[ENTER]" (no change to target directories).

Then, have a little test:

```
$ racket --version
Welcome to Racket v9.0 [cs].
$ 
```

Otherwise, Racket has an easy build system: 
```
$ raco exe random_bitstring_and_flexible_password_generator.rkt
```
..and decent documentation: https://docs.racket-lang.org/

## Execution speed

Though it's syntax and grammar would definitely make a modern Lisp-like language, it's also easy to write slow programs if you are not careful enough or just an inexperienced beginner.

For example, Racket doesn't have a **string builder** or similar concept. Appending many little strings to one big string, with the usual function _string-append_ (https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string-append%29%29), which is part of the _racket/base_ library, can become prohibitively slow in this programming language!

At first, I set a new "record" in slowliness with this solution, resulting at 53 seconds for one program run! After numerous experiments I found this concept to be much faster with 9.9 seconds:

- first I create a mutable vector, which is a native type in Racket, with function _make-vector_ and fill it with strings, which are initialized with the required number of characters; so, some memory allocation is going on here (on the heap presumably)
- then I set the individual vector strings to the required values with _vector-set!_
- finally, I convert the vector first into a list with the _vector->list_ function and then this list with the _string-join_ function (from the _racket/string_ library) into the resulting string
- there are other little (and cheap) tricks involved, but they are not really moving the needle

So, this is basically the old solution (here without user defined functions etc.):

```
#lang racket/base        ; this is supposed to make a little faster solution
(require racket/string)  ; for string-join function

(define END 62500)
(define m 65521)  ; = 2^16 - 15
(define a 17364)
(define c 0)
(define old-seed 0)  ; initialization is needed
(define new-seed 0)
(define x '())  ; empty list

(define ini-string-bits_x "0000000000000000")
(define (vector-of-n-strings n str)
  (make-vector n str))  ; mutable, initialization
(define bits_x-vector (vector-of-n-strings END ini-string-bits_x))

(define ini-string-bits_hex "0000")
(define bits_hex-vector (vector-of-n-strings END ini-string-bits_hex))

(set! old-seed (random 1 m))

(define (main)
  (let masterloop ([i 0])
    (when (< i END)
      ; (printf "\ni = ~a" i)  ; for testing
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      (set! old-seed new-seed)
      (set! x (append x (list new-seed)))  ; append to global list x
      (let ([bits_x_str (Integer_to_bin_string new-seed)]
            [bits_hex_str (Integer_to_hex_string new-seed)])
            (vector-set! bits_x-vector i bits_x_str)
            (vector-set! bits_hex-vector i bits_hex_str)
      )
      (masterloop (+ i 1))))  ; recursion
  ; convert vector of strings into one big string:
  (define bits_x (string-join (vector->list bits_x-vector) ""))
  (define bits_hex (string-join (vector->list bits_hex-vector) ""))
)

(module+ main
  (main))
```

Here's the final and significantly faster solution, where _x_ is not starting as an empty list like above: ![final solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket/random_streams_for_perf_stats.rkt)

Also see item #2 of this list: ![My 5 best practices with Scheme dialects](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#my-5-best-practices-with-scheme-dialects)

Here's a comment for Racket version 8.17 (as of July 2025) on an improvement of the _string-append_ function: https://blog.racket-lang.org/2025/05/racket-v8-17.html:

> The string-append function has improved performance and reduced memory use for long lists of strings in the Racket CS implementation. Differences are clearly noticeable for lists of length 1 million.

Apparently, the Racket developers seem to be aware of the speed issue of the _string-append_ function (CS = Chez Scheme, see below at ![Chez Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket#chez-scheme-cs)).

### FFI's (Foreign Function Interfaces)

By the way: I was not willing to use Racket as a **wrapper language** around C source code for example for a speedier program, that is using its Foreign Function Interface: https://docs.racket-lang.org/foreign/index.html "to run performance sensitive parts of your application in C or C++".

I'm not doing this with any language in my programming language testing project.

However, one could argue that I'm doing such a thing in my Clojure program ![Clojure solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_streams_for_perf_stats_core.clj), which is tapping into the ecosystem of another programming language by using Java's _StringBuilder_ Class, but I think that this is a very different use case, because Clojure is sitting on top of Java's ecosystem and is able to use it seamlessly. And this strategy doesn't change the nature of the Clojure program at all (which still isn't super fast with at least 420 milliseconds execution time).

<br/>

### Chez Scheme (CS)

Racket is a **bytecode** language, nowadays based on Chez Scheme (CS):

- [Racket Virtual Machine Implementations](https://docs.racket-lang.org/guide/performance.html#(part._virtual-machines))
- https://blog.lambdaclass.com/rebuilding-the-racket-compiler-with-chez-scheme/

Chez Scheme is more "complete" and also speedier than other Scheme dialects in average, see at ![The Larceny Benchmarks](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#the-larceny-benchmarks).

For example, the execution time of the Chez version of my microbenchmark program dropped by more than 50% compared to my (hand optimized) Racket program - though the source code files are not identical, see from here: ![What they don't tell you in the Land of Scheme's at first](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#what-they-dont-tell-you-in-the-land-of-schemes-at-first)

However, Chez Scheme programs are by default not meant to be compiled into standalone, binary executables, but to be run like scripts, for example like this: _$ petite --script random_streams_for_perf_stats.ss_ (![Chez source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Chez/random_streams_for_perf_stats.ss))

I didn't succeed with this program to make a standalone executable from a Chez Scheme source code file: https://github.com/Blugatroff/selfcontained-chez. This is the reason why I don't further consider my Chez Scheme version.

<br/>

## Stack and heap usage

Here's another observation with Racket Scheme, here relating to its stack and heap usage (https://phoenixnap.com/kb/stack-vs-heap): as an experiment I declared the final strings as local variables _bits_x__ and _bits_hex__. However, this was a bad idea since the Ubuntu operating system killed this program after a while, presumably to prevent running out of stack memory:

```
...
  ;----------------------  recursive master loop  -----------------------------
  (let masterloop ([i 1]
                   [bits_x_ ""]
                   [bits_hex_ ""])
    (when (< i END)        
      (set! new-seed (modulo (+ (* a old-seed) c) m))
      (set! old-seed new-seed)
      
      (set! x (append x (list new-seed)))  ; append to global list x
      
      (let ([bits_x_str   (Integer_to_bin_string new-seed)]   ; string conversion to binary string with padding
            [bits_hex_str (Integer_to_hex_string new-seed)])  ; string conversion to hex string with padding
            
      (masterloop (+ i 1)
                  (string-append bits_x_ bits_x_str)
                  (string-append bits_hex_ bits_hex_str))))

    (when (= i END)
      (set! bits_x bits_x_)
      (set! bits_hex bits_hex_)))
...
```

<br/>

##_end
