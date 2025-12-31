# Bigloo Scheme

https://www-sop.inria.fr/indes/fp/Bigloo/

---

Bigloo Scheme compiles to very small binary executables, for example like this:

```
$ bigloo -call/cc -O6 random_streams_for_perf_stats.scm -o random_streams_for_perf_stats
```

..with switch _-call/cc_ used for procedure [call-with-current-continuation](https://www-sop.inria.fr/indes/fp/Bigloo/manual-chapter5.html#Control%20Features):

> Note: Since call/cc is difficult to compile efficiently, one might consider using bind-exit instead. For this reason, we decided to enable call/cc only with a compiler option. 

..and switch _-O6_ used for best opimization: [The Bigloo command line](https://www-sop.inria.fr/indes/fp/Bigloo/manual-chapter32.html#G49542)

<br/>

Enter the REPL (Read-Eval-Print Loop) like this: _$ rlwrap bigloo_ and give command _(exit)_ to exit it.

<br/>

## Full microbenchmark program in Bigloo Scheme

Since Bigloo Scheme is (now) listed in the [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments), just because it's the fastest Scheme dialect of the tested ones (with the "speed part"), it's consequential to implement the full microbenchmark program also in [Bigloo Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Bigloo/random_bitstring_and_flexible_password_generator.scm).

However, Bigloo Scheme doesn't have so "big batteries" than Racket Scheme, and thus it's number of lines of source code is higher with 124 versus 110 for Racket Scheme: [LOC ranking list](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/10%20-%20Lines%20Of%20source%20Code%20(LOC)%3A%20verbosity#loc-ranking-list).

[Number of user defined functions](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/10%20-%20Lines%20Of%20source%20Code%20(LOC)%3A%20verbosity#number-of-user-defined-functions) is also higher with 7 versus 5 for Racket Scheme.

But what counts more are **substantial algorithmic differences** between Bigloo and Racket Scheme, even visible in this rather short program.

In short: it's easier to program in Racket Scheme than in Bigloo Scheme from my point of view.

One example is **program execution order**, which can be easily maintained in (interpreted) Racket Scheme with a "natural" order of _:(define ...)_ expressions, like here for variable _n_char_ for example:

```
  ...
  (printf "\ngenerating a random bit stream...")
  ...
  (define n_char (input_a_valid_number n_char_default))
  ...
```

Such a construct is not working as intended in Bigloo Scheme, where procedure _input_a_valid_number_ as a user defined functions would be executed **before** "generating a random bit stream..." is being displayed.

To prevent this in Bigloo Scheme, one must first define _n_char_ and then imperatively set it like this:

```
...
(define n_char 0)
...
  (printf "\ngenerating a random bit stream...")
  ...
  (set! n_char (input_a_valid_number n_char_default))
  ...
```

The definition of _char_set_ also highlights, what it means to have "smaller batteries" in Bigloo Scheme:

```
(define (iota-range low high)
  (if (> low high)
      '()
      (cons low (iota-range (+ low 1) high))))

(define (string-from-range start-point end-point)
  (let* ((points (iota-range start-point end-point))
         (chars  (map integer->char points)))
    (list->string chars)))
...

  (set! char_set
    (if with_special_chars
      (string-from-range 33 126)  ; end of range is inclusive
      (string-append (string-from-range 48 57)    ; false branch
                     (string-from-range 65 90)
                     (string-from-range 97 122))))
```

Here's the definition of string _char_set_ with Racket Scheme, an elegant construct, which doesn't need any user defined functions:

```
  (define char_set
    (if with_special_chars
      (map (lambda (cp) (integer->char cp)) (for/list ([i (in-range 33 127)]) i))   ; end of range is exclusive
      (map (lambda (cp) (integer->char cp)) (append
                                              (for/list ([i (in-range 48 58)]) i)
                                              (for/list ([i (in-range 65 91)]) i)
                                              (for/list ([i (in-range 97 123)]) i)))))  ; all alphanumerical ASCII values
```

<br/>

##_end
