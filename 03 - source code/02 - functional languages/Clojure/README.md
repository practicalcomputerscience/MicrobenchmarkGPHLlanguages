# Clojure

https://clojure.org/

https://leiningen.org/

The official **Clojure Cheat Sheet**, which is practical for orientation in this feature rich language: https://clojure.org/api/cheatsheet

---

Table of contents:

- [Installation tips](#installation-tips)
- [On execution speed in Clojure](#on-execution-speed-in-clojure)
- [Initial struggles with execution speed](#initial-struggles-with-execution-speed)
- [On complexity in Clojure](#on-complexity-in-clojure)
- [Recursive loops with loop-recur: simple at the bottom, simple at the top!](#recursive-loops-with-loop-recur-simple-at-the-bottom-simple-at-the-top)
- [The usual way of error handling in Clojure is imperative](#the-usual-way-of-error-handling-in-clojure-is-imperative)
- [Clojure is a demanding functional programming language](#clojure-is-a-demanding-functional-programming-language)

<br/>

---

### Installation tips

Rename:
- _random_bitstring_and_flexible_password_generator_core.clj_ into _core.clj_
- _random_bitstring_and_flexible_password_generator_project.clj_ into _project.clj_
- _random_streams_for_perf_stats_core.clj_ into _core.clj_
- _random_streams_for_perf_stats_project.clj_ into _project.clj_

..in their related app root directories (named _random_bitstring_and_flexible_password_generator_ and _random_streams_for_perf_stats_ respectively), each built with the Leiningen build tool like this for example:

```
$ lein new app random_bitstring_and_flexible_password_generator
$ cd random_bitstring_and_flexible_password_generator
$ lein check
$ lein run
$ lein uberjar
$ java -jar ./target/uberjar/random_bitstring_and_flexible_password_generator-0.1.0-SNAPSHOT-standalone.jar
```

You may also have a look at this page: [Clojure](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/04%20-%20GraalVM/README.md#clojure)

<br/>

### On execution speed in Clojure

In both _project.clj_ files I'm not skipping aot (ahead-of-time) compilation:

```
...
  ; :main ^:skip-aot random-bitstring-and-flexible-password-generator.core
  :main random-bitstring-and-flexible-password-generator.core
...
```

..with the aim to speed up application startup. See from here: https://clojure.org/reference/compilation

However, I didn't notice any statistically relevant difference to not skipping it with:

```
...
  :main random_streams_for_perf_stats.core
...
```

..in terms of execution speed. Both methods seem to come to the same results statistically.

### Initial struggles with execution speed

As usual, I refer to the "speed part" of the program with this source code: ![random_streams_for_perf_stats](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_streams_for_perf_stats_core.clj)

My first program version with building a vector of integer values (with no pre-allocations of memory) and naive string concatenations on initially two empty strings was a speed desaster. This first version was about 50 times slower than my Python script or Scala program!

<br/>

Later I tapped deeper into Clojure's ecosystem, which extends into the Java ecosystem (https://clojure.org/): 

> Clojure provides easy access to the Java frameworks, ...

.. and discovered Java's _StringBuilder_ class: https://cr.openjdk.org/~pminborg/panama/21/v1/javadoc/java.base/java/lang/StringBuilder.html:

```
  (defn masterloop [n seed]  ; the pseudo random number generator (PRNG)
    (loop ...
           bits_x_ (StringBuilder.)  ; using Java StringBuilder Class
           bits_hex_ (StringBuilder.)]
      ...
      (if (zero? count)                ; Continuation-Passing Style (CPS): Accept part
        [acc_nbr_v (.toString bits_x_) (.toString bits_hex_)]  ; CPS: Return part, here in form of a vector
        (let ...  ; CPS: Continuation part to provide the next step in the computation
          (...
                        ; string concatenations:
                        (.append bits_x_   bits_x_str)  ; using Java StringBuilder Class method .append
                        (.append bits_hex_ bits_hex_str)
          )))
    )
  )
```

This alone, and no other experiments, including experimenting with _Transient Data Structures_ (https://clojure.org/reference/transients), led to an execution speed reduction from around 7400 milliseconds to roughly 420 milliseconds.

<br/>

### On complexity in Clojure

Here I refer to the full program with this source code: ![random_bitstring_and_flexible_password_generator](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_bitstring_and_flexible_password_generator_core.clj) and specifically to this user defined function:

```
(defn input_a_valid_number [n_char]
  (print "\nPassword of" n_char "printable chars OK? 'y' or another integer number >= 8: ")
  (flush)  ; needed, see: https://clojuredocs.org/clojure.core/read-line
  (def answer_str (read-line))

  (if (= "y" answer_str)
    ; (println "n_char after 'y' ==" n_char "==")  ; for testing
    n_char  ; return n_char immediately as a good answer
    (do  ; do can be used to mark a series of expressions that need to be treated as one block
      (try
        (def n_char_ (Integer/parseInt answer_str))  ; Integer/parseInt: calling the Java ecosystem ("Java Interop")
        ; (println "n_char_ ==" n_char_ "==")  ; for testing
        (if (>= n_char_ 8)
          n_char_  ; return a good number
          (do (println "enter an integer number >= 8 or 'y'")
              ; (println "n_char ==" n_char "==")  ; for testing
              (input_a_valid_number n_char)))  ; recursion
        (catch Exception e
          (do (println "enter an integer number >= 8 or 'y'")
              ; (println "n_char ==" n_char "==")  ; for testing
              (input_a_valid_number n_char))))  ; recursion
    )))
```

..because from my point of view Clojure is not the easiest functional programming language to learn.

Even experienced Clojure coders can struggle with it, see for example from below at [Clojure is a demanding functional programming language](clojure-is-a-demanding-functional-programming-language). So, some explanations might be helpful.

I **do not** do certain things at the _input_a_valid_number_ function, like for example:

- using special forms _loop_ and _recur_ as a solution for recursion: https://clojuredocs.org/clojure.core/loop - because this can bring the layman easily into a severe conflict with the **error handling** because correctly placing _recur_ may quickly cause headaches for him
- this means that conventional error handling with a _try-catch_ block is used, a construct which is as imperative programming style as it can get: https://zio.dev/reference/error-management/imperative-vs-declarative/
- using _clojure.edn_ (https://clojuredocs.org/clojure.edn) for potentially "unsafe" user input. As long as it's good enough I just tap into the Java ecosystem and use for example Java method _Integer/parseInt_. A solution for corner cases can be implemented later anyway, but first start simple
- using the "metaprogramming feature" macro: specifically this proposal can be often seen for Clojure, probably for its LISP heritage, as a way out of a _loop-try-catch_ conflict
- using special form _let_: https://clojuredocs.org/clojure.core/let -- when I see _let_ in a Clojure program ("Evaluates the exprs in a lexical context in which the symbols in the binding-forms are bound to their respective init-exprs or parts therein."), it's an indicator to me that things start to become more complicated. Yes, _let_ may be unavoidable and the best solution in cases when
locally introducing new variables is the best way out of an algorithmic challenge

I'm not so surprised to see the _loop-recur_ pair as a common proposal for all kinds of recursive or iteration problems, when students see it at first when (shortly) taught about control flow in Clojure, like here for example: https://soft.vub.ac.be/~tvcutsem/talks/presentations/Clojure-intro.pdf (*)

<br/>

Instead I do this:

- using special form _do_: https://clojuredocs.org/clojure.core/do -- this simple form has become my "Swiss army knife" in Clojure: it's good for my imperative coding style impetus: I just concatenate some expressions to do stuff in a sequential order and the (return value of the) last expression wins
- doing recursion the simple way, with first thinking of how _if-then-else_ works in a specific functional programming language: above I recursively call user defined function _input_a_valid_number_ inside two _do_ forms as part of the _then_-part. And this in the same way I call _input_a_valid_number_ from the _main_ function: no _recur_ (in Clojure), or _let rec_ like in the ![OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml/password_encryption_main.ml) version, to be seen!

<br/>

### Recursive loops with loop-recur: simple at the bottom, simple at the top!

Some other tips for the ubiquitous recursive loops in Clojure, so for the _**loop-recur**_-constructs: keep it simple with:

- try to avoid fancy _atom, (let […] (…)_) or other constructs
- instead, make inside the loop some (immutable) variable definitions with _(def varN (<more or less complex, potentially conditionally based (if-then-else), calc’s for varN>))_ because they can potentially all be used at the final loop expression at _(recur …)_
- some of these calculations can be done in user defined functions outside of this (bigger) loop to maintain good oversight
- with that, you are going to have a rather simple tail recursion at _(recur …)_, which in return is also simplifying the _(loop […] …)_ part: simple at the bottom, simple at the top!
- and: you probably can use some of these intermediate “inside-the-loop-variables” at the _Accept_ and _Return_ parts of the CPS (**Continuation-Passing Style**), that is the last if (Accept)-then (Return)-else (Continuation) construct, with _Continuation_ being the tail recursion

=> with these simple (albeit partly imperative) ideas, you can handle (desired) “inside-the-loop-variables” well in my short experience:

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/Clojure_control_flow_a.png)

from slide #19/50 at (*) above, my annotations in blue color

<br/>

### The usual way of error handling in Clojure is imperative

The official Clojure guide at https://www.clojure.org/guides/learn/flow#_exception_handling shows the imperative _try/catch/finally_ construct ("as in Java") for exception handling.

Website _Clojure Patterns_ also says:

> In Clojure, exception handling is managed using try-catch blocks.

..at: https://clojurepatterns.com/15/2/3/#clojure-implementation

<br/>

But of course, a functional programming language must support the functional approach to error handling too (or **Monadic error handling** or **Declarative error handling**, which are using **Algebraic Data Types** (ADT’s), that is composite data types formed by combining other types), mustn't it?

_Clojure Patterns_ helps out at: _Functional Error Handling with Monads in Clojure_: https://softwarepatternslexicon.com/patterns-clojure/3/6/

It refers to the _Cats Library_ (like in Scala with "Cats" referring to “Category Theory”): https://github.com/funcool/cats

Other solutions for error handling refer to a macro library for Clojure: https://github.com/clojure/algo.monads

Here’s a solution with routines that validate forms: https://adambard.com/blog/acceptable-error-handling-in-clojure/

<br/>

In other words, the typical error handling in Clojure is to be done imperatively and not functionally. There’s probably a reason for this with Clojure as a dynamically typed language sitting on top of Java, since:

> In functional programming, error handling can be challenging because functions are typically pure, meaning they do not have side effects like throwing exceptions.

from: https://dev.to/almaclaine/either-algebraic-data-type-3onk

<br/>

### Clojure is a demanding functional programming language

During my research on Clojure I tumbled into this comment on Clojure in Hacker News (HN) on Oct 1, 2021: https://news.ycombinator.com/item?id=28723447 with this nice slogan: _I write Clojure for food, and Common Lisp for fun._ It shows that obviously I'm not the only (lay)man who struggled with execution speed problems with Clojure.

Here are two replies to this comment with stepwise improvements in execution speed of the original Clojure program:

- 2021: _Fast and Elegant Clojure_: https://bsless.github.io/fast-and-elegant-clojure/
- 2021: _From Elegance to Speed, with Clojure_: https://noahbogart.com/posts/2021-10-02-from-elegance-to-speed-with-clojure/

##_end
