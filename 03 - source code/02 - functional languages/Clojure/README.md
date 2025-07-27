2025-07-27: heavy work in progress

---

https://clojure.org/

https://leiningen.org/

The official **Clojure Cheat Sheet**, which is practical for orientation in this feature rich language: https://clojure.org/api/cheatsheet

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

..in terms of execution speed.

What I did notice is that, even with 20 runs with the execution time measurement script ![exe_times_statistics_for_one_test_case_in_cwd2a](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/exe_times_statistics_for_one_test_case_in_cwd2a) a wide variability of program execution times. On my system the avarage time can vary from around 590 milliseconds to up to 870 milliseconds!

Though, I didn't notice any difference when using command:

```
sudo perf stat -r 20 java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
```

Both methods seem to come to the same results statistically.

#### Initial struggles with execution speed

As usual, I refer to the "speed part" of this program with this source code: ![random_streams_for_perf_stats](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_streams_for_perf_stats_core.clj)

(TBD)



<br/>

### On complexity in Clojure

Here I refer to the full program with this source code: ![random_bitstring_and_flexible_password_generator](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Clojure/random_bitstring_and_flexible_password_generator_core.clj) ..and specifically to this user defined function:

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

..because from my point of view Clojure is not the easiest function programming language to learn. Even experienced Clojure coders can struggle with it, see from below at [Clojure is a demanding functional programming language](clojure-is-a-demanding-functional-programming-language). So, some explanations might be helpful.

I don't do certain things at the _input_a_valid_number_ function, like for example:

(TBD)

Instead I do this:

(TBD)

#### Recursive loops with loop-recur

(TBD)


### The usual way for error handling in Clojure is imperative

(TBD)

### Clojure is a demanding functional programming language

During my research on Clojure I tumbled into these two pages which show that I'm not the only (lay)man who struggled with execution speed problems with Clojure:

- 2021: _Fast and Elegant Clojure_: https://bsless.github.io/fast-and-elegant-clojure/
- 2021: _From Elegance to Speed, with Clojure_: https://noahbogart.com/posts/2021-10-02-from-elegance-to-speed-with-clojure/

Both pages refer to this comment on Clojure in Hacker News (HN) on Oct 1, 2021: https://news.ycombinator.com/item?id=28723447 with this nice slogan: _I write Clojure for food, and Common Lisp for fun._ and this related page: http://johnj.com/from-elegance-to-speed.html

(TBD)

##_end
