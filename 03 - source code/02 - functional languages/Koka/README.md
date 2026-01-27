2026-01-26: Koka is indeed a functional programming language, and thus should be listed here, not under imperative languages

# Koka

Koka is "a strongly typed functional-style language with effect types and handlers":

- http://koka-lang.org/
- https://github.com/koka-lang/koka
  
..and has been in development since 2012 by Daan Leijen, who is still at Microsoft Research (as of September 2025): https://www.microsoft.com/en-us/research/people/daan/

---

My [Koka program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Koka/random_streams_for_perf_stats.kk) didn't make into the benchmark list because it's too slow with over 3 seconds execution time, even after compiling it with the _-O3_ optimization switch: [Languages that were too slow](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#languages-that-were-too-slow)

This program is doing simple string concatenations with the _++_ operator in a recursive loop (_bits_x := bits_x ++ bits_x_str_), while compiling a _list_ for the generated integer random numbers.

As of 2026-01-27, it looks like that this may be not the worst tactics. Since _vectors_ are also faster than _lists_ in this functional language according to my experiments (*), there may be some (little) improvement in terms of execution speed with vectorizing the generated integer random numbers. However, this may lead to some complicated code (by me) with doing the string building for _bits_x_ and _bits_hex_ **inside** such a vectorizing loop.

(*) this means that first creating lists of strings, which are finally joined together, is a substantially slower tactics than simple string concatenations inside the master loop; also according to my experiments.

Unfortunately, the mutable _**array**_ type is not implemented yet ([github.com/koka-lang/koka/blob/dev/lib/std/data/array.kk#L9](https://github.com/koka-lang/koka/blob/dev/lib/std/data/array.kk#L9)), and vectors and **strings**, once created, are immutable in Koka.

<br/>

##_end
