# Koka

Koka is "a strongly typed functional-style language with effect types and handlers":

- http://koka-lang.org/
- https://github.com/koka-lang/koka
  
..and has been in development since 2012 by Daan Leijen, who is still at Microsoft Research (as of July 2025): https://www.microsoft.com/en-us/research/people/daan/

My [Koka program](TBD) didn't make into the benchmark list because it's too slow with 3.2 seconds execution time, even after compilation with the -O2 optimization switch: [Languages that were too slow](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#languages-that-were-too-slow)

Though, it's (still) doing simple string concatenations in a recursive loop, which means that I didn't try to improve this program in terms of execution speed (with the usual suspects array or vector).

Probably it was because of the disheartening fact that Microsoft in over 10 years of development of this language hasn't really pushed this effort obviously.

<br/>

##_end
