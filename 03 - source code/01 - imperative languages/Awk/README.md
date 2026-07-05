2026-07-01: work in progress: tbd

<br/>

# Awk

Awk is **GNU Awk** ("gawk") in my testing system: https://www.gnu.org/software/gawk/manual/gawk.html

<br/>

Originally, and like so often before, I didn't have the idea to implement my microbenchmark program also in Awk. 

But then I noticed this:

- compared to GNU Awk, here using (system-default) version GNU Awk 5.2.1 from 2022, [mawk, an interpreter for the AWK Programming Language](https://invisible-island.net/mawk/), here using version _mawk 1.3.4 20240123_ for inefficiently (recursively) calculating the Fibonacci number of 47 is a speedy thing (on 2026-07-06):

- _$ time mawk -f fib_recursive_argument.awk 47_: _Time: 199s_
- _$ time awk -f fib_recursive_argument.awk 47_: _Time: 659s_, that is 3.3 times slower

However, when doing a benchmark with the ["speed part" of the microbenchmark program](./random_streams_for_perf_stats.awk), the picture flips substantially:

- _$ time mawk -f random_streams_for_perf_stats.awk_ => real	0m1.239s, that's 5.7 times slower!
- _$ time awk -f random_streams_for_perf_stats.awk_ => real	0m0.216s

<br/>

Then I just decided to cheaply transpile also the [Tcl implementation of the full microbenchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Tcl/random_bitstring_and_flexible_password_generator.tcl) into an Awk version.

Tcl looks like another good source language for transpilation, next to [Groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy#groovy). 

<br/>

##_end
