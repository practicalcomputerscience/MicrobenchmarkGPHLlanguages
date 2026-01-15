2026-01-15: work in progress

# C++

https://isocpp.org/

---

The main reason why I implemented the microbenchmark program in C++ are these two pages:

- [Language dependencies](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/80%20-%20language%20dependencies#language-dependencies)
- [Potential "C successors": which one to take?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/85%20-%20C%20successors#potential-c-successors-which-one-to-take)

..and their long term implications from my point of view, that is that the world of computer programming may replace (classical) C (for "system programming") increasingly with something else,
but that (evolving) C++ (for "application programming") will be around for still many years to come.

At least at the moment, including the fact that also C++ (as such) is not doing pointers and array indices bounds checking, C++ is indispensable in the computer programming world.

<br/>

Same like with C, I tried both compilers for compilation of the [speed part](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B/random_streams_for_perf_stats.cpp) of the microbenchmark program, with these results:

compiler version | compiler switches for release | mean exe time in milliseconds | +/-standard deviation
--- | --- | --- | ---
g++ 13.3.0 | -O3 -std=c++20 | 5.9 | +-1.30%
Homebrew clang 21.1.7 | -O3 -std=c++20 -stdlib=libstdc++ | 7.3 | +-1.10%

Mean exe time measured with the usual best out of 3 runs of: _$ sudo perf stat -r 20 ./random_streams_for_perf_stats_g++_ or _$ sudo perf stat -r 20 ./random_streams_for_perf_stats_clang_, respectively.

This result is the opposite of my [C exe time measurements](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C#keeping-using-idiomatic-constructs),
where the clang version is just a little bit faster than the gcc version: 7.8 milliseconds versus 8.2 milliseconds

Generally, both C++ versions are faster than both C versions, though the lead of the g++ version is astonishingly high.

My suspicion here is that with C++ the integer conversion to binary and hexadecimal strings is implemented more efficiently in C++ than in C, at least with my choices made:

language | function call
--- | ---
C | _sprintf(bits_x_str, "%016b", x[i]);_
C++ | _std::format_to(bits_x_str, "{:016b}", x[i]);_

However, I haven't done experiments, with the usual replacements with user defined functions, to support my suspicion.

<br/>

##_end
