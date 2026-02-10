2026-02-10: work in progress

# Benchmarking with inefficient (recursive) Fibonacci number calculations

When I did [JIT experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05c%20-%20PHP%20on%20the%20Zend%20Engine%20virtual%20machine#jit-experiments)
at PHP, I got the idea that having a more focused presentation of different execution formats, specifically of **virtual machines** (though not exclusively),
with an inefficient, that is recursive, Fibonacci number calculation could be a good idea.

It has shown at PHP ([JIT experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05c%20-%20PHP%20on%20the%20Zend%20Engine%20virtual%20machine#jit-experiments))
and Ruby ([JIT experiments with Ruby version 4](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#jit-experiments-with-ruby-version-4)),
but not Dart ([Dart execution speeds diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05b%20-%20Dart%20on%20the%20Dart%20virtual%20machine#dart-execution-speeds-diagram)),
that with string concatenation, just-in-time compilation (JIT) of bytecode of a virtual machine isn't necessarily improving execution speed.

But with (non-trivial) **mathematical calculations**, **just-in-compilation** or even **ahead-of-time** compilation (AOT) almost always improves execution speed.

And a good testing algorithm is the inefficient (recursive) Fibonacci number calculation, as it has been used in 2024 to benchmark different Scheme dialects: [FIB: a classic benchmark, computes fib(n) inefficiently](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#fib-a-classic-benchmark-computes-fibn-inefficiently)

<br/>

First: calculating Fibonacci numbers recursively is not a good idea algorithmically. There are much more efficient algorithms to do this job: [Fast Fibonacci algorithms](https://www.nayuki.io/page/fast-fibonacci-algorithms)

But this page is not about benchmarking algorithms to calculate Fibonacci numbers, but for comparing different execution formats of different programming languages.

<br/>











<br/>

##_end
