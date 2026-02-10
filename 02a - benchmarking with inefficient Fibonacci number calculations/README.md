2026-02-10: work in progress

# Benchmarking with inefficient (recursive) Fibonacci number calculations

When I did [JIT experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05c%20-%20PHP%20on%20the%20Zend%20Engine%20virtual%20machine#jit-experiments)
at PHP, I got the idea that having a more focused presentation of different execution formats, specifically of **virtual machines** (though not exclusively),
with an inefficient, that is recursive, Fibonacci number calculation could be a good idea.

It has shown at PHP ([JIT experiments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05c%20-%20PHP%20on%20the%20Zend%20Engine%20virtual%20machine#jit-experiments))
and Ruby ([JIT experiments with Ruby version 4](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#jit-experiments-with-ruby-version-4)),
but not Dart ([Dart execution speeds diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05b%20-%20Dart%20on%20the%20Dart%20virtual%20machine#dart-execution-speeds-diagram)),
that with string concatenation, just-in-time compilation (JIT) of bytecode of a virtual machine isn't necessarily improving execution speed.

But with (non-trivial) **arithmetic calculations**, just-in-compilation or even ahead-of-time compilation (AOT) almost always improves execution speed.

And a good testing algorithm is the inefficient (recursive) Fibonacci number calculation, as it has been used in 2024 to benchmark different Scheme dialects: [FIB: a classic benchmark, computes fib(n) inefficiently](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#fib-a-classic-benchmark-computes-fibn-inefficiently)

<br/>

By the way: calculating Fibonacci numbers recursively is not a good idea algorithmically. There are much more efficient algorithms for this job: [Fast Fibonacci algorithms](https://www.nayuki.io/page/fast-fibonacci-algorithms)

But this page is not about benchmarking algorithms, but for comparing different execution formats of different programming languages.

<br/>

Same like with the [map coloring problem of Germany](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#the-tldr-execution-speed-diagram) implemented in different Prolog systems, I also provide a [C++ solution](./fib_recursive_small_argument.cpp) just for some orientation on the fast side (and also on the slow side with the
[GMP based solution](./fib_recursive_small_argument_GMP.cpp)!):

> [!CAUTION]
> However and as so often, one has to be careful with correctly calculating Fibonacci numbers in C++ to not exceed the maximum limits of integer numbers!

At first, I naively did a [GMP based implementation](./fib_recursive_small_argument_GMP.cpp), only to realize that while it correctly calculates "higher" Fibonacci numbers, it is also a super-slow implementation!
(GMP = GNU Multiple Precision Arithmetic Library: https://gmplib.org/)

Then I turned to the [unsigned long long integer type](./fib_recursive_small_argument.cpp), which should allow to calculate Fibonacci numbers up to argument 93 with result 12,200,160,415,121,876,738: https://zeptomath.com/calculators/fibonaccinumbers.php?number=93

This result is still under 18,446,744,073,709,551,615 = 2^64 - 1, which should be the highest _unsigned long long_ number in a typical C++ compilation: [C and C++ Integer Limits](https://learn.microsoft.com/en-us/cpp/c-language/cpp-integer-limits?view=msvc-170) (however, calculating this result recursively, even in C++, may not stop before hours!)

<br/>

## Execution speeds table

programming language | execution format | nth Fibonacci number | one program run, internally measured | comments | date
--- | --- | --- | --- | --- | ---
[C++](./fib_recursive_small_argument.cpp) | compiled with _g++ -O3_ | 47 | 3.14 seconds | _unsigned long long_ integer type | 2026-02-10
[C++](./fib_recursive_small_argument_GMP.cpp) | compiled with _g++ -O3 ... -lgmpxx -lgmp_ | 47 | 115.44 seconds | _mpz_class_ for large integers | 2026-02-10
[Dart](./fib_recursive_argument.dart) | _$ dart run ./fib_recursive_argument.dart_ <n> | 47 | 16.15 seconds | JIT compiled (default) | 2026-02-10
[Dart](./fib_recursive_argument.dart) | _$ dartaotruntime ./fib_recursive_argument.aot_ <n> | 47 | 12.89 seconds | AOT compiled | 2026-02-10
[PHP](./fib_recursive_argument.php) | _$ php ./fib_recursive_argument.php_ <n> | 47 | 99.82 seconds | interpreted Zend VM bytecode | 2026-02-10
[PHP](./fib_recursive_argument.php) | _$ php -d opcache.enable_cli=1 -d opcache.jit_buffer_size=100M -d opcache.jit=1255 ./fib_recursive_argument.php_ <n> | 47 | 27.74 seconds | JIT compiled | 2026-02-10
[Ruby](tbd) |  |  |  | interpreted ??? VM bytecode | tbd
[Ruby](tbd) |  |  |  | YJIT compiled | tbd

VM = virtual machine

<br/>

## Execution speeds diagram

Here, above table in a diagram:

TBD

<br/>

##_end
