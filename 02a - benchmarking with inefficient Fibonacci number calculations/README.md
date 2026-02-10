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

And a good testing algorithm is the inefficient (recursive) Fibonacci number calculation, as it has been also used to benchmark different Scheme dialects in 2024: [FIB: a classic benchmark, computes fib(n) inefficiently](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#fib-a-classic-benchmark-computes-fibn-inefficiently)

<br/>

By the way: calculating Fibonacci numbers recursively is not a good idea algorithmically. There are much more efficient algorithms for this job: [Fast Fibonacci algorithms](https://www.nayuki.io/page/fast-fibonacci-algorithms)

But this page is not about benchmarking algorithms, but for comparing different execution formats of different programming languages.

<br/>

Same like with the [map coloring problem of Germany](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming/Prolog#the-tldr-execution-speed-diagram) implemented in different Prolog systems, I also provide a [C++ solution](./fib_recursive_small_argument.cpp) just for some orientation on the fast side:

> [!CAUTION]
> However and as so often, one has to be careful with correctly calculating Fibonacci numbers in C++ to not exceed the maximum limits of integer numbers!

At first, I naively did a [GMP based implementation](./fib_recursive_small_argument_GMP.cpp), only to realize that while it correctly calculates "higher" Fibonacci numbers, it is also a super-slow solution!
(GMP = GNU Multiple Precision Arithmetic Library: https://gmplib.org/)

Then I turned to the [unsigned long long integer type](./fib_recursive_small_argument.cpp), which should allow to calculate Fibonacci numbers up to argument 93 with result 12,200,160,415,121,876,738: https://zeptomath.com/calculators/fibonaccinumbers.php?number=93

This result is still under 18,446,744,073,709,551,615 = 2^64 - 1, which should be the highest _unsigned long long_ number in a typical C++ compilation: [C and C++ Integer Limits](https://learn.microsoft.com/en-us/cpp/c-language/cpp-integer-limits?view=msvc-170) (however, calculating this result recursively, even in C++, may not stop in hours!)

<br/>

## Execution speeds table

programming language | execution format | nth Fibonacci number | one program run, internally measured | comments | date
--- | --- | --- | --- | --- | ---
[Bigloo Scheme](tbd) |  | 47 |  |  | tbd
[C++](./fib_recursive_small_argument.cpp) | compiled with _g++ -O3_ | 47 | 3.14 seconds | _unsigned long long_ integer type | 2026-02-10
[C++](./fib_recursive_small_argument_GMP.cpp) | compiled with _g++ -O3 ... -lgmpxx -lgmp_ | 47 | 115.44 seconds | _mpz_class_ for large integers | 2026-02-10
[Dart](./fib_recursive_argument.dart) | _$ dart run ./fib_recursive_argument.dart_ <n> | 47 | 16.15 seconds | JIT compiled (default) | 2026-02-10
[Dart](./fib_recursive_argument.dart) | _$ dartaotruntime ./fib_recursive_argument.aot_ <n> | 47 | 12.89 seconds | AOT compiled | 2026-02-10
[PHP](./fib_recursive_argument.php) | _$ php ./fib_recursive_argument.php_ <n> | 47 | 99.82 seconds | interpreted Zend VM bytecode | 2026-02-10
[PHP](./fib_recursive_argument.php) | _$ php -d opcache.enable_cli=1 -d opcache.jit_buffer_size=100M -d opcache.jit=1255 ./fib_recursive_argument.php_ <n> | 47 | 27.74 seconds | JIT compiled | 2026-02-10
[Ruby](./fib_recursive_argument.rb) | _$ ruby ./fib_recursive_argument.rb_ <n> | 47 | 150.44 seconds | interpreted YARV VM bytecode | 2026-02-10
[Ruby](./fib_recursive_argument.rb) | _$ ruby --mjit ./fib_recursive_argument.rb_ <n> | 47 | 50.13 seconds | MJIT compiled | 2026-02-10
[Ruby](./fib_recursive_argument.rb) | _$ ruby --yjit ./fib_recursive_argument.rb_ <n> | 47 | 39.76 seconds | YJIT compiled | 2026-02-10

YARV = Yet Another Ruby VM

VM = virtual machine

<br/>

### Ruby

Ruby again proved to be a more complicated affair. But since I was eager to get its [YJIT compilation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#jit-experiments-with-ruby-version-4) running also on my testing system, I installed latest release of version 3, that is version 3.2.10 with the Ruby Version Manager (rvm) following these instructions: https://linuxgenie.net/install-ruby-ubuntu-24-04/

```
$ gpg --keyserver keyserver.ubuntu.com --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
$ curl -sSL https://get.rvm.io | bash -s stable
$ source $HOME/.rvm/scripts/rvm
$ rvm install ruby-3.2.10
$ ruby --version
ruby 3.2.10 (2026-01-14 revision a3a6d25788) [x86_64-linux]
$
```

The results were not disappointing, see from the table above:

- not only was using version 3.2.10 generally a bit faster than using version 3.2.3, using JIT compiling was also faster in both cases, that is using switch _--mjit_ and switch _--yjit_ (which is now the same as switch _--jit_), with YJIT compilation again being the winner like seen here at [JIT experiments with Ruby version 4](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#jit-experiments-with-ruby-version-4) (*)

The Ruby 3.2.10 help says this (_$ ruby --help_) on the JIT compiler switches:

- _mjit -- C compiler-based JIT compiler (default: disabled)_
- _yjit -- in-process JIT compiler (default: disabled)_

By the way: switch _--zjit_ needs a Ruby version >= 4.0 (*)

<br/>

## Execution speeds diagram

Here, above table in a diagram:

TBD: extra R script: _mean_stddev_err_whiskers_recursive_fib.R_

<br/>

##_end
