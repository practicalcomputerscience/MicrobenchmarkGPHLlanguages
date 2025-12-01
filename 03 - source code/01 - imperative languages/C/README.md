# C and Checked C

https://www.open-std.org/jtc1/sc22/wg14/

<br/>

Checked C "for making existing C code more secure":

- https://www.checkedc.org/
- https://github.com/checkedc/checkedc-clang/wiki

---

## Program building tips

With program [random_streams_for_perf_stats.c](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) I compiled directly like this:

```
$ clang random_streams_for_perf_stats.c -O3 -o random_streams_for_perf_stats_clang
```

With program [random_bitstring_and_flexible_password_generator.c](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_bitstring_and_flexible_password_generator.c) I used a [make file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/makefile) for compilation:

```
$ make
```

<br/>

Compiling with [clang](https://clang.llvm.org/get_started.html) instead of the "usual" [gcc](https://gcc.gnu.org/), at least with the compiler versions I used (on 2025-12-01), built a slighlty faster executable of comparable size (with aggressive compiler switches for execution speed in both cases):

- clang: 16,312 bytes produced with command: _$ clang random_streams_for_perf_stats.c -O3 -o random_streams_for_perf_stats_clang_
- gcc: 17,912 bytes produced with command: _$ gcc -Wall -Ofast -faggressive-loop-optimizations random_streams_for_perf_stats.c -o random_streams_for_perf_stats_c_

<br/>

## Checked C

When I tumbled accidentally, like so often, into [Checked C](https://www.checkedc.org/), I gave it a try.

First, I [converted](https://github.com/checkedc/checkedc-clang/tree/main/clang/tools/3c) the existing [C program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) into a Checked C version:

```
$ ~/scripts/Checked_C/CheckedC-Clang-12.0.0git-Linux/bin/3c -addcr -alltypes -output-postfix=checked random_streams_for_perf_stats.c --
~/scripts/C/password_encryption/random_streams_for_perf_stats.c:90:30: warning: invalid conversion specifier 'b' [-Wformat-invalid-specifier]
    sprintf(bits_x_str, "%016b", x[i]);
                         ~~~~^
$ 
```

..only to notice above warning, something which didn't happen when I was compiling this C program with the gcc compiler with switch _-Wall_ for all warnings on.

Long story short, conversion specifier _b_, or _B_, which doesn't make sense here, is indeed not part of the official language specification (https://en.cppreference.com/w/c/io/fprintf), but just works in C and Checked C.

So, I came to the conclusion to also have a user defined function for this job of converting a bounded, positive integer number into its binary representation with 16 '0' and '1' characters in order to get rid off this warning:

```
void integer_to_bin_string(int n, char *binary_str) {
    binary_str[16] = '\0'; // Null-terminate the string
    for (int i = 15; i >= 0; i--) {
        binary_str[i] = (n & 1) ? '1' : '0'; // Set the bit accordingly
        n >>= 1; // Right shift by 1
    }
}
```

With this [solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.checked_c.c), I noticed a significantly faster program. Execution time with compilation command:

```
$ ~/scripts/Checked_C/CheckedC-Clang-12.0.0git-Linux/bin/clang ./random_streams_for_perf_stats.checked_c.c -O3 -o random_streams_for_perf_stats.checked_c
```

..dropped from around 10 milliseconds (with old command _gcc -Wall -Ofast -faggressive-loop-optimizations_) to around 4.7 milliseconds!

Be aware that Checked C brings along its own version of the clang compiler: https://github.com/checkedc/checkedc-clang/releases/tag/CheckedC-Clang-12.0.2

Then I became even more aggressive and also introduced a user defined function for the generation of the hexadecimal representation in a 4-character string with _char *integer_to_hex_string()_, based on the [Ada solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb).

Now the execution time dropped even further to only around 3.2 milliseconds!

<br/>

However, replacing the _sprintf_ function in my C program would not serve the goals of this microbenchmarking well in my opinion. I try to use idiomatic constructs as long as they make sense and thus forego savings in speed or space here and there. The next fastest languages, that is Rust, [Chrystal](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Crystal/random_streams_for_perf_stats_cr.cr), [C3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C3/random_streams_for_perf_stats.c3) and [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Go/random_streams_for_perf_stats.go), all use their own version of string formatting, sometimes with more than one function call, like in [Rust](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Rust/random_streams_for_perf_stats.rs).

I try to use user defined functions only in cases where I cannot find a suitable solution from more or less official libraries, or when I'm not able to tap into 3rd party libraries within a reasonable amount of time, like at my Ada implementation, which by the way more than once served as a good design input to translate less common functionalities into another programming language.

<br/>

However, what I have done now for my official C program, at least for the [speed part](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c), is:

- using a more modern version of the clang compiler with (normal) version 21.1.4 instead of former version 17.0.0, and
- using its execution speed result as my official one for the C program: [Program execution times](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#program-execution-times)

Because I also noticed that with clang version 21.1.4, I'm now able to compile a slightly faster executable than with gcc version 13.3.0. It's now about 98.1 milliseconds versus about 10.0 milliseconds, with the usual best out of 3 runs of _$ perf stat -r 20_.

<br/>

##_end
