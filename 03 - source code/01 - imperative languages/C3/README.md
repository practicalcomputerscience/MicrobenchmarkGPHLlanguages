# C3

https://c3-lang.org/

https://github.com/c3lang/c3c

<br/>

**Will C3 finally become a viable "C for the 21st century" with it's soft migration approach from C?**

C3 looks to me like "Made in Europe", which is always a risk in the programming language world.

C3 would fill a real need for the modernization of C from my point of view. I may start trying it when there's another need to access fast C code from a Python program for
example.

---

## Compilation tips

I played with compiler switches for optimizations of the compiler. These switches can be shown with the _$ c3c --help_ command:

```
-O0  - Safe, no optimizations, emit debug info.
-O1  - Safe, high optimization, emit debug info.
-O2  - Unsafe, high optimization, emit debug info.
-O3  - Unsafe, high optimization, single module, emit debug info.
-O4  - Unsafe, highest optimization, relaxed maths, single module, emit debug info, no panic messages.
-O5  - Unsafe, highest optimization, fast maths, single module, emit debug info, no panic messages, no backtrace.
-Os  - Unsafe, high optimization, small code, single module, no debug info, no panic messages.
-Oz  - Unsafe, high optimization, tiny code, single module, no debug info, no panic messages, no backtrace.
```

I decided for switch _-O3_ for compiling a program that combines more stable execution times with still lower execution times ("wall clock") than other optimization switches according to my experiments.

What does "Safe" or "Unsafe" mean in this context?

I guess this from: https://c3-lang.org/faq/allfeatures/#safe--fast

> Compilation has two modes: “safe” and “fast”. Safe will insert checks for out-of-bounds access, null-pointer deref, shifting by negative numbers, division by zero, violation of contracts and asserts.
> 
> Fast will assume all of those checks can be assumed to always pass. This means that unexpected behaviour may result from violating those checks. It is recommended to develop in “safe” mode.
> 
> If debug symbols are available, C3 will produce a stack trace in safe mode where an error occurs.

I assume that my microbenchmark programs, also in its C3 implementation, have sufficient checks in their source codes to make them feasible for "unsafe" compilation.

<br/>

## On how to do demanding string building in C3

This new solution:

```
    bits_x[byte_nbr..byte_nbr+15] = bits_x_str[0..15];  // 2026-01-25
```

..is not only more idiomatic from my point of view, but also a little bit faster with around 15.4 milliseconds than the old solution:

```
    bits_x[byte_nbr]     = bits_x_str[0];
    bits_x[byte_nbr+1]   = bits_x_str[1];
    bits_x[byte_nbr+2]   = bits_x_str[2];
    bits_x[byte_nbr+3]   = bits_x_str[3];
    bits_x[byte_nbr+4]   = bits_x_str[4];
    bits_x[byte_nbr+5]   = bits_x_str[5];
    bits_x[byte_nbr+6]   = bits_x_str[6];
    bits_x[byte_nbr+7]   = bits_x_str[7];
    bits_x[byte_nbr+8]   = bits_x_str[8];
    bits_x[byte_nbr+9]   = bits_x_str[9];
    bits_x[byte_nbr+10]  = bits_x_str[10];
    bits_x[byte_nbr+11]  = bits_x_str[11];
    bits_x[byte_nbr+12]  = bits_x_str[12];
    bits_x[byte_nbr+13]  = bits_x_str[13];
    bits_x[byte_nbr+14]  = bits_x_str[14];
    bits_x[byte_nbr+15]  = bits_x_str[15];
```

..with around 15.8 milliseconds of execution time (though, this may be well within the statistical errors of the true means).

<br/>

##_end
