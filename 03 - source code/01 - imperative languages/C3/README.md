https://c3-lang.org/

---

#### Compilation tips

I played with compiler switches for optimizations of the compiler of the C replacement language C3. These switches can be shown with the _$ c3c --help_ command:

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

I decided for switch _-O3_ for generating a program that combines more stable execution times with still lower execution times ("wall clock") than other optimization switches.

What does "Safe" or "Unsafe" mean in this context?

I guess this from: https://c3-lang.org/faq/allfeatures/#safe--fast

> Compilation has two modes: “safe” and “fast”. Safe will insert checks for out-of-bounds access, null-pointer deref, shifting by negative numbers, division by zero, violation of contracts and asserts.
> 
> Fast will assume all of those checks can be assumed to always pass. This means that unexpected behaviour may result from violating those checks. It is recommended to develop in “safe” mode.
> 
> If debug symbols are available, C3 will produce a stack trace in safe mode where an error occurs.

I assume that my programs, also in their C3 implementation, have sufficient checks to make them feasible for "unsafe" compilation.

<br/>

##_end
