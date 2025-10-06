# C

---

### Program building tips

With program _random_streams_for_perf_stats.c_ I compiled directly like this:

```
$ gcc -Wall -Ofast -faggressive-loop-optimizations random_streams_for_perf_stats.c -o random_streams_for_perf_stats_c
```

With program _random_bitstring_and_flexible_password_generator.c_ I used a [make file](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/makefile):

```
$ make
```

<br/>

Compiling with [clang](https://clang.llvm.org/get_started.html) instead of the "usual" [gcc](https://gcc.gnu.org/), at least with the compiler versions I used, did not build a faster executable, but program size is significantly smaller with clang (with aggressive compiler switches for execution speed in both cases):

- clang: 8,208 bytes (_$ clang random_streams_for_perf_stats.c -O3 -o random_streams_for_perf_stats_clang_)
- gcc: 17,912 bytes

<br/>

##_end
