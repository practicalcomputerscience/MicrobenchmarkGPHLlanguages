https://ziglang.org/

---

#### Compilation tips

On my target system (![On configuring building and execution environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments)) I'm compiling like this:

```
$ zig build-exe random_streams_for_perf_stats.zig -mcpu=native-avx512f
```

..that is with compiler switch _-mcpu=native-avx512f_ activated.

Background is this: I test the compiled programs with the memory tester program Valgrind (https://valgrind.org/) and if the Zig program would have been compiled without this switch, it would immediately crash. 

My Intel Core i7-11700K @ 3.6GHz desktop CPU still features these 512-bit extensions (great!) to some Single Instruction, Multiple Data (SIMD) instructions: https://en.wikipedia.org/wiki/AVX-512

##_end
