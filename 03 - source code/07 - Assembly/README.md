2026-11-19: work in progress

<br/>

# Assembly

Though this project is meant only for general purpose, high-level programming languages, for fun I just added a version in assembly language.

Assembly language which supports a broad range of modern Intel and AMD (desktop) cpu's, that means using only baseline x86-64 instructions, that is x86-64-v1 instructions,
for 64-bit Linux systems.

As one "natural" possibility for the source language, I took the
[C++](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B/random_streams_for_perf_stats.cpp)
version and asked Duck.ai to transpile it into assembly code for [NASM (Netwide Assembler)](https://www.nasm.us/). That assembly code was working out of the box. I only did some fine tuning
(same output behavior etc):

```
$ sudo apt install nasm  # install NASM if not done yet
...
$ nasm -f elf64 random_streams_for_perf_stats_x86-64-v1.asm -o random_streams_for_perf_stats_x86-64-v1.o
$ gcc -no-pie random_streams_for_perf_stats_x86-64-v1.o -o random_streams_for_perf_stats_x86-64-v1
$ ./random_streams_for_perf_stats_x86-64-v1

generating a random bit stream...
could not write to file: random_bitstring.bin: Permission denied  # here I set the permission status of the target output file to read only as an exception test 
Byte stream has been written to disk under name: random_bitstring.byte
$
```



















<br/>






<br/>

##_end
