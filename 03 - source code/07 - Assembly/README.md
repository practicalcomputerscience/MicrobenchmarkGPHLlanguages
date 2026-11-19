2026-11-19: work in progress

<br/>

# Assembly

Although this project is meant only for general purpose, **high-level** programming languages, for fun I just added a version in assembly language.

At first, only assembly language which supports a broad range of modern Intel and AMD (desktop) cpu's, that means using only baseline x86-64 instructions, that is only
using instructions of the **x86-64-v1** feature level ([x86-64 microarchitecture levels](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#x86-64-microarchitecture-levels)) for 64-bit Linux systems.

As one "natural" possibility for a source language, I took the
[C++](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B/random_streams_for_perf_stats.cpp)
version and asked Duck.ai to transpile it into assembly code for [NASM (Netwide Assembler)](https://www.nasm.us/). That assembly code was working out of the box! I only did some fine tuning
(for same terminal output behavior etc.):

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

When doing execution time measurements, I noticed a great variability between all three batches with command:

```
$ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v1

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
...

 Performance counter stats for './random_streams_for_perf_stats_x86-64-v1' (20 runs):

                 1      context-switches                 #    675.9 cs/sec  cs_per_second       ( +- 22.36% )
                 0      cpu-migrations                   #      0.0 migrations/sec  migrations_per_second
               365      page-faults                      # 246715.3 faults/sec  page_faults_per_second  ( +-  0.05% )
              1.48 msec task-clock                       #      0.6 CPUs  CPUs_utilized         ( +-  1.99% )
            15,275      branch-misses                    #      0.9 %  branch_miss_rate         ( +- 11.86% )  (61.25%)
         2,031,383      branches                         #   1373.1 M/sec  branch_frequency     ( +-  1.17% )
         6,685,604      cpu-cycles                       #      4.5 GHz  cycles_frequency       ( +-  1.99% )
        14,158,560      instructions                     #      2.1 instructions  insn_per_cycle  ( +-  0.05% )
                        TopdownL1                        #     27.1 %  tma_backend_bound      
                                                         #      3.4 %  tma_bad_speculation      ( +-  2.93% )
                                                         #     22.1 %  tma_frontend_bound       ( +- 13.93% )  (38.75%)
                                                         #     47.5 %  tma_retiring             ( +-  3.31% )  (38.75%)

       0.001995884 +- 0.000041714 seconds time elapsed  ( +-  2.09% )

$
```

Outcomes can vary from 2.0 to 3.7 milliseconds, at least, on my testing system!



















<br/>






<br/>

##_end
