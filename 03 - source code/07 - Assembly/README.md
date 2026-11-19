2026-11-19: work in progress

<br/>

# Assembly

Although this project is meant only for general purpose, **high-level** programming languages, for fun I just added versions in assembly language.

<br/>

### Best portability with instructions of the x86-64-v1 feature level

At first, only assembly language which supports a broad range of (modern) Intel and AMD (desktop) cpu's is to be used, which means only using instructions of the **x86-64-v1** feature level ([x86-64 microarchitecture levels](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#x86-64-microarchitecture-levels) (*)) for 64-bit Linux systems.

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

Outcomes can vary from 2.0 to 3.7 milliseconds for one, average program run on my testing system!

Though, this (rough) range means that such an executable is not necessarily faster in average (in the same execution environment) than an executable made from an equivalent [Zig](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig/random_streams_for_perf_stats.zig) program, here with an average program execution time of 3.3 milliseconds (as of 2025-12-18; also best measurement batch out of three).

<br/>

### Good portability also with instructions of the x86-64-v2 feature level

So, I got the idea to ask Google AI (Duck.ai wasn't very helpful here), to transpile [random_streams_for_perf_stats_x86-64-v1.asm](./random_streams_for_perf_stats_x86-64-v1.asm)
into assembly code which may also use instructions of the x86-64-v2 feature level:

```
$ sudo sh -c 'echo 0 > /proc/sys/kernel/nmi_watchdog'  # disable the NMI watchdog to count all events
$ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v2

generating a random bit stream...
...
 Performance counter stats for './random_streams_for_perf_stats_x86-64-v2' (20 runs):

                 1      context-switches                 #   1018.8 cs/sec  cs_per_second       ( +- 19.83% )
                 0      cpu-migrations                   #      0.0 migrations/sec  migrations_per_second
               365      page-faults                      # 371876.4 faults/sec  page_faults_per_second  ( +-  0.04% )
              0.98 msec task-clock                       #      0.4 CPUs  CPUs_utilized         ( +-  1.68% )
             8,062      branch-misses                    #      1.2 %  branch_miss_rate         ( +-  4.77% )
           668,712      branches                         #    681.3 M/sec  branch_frequency     ( +-  0.26% )
         4,424,976      cpu-cycles                       #      4.5 GHz  cycles_frequency       ( +-  1.69% )
         5,063,416      instructions                     #      1.1 instructions  insn_per_cycle  ( +-  0.17% )
                        TopdownL1                        #     43.9 %  tma_backend_bound      
                                                         #      5.0 %  tma_bad_speculation      ( +-  3.22% )
                                                         #     25.1 %  tma_frontend_bound       ( +-  6.62% )
                                                         #     26.1 %  tma_retiring             ( +-  0.16% )

       0.001608377 +- 0.000056225 seconds time elapsed  ( +-  3.50% )

$ sudo sh -c 'echo 1 > /proc/sys/kernel/nmi_watchdog'  # enable the NMI watchdog again. Do this only after your last tests!!
$
```

Also with this executable, outcomes can vary widely, from 1.6 to 2.2 milliseconds for one, average program run (on my testing system).

Nevertheless, this represents, at least in average, a substantial further drop of program execution times.

Even though Google AI didn't say it, I checked (also) executable _random_streams_for_perf_stats_x86-64-v2_ for potentially using **AVX-512** instructions (like the Chapel compiler does with the _--fast_ switch (*)):

```
$ objdump -d -C ./random_streams_for_perf_stats_x86-64-v1 | awk '/^[0-9a-f]+ <.*>:/{fn=$0} /(^|[^a-zA-Z0-9_])(zmm[0-9]+|k[0-7])([^a-zA-Z0-9_]|$)/{print fn}' | uniq
$ objdump -d -C ./random_streams_for_perf_stats_x86-64-v2 | awk '/^[0-9a-f]+ <.*>:/{fn=$0} /(^|[^a-zA-Z0-9_])(zmm[0-9]+|k[0-7])([^a-zA-Z0-9_]|$)/{print fn}' | uniq
$
```

No AVX-512 instructions in sight here, which means that also executable _random_streams_for_perf_stats_x86-64-v2_ has good portability from one modern Linux system to another modern Linux system.

<br/>

### Good portability also with instructions of the x86-64-v3 feature level

Next logical step now is to transpile [random_streams_for_perf_stats_x86-64-v2.asm](./random_streams_for_perf_stats_x86-64-v2.asm)
into assembly code which may also use instructions of the x86-64-v3 feature level:

```
$ sudo perf stat -r 20 ./random_streams_for_perf_stats_x86-64-v3
...
 Performance counter stats for './random_streams_for_perf_stats_x86-64-v3' (20 runs):

                 1      context-switches                 #   1026.3 cs/sec  cs_per_second       ( +- 19.83% )
                 0      cpu-migrations                   #      0.0 migrations/sec  migrations_per_second
               365      page-faults                      # 374612.9 faults/sec  page_faults_per_second  ( +-  0.04% )
              0.97 msec task-clock                       #      0.4 CPUs  CPUs_utilized         ( +-  2.11% )
             7,929      branch-misses                    #      1.2 %  branch_miss_rate         ( +-  4.82% )
           650,982      branches                         #    668.1 M/sec  branch_frequency     ( +-  0.25% )
         4,406,939      cpu-cycles                       #      4.5 GHz  cycles_frequency       ( +-  2.18% )
         4,491,702      instructions                     #      1.0 instructions  insn_per_cycle  ( +-  0.17% )
                        TopdownL1                        #     45.9 %  tma_backend_bound      
                                                         #      4.9 %  tma_bad_speculation      ( +-  3.15% )
                                                         #     25.4 %  tma_frontend_bound       ( +-  6.71% )
                                                         #     23.9 %  tma_retiring             ( +-  0.16% )

       0.001557186 +- 0.000030469 seconds time elapsed  ( +-  1.96% )

$
```

Also with this executable, outcomes can vary widely, from 1.6 to 2.4 milliseconds for one, average program run (on my testing system).

Nevertheless, this outcome does not represent, at least in average, any further drop of program execution times, or as Google AI stated:

> Note: At this point, the CPU computation loop takes only a few microseconds—around ~15,000–25,000 cycles total—meaning the vast majority of your program's wall-clock runtime is now spent inside the Linux kernel / glibc fopen, fwrite, fclose, and puts system calls rather than your assembly loop!

Again, I also checked this executable for potentially using **AVX-512** instructions (which is also not the case here):

```
$ objdump -d -C ./random_streams_for_perf_stats_x86-64-v3 | awk '/^[0-9a-f]+ <.*>:/{fn=$0} /(^|[^a-zA-Z0-9_])(zmm[0-9]+|k[0-7])([^a-zA-Z0-9_]|$)/{print fn}' | uniq
$
```

<br/>

tbd

<br/>

##_end
