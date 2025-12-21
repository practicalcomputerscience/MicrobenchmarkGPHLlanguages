2025-12-10: see [On configuring building and execution environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments): the new SSD drive is faster than the old one; so, one day I have to redo all execution time measurements as shown below

<br/>

# Program execution times

Table of contents:

- [Master diagram with most program environments](#master-diagram-with-most-program-environments)
- [Java native languages Scala, Kotlin and Clojure and their speedup with the GraalVM](#java-native-languages-scala-kotlin-and-clojure-and-their-speedup-with-the-graalvm)
- [Tested Scheme dialects](#tested-scheme-dialects)
- [Programming languages for virtual machines](#programming-languages-for-virtual-machines)
- [The Clojure example](the-clojure-example)

<br/>

My version of _perf-stat_ (https://linux.die.net/man/1/perf-stat) in use is:

```
$ perf --version
perf version 6.14.11
$
```

<br/>

### Master diagram with most program environments

<br/>

![plot](./mean_stddev_err_whiskers%20--%20no%20GraalVM.png)

So far:

- [OCaml](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/OCaml#ocaml) is the fastest [functional programming language](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages#functional-languages),
- [Perl 5](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Perl%205#perl-5) the fastest interpreted language, and
- [C#](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%23#c) the fastest language on a virtual machine.

<br/>

---

### Java native languages Scala, Kotlin and Clojure and their speedup with the GraalVM

GraalVM for the compilation of an ahead-of-time (AOT) native image, that is an standalone, binary executable for Linux :

![plot](./mean_stddev_err_whiskers%20--%20only%20GraalVM.png)

https://www.graalvm.org/

See more details at page [Graal Virtual Machine (GraalVM)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/04%20-%20GraalVM#graal-virtual-machine-graalvm)

<br/>

---

### Tested Scheme dialects

![plot](./mean_stddev_err_whiskers%20--%20only%20Scheme.png)

See more details at page [Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme)

<br/>

---

### Programming languages for virtual machines

![plot](./mean_stddev_err_whiskers%20--%20only%20VM.png)

<br/>

---

### The Clojure example

The execution time of uberJAR file _random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar_, being executed on the Java Virtual Machine, can be measured with this script command: 

```
$ ./exe_times_statistics_for_one_test_case_in_cwd2a java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
...
mean = 787 [milliseconds]
$
```

With the _perf-stat_ program, mean and other summary statistics may look like this:

```
$ sudo perf stat -r 20 java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
...
 Performance counter stats for 'java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar' (20 runs):

          3.070,27 msec task-clock                       #    3,939 CPUs utilized               ( +-  3,23% )
             6.112      context-switches                 #    1,991 K/sec                       ( +-  0,68% )
               242      cpu-migrations                   #   78,820 /sec                        ( +-  3,64% )
            76.138      page-faults                      #   24,798 K/sec                       ( +-  0,88% )
    11.931.501.930      cycles                           #    3,886 GHz                         ( +-  1,02% )
    15.158.326.578      instructions                     #    1,27  insn per cycle              ( +-  0,69% )
     2.954.916.075      branches                         #  962,428 M/sec                       ( +-  0,71% )
        72.941.379      branch-misses                    #    2,47% of all branches             ( +-  0,50% )
                        TopdownL1                 #     24,2 %  tma_backend_bound      
                                                  #     34,0 %  tma_bad_speculation    
                                                  #     21,0 %  tma_frontend_bound     
                                                  #     20,8 %  tma_retiring             ( +-  0,87% )

            0,7794 +- 0,0306 seconds time elapsed  ( +-  3,92% )

$
```

Both results are close and within a standard deviation of +-3,92%.

(results are from September 2025; in October 2025, same like during development time, this JAR file runs faster, like many programs but not all, on the same machine! This is somehow a never ending story :confused:)

<br/>

##_end
