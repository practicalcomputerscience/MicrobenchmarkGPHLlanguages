![plot](./mean_stddev_err_whiskers%20--%20no%20GraalVM.png)

<br/>

![plot](./mean_stddev_err_whiskers%20--%20only%20GraalVM.png)

<br/>

2025-09-08: this is a bad idea:

> #### Bash shell scripts to measure slower execution times

=> all programs, be it standalone executables or scripts, are only to be execution time measured with one method to compare apples with apples!

Here, this is Linux program _**perf-stat**_:

- https://linux.die.net/man/1/perf-stat
- https://commandmasters.com/commands/perf-linux/

<br/>

For example, the execution time of uberJAR file _password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar_, being executed on the Java Virtual Machine, is measured with this command, which is calculating the mean and other summary statistics: 

```
$ sudo perf stat -r 20 java -jar ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
...

Performance counter stats for 'java -jar ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar' (20 runs):

            669,40 msec task-clock                       #    2,551 CPUs utilized               ( +-  7,20% )
             1.965      context-switches                 #    2,935 K/sec                       ( +-  2,41% )
                96      cpu-migrations                   #  143,411 /sec                        ( +-  6,02% )
            23.909      page-faults                      #   35,717 K/sec                       ( +-  0,71% )
     2.094.798.865      cycles                           #    3,129 GHz                         ( +-  1,87% )
     2.356.202.295      instructions                     #    1,12  insn per cycle              ( +-  0,43% )
       467.089.273      branches                         #  697,770 M/sec                       ( +-  0,42% )
        14.852.740      branch-misses                    #    3,18% of all branches             ( +-  0,35% )
                        TopdownL1                 #     20,1 %  tma_backend_bound      
                                                  #     31,8 %  tma_bad_speculation    
                                                  #     29,4 %  tma_frontend_bound     
                                                  #     18,8 %  tma_retiring             ( +-  1,77% )

            0,2624 +- 0,0184 seconds time elapsed  ( +-  7,02% )

$
```

##_end
