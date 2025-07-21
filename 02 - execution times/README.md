![plot](./mean_stddev_err_whiskers%20--%20no%20GraalVM.png)

<br/>

![plot](./mean_stddev_err_whiskers%20--%20only%20GraalVM.png)

<br/>

### Bash shell scripts to measure slower execution times

"Slower" means 100 milliseconds or more for example.

Difference between scripts:

- _exe_times_statistics_for_one_test_case_in_cwd2_ and
- _exe_times_statistics_for_one_test_case_in_cwd2a_

..is the number of arguments. With two arguments (that is _scala_ and _random_streams_for_perf_stats.scala_) use for example:

```
$ ./exe_times_statistics_for_one_test_case_in_cwd2 scala random_streams_for_perf_stats.scala
```

With three arguments use for example:

```
$ ./exe_times_statistics_for_one_test_case_in_cwd2a java -jar random_streams_for_perf_stats.jar
```

Make sure that the access permissions of these scripts are sufficient. I do this for example:

```
$ chmod 755 ./exe_times_statistics_for_one_test_case_in_cwd2
```

##_end
