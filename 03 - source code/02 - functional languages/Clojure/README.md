https://clojure.org/

https://leiningen.org/

---

#### Installation tips

Rename:
- _random_bitstring_and_flexible_password_generator_core.clj_ into _core.clj_
- _random_bitstring_and_flexible_password_generator_project.clj_ into _project.clj_
- _random_streams_for_perf_stats_core.clj_ into _core.clj_
- _random_streams_for_perf_stats_project.clj_ into _project.clj_

..in their related app root directories (named _random_bitstring_and_flexible_password_generator_ and _random_streams_for_perf_stats_ respectively), each built with the Leiningen build tool like this for example:

```
$ lein new app random_bitstring_and_flexible_password_generator
$ cd random_bitstring_and_flexible_password_generator
$ lein check
$ lein run
$ lein uberjar
$ java -jar ./target/uberjar/random_bitstring_and_flexible_password_generator-0.1.0-SNAPSHOT-standalone.jar
```

<br/>

In both _project.clj_ files I'm not skipping aot (ahead-of-time) compilation:

```
...
  ; :main ^:skip-aot random-bitstring-and-flexible-password-generator.core
  :main random-bitstring-and-flexible-password-generator.core
...
```

..with the aim to speed up application startup. See from here: https://clojure.org/reference/compilation

However, I didn't notice any statistically relevant difference to not skipping it with:

```
...
  :main random_streams_for_perf_stats.core
...
```

..in terms of execution speed.

What I did notice is that, even with 20 runs with the execution time measurement script ![exe_times_statistics_for_one_test_case_in_cwd2a](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/02%20-%20execution%20times/exe_times_statistics_for_one_test_case_in_cwd2a) a wide variability of program execution times. On my system the avarage time can vary from around 590 milliseconds to up to 870 milliseconds!

Though, I didn't notice any difference when using command:

```
sudo perf stat -r 20 java -jar ./target/uberjar/random_streams_for_perf_stats-0.1.0-SNAPSHOT-standalone.jar
```

Both methods seem to come to the same results statistically.

<br/>

##_end
