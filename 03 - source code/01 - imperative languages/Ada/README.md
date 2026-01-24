# Ada

https://learn.adacore.com/index.html

https://alire.ada.dev/: ALIRE = Ada LIbrary REpository, plus command-line tool _alr_

https://www.adaic.org/

---

## Installation tips

I modified the default project configuration files, both located in the _./config_ directory of their related projects:

- _random_bitstring_and_flexible_password_generator_config.gpr_
- _random_streams_for_perf_stats_config.gpr_

..for execution speed optimization like this:

```
--  "-Og" -- Optimize for debug
"-O3"
```

See from here about these Ada compiler switches (-- is a comment in Ada; -O3 is for full optimization): https://gcc.gnu.org/onlinedocs/gnat_ugn/Optimization-Levels.html

Otherwise, I made an Ada project like this for example:

```
$ alr init --bin random_bitstring_and_flexible_password_generator
$ cd random_bitstring_and_flexible_password_generator
$ alr build
$ alr run
```

..which is creating an executable named like this and as seen from the project directory:

```
$ ./bin/random_bitstring_and_flexible_password_generator
```

##_end
