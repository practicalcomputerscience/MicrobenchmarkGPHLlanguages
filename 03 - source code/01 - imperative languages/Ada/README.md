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

<br/>

## On how to do demanding string building in Ada

The implemented [C-like solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb) with copying the individual characters of _bits_x_str_ into the big, final string _bits_x_ is still a little bit faster with around 18.8 milliseconds than this solution with around 20.6 milliseconds of execution time (which would be more like the Fortran, C++ or Eiffel solutions):

```
...
   bits_x : String (1 .. upper_limit * STR_LENGTH_BIN) := (others => ' ');
   bits_x_str : String (1 .. STR_LENGTH_BIN);
   ...
   for i in 2 .. upper_limit loop
      ...
      byte_nbr := (i - 2) * STR_LENGTH_BIN + 1;
      bits_x (byte_nbr .. byte_nbr + 15) := bits_x_str;
      ...
   end loop;
...
```

So, I keep the original solution as implemented.

<br/>

##_end
