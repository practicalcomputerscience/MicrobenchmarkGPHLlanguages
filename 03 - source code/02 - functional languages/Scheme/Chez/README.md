# Chez Scheme

https://cisco.github.io/ChezScheme/

See some more information from here: [Chez Scheme (CS)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket/README.md#chez-scheme-cs)

pb = portable bytecode

---

Table of contents:

- [Installation tips](#installation-tips)
- [Ncurses](#ncurses)

<br/>

---

## Installation tips

See from file _IMPLEMENTATION.md_:

> Chez Scheme is a bootstrapped compiler, meaning you need a Chez Scheme compiler to build a Chez Scheme compiler.

You may get, build and install (latest version of) Chez Scheme like this -- and exactly in this order!!

```
$ sudo apt install libncurses-dev  # may be missing, but is needed
$ sudo apt install libx11-dev  # may be missing, but is needed
$ curl -L -O https://github.com/cisco/ChezScheme/releases/download/v10.3.0/csv10.3.0.tar.gz  # get version 10.3.0 as of 2025-12-23
$ tar -xf csv10.3.0.tar.gz  # unpack
$ cd csv10.3.0
$ ./configure
Configuring for ta6le, and will create boot files via pb
$ make  # this may take some time
$ sudo make install
...
$ scheme --version  # test version info of Chez compiler
10.3.0
$ petite --version  # test version info of Petite interpreter
10.3.0
$ cd ..
$
```

Now, you should be able to execute the microbenchmark source code for Chez Scheme like this:

```
$ time scheme --script ./random_streams_for_perf_stats.ss

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.083s

...
$
```

..or like this, with first compiling to “unsafe” code, that is file _random_streams_for_perf_stats.so_, and then running it with the Petite interpreter:

```
$ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q --optimize-level 3
$ time petite --program random_streams_for_perf_stats.so
...
real	0m0.070s
...
$
```

### Ncurses

When you see (much) slower execution times (at a comparable system), in the seconds or even over one minute and more, then something with the Ncurses may be wrong:

> [!IMPORTANT]
> After elaborate experimentation, I found out, also with the help of Big AI ("Chez Scheme is very slow in Ubuntu"), that the installation of the Terminal User Interface (TUI) library _ncurses_, short for "new curses", may be of utmost importance **before** installing Chez Scheme as shown above.

So, when you have forgotten it, just repeat the complete order of commands as shown above. After that, re-check the installed version:

```
$ scheme --version  # test version info of Chez compiler
10.3.0
$ petite --version  # test version info of Petite interpreter
10.3.0
$
```

<br/>

Notes:

- compiling for pb leads to much slower programs than using ta6le, which is for threaded 64-bit Linux.
- command _$ sudo make uninstall_ removes the complete directory _/usr/lib/csv10.3.0_: a new _configuration_, _make_ and _sudo make install_ command chain is needed then (though this should be quick when not doing it the first time); don't forget about the ncurses!

<br/>

##_end
