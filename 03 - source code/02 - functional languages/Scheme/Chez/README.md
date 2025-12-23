# Chez Scheme

https://cisco.github.io/ChezScheme/

See some more information from here: [Chez Scheme (CS)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket/README.md#chez-scheme-cs)

pb = portable bytecode

---

Table of contents:

- [Installation tips](#installation-tips)
- [Standalone executable with the Petite interpreter](#standalone-executable-with-the-petite-interpreter)

<br/>

---

## Installation tips

See from file _IMPLEMENTATION.md_:

> Chez Scheme is a bootstrapped compiler, meaning you need a Chez Scheme compiler to build a Chez Scheme compiler.

This will install the **Petite interpreter**: 

- get tarball file csv10.3.0.tar.gz (as of December 2025) from here: https://github.com/cisco/ChezScheme/releases
- extract it, and
- change into extracted directory _./csv10.3.0_

There do:

```
$ ./configure
Configuring for ta6le
$ make
$ sudo make install
$ petite --version  # test version info
10.3.0
$
```

This will conveniently install the **Chez compiler**, though an older version: 

```
$ sudo apt install chezscheme
...
$ chezscheme --version
9.5.8
$
```

Now, you can execute the Chez Scheme source code like this (*):

```
$ time chezscheme --script ./random_streams_for_perf_stats.ss

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.083s
...
$
```

..which is significantly faster than using the Petite interpreter:

```
$ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q --optimize-level 3
$ time petite --program random_streams_for_perf_stats.so
...
real	0m0.668s
...
$
```

(*) this command can also be done like this, using _scheme-script_:
```
$ time scheme-script ./random_streams_for_perf_stats.ss
```

..or like this with _scheme_:

```
$ time scheme --program ./random_streams_for_perf_stats.ss
```

<br/>

## Standalone executable with the Petite interpreter

I also managed to make a standalone executable with this help: [A Chez program, compiled into a standalone executable.](https://github.com/Kato-Dax/selfcontained-chez/tree/main), though I had to modify the compiler program, now named [compile2.scm](./compile2.scm) to mask this expression: _(vfasl-convert-file custom-boot-file custom-boot-file '()))_, which would systematically lead to an error ("Exception in vfasl: cannot vfasl with unknown endianness") otherwise:

- download zip file from: https://github.com/Blugatroff/selfcontained-chez, which is saved as _selfcontained-chez-main.zip_
- extract it into a working directory: _./selfcontained-chez-main_

There do:

```
$ export SCHEME_DIRS=$(echo /usr/lib/csv10.3.0/pb)
$ cp ./random_streams_for_perf_stats.ss ./selfcontained-chez-main  # copy the source code file into this new directory
$ cd ./selfcontained-chez-main  # change into this new directory
$ ./compile2.scm ./random_streams_for_perf_stats.ss  # compile the source code file with the modified compiler program
$ ./random_streams_for_perf_stats  # run the standalone executable
...
$
```

However, this method only generates an executable, which shows a wallclock time of about 770 milliseconds in my system: _$ time ./random_streams_for_perf_stats_

<br/>

##_end
