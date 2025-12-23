2025-12-23: work in progress

# Chez Scheme

https://cisco.github.io/ChezScheme/

See some more information from here: [Chez Scheme (CS)](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme/Racket/README.md#chez-scheme-cs)

pb = portable bytecode

---

Table of contents:

- [Installation tips](#installation-tips)
- [Preserving the path to Chez Scheme for machine type ta6le](#preserving-the-path-to-chez-scheme-for-machine-type-ta6le)
- [Making a standalone executable](#making-a-standalone-executable)

<br/>

---

## Installation tips

See from file _IMPLEMENTATION.md_:

> Chez Scheme is a bootstrapped compiler, meaning you need a Chez Scheme compiler to build a Chez Scheme compiler.

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
$ chezscheme --version  # test version info of the actual Chez compiler program
10.3.0
$ 
```

Now, you can execute the Chez Scheme source code like this:

```
$ time scheme --script ./random_streams_for_perf_stats.ss

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m7.097s
...
$
```

..or like this, with first compiling to “unsafe” code in an ~.so file and then running it with the Petite interpreter:

```
$ echo '(compile-file "random_streams_for_perf_stats.ss")' | scheme -q --optimize-level 3
$ time petite --program random_streams_for_perf_stats.so
...
real	0m4.146s
...
$
```

Bummer times!

What went wrong?

TBD

<br/>

## Making a standalone executable

I also managed to make a standalone executable with this help: [A Chez program, compiled into a standalone executable.](https://github.com/Kato-Dax/selfcontained-chez/tree/main):

- download zip file from: https://github.com/Blugatroff/selfcontained-chez, which is saved as _selfcontained-chez-main.zip_
- extract it into a working directory: _./selfcontained-chez-main_

However, I noticed that this procedure requires compiling to portable bytecode (pb) to make it working, at least it's the only solution that I've found. So, compile additional Chez Scheme programs with:

```
$ ./configure --pb
Configuring for pb to run on a6le
$ make
...
$ sudo make install
...
$ 
```

Now, with this second installation, directory _/usr/lib/csv10.3.0_ should look like this:

```
$ ls -1 /usr/lib/csv10.3.0
examples
pb
ta6le
$
```

Then do:

```
$ export SCHEME_DIRS=$(echo /usr/lib/csv10.3.0/pb)  # this directory came with the second installation: sudo make install
$ cp ./random_streams_for_perf_stats.ss ./selfcontained-chez-main  # copy the source code file into this new directory
$ cd ./selfcontained-chez-main  # change into this new directory
$ ./compile.scm ./random_streams_for_perf_stats.ss  # compile the source code file with the compiler program
$ ./random_streams_for_perf_stats  # run the standalone executable
...
$
```

However, this method only generates an executable, which shows a wallclock time of about 770 milliseconds in my system: _$ time ./random_streams_for_perf_stats_

<br/>

##_end
