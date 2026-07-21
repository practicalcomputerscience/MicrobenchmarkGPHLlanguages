2026-07-21: work in progress: tbd

<br/>

# Blaise Pascal Compiler

https://github.com/graemeg/blaise

ARC = Automatic Reference Counting

---

Table of contents:

- [Installation tips](#installation-tips)
- tbd
- tbd
- tbd

<br/>

---

## Installation tips

I downloaded this tarball file with the pre-compiled binary _blaise-v0.13.0-linux-x86_64.tar.gz_ from here: https://github.com/graemeg/blaise/releases/tag/v0.13.0

I unpacked that tarball file and expanded my _~/.bashrc_ configuration file like this (activate it after changes with _$ source ~/.bashrc_):

```
export BLAISE_RTL_SRC="$HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/rtl-src"
export PATH="$HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64:$PATH"
```

Building the Blaise Pascal Compiler from sources takes more effort, because this compiler is using [PasBuild](https://github.com/graemeg/pasbuild#pasbuild), which needs a working Free Pascal Compiler,
if you don't install PasBuild's pre-compiled binary: https://github.com/graemeg/pasbuild#6-requirements (I didn't do it).

PasBuild is from the same author, Graeme Geldenhuys, as the Blaise Pascal Compiler.

<br/>

Building and running the _hello.pas_ example worked like this:

```
$ sudo mkdir /rtl  # do this only once
$ sudo chmod 777 /rtl  # do this only once
$ blaise --source ./blaise-v0.13.0-linux-x86_64/hello.pas --output hello
$ ./hello
Hello from Blaise!
$
```

<br/>

## Microbenchmark program: speed part

The original [random_streams_for_perf_stats.pp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_streams_for_perf_stats.pp) program for Object Free Pascal needed **significant amount of refactoring** to make it work for the Blaise Pascal Compiler.

I practically re-developed it from the ground up with lots of help from Google AI: [random_streams_for_perf_stats_blaise.pp](./random_streams_for_perf_stats_blaise.pp)

Almost the whole established type system of Free Pascal has gone!

Building and running a standalone executable for Linux (still takes) some effort as these commands show:

```
$ sudo mkdir /rtl  # do this only once if not done yet
$ sudo chmod 777 /rtl  # do this only once if not done yet
$ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
--linker external \
--source random_streams_for_perf_stats_blaise.pp \
--output random_streams_for_perf_stats_blaise
$ 
```

It's essential that you check all paths in your system like this for example: _--unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src_

There may be a more elegant way already now to make a standalone executable for Linux, but that was my final workflow with lots of help from Google AI again.

tbd

I)
$ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
--source random_streams_for_perf_stats_blaise.pp \
--emit-ir > random_streams_for_perf_stats.qbe
$ qbe -o random_streams_for_perf_stats.s random_streams_for_perf_stats.qbe  # from IR to assembly code
$ cc -c random_streams_for_perf_stats.s -o random_streams_for_perf_stats.o  # from assembly code to object code
>>>>>>>> test: -march=native -O3 -flto <<<<<<<<<<<<<<<<<<<<<<<<<< tbd

---
II) compile the RTL (runtime lib) to get it into a linkable state:
blaise-0.13.0.tar.gz --> unzip:
~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts$
./build-rtl-objects.sh $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/blaise ./compiler/target/rtl
./compiler/target/rtl/rtl.platform.o
./compiler/target/rtl/runtime.atomic.o
./compiler/target/rtl/runtime.setjmp.o
./compiler/target/rtl/runtime.utf8.o
./compiler/target/rtl/runtime.mem.o
./compiler/target/rtl/runtime.str.o
./compiler/target/rtl/runtime.set.o
./compiler/target/rtl/runtime.arc.o
./compiler/target/rtl/runtime.weak.o
./compiler/target/rtl/runtime.float.o
./compiler/target/rtl/runtime.thread.o
./compiler/target/rtl/runtime.exc.o
./compiler/target/rtl/runtime.errno.linux.o
./compiler/target/rtl/rtl.platform.layout.linux.o
./compiler/target/rtl/rtl.platform.posix.o
$
---
III) Linking the app:
$ cc -o random_streams_for_perf_stats_qbe \
  random_streams_for_perf_stats_qbe.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/rtl.platform.posix.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/rtl.platform.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/rtl.platform.layout.linux.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.atomic.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.arc.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.exc.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.mem.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.setjmp.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.str.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.float.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.utf8.o \
  ~/scripts/Blaise_Pascal_Compiler/blaise-0.13.0/scripts/compiler/target/rtl/runtime.weak.o
$
$ time ./random_streams_for_perf_stats_qbe => real	0m0.029s <<<<<<



<br/>

## Full microbenchmark program

Since I've implemented the complete microbenchmark program for the Free Pascal, Unleashed compiler ([random_bitstring_and_flexible_password_generator_unleashed.pp](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Free%20Pascal/random_bitstring_and_flexible_password_generator_unleashed.pp)) as a "soft" migration, I also did that for the Blaise Pascal Compiler, again as a "harsh" migration: [random_bitstring_and_flexible_password_generator_blaise.pp](./random_bitstring_and_flexible_password_generator_blaise.pp)


tbd

<br/>

##_end
