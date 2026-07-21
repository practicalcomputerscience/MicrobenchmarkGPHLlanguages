2026-07-21: work in progress: tbd

<br/>

# Blaise Pascal Compiler

https://github.com/graemeg/blaise

ARC = Automatic Reference Counting

<br/>

Table of contents:

- [Installation tips](#installation-tips)
- tbd
- tbd
- tbd

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

The original [random_streams_for_perf_stats.pp](tbd) program for Object Free Pascal needed **significant amount of refactoring** to make it work for the Blaise Pascal Compiler.
I practically re-developed it from the ground up with lots of help from Google AI. Almost the whole established type system of Free Pascal is gone here!

tbd

<br/>

##_end
