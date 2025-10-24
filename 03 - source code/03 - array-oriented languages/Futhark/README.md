# Futhark

https://futhark-lang.org/ (*)

Table of contents:

- [Futhark as an array-oriented, pure functional programming language](#futhark-as-an-array-oriented-pure-functional-programming-language)
- [Installation tips](#installation-tips)
- [Entry point 'main'](#entry-point-main)
- [Using OpenCL](#using-opencl)
- [Interaction with gnuplot example](#interaction-with-gnuplot-example)

<br/>

---

## Futhark as an array-oriented, pure functional programming language

With Futhark I found an array-oriented language which compiles to standalone, native binary executables for Linux, even by default via compiling first to C source code.

> [!NOTE]
> However, Futhark is not a general purpose programming language!

And this is the reason why I sadly have to put it onto this [list](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#futhark), because I cannot implement my microbenchmark program solely in this language, or only some parts of it. Something which is one key idea of this language:

> Futhark is not intended to replace existing general-purpose languages. The intended use case is that Futhark is only used for relatively small but compute-intensive parts of an application. The Futhark compiler generates code that can be easily integrated with non-Futhark code. For example, you can compile a Futhark program to a Python module that internally uses PyOpenCL to execute code on the GPU, yet looks like any other Python module from the outside...

from (*) above.

[Futhark](https://github.com/diku-dk/futhark) calls itself "a data-parallel functional programming language", or more precisely:

> Futhark is a completely **pure language**, with no cheating through monads, effect systems, or anything of the sort.

from: https://futhark.readthedocs.io/en/latest/versus-other-languages.html#evaluation

(my emphasis in bold)

## Installation tips

For installation I followed this advice: [1.3. Installing from a precompiled snapshot](https://futhark.readthedocs.io/en/stable/installation.html#installing-from-a-precompiled-snapshot), extracted binary snapshots _futhark-nightly-linux-x86_64.tar.xz_ and ran: _$ sudo make install_ in the extracted directory.

The installation can be tested with:

```
$ futhark -V
Futhark 0.26.0 (prerelease - include info below when reporting bugs).
...
$ 
```

## Entry point 'main'

Even with not being a general purpose programming language, I played around with it.

The first test was to make a standalone executable, for example from this program, named _vector_length.fut_, which is calculating - in connection with a shell pipe command - the Euclidean length of a vector based on the shell provided components. The idea here is to have a _main_ entry point for a standalone executable:

```
def main [n] (vec: [n]f32) : f32 =
  let squares = map (**2) vec
  let sum = reduce (+) 0.0 squares
  in f32.sqrt sum
```

This source code was my model: https://pema.dev/2022/07/29/interestinglangs/#futhark. (For Futhark version 0.26.0) I had to replace function name _vector_length_ with _main_.

Compiling and running this program is straightforward:

```
$ futhark vector_length.fut
$ echo [1, 2, 3] | vector_length
3.741657f32
$
```

Be aware that type _f32_ is also provided at the result.

## Using OpenCL

But its even getting better. When OpenCL is installed on your Linux system, you can compile the program with the _opencl_ command and run it like this:

```
$ futhark opencl vector_length.fut -o vector_length_opencl
$ echo [1, 2, 3] | ./vector_length_opencl
3.741657f32
$
```

The installation of OpenCL can be checked with _clinfo_:

```
$ clinfo
...
ICD loader properties
  ICD loader Name                                 OpenCL ICD Loader
  ICD loader Vendor                               OCL Icd free software
  ICD loader Version                              2.3.2
  ICD loader Profile                              OpenCL 3.0
$
```

ICD = Installable Client Driver

If missing, _clinfo_ can be installed in Ubuntu like this: _$ sudo apt install clinfo_

Here may be help, if OpenCL is missing from the system (I didn't test it): [Getting started with OpenCL on Ubuntu Linux](https://github.com/KhronosGroup/OpenCL-Guide/blob/main/chapters/getting_started_linux.md#getting-started-with-opencl-on-ubuntu-linux)

## Interaction with gnuplot example

I wanted to run this example for the interaction of Futhark with [gnuplot](http://www.gnuplot.info/): [Plotting a histogram](https://futhark-lang.org/examples/plot-histogram.html), but it took me some time to understand how to make it work.

The program(!) shown below, named _plot-histogram.fut_, is using a _directive_, which "will be executed and replaced with its output" as "a way to show the result of running a function"; here the _> :gnuplot_ directive, which is used two times:

```
-- # Plotting a histogram
def plot k n : ([]i64,[]i32) =
  let vs = iota n |> map (f64.i64 >-> f64.cos >-> (+1) >-> (*(f64.i64 k/2)) >-> i64.f64)
  in (iota k,
      hist (+) 0 k vs (replicate n 1))

-- > :gnuplot {data=plot 10 10000};
-- set style fill solid 1.0
-- plot data with boxes
-- > :gnuplot {data=plot 100 10000};
-- set style fill solid 1.0
-- plot data with boxes
```

from source https://futhark-lang.org/examples/plot-histogram.fut, which I shortened a bit.

Execute this program with the _literate_ command:

```
$ futhark literate plot-histogram.fut
$
```

This will produce a couple of files, which are in the same directory:

- _plot-histogram.c_, a big C source code file
- _plot-histogram_, its compiled program, which is not supposed to be run standalone
- _plot-histogram.md_, a prettyprinted Markdown file of the Futhark source code

In subdirectory _plot-histogram-img_ there are:

- _4198323d5d75364ae598af5f5e52a654-plot.png_, the picture from the first directive
- _87c2d0d3b263e658075945d0208bf075-plot.png_, the picture from the second directive
- _CACHEDIR.TAG_, a cache directory tag file: https://bford.info/cachedir/

Here the first picture:

![plot](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/03%20-%20array-oriented%20languages/Futhark/4198323d5d75364ae598af5f5e52a654-plot.png)

<br/>

##_end
