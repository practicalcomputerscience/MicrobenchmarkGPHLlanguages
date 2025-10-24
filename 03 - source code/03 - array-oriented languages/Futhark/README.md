# Futhark

https://futhark-lang.org/ (*)

With Futhark I found an array-oriented language which compiles to standalone, native binary executables for Linux, even by default via compiling first to C source code.

> [!NOTE]
> However, Futhark is not a general purpose programming language!

And this is the reason why I sadly have to put it onto this [list](TBD), because I cannot implement my microbenchmark program solely in this language, or only some parts of it. Something which is one key idea of this language:

> Futhark is not intended to replace existing general-purpose languages. The intended use case is that Futhark is only used for relatively small but compute-intensive parts of an application. The Futhark compiler generates code that can be easily integrated with non-Futhark code. For example, you can compile a Futhark program to a Python module that internally uses PyOpenCL to execute code on the GPU, yet looks like any other Python module from the outside...

from (*) above.

[Futhark](https://github.com/diku-dk/futhark) calls itself "a data-parallel functional programming language", or more precisely:

> Futhark is a completely **pure language**, with no cheating through monads, effect systems, or anything of the sort.

from: https://futhark.readthedocs.io/en/latest/versus-other-languages.html#evaluation

(my emphasis in bold)

---

## Installation tips

(TBD)

## Entry point 'main'

Even with not being a general purpose programming language, I played around with it.

The first test was to make a standalone executable, for example from this program, named _vector_length.fut_, which is calculating - in connection with a shell command - the Euclidean length of a vector based on the shell provided components. The idea here is to have a _main_ entry point for a standalone executable:

```
def main [n] (vec: [n]f32) : f32 =
  let squares = map (**2) vec
  let sum = reduce (+) 0.0 squares
  in f32.sqrt sum
```

This source code was my model: https://pema.dev/2022/07/29/interestinglangs/#futhark. (For Futhark version 0.26.0) I had to replace function name _vector_length_ with _main_.

Compiling and running it is straightforward:

```
$ futhark vector_length.fut
$ echo [1, 2, 3] | vector_length
3.741657f32
$
```

Be aware that type _f32_ is also provided at the result.

## OpenCL

But its getting even better. When OpenCL is installed on your Linux system, you can compile and run the program like this:

```
$ futhark opencl vector_length.fut -o vector_length_opencl
$ echo [1, 2, 3] | ./vector_length_opencl
3.741657f32
$

The installation of OpenCL can be checked with _clinfo_:

$ clinfo
...
ICD loader properties
  ICD loader Name                                 OpenCL ICD Loader
  ICD loader Vendor                               OCL Icd free software
  ICD loader Version                              2.3.2
  ICD loader Profile                              OpenCL 3.0
$

ICD = Installable Client Driver

If missing, _clinfo_ can be installed in Ubuntu like this: _sudo apt install clinfo_

Here may be help, if OpenCL is missing from the system (I didn't test it): [Getting started with OpenCL on Ubuntu Linux](https://github.com/KhronosGroup/OpenCL-Guide/blob/main/chapters/getting_started_linux.md#getting-started-with-opencl-on-ubuntu-linux)

## Interaction with Gnuplot

(TBD)

<br/>

##_end
