2025-01-05: work in progress

# Fortran

https://fortran-lang.org

<br/>

---

## Fortran is computer programming legacy

The fact that Fortan doesn't have an official, integrated **standard library** is a bummer (https://stdlib.fortran-lang.org/): 

> The Fortran Standard, as published by the ISO (https://wg5-fortran.org/), does not have a Standard Library.

This would require to think about how to get _reasonable_ access to this standard library, and its useful functions, in your system without starting another monster configuration workflow. I have not done this.

The second bummer are the legacy _print_ and _write_ functions with their notorious desire to "reserve the first column of all output for carriage control" ([Formatted Input/Output](https://cyber.dabamos.de/programming/modernfortran/formatted-input-output.html)) and
to automatically append a line feed (LF) control character when writing strings to files.

It seems to me that the _write_ function provides more flexibility for acceptable output formatting than the _print_ function; see for example:

```
                write(*, '(A, A)') "Bit stream has been written to disk under name:  ", trim(filename)
```

..where _'(A, A)'_ signifies two strings of **A**rbitrary length.

<br/>

On block comments in Fortran:

```
go to 100
 ! CHUNK OF CODE YOU WANT TO COMMENT OUT
100 continue
```

seen from here: https://stackoverflow.com/questions/22049963/how-to-do-block-comment-in-fortran

---

## Installation tips

I installed Fortan, that is installing its compiler, from Ubuntu's package manager as recommended here at: [Hello world](https://fortran-lang.org/learn/quickstart/hello_world/)

```
$ sudo apt install gfortran
$ gfortran --version
GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
...
$
```

## Building tips

I checked out potentially useful compiler switches to compile a fast executable, but in vain:

```
$ gfortran --help:
...
Options starting with -g, -f, -m, -O, -W, or --param are automatically passed on to the various sub-processes invoked by gfortran
...
$ gfortran --help=optimizers
...
#  testing compiler switches for execution speed:
#                                       --> ~0.049s (no optimization)
#   -O3                                 --> ~0.049s
#   -Ofast                              --> ~0.050s
#   -O3 -faggressive-loop-optimizations --> ~0.050s
#   -O3 -ffast-math                     --> ~0.052s
#   -march=native                       --> ~0.049s
#   -funroll-loops                      --> ~0.050s
#
$ gfortran --help=warnings
...
$
```

In the end, and also after reading this article: [Best Practices for Optimizing Performance in Fortran Applications - A Comprehensive Guide](https://moldstud.com/articles/p-best-practices-for-optimizing-performance-in-fortran-applications-a-comprehensive-guide) from August 2025, I've chosen to only take three extra warning switches,
where I think that the _-fcheck=all_ switch is specifically useful when you are used to work with indices that start with 0, instead of 1 at Fortran, like in other programming languages:

```
$ gfortran -Wall -Wextra -fcheck=all random_streams_for_perf_stats.f90 -o random_streams_for_perf_stats
```

TBD

<br/>

##_end
