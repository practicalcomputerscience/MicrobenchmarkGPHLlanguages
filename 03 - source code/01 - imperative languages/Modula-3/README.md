2026-07-09: work in progress

test on linux270k: start with version d5.11.4 and only update dir _bootstrap_ with d5.12.0 sources! tbd <<<<<<<<<<<<<<<<<<<<<<

<br/>

# Modula-3

Here in the actively maintained **Critical Mass Modula-3** implementation (**CM3**):

https://github.com/modula3/cm3

https://modula3.github.io/cm3/help/interfaces.html

<br/>

> Beginning in 1986, yet another language descendant, Modula-3, was produced by a multi-industry team outside ETH.[6] It adds support for exception handling, garbage collection, generic programming (similar to C++ templates), marking of unsafe code, multithreading, and object-oriented programming.

from PDF:  https://www.math.utah.edu/~beebe/modula-2/app-modula-2-2025-01-13.pdf

<br/>

Modula-2+ was the bridge between [Modula-2](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2#modula-2) and Modula-3, specifically designed for large systems, and added these new main features:

- exception handling
- automatic storage management (garbage collection)
- concurrency for multiprocessors: Modula-2 only supports coroutines 
- type system extensions

from 1986 (PDF): https://softwarepreservation.computerhistory.org/modula2+/doc/Rovner-1986.pdf

<br/>

"The Modula-3 Type System", 1989 (PDF): http://lucacardelli.name/Papers/Modula3TypeSystem.A4.pdf

<br/>

## Installation tips

> [!WARNING]
> After some experimentation, I can say that initially anything else than following these official instructions: [Getting Started: Linux](https://github.com/modula3/cm3/wiki/Getting-Started%3A-Linux) for older version d5.11.4 is a hot mess! (unless you are an expert of course)

So, this is what I did to get a working CM3 system:

```
$ sudo apt-get install build-essential cmake python3  # installing prerequisites
...
$ sudo apt-get install libglu1-mesa-dev xorg-dev  # prerequisites for the full system, which is being built here
...
$ curl -LO https://github.com/modula3/cm3/releases/download/d5.11.4/cm3-dist-AMD64_LINUX-d5.11.4.tar.xz
...
$ tar xf cm3-dist-AMD64_LINUX-d5.11.4.tar.xz
$
```

Only this tarball file has the whole infrastructure included:

```
$ ls cm3-dist-AMD64_LINUX-d5.11.4
bootstrap        COPYRIGHT-COLUMBIA  COPYRIGHTS       examples                     m3-games    m3overrides  m3-ui           README-unicode-summary
caltech-other    COPYRIGHT-DEC       COPYRIGHT-XEROX  getting-started-windows.txt  m3-lectern  m3-pkgtools  m3-win          scratch
caltech-parser   COPYRIGHT-INTEL     docs             m3-comm                      m3-libs     m3-scheme    m3-www          scripts
COPYRIGHT-BSD    COPYRIGHT-JDP       elego            m3-db                        m3-mail     m3-sys       README          tools
COPYRIGHT-CMASS  COPYRIGHT-PURDUE    ESC              m3-demo                      m3-obliq    m3-tools     README-unicode
$
```

Continue with:

```
$ mkdir build
$ cd build
$ ../cm3-dist-AMD64_LINUX-d5.11.4/scripts/concierge.py install --prefix $HOME/cm3 all
...  # this may take a while!
$
```

Then I expanded my _~/.bashrc_ configuration file with line: _export PATH="$HOME/cm3/bin:$PATH"_, and activated it with _$ source ~/.bashrc_ to make a version test:

```
$ cm3 --version
Critical Mass Modula-3 version d5.11.4
  GitInfo: unknown
  last updated: 2021-10-07
  compiled: 2026-07- 9 22:12:03
  configuration: ~/cm3/bin/cm3.cfg
  host: AMD64_LINUX
  target: AMD64_LINUX

$
```

Super-important is this information: _configuration: ~/cm3/bin/cm3.cfg_ (~ denotes the home directory of the user):

```
$ cat ~/cm3/bin/cm3.cfg
readonly M3_BACKEND_MODE = "C"
if not defined("SL") SL = "/" end
if not defined("HOST") HOST = "AMD64_LINUX" end
if not defined("TARGET") TARGET = HOST end
INSTALL_ROOT = (path() & SL & "..")
include(path() & SL & "config" & SL & TARGET)
$
```

If this file is missing, you got a problem and should start all over again!

<br/>

I created individual projects for both programs, the "speed part" and the complete microbenchmark. So, rename the source code and make files accordingly inside their project directories (both files go into the same root project directory):

- _random_streams_for_perf_stats_Main.m3_ --> _Main.m3_
- _random_streams_for_perf_stats_m3makefile_ --> _m3makefile_
- _random_bitstring_and_flexible_password_generator_Main.m3_ --> _Main.m3_
- _random_bitstring_and_flexible_password_generator_m3makefile_ --> _m3makefile_

<br/>

### On keeping the (Critical Mass) Modula-3 source code idiomatic

The [Modula-2 implementation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2/random_streams_for_perf_stats.mod) of the "speed part" of the microbenchmark program is a very fast executable with an execution time of about only 6 milliseconds.

However, I didn't refactor the [CM3 source code](tbd) the make also this implementation very speedy, even though it's a bit slow for a compiled program with about 78 milliseconds of program execution time. I want to keep the Modula-3 program as idiomatic as I can possibly do. CM3 has numerous standard libraries: https://modula3.github.io/cm3/help/interfaces.html

Consequently, the number of source lines of code of the Modula-3 program with tbd is much shorter than the number of source lines of code of its [Modula-2 counterpart](tbd) with tbd.
I'm only using inbuilt functions in my CM3 implementation.

<br/>





<br/>

p.s.: Critical Mass: where did this name come from? 

> ..a commercial compiler named CM3 maintained by one of the chief implementors prior at DEC SRC who was hired before DEC being sold to Compaq, an integrated development environment (IDE) named Reactor and an extensible Java virtual machine .. were offered by Critical Mass, Inc., but that company ceased active operations in 2000..

from: https://en.wikipedia.org/wiki/Modula-3

<br/>

##_end
