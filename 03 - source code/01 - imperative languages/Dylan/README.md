2026-06-27: work in progress: tbd

tbd: toc

<br/>

# Dylan

DyLan = Dynamic Language

Here, surviving implementation Open Dylan is meant: https://opendylan.org (*)

Here, I use _Dylan_ synonymously for _Open Dylan_.

<br/>

## Idea of Dylan

Though Dylan apparently started as a superset of [Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme), it looks to me that it soon became another multi-paradigm language,
where I had no problems to implement a very "imperative" version of my microbenchmark program with [random-bitstring-and-flexible-password-generator.dylan](./random-bitstring-and-flexible-password-generator.dylan).

Dylan was first published as a language draft for the [Apple Newton device](https://en.wikipedia.org/wiki/Apple_Newton) in 1993: [Dylan (Dynamic Language), A multi-paradigm language, Oliver Juwig, sd&m, Aachen, 12. Februar 2003](https://verify.rwth-aachen.de/fp02/Folien/Dylan.pdf)

Fom (*): 

> Dylan is an object-functional language originally created by Apple for the Newton. Dylan is a direct descendant of Scheme and CLOS (without the Lisp syntax) with a programming model designed to support efficient machine code generation, including fine-grained control over dynamic and static behaviors.

In official [An Introduction to Dylan](https://opendylan.org/intro-dylan/index.html#an-introduction-to-dylan), "object-functional" morphed into "object-oriented": 

> Dylan is an object-oriented dynamic language designed for efficient compilation.
> It uses an algebraic infix syntax similar to Pascal or C, but supports an object model not unlike the Common Lisp Object System (CLOS).

I generally see a certain nearness of Dylan code to [Common Lisp code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Common%20Lisp#common-lisp), so I guess that I could have also implemented a very functional version of [random-bitstring-and-flexible-password-generator.dylan](./random-bitstring-and-flexible-password-generator.dylan).

<br/>

## Installation tips

I took latest pre-compiled Dylan sources _opendylan-2026.1-x86_64-linux.tar.bz2_ (as of 2026-06-28) from here: https://github.com/dylan-lang/opendylan/releases/tag/v2026.1.0,
unzipped it, added to my _~/.bashrc_ configuration file line: _export PATH="$HOME/scripts/Dylan/opendylan-2026.1-x86_64-linux/opendylan-2026.1/bin:$PATH"_,
and activated it with command: _$ source ~/.bashrc_

<br/>

## Making a simple Dylan application

One Dylan application is producing numerous shared object files (~.so*). All these shared object files are not very beneficial for easy porting an Dylan application from its source Linux system to another target Linux system.

So, I followed the approach to only make one executable library by applying switch _--simple_ at the build tool command _deft_: [Hello World](https://opendylan.org/getting-started-cli/hello-world.html#hello-world), something which is anyway producing a number of _~.so*_ files, see below.

Notice that application name _random_streams_for_perf_stats_ is not possible, thus name _random-streams-for-perf-stats_ for the application and main executable file:

```
$ deft new application --simple random-streams-for-perf-stats
$ cd ./random-streams-for-perf-stats  # change into the project root directory
```

Now, fix the pre-generated [library.dylan](./library.dylan%20for%20random-streams-for-perf-stats.dylan) file in the project root directory:

```
Module: dylan-user
Synopsis: Module and library definition for simple executable application

define library random-streams-for-perf-stats
  use common-dylan;
  use io, import: { format-out };
  use io;                 // 2026-06-27
  use system;             // 2026-06-27
end library;

define module random-streams-for-perf-stats
  use common-dylan;
  use simple-random;      // 2026-06-27
  use common-extensions;  // 2026-06-27
  use format-out;
  use streams;            // 2026-06-27
  use file-system;        // 2026-06-27
end module;
```

Now, the application should be buildable and executable like this:

```
$ deft build --all
Open Dylan 2026.1

Opened project random-streams-for-perf-stats (~/scripts/Dylan/random-streams-for-perf-stats/random-streams-for-perf-stats.lid)
Processing library random-streams-for-perf-stats
Loading namespace for library random-streams-for-perf-stats
Build of 'random-streams-for-perf-stats' completed
Linking random-streams-for-perf-stats
Building targets: exe within ~/scripts/Dylan/random-streams-for-perf-stats/_build/build/random-streams-for-perf-stats/
$ _build/bin/random-streams-for-perf-stats

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

<br/>

### Porting a simple Dylan application

Porting a "simple" application like above from its source Linux system, the system where it has been built, to a target system, for testing for example,
is possible, if certain shared object files are also copied to the target system, into the same directory as the main executable _random-streams-for-perf-stats_ for convenience.

Of course, one could also copy the whole project directory from the source system to the target system:

- ./_build/lib/libcommon-dylan.so
- ./_build/lib/libcollections.so
- ./_build/lib/libdylan.so
- ./_build/lib/libio.so
- ./_build/lib/librandom-streams-for-perf-stats.so
- ./_build/lib/libsystem.so
- ./_build/lib/libunwind.so
- ./_build/lib/libunwind.so.1
- ./_build/bin/random-streams-for-perf-stats  # the main executable

At the Linux target system, set environment variable _LD_LIBRARY_PATH_ to the local directory of the Linux executable, and run the application like this:

```
$ export LD_LIBRARY_PATH=.
$ chmod 755 ./random-streams-for-perf-stats
$ ./random-streams-for-perf-stats

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$ 
```

<br/>

##_end
`

