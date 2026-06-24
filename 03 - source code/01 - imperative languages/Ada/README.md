# Ada

https://learn.adacore.com/index.html

https://alire.ada.dev/: ALIRE = Ada LIbrary REpository, plus command-line tool _alr_

https://www.adaic.org/

GNAT = GNU NYU Ada Translator, an open-source Ada compiler of the [GNU Compiler Collection](https://gcc.gnu.org/)

---

Table of contents:

- [Installation tips](#installation-tips)
- [On how to do demanding string building in Ada](#on-how-to-do-demanding-string-building-in-ada)
- [GNAT, the GNU NYU Ada Translator](#gnat-the-gnu-nyu-ada-translator)
- [SPARK for deductive formal program verification](#spark-for-deductive-formal-program-verification)
- [SPARK: No Dynamic Checks or Defensive Code](#tbd)

---

<br/>

## Installation tips

I started installing Ada from page https://alire.ada.dev/ with downloading the _alr-2.1.0-bin-x86_64-linux.zip_ file with button "Download Alire for Linux".

I unzipped that file into my Ada root directory, and added to my _~/.bashrc_ file line: _export PATH="$PATH:~/scripts/Ada/alr-2.1.0-bin-x86_64-linux/bin"_, which I then activated with: _$ source ~/.bashrc_

<br/>

I modified the default project configuration files, both located in the _./config_ subdirectory of their related projects (see below how to start a project):

- _random_bitstring_and_flexible_password_generator_config.gpr_
- _random_streams_for_perf_stats_config.gpr_

..for execution speed optimization like this:

```
--  "-Og" -- Optimize for debug
"-O3"
```

See from here about these Ada compiler switches (-- is a comment in Ada; -O3 is for full optimization): https://gcc.gnu.org/onlinedocs/gnat_ugn/Optimization-Levels.html

Though, I've seen that compiler switch _--release_ is anyway changing this configuration file automatically to something like this:

```
   ...
   Ada_Compiler_Switches := Ada_Compiler_Switches &
          (
            "-O3" -- Optimize for performance
           ,"-gnatn" -- Enable inlining
           ,"-ffunction-sections" -- Separate ELF section for each function
           ,"-fdata-sections" -- Separate ELF section for each variable
           ,"-gnatW8" -- UTF-8 encoding for wide characters
          );
   ...
```

<br/>

Otherwise, I made and ran an Ada project like this for example:

```
$ alr init --bin random_streams_for_perf_stats  # create an Alire project
# then, I just pressed [ENTER] numerous times to generate mostly empty data for the project description
$ cd random_streams_for_perf_stats
# copy source code file random_streams_for_perf_stats.adb into subdirectory ./src
# copy configuration file random_streams_for_perf_stats_config.gpr into subdirectory ./config
$ alr build --release  # try to build the executable in release mode
# initially, Alire will install some up-to-date toolchain now
$ alr run
ⓘ Building random_streams_for_perf_stats=0.1.0-dev/random_streams_for_perf_stats.gpr...
gprbuild: "random_streams_for_perf_stats" up to date
✓ Build finished successfully in 0.21 seconds.

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$ 
```

This workflow created an executable in project subdirectory: _./bin/random_streams_for_perf_stats_

<br/>

## On how to do demanding string building in Ada

The implemented [C-like solution](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ada/random_streams_for_perf_stats.adb) with copying the individual characters of _bits_x_str_ into the big, final string _bits_x_ is still a little bit faster with around 18.8 milliseconds than this solution with around 20.6 milliseconds of execution time (which would be more like the Fortran, C++ or Eiffel solutions):

```
...
   bits_x : String (1 .. upper_limit * STR_LENGTH_BIN) := (others => ' ');
   bits_x_str : String (1 .. STR_LENGTH_BIN);
   ...
   for i in 2 .. upper_limit loop
      ...
      byte_nbr := (i - 2) * STR_LENGTH_BIN + 1;
      bits_x (byte_nbr .. byte_nbr + 15) := bits_x_str;
      ...
   end loop;
...
```

So, I keep the original solution as implemented.

See also below at [SPARK for deductive formal program verification]().

<br/>

#### GNAT, the GNU NYU Ada Translator

Command:

```
$ alr toolchain
CRATE       VERSION STATUS  NOTES                
gprbuild    25.0.1  Default 
gnat_native 15.2.1  Default 
$ 
```

..shows that the [GNAT](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/about_this_guide.html#) open-source compiler is being used, and which is part of the GNU Compiler Collection [GCC](https://gcc.gnu.org/):

>  By default, GNAT assumes Ada 2012...

from: https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/about_this_guide.html#

The _alr build_ command has also generated this file, among other files: 

_./random_streams_for_perf_stats/obj/development/random_streams_for_perf_stats.o_

..which is a directly compiled object code file, and which can then be examined with some tools like this for example:

```
$ objdump -f ./obj/development/random_streams_for_perf_stats.o

./obj/development/random_streams_for_perf_stats.o:     file format elf64-x86-64
architecture: i386:x86-64, flags 0x00000011:
HAS_RELOC, HAS_SYMS
start address 0x0000000000000000

$
```

<br/>

## SPARK for deductive formal program verification

SPARK is a subset of the Ada language: https://www.adacore.com/languages/spark

So, Ada program [random_streams_for_perf_stats.adb](./random_streams_for_perf_stats.adb) needs some changes to pass formal program verification. Google AI helped me with the necessary modifications.

Instead of 1 source code file, there are 3 now:

- [random_streams_for_perf_stats_spark.adb](./SPARK/random_streams_for_perf_stats_spark.adb)
- [stream_generator_spark.adb](./SPARK/stream_generator_spark.adb)
- [stream_generator_spark.ads](./SPARK/stream_generator_spark.ads)

The principal workflow, after source code files are ready, is like this:

```
$ alr init --bin random_streams_for_perf_stats_spark  # create an Alire project
$ cd random_streams_for_perf_stats_spark
# copy the 3 source code files into subdirectory ./src
$ alr build
# fix the code in files ~.adb + ~.ads, so that no errors and warnings are left
$ alr build -- release  # try to build the executable in release mode
$ alr run  # make a program test run
$ alr exec gnatprove -- --mode=prove  # the is the center piece of the workflow: it's really -- --mode=prove
# fix potential code deficits
$ alr build -- release  # build again
$ alr run  # make a final test run
$
```

Hopefully, in project subdirectory _./obj/development/gnatprove/_ a good looking report has been generated at file [gnatprove.out](./SPARK/gnatprove.out):

```
=========================
Summary of SPARK analysis
=========================

-------------------------------------------------------------------------------------
SPARK Analysis results        Total        Flow        Provers   Justified   Unproved
-------------------------------------------------------------------------------------
Data Dependencies                 .           .              .           .          .
Flow Dependencies                 .           .              .           .          .
Initialization                    8           8              .           .          .
Non-Aliasing                      .           .              .           .          .
Run-time Checks                  30           .      30 (CVC5)           .          .
Assertions                       10           .      10 (CVC5)           .          .
Functional Contracts              1           .    1 (Trivial)           .          .
LSP Verification                  .           .              .           .          .
Termination                       4           2       2 (CVC5)           .          .
Concurrency                       .           .              .           .          .
-------------------------------------------------------------------------------------
Total                            53    10 (19%)       43 (81%)           .          .


max steps used for successful proof: 49

============================
Most difficult proved checks
============================

No check found with max time greater than 1 second
...
```

<br/>

Interestingly, compiled executable _random_streams_for_perf_stats_spark_ runs faster than the original Ada executable _random_streams_for_perf_stats_:

- 15.5 milliseconds (SPARK) versus 18.8 milliseconds (Ada), that's about 17% faster 

(best batch out of 3 with command: _sudo perf stat -r 20 ./random_streams_for_perf_stats..._)

<br/>

#### SPARK: No Dynamic Checks or Defensive Code

It looks like that a typical SPARK program has a good chance to run faster than its counterpart in Ada, because with a SPARK program the GNAT compiler can take away the "breaks", which are typically inserted into a compiled Ada program:

> SPARK proves that defensive code and other run-time checks that may be inserted in the code will never fail. This allows the compiler to remove them from the compiled code, ensuring optimal efficiency while retaining guarantees of integrity.

from: https://www.adacore.com/languages/spark

<br/>

##_end
