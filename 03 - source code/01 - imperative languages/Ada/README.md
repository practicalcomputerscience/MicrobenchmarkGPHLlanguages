# Ada

https://learn.adacore.com/index.html

https://alire.ada.dev/: ALIRE = Ada LIbrary REpository, plus command-line tool _alr_

https://www.adaic.org/

GNAT = GNU NYU Ada Translator, an open-source Ada compiler of the [GNU Compiler Collection](https://gcc.gnu.org/)

---

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

Otherwise, I made and ran an Ada project like this for example:

```
$ alr init --bin random_streams_for_perf_stats
# then, I just pressed [ENTER] numerous times to generate mostly empty data for the project description
$ cd random_streams_for_perf_stats
# copy source code file random_streams_for_perf_stats.adb into subdirectory ./src
# copy configuration file random_streams_for_perf_stats_config.gpr into subdirectory ./config
$ alr build --release
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

..which has created an executable as seen from the project root directory like this:

```
$ ./bin/random_streams_for_perf_stats
```

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

##_end
