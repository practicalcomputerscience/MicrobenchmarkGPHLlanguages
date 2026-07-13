2026-07-10: work in progress

- tbd: "speed part" in: Oberon+: Oberon with extensions (OBX)
- tbd: toc

<br/>

# Oberon

<br/>

## Idea of Oberon

> Oberon is a general-purpose programming language that evolved from Modula-2.

from: The Programming Language Oberon, (Revision 1. 10. 90), N.Wirth (PDF): https://people.inf.ethz.ch/wirth/Oberon/Oberon.Report.pdf

And not only that, but Oberon is also an example of how to damage an ecosystem, together with the Modula ecosystem, by **fragmenting** it. I found these _original_ flavors of Oberon:

- Oberon since 1988: https://onlinelibrary.wiley.com/doi/10.1002/spe.4380180706
- Oberon-2, "essentially Oberon with a few extensions", since 1990 (PDF): https://people.inf.ethz.ch/wirth/Oberon/Oberon.Report.pdf. See also at [EDM2/Oberon-2](https://www.edm2.com/index.php/Oberon-2) (~).
- Active Oberon ("Do the Fish Really Need Remote Control? A Proposal for Self-Active Objects in Oberon") since 1997: https://link.springer.com/chapter/10.1007/3-540-62599-2_41. It had "explicit support for programming multicore processors" (~).
- Persistent Oberon from 2000: https://link.springer.com/chapter/10.1007/978-3-540-76637-7_6
- Oberon-07/16, also known as "Revised Oberon" since 2007 (PDF): https://people.inf.ethz.ch/wirth/Oberon/Oberon07.Report.pdf
- "Zonnon is a descendant of Active Oberon that adds safety features and a stronger object system." (~)

See from here about some differences between (some) Oberon dialects: [Motivation for a new Oberon version](https://oberon-lang.github.io/2021/07/15/motivation-for-a-new-oberon-version.html)

<br/>

To me it looks a bit that Oberon was Wirth's attempt to not miss the already rolling object-oriented programming (OOP) train,
since [Modula-2](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2#modula-2)
was not explicitely designed for OOP as published in 1980: [MODULA-2, Wirth, Niklaus](https://doi.org/https://doi.org/10.3929/ethz-a-000189918),
and its fully OOP-capable successor [Modula-3](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3#modula-3), first published in 1988, no longer under his control.

I guess that this paper from 1989 highlights his scepticism and dilemma:

> It is a sad fact that our field of Computer Science is overly dominated by fads.
> 
> ...
> 
> The most recent slogan is object-oriented programming.
> 
> ...
> 
> It is by no means accidental that the paradigm of object-oriented programming - we bow to convention and adopt the misnomer - originated
> in the application area of simulation of system with discrete events.

from: "Modula-2 and object-oriented programming ...", ..., Author(s): Wirth, Niklaus: https://doi.org/https://doi.org/10.3929/ethz-a-000523424

<br/>

Another aim of Oberon was:

> ..to reduce the complexity of programming languages, of Modula in particular.

from: "Programming, A Tutorial, A derivative of Programming in Modula-2 (1982)", Niklaus Wirth (rev. 5.10.2015), PDF: https://people.inf.ethz.ch/wirth/Oberon/PIO.pdf

Something which was soon revised, at least to some extent; see below at [Oberon+](#tbd).

<br/>

Ironically, Modula-2 and Modula-3 are still around somehow in 2025, under their original names, but what happened to Oberon? And in what _implementation_ still usable on Linux in year 2026?

This page gave me advice: https://fruttenboel.nl/obc/Main.html

> OBC is by far the best Oberon compiler. ETHZ versions of Oberon are dead, OBC is the way to go 

<br/>

## Oxford Oberon-2 Compiler (OBC)

User manual for version 3 (PDF): https://spivey.oriel.ox.ac.uk/wiki/images/c/ce/Obcman-3.1.pdf

> [!NOTE]
> The OBC Library, see at chapter 5, is really slim. For example, in module _String_ only one operation is provided so far!

Thus, and so far (as of 2026-07-13), the OBC implementation of the microbenchmark program is the only one where a simple string comparison is implemented in (another) user defined function (or here procedure), named _StringsEqual_!

<br/>

The OBC usually compiles to bytecode, that is into _*.k_ files, for its own virtual machine, where it gets just-in-time (JIT) compiled, so like usually with Java and the Java Virtual Machine (JVM).

In other words, OBC generated "executables" are not portable to another Linux machine without prior OBC installation.

<br/>

Principially, I followed the instructions at [Installing OBC release 3.3](https://spivey.oriel.ox.ac.uk/corner/Installing_OBC_release_3.3) with the goal to install the pre-compiled sources in Debian package _obc_3.3.0_amd64.deb_ on my Ubuntu 24 LTS system (with 64 bits).

But two pre-requisites made problems for a correct and complete installation, and that have been these packages:

- libgtksourceview
- libffi7

I fixed them like this:

```
$ sudo apt --fix-broken install
...
$ sudo apt install libgtksourceview-3.0-1  # this installation should work now
...
$ wget http://es.archive.ubuntu.com/ubuntu/pool/main/libf/libffi/libffi7_3.3-4_amd64.deb  # downloading libffi7
...
$ sudo dpkg -i libffi7_3.3-4_amd64.deb  # installing libffi7
...
$
```

Now the Linux system should be in a condition to correctly and competely install the Oxford Oberon-2 compiler into these, usual directories:

- _/usr/bin/obc_
- _/usr/lib/obc_
- _/usr/share/man/man1/obc.1.gz_

..like this:

```
$ sudo dpkg -i obc_3.3.0_amd64.deb
(Reading database ... 393352 files and directories currently installed.)
Preparing to unpack obc_3.3.0_amd64.deb ...
Unpacking obc (3.3.0) over (3.3.0) ...
Setting up obc (3.3.0) ...
Processing triggers for man-db (2.12.0-4build2) ...
$
```

This looks good now, and so I do a version check:

```
$ obc -v
Oxford Oberon-2 compiler driver version 3.3.0 [build hg-64b7090cfb52]
Oxford Oberon-2 compiler version 3.3.0 [build hg-64b7090cfb52]
Oxford Oberon-2 linker version 3.3.0 [build hg-64b7090cfb52]
Oxford Oberon-2 runtime system version 3.3.0 [build hg-64b7090cfb52] (JIT)
Oxford Oberon-2 library version 3.3.0 [build hg-64b7090cfb52]
$
```

<br/>

### A first OBC test: mand04.obn

I took source code file _mand04.mod_ unchanged from here: https://fruttenboel.nl/obc/index.html, though changed its file extension to [mand04.obn](./mand04.obn), like all my Oberon sources to distinguish them from my [Modula-2 sources](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-2), and compiled it with the Oxford Oberon-2 compiler in version 3.3.0:

```
$ obc -o mand04 mand04.obn
$ ./mand04 -0.6735 -0.3625 200 300000000
```

Voilà!

![plot](./mand04%20-0.6735%20-0.3625%20200%20300000000.png)

<br/>

### OBC compiled microbenchmark program

Oberon source code can be also compiled with activated switch _-b_ to disable runtime checks: _$ obc -b -o RandomStreamsForPerfStats_b RandomStreamsForPerfStats.obn_

However, in case of the ["speed part"](./RandomStreamsForPerfStats.obn) of the microbenchmark program it doesn't provide for any statistical relevant difference in terms of execution speed,
which is about 27 milliseconds.

Switching off JIT compilation, and let only the interpretation run, with compilation command: _$ obc -j0 -o RandomStreamsForPerfStats_j0 RandomStreamsForPerfStats.obn_ will make 
an "executable" which runs for about 97 milliseconds.

<br/>

#### OBNC

I also shortly tested the OBNC compiler (https://miasap.se/obnc/), only to find out that OBC's library files (copy and rename _*.m_ in local OBNC project directory to _*.mod_), as used in source code files _mand04.obn_ and _RandomStreamsForPerfStats.obn_, cannot be compiled due to many "errors" as seen by OBNC. So, I stopped further experiments with it.

<br/>

#### Oberon V4

Oberon V4 as last maintained at the Johannes Kepler University in Linz/Austria is a 32-bit system, which possibly can be installed from these two sources:

- old (from 2004): https://sourceforge.net/projects/oberon/files/Oberon%20V4%20for%20GNU_Linux/1.7.02/ (+)
- new (from 2026): https://github.com/btreut/Oberon4Linux-Bootstrap (*): I merged these files into the root directory of unzipped file _oberon-1.7.02.tar.gz_ from (+), and have overwritten any older files

Read file _Changes.md_ from (*) for a detailed installation process. However, I didn't try to install it for its very old age.

<br/>

## Oberon+

> [!WARNING]
> The History of Oberon also shows that too much minimalism in the design of a programming language is probably more detrimental to its success than overly complexity (like in C++ or Rust).

Otherwise, what else should be the motivation to make and maintain another derivative of Oberon, here:

[The Oberon+ Programming Language](https://oberon-lang.github.io/)

https://github.com/rochus-keller/Oberon

(bytecode for [CLI/ECMA-335](https://ecma-international.org/publications-and-standards/standards/ecma-335/) is actually bytecode for Microsofts's .NET runtime environment)

> From these considerations a new language emerged, which I call Oberon+ (i.e. “Oberon with extensions”, abbreviated OBX); it is based on Oberon-07, Oberon-2 and Oberon 90, with all the elements of these languages, plus the - from my point of view - most essential missing features and a lot of simplifications and increased flexibility. Oberon+ is - so to say - the Oberon I personally would have hoped for; from my point of view it represents modern simplicity in programming.

from: https://oberon-lang.github.io/2021/07/15/motivation-for-a-new-oberon-version.html

Also see from here: [EDM2/Oberon-2](https://www.edm2.com/index.php/Oberon-2) (~)

> Oberon-2 is an extension of Oberon developed in 1992 by Hanspeter Mössenböck that adds stronger object-orientation and brings back a couple of features from Modula-2.

tbd

<br/>

### Oberon-2 versus Modula-2

from [Comparison of Oberon-2 with Modula-2](https://www.modulaware.com/zel/oberon/compo2m2.htm):

Oberon-2 has no:

- subranges, which are no longer needed for array index range declarations, because array bounds generally start at 0
- enumerations
- unsigned type (CARDINAL)
- nested modules, which did not add any functionality
- variant records, which are replaced by the safer concept of extensible records
- WITH-statement of the Modula-2 language, which had many disadvantages in respect to program readability and thus severed maintainability
- qualified import, i.e. imported identifiers must be qualified with the module name or its alias
- separate definition- and implementation-module 

<br/>

tbd

<br/>

##_end
