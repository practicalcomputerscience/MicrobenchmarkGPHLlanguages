# Mocka Modula-2 compiler for 32-bit Linux and X Window graphics

m2 = Modula-2

GMD = the original, German host of the development of the Mocka compiler. This organization started as the Gesellschaft für Mathematik und Datenverarbeitung mbH in 1968 ("GMD"); was renamed into GMD-Forschungszentrum Informationstechnik GmbH in 1995; and finally merged into the Fraunhofer Society in 2001: https://de-academic.com/dic.nsf/dewiki/488406

X Window System: https://www.x.org/

Mocka is correctly abbreviated MocKa (or MOCKA) and means: **MOdula-2 Compiler KArlsruhe** from the then: Institut für Programm- und Datenstrukturen
at the University of Karlsruhe, Adenauerring 20a, 76131 Karlsruhe (in Germany), a structure which doesn't exist anymore (nowadays, the related top organization is the Karlsruhe Institute of Technology, KIT).

---

Table of contents:

- [Motivation](#motivation)
- [Installation of a 32-bit Linux as a virtual machine](#installation-of-a-32-bit-linux-as-a-virtual-machine)
- [Setting up the Mocka Modula-2 compiler in version 0608m](#setting-up-the-mocka-modula-2-compiler-in-version-0608m)
- [Building the X Window resources](#building-the-x-window-resources)
- [Building the Mandelbrot set application mand01.mod](#building-the-mandelbrot-set-application-mand01mod)

<br/>

---

## Motivation

I wanted to compile Jan Verhoeven's Modula-2 source code file [mand01.mod](https://fruttenboel.nl/mocka/data/mand01.mod) for the Mocka compiler on my own system,
so I could make the _mand01_ executable by myself, a program which can make nice Mandelbrot set diagrams in a X11 window as seen on this page: https://fruttenboel.nl/mocka/mandel.html

<br/>

My installation instructions below are based on these two original sources by author Jan Verhoeven and then a third one:

- (1) [Setup of Mocka 0608m](https://fruttenboel.nl/mocka/setup.html) 
- (2) [Mocka: x11 Foreign module](https://fruttenboel.nl/mocka/simplex11.html)
- (3) https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master/GMD_MocKa_Compiler

<br/>

## Installation of a 32-bit Linux as a virtual machine

Since the Mocka compiler is a 32-bit program, I installed a dedicated 32-bit Linux system (4 GB memory; 4 cpu's; **no UEFI**; standard virtualization features on) as a guest operating system (OS) on a ("normal") 64-bit Ubuntu 24 LTS system as its host.

I'm using Oracle's VirtualBox for Linux (version 7.2.12 r174389) as my virtual machine hoster: https://www.virtualbox.org/wiki/Linux_Downloads

I have taken one of the last 32-bit Ubuntu distributions with iso file _xubuntu-18.04.5-desktop-i386.iso_ from here:

- https://cdimage.ubuntu.com/xubuntu/releases/18.04.5/release/,
- or alternatively from here: https://ftp.bit.nl/ubuntu-releases/cdimages/xubuntu/18.04/release/

<br/>

After the installation of the guest OS, for convenience (shared clipboard service etc) I additionally mounted and installed the Guest Additions (GA) in the Xubuntu 18.04.5 LTS (32-bit) guest OS: [Guest Additions](https://www.virtualbox.org/manual/topics/guestadditions.html), something which I highly recommend to do. Here's a rough description:

- on the top menu of the virtual machine, go to "Devices", then press on menu item "Insert Guest Additions CD image..."
- at its just created desktop symbol, run at its right mouse button menu the "Mount Volume" command
- open this volume (again, a right mouse button menu item) and do "Open Terminal Here" in the opened directory, again a right mouse button menu item
- now you can list the directory content with the _$ ls -1_ command for example:

```
$ ls -1
AUTORUN.INF
autorun.sh
cert
NT3x
OS2
runasroot.sh
TRANS.TBL
VBoxDarwinAdditions.pkg
VBoxDarwinAdditionsUninstall.tool
VBoxLinuxAdditions-arm64.run
VBoxLinuxAdditions.run
VBoxSolarisAdditions.pkg
VBoxWindowsAdditions-amd64.exe
VBoxWindowsAdditions-arm64.exe
VBoxWindowsAdditions.exe
VBoxWindowsAdditions-x86.exe
windows11-bypass.reg
$
```

You can only copy and paste such content of the virtual machine (with simple means) **after** you have installed the Guest Additions :wink:

In the opened terminal shell, run this script as root user: _$ sudo ./VBoxLinuxAdditions.run_

For a check, I rebooted this virtual machine with the power button at the **right hand side, top corner menu**:

> [!CAUTION]
> If possible, always shut down a virtual machine carefully. Don't shut down your host operating system while a virtual machine is still active. This may corrupt it beyond repair!

<br/>

## Setting up the Mocka Modula-2 compiler in version 0608m

> [!IMPORTANT]
> Make sure that source files haves UNIX-like line endings with only a line feed (LF) control character (and not Windows-like [CR]+[LF] endings).

<br/>

I started with downloading sources in tar archive _mocka.tgz_ ("original Mocka") from here: https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master/GMD_MocKa_Compiler/TAR_Archives

I cannot say for what the other and bigger sources _m2.tgz_ are for.

I have not tested any 64-bit installations: "CHANGES: Adjusted to compile on a 64bit Ubuntu based Linux system.." as seen in (3). Also see "I will use Mocka as a compiler only on 32 bit machines." at the bottom of page (1).

<br/>

As recommended ("PLEASE USE 0608m version of the Mocka Modula-2 Compiler.") in (3), I used tarball file _mocka.tgz_ as originally created by Dr Maurer of the FU Berlin. According to source (1), the 0608m Mocka compiler has been "partly rewritten by Dr Maurer of the FU Berlin. It differs from standard Mocka as follows:"

- the md and mi file extensions from Mocka are back to def and mod
- all files produced by mocka are placed in a subdirectory called './m2bin'.
- executables are stripped by default
- executables are stored in the base directory and are symlinks in ./m2bin
- it has been compiled with a recent version of the libraries
- it comes with full instructions and a set of scripts for installing
- it is ready to produce GUI programs with X-windows!

<br/>

The following instructions are more or less following chapter "Step 2: install mocka 0608m original" and beyond at page (1).

First, I logged into a Bash shell as root, that is the Linux superuser. The prompt character in Xubuntu changed to '#':

```
$ sudo -i
...
# cp mocka.tgz /usr/local  # copy the original sources to their installation root directory
# cd /usr/local  # change into the installation root directory
# tar xfz mocka.tgz  # unpack this tarball file
#
```

Then I expanded my _~/.bashrc_ configuration file with these two lines:

```
export MOCKA=/usr/local/mocka
export MOCKALINK=-lX11
```

> [!IMPORTANT]
> Now come the real hacks.

This was only for the _~/.bashrc_ configuration file of **my (normal) user**, not the root user who is supposed to do the installation work!

So, also add those two lines to the _/root/.bashrc_ configuration file of the root user. Best is now to restart the Bash shell and login there again as root user. 

Still, something is missing in my new 32-bit Xubuntu system: a C compiler!

Consequently, install one (as root user):

```
# apt update
...
# apt-get upgrade
...
# apt install gcc
...
# gcc --version
gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0
Copyright (C) 2017 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#
```

<br/>

By the way: I'm using the **nano editor** to do changes of config file _$ .bashrc_ and other files inside the virtual machine: https://www.nano-editor.org/

The nano editor can partly be operated with items of the right mouse button menu. Do a [CTRL]-[O] for writing a new output of the changed file and [CTRL]-[X] for leaving the nano editor.

<br/>

Now install the Mocka compiler documentation (as root user again):

```
# cd /usr/local/mocka/man1
# gzip mc.1
# mkdir -p /usr/local/man/man1  # create this directory if still missing!
# cp mc.1.gz /usr/local/man/man1/mocka.1.gz
# man mc  # do a documentation check
MC(1)                                       General Commands Manual                                       MC(1)

NAME
       mc - MOCKA, Modula-2 Compiler Karlsruhe running on Linux

SYNOPSIS
       mc [-s module | -c module | -p module]

       [-d  dir] ...  [-index] [-noindex] [-range] [-norange] [-static] [-nostatic] [-elf] [-noelf] [-g] [-nog]
       [-gc] [-nogc] [-ge] [-noge] [-S] [-noS]

       [-D directory] [-link script] [-edit script] [-list script] [-asm script] [-syslib dir]

DESCRIPTION
       mc is used to compile and link programs written in Modula-2.
...
# 
```

By the way: the Mocka compiler infrastructure is basically a **collection of scripts for the standard system shell**. The standard system shell in case of Ubuntu, or here Xubuntu, is usually the Bash shell by default, and can be looked up like this: 

```
# echo $SHELL
/bin/bash
#
```

Then I built compiler resources with commands:

```
# cd /usr/local/mocka/lib
# chmod 755 machen  # make this shell script executable if needed
# ./machen
LREAL.c:28:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
 BEGIN_LREAL(){}
 ^~~~~~~~~~~
libc.c:4:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
 BEGIN_libc(){}
 ^~~~~~~~~~
miscc.c:17:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
 BEGIN_miscc(){}
 ^~~~~~~~~~~
Mocka 0608m
>> .. Compiling Implementation of BasicIO I/0008 II/0008
.. Compiling Implementation of Storage I/0005 II/0005
.. Compiling Implementation of ByteIO I/0018 II/0018
.. Compiling Implementation of RealConv I/0013 II/0013
.. Compiling Implementation of TextIO I/0027 II/0027
.. Compiling Definition of Signals
.. Compiling Implementation of Signals I/0001 II/0001
.. Compiling Implementation of MemPools I/0004 II/0004
.. Compiling Implementation of Clock I/0004 II/0004
.. Compiling Program Module tst I/0001 II/0001
.. Linking tst
>> 
./machen: line 26: mockabin/tst: No such file or directory
# chmod 755 makemockabin  # make this shell script executable if needed
# ./makemockabin
./makemockabin: 11: ./makemockabin: m2: not found
./makemockabin: 11: ./makemockabin: m2: not found
./makemockabin: 11: ./makemockabin: m2: not found
./makemockabin: 11: ./makemockabin: m2: not found
./makemockabin: 11: ./makemockabin: m2: not found
./makemockabin: 14: ./makemockabin: m2: not found
LREAL.c:28:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
 BEGIN_LREAL(){}
 ^~~~~~~~~~~
libc.c:4:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
 BEGIN_libc(){}
 ^~~~~~~~~~
miscc.c:17:1: warning: return type defaults to ‘int’ [-Wimplicit-int]
 BEGIN_miscc(){}
 ^~~~~~~~~~~
./makemockabin: 22: ./makemockabin: m2: not found
./makemockabin: 24: ./makemockabin: tst: not found
#
```

At first, this doesn't look too good (on a brand new Linux system), but it created things like the _tst_ executable:

```
# ./tst
Hello, world!
#
```

Finally, I added these two symbolic links to compiler script _/usr/local/mocka/sys/m2_:

```
# ln -s /usr/local/mocka/sys/m2 mocka
# ln -s /usr/local/mocka/sys/m2 m2
```

So, the Mocka compiler can now be called with command _mocka_ or _m2_ for **all** system users and system wide.

This is the symbol-linked Mocka script for compiling and linking:

```
# cat /usr/local/mocka/sys/m2
#!/bin/sh
#
# Christian Maurer   v. 14. August 2006

mkdir -p m2bin

$MOCKA/sys/Mc \
  -link $MOCKA/sys/link \
  -edit $MOCKA/sys/edit \
  -list $MOCKA/sys/list \
  -asm  $MOCKA/sys/asm \
  -syslib $MOCKA/lib/m2bin \
  -index -range -S \
  -d ./m2bin -D ./m2bin $MOCKAM2 \
  -g -elf $*
# 
```

<br/>

By the way: I didn't change Mocka's default editor vi (PDF): [Vi Quick Reference](https://ex-vi.sourceforge.net/viin/quickref.pdf)

Background: the Mocka compiler is also an interactive compiler with its own prompt system (which is not very comfortable).
In case of a **compilation error** it automatically opens the related source in the vi editor:

> [!TIP]
> Scroll down, or go down with vi means, to the first error, which is marked in the vi editor view, and fix it!

In case that everything is OK, the vi editor is not opened, see below for an example.

<br/>

You can now exit the root shell and return to your normal user with command _\# exit_ and change into the working directory with your Mocka resources.

You may install the _rlwrap_ utility for betting editing comfort: _$ sudo apt install rlwrap_

Run a test like this for example:

```
$ rlwrap mocka -info
Options in effect:  index, range, blip, elf, S, g, gc, ge, nostatic
  Current Library       : ./m2bin
  Secondary Libraries   :  ./m2bin /usr/local/mocka/lib/m2bin
  List  Script          : /usr/local/mocka/sys/list
  Edit  Script          : /usr/local/mocka/sys/edit
  Link  Script          : /usr/local/mocka/sys/link
  Asm   Script          : /usr/local/mocka/sys/asm
Mocka 0608m
>> -help
Mocka 0608m
mc [options] module
options: -info show options in affect and scripts
         -options show detailed options
         -s compile definition of module
         -c compile implemenatation of module
         -p link module
>> q
$
```

Command _q_ leaves the compiler REPL.

<br/>

## Building the X Window resources

The following instructions are more or less following chapter "Graphic programming with Mocka" and beyond at page (2).

> [!IMPORTANT]
> Now come the next hacks.

This time by author Jan Verhoeven himself. Do not take the first versions of _x11.def_ and _x11.c_ !

Take both files only in their refined versions at chapter "Select another font". Only these can be compiled and linked correctly without any modifications:

- [x11.def](./x11.def)
- [x11.c](./x11.c) with one addition of mine: _#include <stdlib.h>_

<br/>

On an "empty" Linux system, the client interface to the X Window System must be installed before the real action can start with command _$ gcc -lX11 -c x11.c_:

```
$ sudo apt install libx11-dev
...
$ gcc -lX11 -c x11.c  # compile the C module to an object file
$ mkdir -p ./m2bin  # make this project resources directory if missing
$ cp x11.o ./m2bin  # copy the just created object file to it
$ mocka -s x11  # compile definition of module x11
$ 
```

We just created two new files in subdirectory _./m2bin_: _x11.d_ and _x11.r_, where the .d symbol file is used for inter module type checking and the .r reference file for linking.

<br/>

## Building the Mandelbrot set application mand01.mod

Now we should be able to compile and link the Modula-2 application source code file [mand01.mod](./mand01.mod) as shown at top chapter "Creating the mandelbrot set" of page: https://fruttenboel.nl/mocka/mandel.html

But the compilation and linking command isn't working like this:

```
$ mocka -p mand01
Cannot find reference file for module 'mand01'
$
```

So, we use the compiler interactively:

```
$ mocka
Mocka 0608m
>> p mand01
.. Compiling Program Module mand01 I/0006 II/0006
.. Linking mand01
>> q
$
```

Or, thanks to Google AI, we can also do it like this while staying on the console (if the compilation is OK):

```
$ echo "p mand01" | mocka
Mocka 0608m
>> .. Compiling Program Module mand01 I/0006 II/0006
.. Linking mand01
>>
$
```

All in all, these files have been created in the project resources subdirectory:

```
$ ls -1 m2bin
_M2ROOT.o
_M2ROOT.s
mand01  # this is a link to the just generated ../mand01 executable
mand01.o
mand01.r
mand01.s
x11.d
x11.o
x11.r
$
```

..where _mand01.s_ is the assembler file for the module and _mand01.r_ the reference file for linking.

Now, let's run executable _mand01_ in the project root directory and with the first parameter set as shown here: https://fruttenboel.nl/mocka/mandel.html

```
$ ./mand01 -0.372 -0.65 25000 70000
```

<br/>

![plot](./mand01%20-0.372%20-0.65%2025000%2070000.png)

<br/>

> [!TIP]
> In case of compilation or linking problems, including version problems, you may just delete the whole subdirectory _./m2bin_ and start all over. 

<br/>

## Microbenchmark program: speed part

I got curious how fast executables the Mocka Modula-2 compiler can make, at least with my microbenchmark program: [RandomStreams.mod](./RandomStreams.mod) (this compiler has quite strict module naming rules, while identifier naming isn't so strict as originally conceived for the Modula-2 language)

On the same virtual 32-bit platform, the numbers are very clear:

language | compiler | mean execution time measured with _$ multitime -n 10 <executable>_
--- | --- | ---
Modula-2 | Mocka 0608m | 25 milliseconds
C | gcc version 7.5.0 | 5 milliseconds

In other words: 5 times slower, while on my "official" testing system the gm2 13.3.0 compiled Modula-2 executable beats its C counterpart compiled with clang 22.1.8 (and idiomatic usage of the _sprintf_ function) by about -30%: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

<br/>

##_end
