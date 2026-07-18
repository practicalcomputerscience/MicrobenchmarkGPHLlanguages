2026-07-17: work in progress

<br/>

[Professor Niklaus Wirth](http://pascal.hansotten.com/niklaus-wirth/)

<br/>

# Free Pascal

https://www.freepascal.org/

Free Pascal supports:

- Objective-Pascal to access the Mac OS X system framework, which usually happens with Objective-C
- Object Pascal

..for example.

Also see: [What is Free Pascal (FPC)?](https://www.freepascal.org/faq.var#WhatIsFP)

Free Pascal is shipped with **a lot of documentation** inside the _doc-pdf.tar.gz_ file (with 8 PDF files)
inside tarball file _fpc-3.2.2.x86_64-linux.tar_ (as of 2026-07-17) from here: https://www.freepascal.org/down/x86_64/linux-hungary.html,
or online from here: https://www.freepascal.org/docs.html

The starting document for me was the **User’s Guide** for Free Pascal (PDF): https://downloads.freepascal.org/fpc/docs-pdf/user.pdf

Self-hosted Free Pascal supports a lot of platforms, including for example iOS.

The Free Pascal compiler is supporting these modes:

compiler switch | description
--- | ---
-Mfpc            | Free Pascal dialect (default)
-Mobjfpc         | **FPC mode with Object Pascal support**: I'm using this mode in my Free Pascal implementation to be near my (modern) [Modula-3 implementation](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Modula-3/random_streams_for_perf_stats_Main.m3)
-Mdelphi         | Delphi 7 compatibility mode
-Mtp             | TP/BP 7.0 compatibility mode
-Mmacpas         | Macintosh Pascal dialects compatibility mode
-Miso            | ISO 7185 mode
-Mextendedpascal | ISO 10206 mode for Extended Pascal: Free Pascal still doesn't fully support it
-Mdelphiunicode  | Delphi 2009 and later compatibility mode

<br/>

BP = Borland Pascal

TP = Turbo Pascal: https://en.wikipedia.org/wiki/Turbo_Pascal

<br/>

Free Pascal comes with a text version of an Integrated Development Environment (IDE), a graphical version exits with [Lazarus](https://www.lazarus-ide.org/) for example.

<br/>

## Installation tips

I just ran the _./install.sh_ script of the (unzipped) tarball file _fpc-3.2.2.x86_64-linux.tar_ with my normal Linux user.
It will ask the user a couple of questions for the desired configuration. I installed Free Pascal into my home directory.

Then I added line _export PATH="$HOME/fpc-3.2.2/bin:$PATH"_ to my _~/.bashrc_ configuration file, which I then activated with: _$ source ~/.bashrc_

A first version test after installation:

```
$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$
```

Free Pascal can also be installed like this in (Ubuntu) Linux: _$ sudo apt install fp-compiler-3.2.2_, or in its text based IDE version: _$ sudo apt install fp-ide-3.2.2_

Following the User’s Guide, I compiled and ran a _Hello world_ program:


```
$ cp $HOME/fpc-3.2.2/share/doc/fpc-3.2.2/examples/text/hello.pp .
$ cat hello.pp
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by the Free Pascal Development Team
...
 **********************************************************************}

program hello;

  begin
     writeln('Hello world');
  end.

$ fpc hello
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling hello.pp
Linking hello
21 lines compiled, 0.0 sec
$ ./hello
Hello world
$
```

<br/>

This compiler command is interesting: it shows "a list of supported FPU (Floating-Point Unit) instruction sets", which are actually SIMD (Single Instruction, Multiple Data) instruction set extensions to the x86 instruction set architecture for microprocessors from Intel and AMD: https://en.wikipedia.org/wiki/List_of_x86_SIMD_instructions

```
$ fpc -if
SSE64
SSE3
SSSE3
SSE41
SSE42
AVX
AVX2
$
```



<br/>

#### Extended Pascal according to ISO 10206

Free Pascal's support of Extended Pascal according to [ISO/IEC 10206:1991](https://www.iso.org/standard/18237.html) is still minimal: https://gitlab.com/freepascal.org/fpc/source/-/work_items/32549

Many years ago there was still GNU Pascal around, which claimed to support "most of ISO 10206 Extended Pascal": https://www.gnu-pascal.de/gpc/h-about.html#lang

However, it's nowadays a tinkering job to get it running in a modern 64-bit Linux system: https://github.com/hebisch/gpc, so, I don't do it.

The only difference between my [ISO 7185](tbd) and [ISO 10206](tbd) implementations is that procedures _Integer_to_bin_string_ and _Integer_to_hex_string_ became functions.

However, in ISO 10206 mode (Extended Pascal), Free Pascal has the required _TimeStamp_ type and _GetTimeStamp_ procedure implemented, which serves as a simple, random seed:

```
t            : TimeStamp;
...
  GetTimeStamp (t);
  x[0] := (t.Second mod (m - 2)) + 1;
```

<br/>


<br/>




<br/>


##_end
