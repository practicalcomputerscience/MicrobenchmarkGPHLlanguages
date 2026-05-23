# GNU Smalltalk

https://www.gnu.org/software/smalltalk/

https://github.com/gnu-smalltalk/smalltalk/tree/master (*)

2020: [The Rise and Fall of Commercial Smalltalk](https://wirfs-brock.com/allen/posts/914) by Allen Wirfs-Brock (**)

PDF document: [SMALLTALK-72 INSTRUCTION MANUAL](https://smalltalkzoo.computerhistory.org/papers/Smalltalk72_Manual.pdf), The Learning Research Group, Xerox Palo Alto Hesearch Center, by ADELE GOLDBERG AND ALAN KAY, EDITORS, 1976

Standardization of Smalltalk with: ANSI (InterNational Committee for Information Technology Standards) 319-1998 (R2007): https://webstore.ansi.org/standards/incits/ansiincits3191998r2007

---

Table of contents:

- [Idea of Smalltalk: a complete software application platform and development environment](#idea-of-smalltalk-a-complete-software-application-platform-and-development-environment)
- [Lists of Smalltalk implementations](#lists-of-smalltalk-implementations)
  - [Squeak](#squeak)
  - [Pharo](#pharo)
  - [Cuis](#cuis)
- [GNU Smalltalk for a text‑based workflow](#gnu-smalltalk-for-a-textbased-workflow)
- [Installation and compilation tips of GNU Smalltalk](#installation-and-compilation-tips-of-gnu-smalltalk)

<br/>

---

## Idea of Smalltalk: a complete software application platform and development environment

"a complete software application platform and development environment" is from (**).

<br/>

Smalltalk was the U.S.'s big start into object-oriented programming (OOP) and also the usage of **graphical user interfaces** (GUI's) for coding.

But over 50 years into this fully object-oriented, dynamically typed and reflective programming language, there's one concerning question when starting to code:

> [!IMPORTANT]
> What Smalltalk implementation to use?

[Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme) has dialects, [Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming#logic-programming) has systems, and Smalltalk has implementations, where nowadays only a few have a language name with "Smalltalk" being a part of it.

Even porting "Hello, world!" 1:1 from one Smalltalk implementation to the other has a high chance of failure.

In the 70ies alone, four versions of Smalltalk were developed: Smalltalk-71, Smalltalk-72, Smalltalk-76 and Smalltalk-80 (https://drcuis.github.io/TheCuisBook/Preface-_0028Solutions_0029.html). 

<br/>

Same like [Ruby](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Ruby#mjit-in-2018), Smalltalk apparently also belongs to a group of programming languages, which have seen some generations of virtual machine development: [Virtual Machines in Squeak](https://squeak.org/development/#virtual-machines), even including ahead-of-time compilation with the GraalVM: [TruffleSqueak](https://github.com/hpi-swa/trufflesqueak/).

<br/>

### Lists of Smalltalk implementations

Here's a list, but apparently not maintained anymore: http://www.smalltalk.org/#SmalltalkSystems

Another (outdated) list at "Implementations" on this web page: https://mvolkmann.github.io/programming/Smalltalk.html

After a while of reading, two major open source implementations emerged:

#### Squeak

[Squeak](https://squeak.org/) is a Smalltalk environment created by the original Smalltalk-80 team, initially published in 1996.

Latest release (as of May 2026): https://files.squeak.org/trunk/Squeak6.1alpha-23704-64bit/

Running script _$ squeak.sh_ starts the "Smalltalk programming system", where configurations can be done initially.

Front-end development can be done with [SqueakJS](https://squeak.js.org/) to run unmodified Smalltalk images (and Pharo and Cuis, too).
    
#### Pharo

[Pharo](https://pharo.org/) is the leading French implementation of Smalltalk and wants to provide "The immersive programming experience". It started as a fork from Squeak in 2008, and is the "industrial version" of Squeak.
  
Front-end development can be done with [PharoJS](https://pharojs.org/).

See also ["Hello, world!" in Pharo works differently]() at the bottom of this page.

#### Cuis

[Cuis](https://cuis.st/) aims for simplicity:

> Cuis is a free Smalltalk-80 environment with a specific set of goals: being simple and powerful.

The Cuis IDE (Integrated Development Environment) for printing "Hello, world!" on the "console", or in the "terminal", looks like this in Ubuntu 24:

![plot](./GUI%20Cuis%20version%207.6a.png)

<br/>

> [!NOTE]
> So, a typical Smalltalk environment is an image at its core, that is an object space with libraries, where everything is organized as objects rather than files and folders.

<br/>

### GNU Smalltalk for a text‑based workflow

https://www.gnu.org/software/smalltalk/

> GNU Smalltalk is an implementation that closely follows the Smalltalk-80 language as described in the book `Smalltalk-80: the Language and its Implementation' by Adele Goldberg and David Robson.

From (*); this book is also called the "Blue book": https://wiki.squeak.org/squeak/64

..and further:

> Unlike other Smalltalks (including Smalltalk-80), GNU Smalltalk emphasizes Smalltalk's rapid prototyping features rather than the graphical and easy-to-use nature of the programming environment...

So, in contrast to the graphical systems of Squeak, Pharo and Cuis, which provide an image‑based workflow, GNU Smalltalk is for a (traditional) text‑based workflow. However:

> [!IMPORTANT]
> GNU Smalltalk hasn't been updated since more than 10 years: https://ftp.gnu.org/gnu/smalltalk/

However, even without any (substantial) updates in over 10 years, the text‑based workflow of GNU Smalltalk fits well to all the other language versions of the microbenchmark program in this repository.

<br/>

## Installation and compilation tips of GNU Smalltalk

See also the [GNU Smalltalk User’s Guide](https://www.gnu.org/software/smalltalk/manual/gst.html).

I downloaded and extracted file _smalltalk-3.2.91.tar.xz_ from here: https://ftp.gnu.org/gnu/smalltalk/

But first, I had to install this package on my system, because GNU Smalltalk depends on it:

```
$ sudo apt-get install libsigsegv-dev
...
$
```

Then I continued in the extracted subdirectory with the common triple jump of:

```
$ ./configure
...
$ make
...
$ make check
...
ERROR: 133 tests were run,
8 failed (1 expected failure).
2 tests were skipped.
...
$ sudo make install
...
$
```

Some verifications of this installation:

```
$ gst --version
GNU Smalltalk version 3.2.91
Copyright 2009 Free Software Foundation, Inc.
Written by Steve Byrne (sbb@gnu.org) and Paolo Bonzini (bonzini@gnu.org)
...
$ cat hello_world_gnu.st
'Hello, world!' printNl
$ gst hello_world_gnu.st
'Hello, world!'
$ 
```

<br/>

#### "Hello, world!" in Pharo works differently

..because the "Pharo st command works with st files in chunk format. The chunk format uses ! character as delimiter.", see from here: https://github.com/pharo-project/pharo/issues/19220#issuecomment-3804245257

```
$ cat hello_world_pharo.st
Transcript show: 'Hello, world!!'; cr.
Smalltalk snapshot: false andQuit: true.
$ ./pharo --headless Pharo.image ./random_streams_for_perf_stats_pharo.st 
Hello, World!
$ 
```

A short Pharo installation in the current working directory, so be careful, is given at https://pharo.org/:

```
$ wget -O- https://get.pharo.org/64 | bash
...
$
```

<br/>

##_end
