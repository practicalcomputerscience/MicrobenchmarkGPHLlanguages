2026-05-21: work in progress

see remaining tbd's

<br/>

# GNU Smalltalk

https://www.gnu.org/software/smalltalk/

https://github.com/gnu-smalltalk/smalltalk/tree/master (*)

2020: [The Rise and Fall of Commercial Smalltalk](https://wirfs-brock.com/allen/posts/914) by Allen Wirfs-Brock (**)

PDF document: [SMALLTALK-72 INSTRUCTION MANUAL](https://smalltalkzoo.computerhistory.org/papers/Smalltalk72_Manual.pdf), The Learning Research Group, Xerox Palo Alto Hesearch Center, by ADELE GOLDBERG AND ALAN KAY, EDITORS, 1976

<br/>

## Idea of Smalltalk: a complete software application platform and development environment

"a complete software application platform and development environment" is from (**).

<br/>

Smalltalk was the U.S.'s big start into object-oriented programming (OOP) and also the usage of **graphical user interfaces** (GUI's) for coding.

But over 50 years into this fully object-oriented, dynamically typed and reflective programming language, there's one concerning question when starting to code:

> [!IMPORTANT]
> What Smalltalk implementation to use?

[Scheme](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Scheme#scheme) has dialects, [Prolog](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/04%20-%20logic%20programming#logic-programming) has systems, and Smalltalk has implementations, where nowadays only a few have a language name with "Smalltalk" being a part of it.

Even porting "Hello, World!" 1:1 from one implementation to the other has a high chance of failure.

In the 70ies alone, four versions of Smalltalk were developed: Smalltalk-71, Smalltalk-72, Smalltalk-76 and Smalltalk-80 (https://drcuis.github.io/TheCuisBook/Preface-_0028Solutions_0029.html). 

<br/>

### Lists of Smalltalk implementations

Here's a list, but apparently not maintained anymore: http://www.smalltalk.org/#SmalltalkSystems

Another (outdated) list at "Implementations" at this web page: https://mvolkmann.github.io/programming/Smalltalk.html

After a while of reading, two major open source implementations emerged:

#### Squeak

[Squeak](https://squeak.org/) is a Smalltalk environment created by the original Smalltalk-80 team.

Latest release (as of May 2026): https://files.squeak.org/trunk/Squeak6.1alpha-23704-64bit/

Running script _$ squeak.sh_ starts the "Smalltalk programming system", where configurations can be done initially.
    
#### Pharo

[Pharo](https://pharo.org/) is the leading French implementation of Smalltalk and wants to provide "The immersive programming experience". It started as a fork from Squeak in 2008, and is the "industrial version" of Squeak.
  
Front-end development can be done with [PharoJS](https://pharojs.org/).

#### Cuis

[Cuis](https://cuis.st/) aims for simplicity:

> Cuis is a free Smalltalk-80 environment with a specific set of goals: being simple and powerful.

The Cuis IDE (Integrated Development Environment) for printing "Hello, World!" on the "console", or in the "terminal", looks like this in Ubuntu 24:

![plot](./GUI%20Cuis%20version%207.6.png)

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
> GNU Smalltalk hasn't been updated anymore since more than 10 years: https://ftp.gnu.org/gnu/smalltalk/

However, even without no (substantial) updates in over 10 years, the text‑based workflow of GNU Smalltalk fits well to all the other language versions of the microbenchmark program in this repository.

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

Some small verifications of this installation:

```
$ gst --version
GNU Smalltalk version 3.2.91
Copyright 2009 Free Software Foundation, Inc.
Written by Steve Byrne (sbb@gnu.org) and Paolo Bonzini (bonzini@gnu.org)
...
$
$ cat hello_world_gnu.st
'Hello, world' printNl
$ gst hello_world_gnu.st
'Hello, world'
$ 
```

Have I told you that the "most successful" version of "Hello, world!" for Pharo is this source code file:

```
Transcript show: 'Hello, world'; cr.
Smalltalk snapshot: false andQuit: true.
```

..a file where the '!' character is missing because it would cause an error when executing this file on the [Pharo virtual machine]():

```
$ ~/scripts/Smalltalk/pharo-launcher-linux-3.4.3-x64/pharo-launcher/pharo-vm/pharo --headless Pharo13.0-SNAPSHOT-64bit-374678e2d5.image hello_world_pharo.st
tbd
$
```

<br/>

##_end
