# Forth

https://gforth.org/

https://forth-standard.org/

aus = address units

<br/>

Programming in postfix notation, also called Reverse Polish Notation, where operators follow their operands, here in the [Gforth](https://gforth.org/) implementation.

> Stack machines offer processor complexity that is much lower than that of CISC (Complex Instruction Set Computers) machines,
> and overall system complexity that is lower than that of either RISC (Reduced Instruction Set Computers) or CISC machines. They do this without requiring complicated compilers or cache control hardware for good performance.

from: "Stack Computers: the new wave", Philip Koopman, 1989: https://users.ece.cmu.edu/~koopman/stack_computers/sec1_1.html

<br/>

> Forth has been in use from 1972 on..

from: https://www.forth.com/starting-forth/0-starting-forth/

<br/>

[A Glossary of Forth Primitives ](https://users.ece.cmu.edu/~koopman/stack_computers/appb.html)

<br/>

## Installation tips

After some experimentation, I noticed that I need a working Gforth implementation to build the latest version of Gforth! So, I started like this:

```
$ sudo apt  install gforth
...
$ gforth --version
gforth 0.7.3
$
```

Do _$ make clean >/dev/null 2>&1 || true_, if you have messed up a build before.

That's good enough to build latest version 0.7.9 from sources in tarball file _gforth.tar.xz_ from here: https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/current/

After unzipping that file, I followed instructions as given in _./gforth/gforth-0.7.9_20260610/INSTALL.md_:

```
$ cd ./gforth/gforth-0.7.9_20260610
$ BUILD_FROM=tarball
$ source ./install-deps.sh
...
The following packages have unmet dependencies:
...
$ 
```

I was still missing some packages, which I installed like this, and further ignored above warnings (for those packages in their newest form anyway):

```
$ sudo apt install libtool libtool-bin swig
...
$
```

Then I noticed that I had to do more installations:

```
$ sudo ./install-swig.sh
...
Installation complete
$
```

Only then I could run the _configure_ command correctly, and start building with _make_:

```
$ ./configure 
...

*** Config summary: everything fine ***
$ make
...
*** Check successful ***
*** no performance problems ***
    in gforth-fast
*** no performance problems ***
    in gforth
*** no performance problems ***
    in libgforth-fast
*** no performance problems ***
    in libgforth
$ sudo make install
...
============= INSTALL SUCCEEDED =============
Bash users: type 'hash -r' to empty the cache
$ hash -r
$ gforth --version
gforth 0.7.9_20260610 amd64
$
```

Voilà! Gforth in its latest version! (which already looks improved at the REPL, see below, compared to version 0.7.3)

<br/>

## From Forth to Factor

I highly recommend to first have a look into the official [Forth Tutorial](https://net2o.de/gforth/Tutorial.html) before doing anything more meaningful than "Hello, world!" in Forth:

```
$ gforth
Gforth 0.7.9_20260610
Authors: Anton Ertl, Bernd Paysan, Jens Wilke et al., for more type `authors'
Copyright © 2025 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `help' for basic help
\  ok
\ user input starts now:  ok
cr ." Hello, world from Gforth!" cr
Hello, world from Gforth!
 ok
bye
$
```

<br/>

However, when I continued with the Tutorial, I got doubts: should I really go on with this rather low-level programming language?

Then I discovered much younger stack-oriented language [Factor](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/06%20-%20stack-oriented%20languages/Factor#factor), and decided to only continue with that language as a representative of stack-oriented programming.

<br/>

##_end
