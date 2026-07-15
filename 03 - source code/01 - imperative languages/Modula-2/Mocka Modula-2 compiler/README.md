2026-07-15: work in progress

<br/>

# Mocka Modula-2 compiler for 32-bit Linux

Motivation: I wanted to compile Jan Verhoeven's Modula-2 source code file [mand01.mod](https://fruttenboel.nl/mocka/data/mand01.mod) for the Mocka compiler
on my own system, so I could make the _mand01_ executable by myself, a program which draws this nice X11 window:

tbd

..with command:

```
$ ./mand01 -0.372 -0.65 25000 70000
```

tbd 

<br/>

tbd

The two most important sources of information are these:

- https://fruttenboel.nl/mocka/simplex11.html
- https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master

<br/>

By the way: Mocka is correctly abbreviated MocKa (or MOCKA) and means: **MOdula-2 Compiler KArlsruhe** from the then: Institut für Programm- und Datenstrukturen
at the University of Karlsruhe, Adenauerring 20a, 76131 Karlsruhe (in Germany), a structure which doesn't exist anymore (nowadays, it's the Karlsruhe Institute of Technology, KIT).

<br/>

I didn't succeed with getting the 32-bit Mocka Modula-2 compiler running in a 64-bit Linux system: tbd

It's apparently anyway so easy as explained here: tbd

However, I managed with the two primary sources listed above.

tbd

<br/>

## 32-bit Linux as a virtual machine

tbd

_xubuntu-18.04.5-desktop-i386.iso_

tbd

GA guest installations: tbd

<br/>

## Building the mand01 executable (in 32 bit)

```
$ sudo apt update
$ sudo apt install libx11-dev
...
$
```

<br/>

##_end
