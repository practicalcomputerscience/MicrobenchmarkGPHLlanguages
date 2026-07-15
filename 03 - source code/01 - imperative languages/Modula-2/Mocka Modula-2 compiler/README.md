2026-07-15: work in progress

tbd: toc

<br/>

# Mocka Modula-2 compiler for 32-bit Linux

m2 = Modula-2

Mocka is correctly abbreviated MocKa (or MOCKA) and means: **MOdula-2 Compiler KArlsruhe** from the then: Institut für Programm- und Datenstrukturen
at the University of Karlsruhe, Adenauerring 20a, 76131 Karlsruhe (in Germany), a structure which doesn't exist anymore (nowadays, it's the Karlsruhe Institute of Technology, KIT).

---

Table of contents:

tbd

<br/>

---

### Motivation

I wanted to compile Jan Verhoeven's Modula-2 source code file [mand01.mod](https://fruttenboel.nl/mocka/data/mand01.mod) for the Mocka compiler on my own system,
so I could make the _mand01_ executable by myself, a program which can make nice drawings in a X11 window with execution command:

```
$ ./mand01 -0.372 -0.65 25000 70000
```

As seen on this page: https://fruttenboel.nl/mocka/

<br/>

For the installations as described below, these two sources of information have been the most important ones:

- <br/>
- https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master

<br/>

## Installation of a 32-bit Linux as a virtual machine

Since the Mocka compiler is a 32-bit program, I installed a dedicated 32-bit Linux system (4 GB memory; 4 cpu's; no UEFI; standard virtualization features on) as a guest operating system (OS) on a ("normal") 64-bit Ubuntu 24 LTS system as its host.

I'm using Oracle's VirtualBox for Linux (version 7.2.12 r174389) as my virtual machine hoster: https://www.virtualbox.org/wiki/Linux_Downloads

<br/>

I have taken one of the last 32-bit Ubuntu distributions with iso file _xubuntu-18.04.5-desktop-i386.iso_ from here:

- https://cdimage.ubuntu.com/xubuntu/releases/18.04.5/release/,
- or alternatively here: https://ftp.bit.nl/ubuntu-releases/cdimages/xubuntu/18.04/release/

<br/>

After the installation of the guest OS, for convenience (shared clipboard etc), and as a last step, I also mounted and installed the Guest Additions (GA) on the Xubuntu 18.04.5 LTS (32-bit) guest OS: [Guest Additions](https://www.virtualbox.org/manual/topics/guestadditions.html), something which I highly recommend to do.

<br/>

## Setting up the Mocka Modula-2 compiler

I started with downloading sources in tar archive _mocka.tgz_ from here: https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master/GMD_MocKa_Compiler/TAR_Archives

I cannot say for what the other and bigger sources _m2.tgz_ are for.



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
