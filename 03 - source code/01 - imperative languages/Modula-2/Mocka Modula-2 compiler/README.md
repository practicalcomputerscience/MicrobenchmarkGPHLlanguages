2026-07-15: work in progress

tbd: toc

<br/>

# Mocka Modula-2 compiler for 32-bit Linux and X Window graphics

m2 = Modula-2

GMD = tbd

X Window System: https://www.x.org/

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

..as seen on this page: https://fruttenboel.nl/mocka/mandel.html

<br/>

For the installation instructions described below, these two sources of information have been the most important ones to me:

- (1) GMD_MocKa_Compiler: https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master/GMD_MocKa_Compiler, based on this original source: [Setup of Mocka 0608m](https://fruttenboel.nl/mocka/setup.html) by Jan Verhoeven (1b)
- (2) https://fruttenboel.nl/mocka/simplex11.html

<br/>

## Installation of a 32-bit Linux as a virtual machine

Since the Mocka compiler is a 32-bit program, I installed a dedicated 32-bit Linux system (4 GB memory; 4 cpu's; no UEFI; standard virtualization features on) as a guest operating system (OS) on a ("normal") 64-bit Ubuntu 24 LTS system as its host.

I'm using Oracle's VirtualBox for Linux (version 7.2.12 r174389) as my virtual machine hoster: https://www.virtualbox.org/wiki/Linux_Downloads

<br/>

I have taken one of the last 32-bit Ubuntu distributions with iso file _xubuntu-18.04.5-desktop-i386.iso_ from here:

- https://cdimage.ubuntu.com/xubuntu/releases/18.04.5/release/,
- or alternatively from here: https://ftp.bit.nl/ubuntu-releases/cdimages/xubuntu/18.04/release/

<br/>

After the installation of the guest OS, for convenience (shared clipboard service etc) I additionally mounted and installed the Guest Additions (GA) in the **Xubuntu 18.04.5 LTS (32-bit)** guest OS: [Guest Additions](https://www.virtualbox.org/manual/topics/guestadditions.html), something which I highly recommend to do. Here's a rough description:

- on the top menu of the virtual machine, go to "Devices", then press on menu item "Insert Guest Additions Cd image..."
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

Irony: you can only copy and paste such content of the virtual machine (with simple means) **after** you have installed the Guest Additions :wink:

Tn the opened terminal shell, run this script as root user: _$ sudo ./VBoxLinuxAdditions.run_

For a check, I rebooted this virtual machine with the power button behind the **very right hand side, very top menu**. This reboot should take only a small amount of time relatively, if it's a healthy system.

> [!CAUTION]
> If possible, always shut down a virtual machine carefully. Don't shut down your host operating system while a virtual machine is still active. This may corrupt it beyond repair!

<br/>

## Setting up the Mocka Modula-2 compiler in version 0608m

I started with downloading sources in tar archive _mocka.tgz_ ("original Mocka") from here: https://github.com/GunterMueller/Mocka_Modula-2_Compilers_and_Murus/tree/master/GMD_MocKa_Compiler/TAR_Archives

I cannot say for what the other and bigger sources _m2.tgz_ are for.

I have not tested any 64-bit installations: "CHANGES: Adjusted to compile on a 64bit Ubuntu based Linux system.." as seen in (1). Also see "I will use Mocka as a compiler only on 32 bit machines." at the bottom of page (1b).

<br/>

As recommended ("PLEASE USE 0608m version of the Mocka Modula-2 Compiler.") in (1), I used tarball file _mocka.tgz_ as originally created by Dr Maurer of the FU Berlin. According to source (1b) the 0608m Mocka compiler has been "partly rewritten by Dr Maurer of the FU Berlin. It differs from standard Mocka as follows:"

- the md and mi file extensions from Mocka are back to def and mod
- all files produced by mocka are placed in a subdirectory called './m2bin'.
- executables are stripped by default
- executables are stored in the base directory and are symlinks in ./m2bin
- it has been compiled with a recent version of the libraries
- it comes with full instructions and a set of scripts for installing
- it is ready to produce GUI programs with X-windows!

<br/>

The following instructions are more or less following chapter "Step 2: install mocka 0608m original" and beyond at page (1b).

First, I logged into a Bash shell as root, that is the Linux superuser:

```
$ sudo -i
...
$ cp mocka.tgz /usr/local  # copy the original sources to their installation root directory
$ tar xfz mocka.tgz  # unpack this tarball file
$
```

Then I expanded my _~/.bahrc_ configuration file with these two lines:

```
export MOCKA=/usr/local/mocka
export MOCKALINK=-lX11
```

By the way: I'm using the **nano editor** to do changes on config file _$ .bashrc_ and other files inside the virtual machine: https://www.nano-editor.org/

Best is now to restart the Bash shell and re-login as a root user. Now install the Mocka compiler documentation:

```
$ cd /usr/local/mocka/man1
$ gzip mc.1
$ cp mc.1.gz /usr/local/man/man1/mocka.1.gz
```

By the way: the Mocka compiler infrastructure is basically a **collection of scripts for the standard system shell**. The standard system shell in case of Ubuntu, or here Xubuntu, is usually the Bash shell by default, and can be looked up like this: 

```
$ echo $SHELL
/bin/bash
$
```

Then I built the compiler with commands:

```
$ cd /usr/local/mocka/lib
$ chmod 755 machen  # make this shell script executable
$ chmod 755 makemockabin  # make this shell script executable
$ ./machen
...
$ ./makemockabin
...
$
```

Finally, I added these two symbolic links to compiler script _/usr/local/mocka/sys/m2_:

```
$ cd /usr/local/bin
$ ln -s /usr/local/mocka/sys/m2 mocka
$ ln -s /usr/local/mocka/sys/m2 m2
```

This is the Mocka compiler script (when using root user: prompt is # now):

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

By the way: I didn't change Mocka's default editor vi (PDF): [Vi Quick Reference](https://ex-vi.sourceforge.net/viin/quickref.pdf)

Background: the Mocka compiler is also an interactive compiler with its own prompt system (which is not very comfortable).

So, in any compilation case tbd


Back in the Mocka working directory with source code file _mand01.mod_ from here: tbd

tbd

<br/>

## Building pre-reqPre-requisite _ for X Setting up the Mocka Modula-2 compiler in version 0608m

```
$ sudo apt update
$ sudo apt install libx11-dev
...
$
```
```
$ mocka -p mand01
Cannot find reference file for module 'mand01'
$ 
```

















<br/>

## Building the mand01 executable (in 32 bit)



<br/>

##_end
