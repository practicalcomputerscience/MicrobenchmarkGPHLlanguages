2026-06-06: work in progress

<br/>

# LLVM compiler infrastructure

LLVM = "Low Level Virtual Machine" in its earlier days

_clang_ = the clang compiler (in Linux and Windows), which is usually installed separately in Linux

https://llvm.org/

https://github.com/llvm/llvm-project

<br/>

Numerous programming languages are using the LLVM compiler infrastructure. However, sometimes a specific version of LLVM is required.

So, I started this dedicated page to collect some of the information about and around LLVM.

---

Table of contents:

- [Easy installation in Ubuntu 24](#easy-installation-in-ubuntu-24)
- [LLVM installation of a specific LLVM version in Ubuntu 24](#llvm-installation-of-a-specific-llvm-version-in-ubuntu-24)
- [Linux installation directories](#linux-installation-directories)
- [Switching between several LLVM installations](#switching-between-several-llvm-installations)
- [Easy installation in Windows 11](#easy-installation-in-windows-11)
- [LLVM's Polly loop optimizer](#llvms-polly-loop-optimizer)

<br/>

---

### Easy installation in Ubuntu 24

In Ubuntu 24, the easiest command to install a modern version of LLVM is this:

```
$ sudo apt install llvm
...
$ llvm-config --version
23.0.0  # as of 2026-06-06
$ 
```

However, this installation only installs the core LLVM framework and not the clang compiler frontend, see below at [Switching between several LLVM installations](#switching-between-several-llvm-installations).

<br/>

### LLVM installation of a specific LLVM version in Ubuntu 24

With the help of Google AI, here I additionally installed LLVM version 21 for example:

```
$ wget https://apt.llvm.org/llvm.sh  # download the official LLVM setup script
...
$ chmod +x llvm.sh  # grant execution permissions to the script
$ sudo ./llvm.sh 21  # run the script specifically targeting version 21; this takes time!
...
$
```

<br/>

### Linux installation directories

In Linux, usually this is the place to find LLVM installations:

```
$ ls /usr/include/ -l
...
drwxr-xr-x  3 root root   4096 Feb 17 16:15 llvm-14
drwxr-xr-x  3 root root   4096 Feb 14 21:35 llvm-19
drwxr-xr-x  3 root root   4096 Jun  6 12:34 llvm-21
drwxr-xr-x  3 root root   4096 Jun  6 12:02 llvm-23
drwxr-xr-x  3 root root   4096 Feb 17 16:15 llvm-c-14
drwxr-xr-x  3 root root   4096 Feb 14 21:35 llvm-c-19
drwxr-xr-x  3 root root   4096 Jun  6 12:34 llvm-c-21
drwxr-xr-x  3 root root   4096 Jun  6 12:02 llvm-c-23
...
$
```

<br/>

### Switching between several LLVM installations

This task takes more effort, because we have to tell Ubuntu about the installed versions, something which creates a selectable menu:

```
$ sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-14 14 \
--slave /usr/bin/clang++ clang++ /usr/bin/clang++-14 \
--slave /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-14
$ sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-19 19 \
--slave /usr/bin/clang++ clang++ /usr/bin/clang++-19 \
--slave /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-19
$ sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-21 21 \
--slave /usr/bin/clang++ clang++ /usr/bin/clang++-21 \
--slave /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-21
$
```

However, command _sudo apt install llvm_, which installed LLVM version 23, has been installed into directory: _/usr/lib/llvm-23_, and only installed the core LLVM framework and not the clang compiler frontend. That installation can be done like this (in Ubuntu 24):

```
$ sudo apt-get install clang-23 llvm-23-dev libclang-common-23-dev libclang-23-dev
...
$ 
```

Check the clang compiler:

```
$ /usr/lib/llvm-23/bin/clang --version
Ubuntu clang version 23.0.0 (++20260601024917+256d09201cf9-1~exp1~20260601024941.1659)
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/lib/llvm-23/bin
$
```

..and register it:

```
$ sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-23 23 \
--slave /usr/bin/clang++ clang++ /usr/bin/clang++-23 \
--slave /usr/bin/llvm-config llvm-config /usr/bin/llvm-config-23  # command stops here!!
update-alternatives: using /usr/bin/clang-23 to provide /usr/bin/clang (clang) in auto mode
$
```

<br/>

Hopefully, all LLVM versions are registered now successfully, and thus the LLVM configuration menu can be called:

```
$ sudo update-alternatives --config clang
There are 4 choices for the alternative clang (providing /usr/bin/clang).

  Selection    Path               Priority   Status
------------------------------------------------------------
* 0            /usr/bin/clang-23   23        auto mode
  1            /usr/bin/clang-14   14        manual mode
  2            /usr/bin/clang-19   19        manual mode
  3            /usr/bin/clang-21   21        manual mode
  4            /usr/bin/clang-23   23        manual mode

Press <enter> to keep the current choice[*], or type selection number: 
$
```

<br/>

### Easy installation in Windows 11

Look for latest, or suitable, _LLVM*win64.exe_ _installer_ file ("Windows x64 (64-bit)") from: https://github.com/llvm/llvm-project/releases

Execute this file and reboot Windows 11.

Test the installation at the Windows shell ("_cmd_", "Command Prompt") like this:

```
> clang --version
clang version 20.1.1
Target: x86_64-pc-windows-msvc
Thread model: posix
InstalledDir: C:\Program Files\LLVM\bin

>
```

<br/>

### LLVM's Polly loop optimizer

Here's something about [LLVM's Polly loop optimizer](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Inko#llvms-polly-loop-optimizer).

<br/>

##_end
