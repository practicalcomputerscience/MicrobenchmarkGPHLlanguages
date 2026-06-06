2026-06-06: work in progress

<br/>

# LLVM compiler infrastructure

LLVM = "Low Level Virtual Machine" in its earlier days

_clang_ = the clang compiler (in Linux and Windows), which is usually installed separately in Linux

https://llvm.org/

<br/>

Numerous programming languages are using the LLVM compiler infrastructure. However, sometimes a specific version of LLVM is required.

So, I started this dedicated page to collect some of the information about and around LLVM.

<br/>

### Easy installation in Ubuntu 24

In Ubuntu 24, the easiest command to install a modern version of LLVM is this:

```
$ sudo apt install llvm
...
$ llvm-config --version
23.0.0  # as of 2026-06-06
$ 
```

<br/>

### LLVM installation of a specific LLVM version in Ubuntu 24

With the friendly help of Google AI, here I additionally install LLVM version 21 for example:

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
drwxr-xr-x  3 root root   4096 Jun  6 12:02 llvm-23
drwxr-xr-x  3 root root   4096 Feb 17 16:15 llvm-c-14
drwxr-xr-x  3 root root   4096 Feb 14 21:35 llvm-c-19
drwxr-xr-x  3 root root   4096 Jun  6 12:02 llvm-c-23
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

tbd

<br/>

tbd

<br/>

tbd

<br/>

##_end
