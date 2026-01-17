2026-01-17: work in progress

# D

https://dlang.org/

<br/>

One of the first languages, since 1999, with the aim to overcome the deficits of C++: 

2020: _Origins of the D Programming Language_

https://dl.acm.org/doi/abs/10.1145/3386323

by: WALTER BRIGHT, The D Language Foundation, USA; ANDREI ALEXANDRESCU, The D Language Foundation, USA; MICHAEL PARKER, The D Language Foundation, USA

> As its name suggests, the initial motivation for the D programming language was to improve on C and C++ while keeping their spirit. The D language was to preserve the efficiency, low-level access, and Algol-style
syntax of those languages. The areas D set out to improve focused initially on rapid development, convenience, and simplifying the syntax without hampering expressiveness.

However, one could argue that something similar to Java and its contenders happened here too: [Old computer programming languages learning new tricks](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list#old-computer-programming-languages-learning-new-tricks)

---

You have the choice of three compilers (in Linux and architectures i386, amd64):

- DMD ("Digital Mars D compiler"): Official reference compiler
- GDC: GCC-based D compiler
- LDC: LLVM-based D compiler

On "Which compiler should I use?" this answer is given: "For beginners, DMD is the recommended choice, as it is the implementation closest to the D Language Specification." from: https://wiki.dlang.org/Compilers

However: **"GDC and LDC both generate substantially faster binaries than DMD."**

So, I'll start with GDC after my experience that g++ v.13.3.0 compiled a faster executable than Homebrew clang 21.1.7: [C++](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/C%2B%2B#c)

I installed GDC like this:

```
($ sudo apt install zlib1g=1:1.3.dfsg-3.1ubuntu2  # this version was needed in my system; just as a tip here when version conflicts like this exist)
$ sudo apt install gdc
...
$ gdc --version
gdc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
...
$
```

TBD

<br/>

##_end
