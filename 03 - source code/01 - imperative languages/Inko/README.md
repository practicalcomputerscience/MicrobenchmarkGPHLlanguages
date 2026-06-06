# Inko

https://inko-lang.org/

https://github.com/inko-lang/inko

---

Table of contents:

- [Installation tips](#installation-tips)
- [LLVM's Polly loop optimizer](#llvms-polly-loop-optimizer)
- [Regular expressions](#regular-expressions)

<br/>

---

## Installation tips

Things got much easier with Inko version 0.19.1 compared to the version that I used before in this language in the making, which was version 0.18.1.

I installed Inko version 0.19.1 like this, leaning to chapter _**From source**_ and starting with the latest release tarball: https://docs.inko-lang.org/manual/latest/setup/installation/ (*)

Version numbers can be looked up from here (as of 2026-06-06): https://github.com/inko-lang/inko/tags

```
$ VER='0.19.1'
$ mkdir $VER
$ curl https://releases.inko-lang.org/$VER.tar.gz -o $VER.tar.gz
$ tar -C $VER -xf $VER.tar.gz
$ cd $VER
$ cargo build --release
...
$ make
...
$ sudo make install
...
$ inko --version
inko 0.19.1
$
```

This is the Rust version I've been using here ("Inko's native code compiler is written in Rust and uses LLVM as its backend." (*)):

```
$ rustc -V
rustc 1.92.0 (ded5c06cf 2025-12-08)
$
```

..and this the LLVM version:

```
$ llvm-config --version
21.1.7
$
```

<br/>

### LLVM's Polly loop optimizer

When running command _$ cargo build --release_, it may happen that LLVM's Polly loop optimizer is missing:

```
$ cargo build --release
...
error: could not find native static library `Polly`, perhaps an -L flag is missing?
...
$
```

You can install the missing Polly library for the targeted LLVM version like this (in Ubuntu), here for LLVM version 21 or example:

```
$ sudo apt install libpolly-21-dev libclang-common-21-dev
...
$
```

> [!IMPORTANT]
> Essential before you restart the build: reset the Rust build which went wrong before!

```
$ cargo clean  # clear the cargo cache
     Removed 723 files, 219.0MiB total
$ cargo build --release  # restart the build
   Compiling libc v0.2.185
   Compiling cfg-if v1.0.4
   Compiling find-msvc-tools v0.1.9
...
   Compiling ureq v3.3.0
   Compiling inko v0.20.0 (~/scripts/Inko/0.20.0/inko)
    Finished `release` profile [optimized] target(s) in 8.21s
$
```

Now you should be able to restart the compilation of Inko from its sources (as shown above):

```
$ make
...
$ sudo make install
...
$ inko --version
inko 0.20.0
$
```

Voilà!

<br/>

#### Regular expressions

At the moment, Inko doesn't support natively regular expressions. There has been package: [Inko Regex](https://github.com/dusty-phillips/inko-regex/tree/main#inko-regex), which is also listed here (as of 2026-06-06): https://inko-lang.org/packages/

However, and at least with Inko version 0.20.0, I can't compile source code file [regex.inko](https://github.com/dusty-phillips/inko-regex/blob/main/src/regex.inko):

```
$ inko build regex.inko 
regex.inko:1:18 error(invalid-syntax): expected an identifier or keyword
$
```

Well, the _Inko Regex_ package hasn't been updated for 3 years now.

So, I will leave current program [random_bitstring_and_flexible_password_generator.inko](./random_bitstring_and_flexible_password_generator.inko) as it is now, and won't refactor it for using regular expressions.

<br/>

##_end
