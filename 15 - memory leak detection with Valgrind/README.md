# Memory leak detection with Valgrind

[Memory safety](https://en.wikipedia.org/wiki/Memory_safety) of programming languages has been a big talking point in recent years: https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2023/p2771r0.html

While reading about this topic, I tumbled over the program **Valgrind**: https://valgrind.org/

> There are Valgrind tools that can automatically detect many memory management and threading bugs, and profile your programs in detail.

<br/>

Table of contents:

- [Installation tips](#installation-tips)
- [Summaries table](#summaries-table)
- [Rust](#rust)
- [On other languages](#on-other-languages)
- [Changing source code to get the program through Valgrind](#changing-source-code-to-get-the-program-through-valgrind)
- [Compiling the source code in a special way to get the program through Valgrind](#compiling-the-source-code-in-a-special-way-to-get-the-program-through-valgrind)

---

## Installation tips

For Ubuntu 24 LTS, I followed these tips to build the Valgrind program from sources: https://idroot.us/install-valgrind-ubuntu-24-04/

My installed version is:

```
$ valgrind --version
valgrind-3.25.1
$
```

..and I use this program like this, here for the binary executable program named _random_bitstring_and_flexible_password_generator_, which has been built from the [Crystal source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Crystal/random_bitstring_and_flexible_password_generator.cr) for example:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
==5443== Memcheck, a memory error detector
==5443== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==5443== Using Valgrind-3.25.1 and LibVEX; rerun with -h for copyright info
==5443== Command: ./random_bitstring_and_flexible_password_generator
==5443== 
...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

Password of 12 printable chars OK? 'y' or another integer number >= 8: ==5443== Warning: client switching stacks?  SP change: 0x1ffeffe2d0 --> 0x594efb8
==5443==          to suppress, use: --max-stackframe=137328522008 or greater
==5443== Warning: client switching stacks?  SP change: 0x594e3f0 --> 0x614efb8
==5443==          to suppress, use: --max-stackframe=8391624 or greater
==5443== Warning: client switching stacks?  SP change: 0x614e210 --> 0x594e3f0
==5443==          to suppress, use: --max-stackframe=8388128 or greater
==5443==          further instances of this message will not be shown.
y

Do you want me to use special characters like .;,+*... ? 'y' or 'n': y

Your password of 12 characters is: JEKg&T7qK5v.
==5443== 
==5443== HEAP SUMMARY:
==5443==     in use at exit: 8,192 bytes in 1 blocks
==5443==   total heap usage: 15 allocs, 14 frees, 12,336 bytes allocated
==5443== 
==5443== LEAK SUMMARY:
==5443==    definitely lost: 8,192 bytes in 1 blocks
==5443==    indirectly lost: 0 bytes in 0 blocks
==5443==      possibly lost: 0 bytes in 0 blocks
==5443==    still reachable: 0 bytes in 0 blocks
==5443==         suppressed: 0 bytes in 0 blocks
==5443== Rerun with --leak-check=full to see details of leaked memory
==5443== 
==5443== Use --track-origins=yes to see where uninitialised values come from
==5443== For lists of detected and suppressed errors, rerun with: -s
==5443== ERROR SUMMARY: 674 errors from 36 contexts (suppressed: 0 from 0)
$ 
```

I ran Valgrind only on programs from compiled languages, not scripting languages like Python for example.

Another, maybe overlooked fact: I could repeat all test results as far as I have done repeated tests.

<br/>

## Summaries table

programming language | HEAP SUMMARY: in use at exit | LEAK SUMMARY: still reachable | All heap blocks were freed -- no leaks are possible ?
--- | --- | --- | ---
Ada | 3,592 bytes in 3 blocks | 3,592 bytes in 3 blocks | no
C | 0 bytes in 0 blocks | -- | yes <<<<<<
C3 | 0 bytes in 0 blocks | -- | yes <<<<<<
Chapel | program doesn't finish | -- | no
Common Lisp | 1,538,080 bytes in 17 blocks | 1,275,640 bytes in 15 blocks | no
Crystal | 8,192 bytes in 1 blocks | 0 bytes in 0 blocks | no
FreeBASIC | 26,542 bytes in 43 blocks | 26,542 bytes in 43 blocks | no
Go | 0 bytes in 0 blocks | -- | yes <<<<<<
Inko | Segmentation fault (core dumped) | -- | no
Koka | program doesn't finish | -- | no
Mojo | 5,702 bytes in 7 blocks | 5,702 bytes in 7 blocks | no
OCaml | 6,591,286 bytes in 66 blocks | 3,355,334 bytes in 63 blocks | no
Racket Scheme | Valgrind is doing nothing | -- | no
Roc | 0 bytes in 0 blocks | -- | yes <<<<<<
Rust | 8,192 bytes in 1 blocks | 8,192 bytes in 1 blockss | no
Standard ML (MLton) | 0 bytes in 0 blocks | -- | yes <<<<<<
Swift | 2,001,798 bytes in 38 blocks | 2,001,670 bytes in 35 blocks | no
V | 0 bytes in 0 blocks | -- | yes <<<<<<
Zig | 0 bytes in 0 blocks | -- | yes <<<<<<

<br/>

So, my main focus was on the possibly best test outcome, that is: _All heap blocks were freed -- no leaks are possible_, like with C for example:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
...

Your password of 12 characters is: SMm`=URaYA~]
==6207== 
==6207== HEAP SUMMARY:
==6207==     in use at exit: 0 bytes in 0 blocks
==6207==   total heap usage: 6 allocs, 6 frees, 11,184 bytes allocated
==6207== 
==6207== All heap blocks were freed -- no leaks are possible
==6207== 
==6207== Use --track-origins=yes to see where uninitialised values come from
==6207== For lists of detected and suppressed errors, rerun with: -s
==6207== ERROR SUMMARY: 36 errors from 4 contexts (suppressed: 0 from 0)
$
```

Only looking by above table, I would implement a security related program only with these programming languages (at the moment):

- C
- C3
- Go
- Roc
- Standard ML (MLton)
- V
- Zig

<br/>

#### Rust

From point of view of Valgrind, even Rust is potentially not a (totally) "memory safe" language. After the Rust based executable exited, there were _still reachable: 8,192 bytes in 1 blocks_. 

In my [Rust program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Rust/random_bitstring_and_flexible_password_generator.rs), it's this line of source code which causes the memory leak:

```
_ = io::stdin().read_line(&mut answer_str);  // reading user input: no "?" here when main function has no result handling
```

Here's the background of this phenomenon: https://www.reddit.com/r/rust/comments/u9gx5t/comment/i5rf956/

So, one may have to wait for a Valgrind fix here.

It also shows that above test results and my conclusions have to be taken with a grain of salt.

<br/>

### On other languages

Not all binary executable programs are finishing and have to be manually interupted by pressing CTRL+C keys. The **Chapel** version is one example:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
==6129== Memcheck, a memory error detector
==6129== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==6129== Using Valgrind-3.25.1 and LibVEX; rerun with -h for copyright info
==6129== Command: ./random_bitstring_and_flexible_password_generator
==6129== 
hwloc x86 backend cannot work under Valgrind, disabling.
May be reenabled by dumping CPUIDs with hwloc-gather-cpuid
and reloading them under Valgrind with HWLOC_CPUID_PATH.
...
^C
==6129==
==6129== Process terminating with default action of signal 2 (SIGINT)
...
$
```

<br/>

The **Koka** program falls into the same category, though with a different console output.

<br/>

The **Racket Scheme** program shows another phenomemon:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
==6154== Memcheck, a memory error detector
==6154== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==6154== Using Valgrind-3.25.1 and LibVEX; rerun with -h for copyright info
==6154== Command: ./random_bitstring_and_flexible_password_generator
==6154== 

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

Password of 12 printable chars OK? 'y' or another integer number >= 8: y

Do you want me to use special characters like .;,+*... ? 'y' or 'n': y

Your password of 12 characters is: Afv{EApa#?1q
$
```

This program evades its profiling with Valgrind, at least when running it in the (simple) way as shown above.

With the other tested Scheme dialects, I only implemented the "random_streams_for_perf_stats" programs. All of them had _still reachable_ leaks:

- Bigloo: _still reachable: 2,512 bytes in 2 blocks_
- CHICKEN: _still reachable: 27,433,592 bytes in 5,303 blocks_
- Gambit: _still reachable: 2,048 bytes in 2 blocks_

### Changing source code to get the program through Valgrind

At least in one instance, here with [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo/random_bitstring_and_flexible_password_generator.mojo), I modified the source code to get the program through Valgrind without crashing it:

```
...
    # this makes problems with valgrind --> Illegal instruction (core dumped)
    # when creating the password below
    # var char_set: String = ""
    # if WITH_SPECIAL_CHARS == True:
    #     # add chars dec 33 .. dec 126:
    #     for i in range(33,127):
    #       char_set += chr(i)
    # else:
    #     char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
...
```

So, variable _char_set_ is not in use in the Mojo program anymore.

<br/>

I didn't try to re-write the [Inko program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Inko/random_bitstring_and_flexible_password_generator.inko) to see if I can make the _Segmentation fault (core dumped)_ go away:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
==5800== Memcheck, a memory error detector
==5800== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==5800== Using Valgrind-3.25.1 and LibVEX; rerun with -h for copyright info
==5800== Command: ./random_bitstring_and_flexible_password_generator
==5800== 
...
Segmentation fault (core dumped)
$
```

<br/>

### Compiling the source code in a special way to get the program through Valgrind

The [Zig program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig/random_bitstring_and_flexible_password_generator.zig) shows another potential impact of Valgrind to a program version.

Since the CPU of my [test system](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments), that is an Intel(R) Core(TM) i7-11700K CPU, (still) features the [AVX-512 extension](https://www.intel.com/content/www/us/en/architecture-and-technology/avx-512-overview.html), I had to use Zig compiler switch _-mcpu=native-avx512f_ to compile an executable, which is not crashing when running it with Valgrind:

```
$ zig build-exe random_bitstring_and_flexible_password_generator.zig -mcpu=native-avx512f
```

This compiler switch is not needed when running the Zig executable without Valgrind.

<br/>

##_end
