# Memory leak detection with Valgrind

[Memory safety](https://en.wikipedia.org/wiki/Memory_safety) of programming languages has been a big talking point in recent years: https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2023/p2771r0.html

While reading about this topic, I tumbled over the program **Valgrind**: https://valgrind.org/

> There are Valgrind tools that can automatically detect many memory management and threading bugs, and profile your programs in detail.

<br/>

Table of contents:

- [Installation tips](#installation-tips)
- [Summaries table](#summaries-table)
- [Rust](#rust)
- [Swift](#swift)
- [On other languages](#on-other-languages)
- [Changing source code to get the executable through Valgrind](#changing-source-code-to-get-the-executable-through-valgrind)
- [Compiling the source code in a special way to get the executable through Valgrind](#compiling-the-source-code-in-a-special-way-to-get-the-executable-through-valgrind)

---

## Installation tips

For Ubuntu 24 LTS, I followed these tips to build the Valgrind program from sources: https://idroot.us/install-valgrind-ubuntu-24-04/

My installed version is:

```
$ valgrind --version
valgrind-3.27.0.GIT
$
```

..and I use this program like this, here for the binary executable program named _random_bitstring_and_flexible_password_generator_, which has been built from the [Crystal source code](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Crystal/random_bitstring_and_flexible_password_generator.cr) for example:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
==23314== Memcheck, a memory error detector
==23314== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==23314== Using Valgrind-3.27.0.GIT and LibVEX; rerun with -h for copyright info
==23314== Command: ./random_bitstring_and_flexible_password_generator
==23314==
...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

Password of 12 printable chars OK? 'y' or another integer number >= 8: ==23314== Warning: client switching stacks?  SP change: 0x1ffeffeae0 --> 0x5957fb8
==23314==          to suppress, use: --max-stackframe=137328487208 or greater
==23314== Warning: client switching stacks?  SP change: 0x5957410 --> 0x6157fb8
==23314==          to suppress, use: --max-stackframe=8391592 or greater
==23314== Warning: client switching stacks?  SP change: 0x61572a0 --> 0x5957410
==23314==          to suppress, use: --max-stackframe=8388240 or greater
==23314==          further instances of this message will not be shown.
y

Do you want me to use special characters like .;,+*... ? 'y' or 'n': y

Your password of 12 characters is: ^0JSm*SO;|RX
==23314==
==23314== HEAP SUMMARY:
==23314==     in use at exit: 8,192 bytes in 1 blocks
==23314==   total heap usage: 15 allocs, 14 frees, 12,336 bytes allocated
==23314==
==23314== LEAK SUMMARY:
==23314==    definitely lost: 8,192 bytes in 1 blocks
==23314==    indirectly lost: 0 bytes in 0 blocks
==23314==      possibly lost: 0 bytes in 0 blocks
==23314==    still reachable: 0 bytes in 0 blocks
==23314==         suppressed: 0 bytes in 0 blocks
==23314== Rerun with --leak-check=full to see details of leaked memory
==23314==
==23314== Use --track-origins=yes to see where uninitialised values come from
==23314== For lists of detected and suppressed errors, rerun with: -s
==23314== ERROR SUMMARY: 695 errors from 38 contexts (suppressed: 0 from 0)
$ 
```

I ran Valgrind only on programs from compiled languages, not scripting languages like Python or Ruby for example.

Another, maybe overlooked fact: I could repeat all test results as far as I have done repeated tests.

<br/>

## Summaries table

programming language | HEAP SUMMARY: in use at exit | LEAK SUMMARY: still reachable | All heap blocks were freed -- no leaks are possible ? | test date
--- | --- | --- | --- | ---
Ada | 3,592 bytes in 3 blocks | 3,592 bytes in 3 blocks | no | 2026-01-10
C | 0 bytes in 0 blocks | -- | yes <<<<<< | 2025-12-18
C++ | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-15
C3 | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-10
Chapel | program doesn't finish | -- | no | 2025-12-18
Common Lisp | 554,806 bytes in 12 blocks | 292,358 bytes in 10 blocks | no | 2026-01-10
Crystal | 8,192 bytes in 1 blocks | 0 bytes in 0 blocks | no | 2025-12-18
D | 256 bytes in 4 blocks | 224 bytes in 3 blocks | no | 2026-01-21
Eiffel (Liberty) | 4,521,992 bytes in 294 blocks | 4,521,992 bytes in 294 blocks | no | 2026-01-24
Fortran | 40 bytes in 1 blocks | 40 bytes in 1 blocks | no | 2026-01-06
FreeBASIC | 26,542 bytes in 43 blocks | 26,542 bytes in 43 blocks | no | 2026-01-10
Go | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-10
Inko | Segmentation fault (core dumped) | -- | no | 2025-12-18
Koka | program doesn't finish | -- | no | 2025-12-18
Mercury | 995 bytes in 3 blocks | 944 bytes in 2 blocks | no | 2026-01-10
Mojo | SIGILL signal raised which killed the program | -- | no | 2026-01-10
Nim | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-13
OCaml | 4,474,152 bytes in 93 blocks | 4,474,152 bytes in 93 blocks | no | 2026-01-10
Odin | 0 bytes in 0 blocks | --  | yes <<<<<< |  2026-01-08
Prolog, SWI | 20,529,534 bytes in 178,711 blocks | 19,486,208 bytes in 162,301 blocks | no | 2026-01-10
Scheme, Racket | Valgrind is doing nothing | -- | no | 2026-01-10
Roc | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-10
Rust | 8,648 bytes in 2 blocks | 8,648 bytes in 2 blocks | no | 2025-12-18
Standard ML (MLton) | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-10
Swift | 2,001,942 bytes in 40 blocks | 2,001,814 bytes in 37 blocks | no | 2026-01-10
V | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-10
Zig | 0 bytes in 0 blocks | -- | yes <<<<<< | 2026-01-10

<br/>

So, my main focus was on the possibly best test outcome, that is: _All heap blocks were freed -- no leaks are possible_, like with C for example:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
...

Your password of 12 characters is: }!033|JOt2T_
==23336== 
==23336== HEAP SUMMARY:
==23336==     in use at exit: 0 bytes in 0 blocks
==23336==   total heap usage: 6 allocs, 6 frees, 11,184 bytes allocated
==23336== 
==23336== All heap blocks were freed -- no leaks are possible
==23336== 
==23336== Use --track-origins=yes to see where uninitialised values come from
==23336== For lists of detected and suppressed errors, rerun with: -s
==23336== ERROR SUMMARY: 25 errors from 3 contexts (suppressed: 0 from 0)
$
```

Only looking by the table above, I would implement a security related program only with these programming languages:

- C
- C++
- C3
- Go
- Nim
- Odin
- Roc
- Standard ML (MLton)
- V
- Zig

<br/>

#### Rust

From point of view of Valgrind, even Rust is potentially not a (totally) "memory safe" language. After the Rust based executable exited, there were _still reachable: 8,648 bytes in 2 blocks_. 

In my [Rust program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Rust/random_bitstring_and_flexible_password_generator.rs), it's this line of source code which causes the memory leak:

```
_ = io::stdin().read_line(&mut answer_str);  // reading user input: no "?" here when main function has no result handling
```

Here's the background of this phenomenon: https://www.reddit.com/r/rust/comments/u9gx5t/comment/i5rf956/

So, one may have to wait for a Valgrind fix here.

It also shows that above test results and my conclusions have to be taken with a grain of salt.

#### Swift

Swift is a "super memory leaker" according to my tests. Even this little program, which only features some immutable variable declarations:

```
let END  = 100  // for testing
let m    = 65521  // = 2^16 - 15
let a    = 17364
let c    = 0
```

..is causing memory leaks at program exit, here:

```
...
    ==20779== HEAP SUMMARY:
    ==20779==     in use at exit: 1,162 bytes in 7 blocks
...
```

<br/>

### On other languages

Not all executable programs are finishing and have to be manually interrupted by pressing CTRL+C keys. The **Chapel** version is one example:

```
$ valgrind ./random_bitstring_and_flexible_password_generator
==23385== Memcheck, a memory error detector
==23385== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==23385== Using Valgrind-3.27.0.GIT and LibVEX; rerun with -h for copyright info
==23385== Command: ./random_bitstring_and_flexible_password_generator
==23385==
==23385== Warning: client switching stacks?  SP change: 0x6d743b8 --> 0x7c00f78
==23385==          to suppress, use: --max-stackframe=15256512 or greater
...
^C
==23385==
==23385== Process terminating with default action of signal 2 (SIGINT)
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

With the other tested Scheme dialects, I only implemented the "random_streams_for_perf_stats" programs. All of them had _still reachable_ leaks when exiting:

- Bigloo: _still reachable: 2,512 bytes in 2 blocks_
- CHICKEN: _still reachable: 27,433,592 bytes in 5,303 blocks_
- Gambit: _still reachable: 2,048 bytes in 2 blocks_

<br/>

### Changing source code to get the executable through Valgrind

At least in one instance, here with [Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo/random_bitstring_and_flexible_password_generator.mojo), I modified the source code to get the executable through Valgrind without crashing it. It worked.

But then I updated the Mojo version to _Mojo 0.26.1.0.dev2025121217 (3e295ef6)_, like in other languages for a better initial random seed, and Valgrind killed the new executable. So, I re-implemented the original version of this program, knowing that it will be killed when running it with Valgrind, which is indeed the case.

<br/>

I didn't try to re-write the [Inko program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Inko/random_bitstring_and_flexible_password_generator.inko) to see if I can make the _Segmentation fault (core dumped)_ go away:

```
$ valgrind ./build/release/random_bitstring_and_flexible_password_generator
==23373== Memcheck, a memory error detector
==23373== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==23373== Using Valgrind-3.27.0.GIT and LibVEX; rerun with -h for copyright info
==23373== Command: ./build/release/random_bitstring_and_flexible_password_generator
==23373==
...
Segmentation fault (core dumped)
$
```

<br/>

### Compiling the source code in a special way to get the executable through Valgrind

The [Zig program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig/random_bitstring_and_flexible_password_generator.zig) shows another potential impact of Valgrind on a program version; see from chapter: [Compilation tips](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Zig#compilation-tips)

<br/>

##_end
