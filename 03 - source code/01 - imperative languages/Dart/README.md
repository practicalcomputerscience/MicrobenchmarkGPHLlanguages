# Dart

https://dart.dev/

https://dart.dev/tutorials/server/get-started

https://github.com/dart-lang/sdk

JIT = just-in-time compiled

AOT = ahead-of-time compiled

---

Table of contents:

- [Idea of Dart](#idea-of-dart)
- [Installation tips](#installation-tips)
- [Execution forms](#execution-forms)
- [JIT compilation](#jit-compilation)
- [Compilation to JavaScript](#compilation-to-javascript)
- [JIT snapshot compilation](#jit-snapshot-compilation)
- [AOT snapshot compilation](#aot-snapshot-compilation)
- [Standalone (or self-contained) executable (which may not be portable so easily)](#standalone-or-self-contained-executable-which-may-not-be-portable-so-easily)
- [WebAssembly](#webassembly)
- [Dart execution speeds diagram](#dart-execution-speeds-diagram)

<br/>

---

## Idea of Dart

At first, I also wanted to cramp Google's Dart into page [From "back-end" to "front-end" programming languages, and back](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20%22web%20languages%22%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#from-back-end-to-front-end-programming-languages-and-back) as just another "web language".

But then I discovered that Dart right from start evolved around its own and rich ecosystem, with "front-end" and "back-end" in mind, which allows Dart source code to be run on a variety of platforms.

So, I came to the conclusion that Dart deserves it's own page. A page, which has become much longer than initially anticipated by me. 

By the way: Dart was officially introduced in 2011 as a "programming language for building web applications": [Dart: A language for structured web programming ](https://blog.chromium.org/2011/10/dart-language-for-structured.html)

<br/>

## Installation tips

For Ubuntu 24 LTS, I installed and shortly tested Dart like this:

```
$ brew install dart-sdk
...
$ dart --version
Dart SDK version: 3.10.8 (stable) (Tue Jan 27 00:02:04 2026 -0800) on "linux_x64"
$ 
```

In Ubuntu, Dart can also be installed with the [Flutter web framework](https://flutter.dev/) like this: _$ sudo snap install flutter_

<br/>

## Execution forms

Dart features a not so small variety of formats and environments: [Dart: The platforms](https://dart.dev/overview#platform)

### JIT compilation

In Linux, the "default mode" on the console is to let the Dart **virtual machine** just-in-time compile and run [Dart source code](./random_streams_for_perf_stats.dart) like this, see at [dart run](https://dart.dev/tools/dart-run):

```
$ dart run ./random_streams_for_perf_stats.dart

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$ 

```

However, this command works too:

```
$ dart ./random_streams_for_perf_stats.dart
```

Page [dart: The Dart command-line tool](https://dart.dev/tools/dart-tool) says on the _run_ command:

> Prefer dart run.

While _$ dart run random_streams_for_perf_stats.dart_ takes about 220 milliseconds, running this microbenchmark program ("speed part" only) without the _run_ command takes only about 150 milliseconds.

Why is that?

"Big AI" has answers to this, but doesn't tell original sources that can explain something. Dart's official documentation is also shy about this phenomenon.

Anyway, I guess it's clear that _$ dart run \<~.dart\>_ comes with overhead that _$ dart \<~.dart\>_ just doesn't have.

It also seems that there's no switch for command _$ dart run_ to shut off the JIT compilation (just for curiosity).

<br/>

### Compilation to JavaScript

Also Dart source code can be explicitely transpiled into JavaScript source code (nowadays), which is then executable on node.js.

However, the original [Dart program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dart/random_streams_for_perf_stats.dart) cannot be simply transpiled into JavaScript source code which can then work unmodfied on node.js. There are two (major) reasons for this:

- writing to the file system needs direct bindings from Dart to the _fs_ (file system) resource of node.js
- the transpiled JavaScript code has to be prepended with some preamble, because standalone engines like node.js lack browser-specific globals that Dart's generated code expects (Google AI)

So, here are the modified parts of a Dart program named [random_streams_for_perf_stats_js.dart](./random_streams_for_perf_stats_js.dart), which is suitable starting point for node.js:

```
...
import 'package:node_interop/fs.dart';  // Direct fs bindings

...
  // Write bit stream to disk:
  try {
    // Accessing node's fs module
    fs.writeFileSync(fileBitsX, bitsX.toString());
    print("Bit stream has been written to disk under name:  $fileBitsX");
  } catch (e) {
    print("could not write to file: $fileBitsX ! -- ${e.toString()}");
  }
...
```

Next, we need a configuration file called [pubspec.yaml](./pubspec.yaml) to declare the dependency on _node_interop_:

```
name: random_streams_for_perf_stats_js
description: A logic-only script using Node.js fs.

environment:
  sdk: '>=3.0.0 <4.0.0'

dependencies:
  node_interop: ^2.1.0
```

..which we will download with command:

```
$ dart pub get
```

Then, we can transpile into an intermediate version of a JavaScript file, here called _random_streams_for_perf_stats_js.js_:

```
$ dart compile js ./random_streams_for_perf_stats_js.dart -o ./random_streams_for_perf_stats_js.js
```

<br/>

Next step is a add a preamble to that file, because standalone engines like node.js lack browser-specific globals (like _self_ or _window_) that Dart's generated code expects. You can download file _preamble.js_ from here: https://github.com/mbullington/node_preamble.dart/blob/master/lib/preamble.js

..and prepend it:

```
$ cat preamble.js random_streams_for_perf_stats_js.js > random_streams_for_perf_stats_js.node.js
```

..to produce the final JavaScript file, here named _random_streams_for_perf_stats_js.node.js_, which can then be executed correctly on node.js hopefully:

```
$ node ./random_streams_for_perf_stats_js.node.js

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

<br/>

The JavaScript file generated with command _$ dart compile js ..._ implicitly applied optimization level 1. There are 5 optimization levels (see with _$ dart compile js -h -v_), though. I found out that, at least in this case, _-O1_ is as good as it gets:

Optimization level | real execution time (1 program run only) | final ~.js file size (incl. preamble) | comment
--- | --- | --- | ---
-O0 | 53 milliseconds | 375,380 bytes | 
-O1 | 45 milliseconds | 145,330 bytes | default, implicit optimization level; measured with _multitime -n 20 node ./random_streams_for_perf_stats_js.node.js_
-O2 | 45 milliseconds | 57,063 bytes | 
-O3 | 43 milliseconds | 55,828 bytes | 
-O4 | 45 milliseconds | 54,586 bytes | 

However, with increasing the optimization level the size of the generated JavaScript file is shrinking.

<br/>

### JIT snapshot compilation

A JIT compilation can be snapshot from "a warm JIT" like this:

```
$ dart compile jit-snapshot random_streams_for_perf_stats.dart
$ dart run ./random_streams_for_perf_stats.jit
...
$
```

Here, command _dart run ./random_streams_for_perf_stats.jit_ is substantially slower than command _dart ./random_streams_for_perf_stats.jit_: around 107 milliseconds versus around 38 milliseconds

By the way: help command _$ dart compile jit-snapshot --help_ doesn't show any possibilities for refined optimization.

<br/>

### AOT snapshot compilation

An Ahead-of-time compilation can be snapshot like this:

```
$ dart compile aot-snapshot random_streams_for_perf_stats.dart
$ dartaotruntime ./random_streams_for_perf_stats.aot
...
$
```

_dartaotruntime_ provides a minimal runtime for running Dart AOT modules: https://github.com/dart-lang/sdk/blob/8c7e9a045ca46f4430494810a62eddb960e76bc2/README.dart-sdk#L8

..and is the only command which works with Dart AOT modules.

<br/>

### Standalone (or self-contained) executable (which may not be portable so easily)

A Dart script can be compiled directly to a "standalone executable" like this:

```
$ dart compile exe ./random_streams_for_perf_stats.dart
$ ./random_streams_for_perf_stats.exe
...
$
```

..where running _./random_streams_for_perf_stats.exe_ takes the same time than running the AOT snapshot from above.

<br/>

However, executable _random_streams_for_perf_stats.exe_ is often not portable to another, "foreign" Linux system, a Linux system which doesn't feature the same Linux kernel version for _linux-vdso.so.1_ (in this case for kernel version _6.14.0-37-generic_; see with command: _$ uname -a_) for example.

This executable has also other dependencies, which can be seen with the [ldd](https://www.man7.org/linux/man-pages/man1/ldd.1.html) command, and which may also be missing at the target Linux system:

```
$ ldd ./random_streams_for_perf_stats.exe
linux-vdso.so.1 (0x00007ffff4e52000)
libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007310ecbde000)
libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007310ecbd9000)
libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007310ec517000)
libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007310ec200000)
/home/linuxbrew/.linuxbrew/lib/ld.so => /lib64/ld-linux-x86-64.so.2 (0x00007310ecbf7000)
$
```

_linux-vdso.so.1_ is a "virtual ELF dynamic shared object" (ELF = Executable and Linkable Format), which "is a small shared library that the kernel automatically maps into the address space of all user-space applications." See from: https://man7.org/linux/man-pages/man7/vdso.7.html. It's used in (Linux) programming languages where dynamic executables are compiled by default, and these are the most compiled programming languages: [Portability of programs](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/70%20-%20portability%20from%20Linux%20to%20Linux#portability-of-programs)

So, in this case the easiest solution from my point of view would be to install a suitable Dart version on the target Linux system, and then compile the Dart script into a specific executable program version again.

<br/>

### WebAssembly

Nowadays, also Dart source code can be compiled to WebAssembly: [WebAssembly (Wasm) compilation](https://dart.dev/web/wasm)

```
$ dart compile wasm -Da=1 ./random_streams_for_perf_stats.dart
$ node ./random_streams_for_perf_stats.mjs
$ # no program execution!
```

But:

> The compiled Wasm output currently targets JavaScript environments (such as browsers), and thus currently doesn't support execution in standard Wasm run-times like wasmtime and wasmer.

Command _$ dart compile wasm -Da=1 \<~.dart\>_ generated a wrapper JavaScript file named _random_streams_for_perf_stats.mjs_ (which doesn't work on node.js), and other files:

```
$ ls -1
random_streams_for_perf_stats.dart
random_streams_for_perf_stats.mjs
random_streams_for_perf_stats.support.js
random_streams_for_perf_stats.unopt.wasm
random_streams_for_perf_stats.unopt.wasm.map
random_streams_for_perf_stats.wasm
random_streams_for_perf_stats.wasm.map
$ 
```

So, that's the end of this development starting with _random_streams_for_perf_stats.dart_. However, starting with this program in its **C version**, I succeeded with making a WebAssembly based program as described here: [The WebAssembly (Wasm) virtual machine](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20%22web%20languages%22%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#the-webassembly-wasm-virtual-machine)

<br/>

## Dart execution speeds diagram

![plot](./mean_stddev_err_whiskers%20--%20only%20Dart.png)

The same Dart program can tally a wide range of execution times, depending on its final format and execution environment.

<br/>

##_end
