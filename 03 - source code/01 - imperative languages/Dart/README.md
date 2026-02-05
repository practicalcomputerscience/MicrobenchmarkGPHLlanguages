2026-02-02: work in progress

- tbd: benchmark Dart to JS for node.js
- tbd: have its own exe speeds diagram: programming_languages_exe_speeds_Dart.csv
- tbd: put the AOT version into the master diagram

# Dart

https://dart.dev/

https://dart.dev/tutorials/server/get-started

---

## Idea of Dart

At first, I also wanted to cramp Google's Dart into page [From "back-end" to "front-end" programming languages, and back](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20%22web%20languages%22%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#from-back-end-to-front-end-programming-languages-and-back) as a "web language".

For example, in 2011 Dart was officially introduced as a "programming language for building web applications": [Dart: A language for structured web programming ](https://blog.chromium.org/2011/10/dart-language-for-structured.html)

But then I discovered that Dart right from start evolved around its own and rich ecosystem, with "front-end" and "back-end" in mind, which allows Dart source code to run on a variety of plattforms.

So, I came to the conclusion that Dart deserves it's own page. 

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

Page [dart: The Dart command-line tool](https://dart.dev/tools/dart-tool) says this on the _run_ command:

> Prefer dart run.

While _$ dart run random_streams_for_perf_stats.dart_ takes about 220 milliseconds, running this microbenchmark program ("speed part" only) without the _run_ command takes only about 150 milliseconds.

Why is that?

"Big AI" has answers to this, but doesn't tell sources that can explain something. Dart's official documentation is also shy about this phenomenon.

Anyway, I guess it's clear that _dart run <~.dart>_ comes with overhead that _dart <~.dart>_ doesn't have.

It also seems that there's no switch for _dart run_ to shut off the JIT compilation.

### Compilation to JavaScript

Also Dart source code can be explicitely transpiled into JavaScript (nowadays), then executed on node.js like this:

```
$ dart compile js ./random_streams_for_perf_stats.dart  # implicitly compiles with optimization level -O1
$ node ./random_streams_for_perf_stats.js
could not write to file: random_bitstring.bin ! -- Unsupported operation: _Namespace <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$
```

There are 5 optimization levels, from _-O0_ to _-O4_, (see with _$ dart compile js -h -v_).

TBD

### JIT snapshot compilation

A JIT compilation can be snapshot from "a warm JIT" like this:

```
$ dart compile jit-snapshot random_streams_for_perf_stats.dart
$ dart run ./random_streams_for_perf_stats.jit
...
$
```

Here, command _dart run ./random_streams_for_perf_stats.jit_ is substantially slower than command _dart ./random_streams_for_perf_stats.jit_: around 107 milliseconds versus around 38 milliseconds

TBD

### AOT snapshot compilation

An Ahead-of-time compilation can be snapshot like this:

```
$ dart compile aot-snapshot random_streams_for_perf_stats.dart
$ dartaotruntime ./random_streams_for_perf_stats.aot
...
$
```

_dartaotruntime_ provides a minimal Dart runtime for running AOT modules: https://github.com/dart-lang/sdk/blob/8c7e9a045ca46f4430494810a62eddb960e76bc2/README.dart-sdk#L8

..and is the only command which works with AOT modules.

TBD

### Standalone (or self-contained) executable

```
$ dart compile exe ./random_streams_for_perf_stats.dart
$ ./random_streams_for_perf_stats.exe
...
$
```

Running _./random_streams_for_perf_stats.exe_ takes the same time than running the AOT snapshot from above.

TBD

Portability of ./random_streams_for_perf_stats.exe ?!? _bash: ./random_streams_for_perf_stats.exe: cannot execute: required file not found_

--> make a proper Dart project here first: tbd


TBD

### WebAssembly

Nowadays, Dart can be compiled to WebAssembly: [WebAssembly (Wasm) compilation](https://dart.dev/web/wasm)

Though:

> The compiled Wasm output currently targets JavaScript environments (such as browsers), and thus currently doesn't support execution in standard Wasm run-times like wasmtime and wasmer.

```
$ dart compile wasm -Da=1 ./random_streams_for_perf_stats.dart
$ node ./random_streams_for_perf_stats.mjs
$ <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
```

So, command dart compile wasm produces a wrapper JavaScript file named _random_streams_for_perf_stats.mjs_.

TBD



<br/>

##_end
