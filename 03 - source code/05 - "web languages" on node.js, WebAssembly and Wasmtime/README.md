2026-01-31: work in progress

- Wasmtime for compiling to "compiled wasm" (~.cwasm) files?
- ReScript?? TBD

# From "back-end" to "front-end" programming languages

Table of contents:

- [Idea for this page](#idea-for-this-page)
- [TypeScript and JavaScript](#typescript-and-javascript)
- [Why is the TypeScript variant slower than the equivalent JavaScript variant?](#why-is-the-typescript-variant-slower-than-the-equivalent-javascript-variant)
- [The WebAssembly (Wasm) virtual machine](#the-webassembly-wasm-virtual-machine)
- [The Wasmtime runtime](#the-wasmtime-runtime)


<br/>

---

## Idea for this page

Originally, this page was only meant to show some quick implementations of the "speed part" of the microbenchmark program to be executed on [node.js](https://nodejs.org/en).
"Web programming" was not even on my long list. However, this has changed for two reasons:

- the transpilation from Standard ML to JavaScript with [LunarML](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML#transpiling-from-standard-ml-to-lua-and-javascript-with-lunarml), resulting in a monster big file that contains ES (ECMAScript) modules (~.mjs): [random_streams_for_perf_stats.mjs](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML/random_streams_for_perf_stats.mjs), and
- my [Groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy#groovy) implementation, with Groovy often being described as a "scripting language for the Java Virtual Machine", and which "can largely be viewed as a superset of Java": [Introducing Groovy](https://www.oracle.com/technical-resources/articles/java/groovy.html)

<br/>

From that point on, it was only a small step to transpile program [random_streams_for_perf_stats.groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy/random_streams_for_perf_stats.groovy), which is here type annotated for speedy, static compilation,
with the help of Duck.ai (because the [tsc compiler](https://manpages.debian.org/testing/node-typescript/tsc.1.en.html), version 5.9.3, tumbled over warnings):

- first, into [TypeScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/05%20-%20node.js%20for%20%22web%20languages%22/random_streams_for_perf_stats.ts), and
- then from there into [JavaScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/05%20-%20node.js%20for%20%22web%20languages%22/random_streams_for_perf_stats.js), again with Duck.ai

<br/>

From the "old school combo" JavaScript and node.js, it was then again only a minor step into WebAssembly, and there another, albeit bigger step into Wasmtime.

With WebAssembly and specifically Wasmtime, where source code has a good chance to no longer "meet" some JavaScript code, the line between "back-end" and "front-end" programming languages
has become blurry in the last couple of years.

<br/>

## TypeScript and JavaScript

JavaScript is a "a dynamic just-in-time compiled language": https://www.assemblyscript.org/introduction.html#frequently-asked-questions

<br/>

The TypeScript script should work out of the box for node.js version 22.21.0 or higher (_$ node -v_). Version 18.19.1, coming as standard with Ubuntu 24 LTS for example, is too old for it.

In Linux you can upgrade the node.js version with the nvm, the **Node Version Manager** which allows a node.js installation per Linux user, like this, see from here: https://linux.how2shout.com/how-to-install-nvm-on-ubuntu-24-04-or-22-04-linux/:

```
$ sudo apt install curl build-essential libssl-dev -y
...
$ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash
...
$ source ~/.bashrc  # re-activate your Bash shell configuration
$ nvm --version  # check nvm version
0.40.4
$ nvm install --lts  # install latest long term support version
...
$ nvm ls  # check all locally available versions, including the default version
...
$ node -v
v24.13.0  # this version is pretty modern (though, I'm still using v.22.21.0 for official benchmarking)
$ node ./random_streams_for_perf_stats.ts

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

Voilà!

I think this is the easiest way before getting into another version hell to make this little TypeScript script running in older node.js environments.

<br/>

This leaves this question to me: 

## Why is the TypeScript variant slower than the equivalent JavaScript variant?

![plot](./mean_stddev_err_whiskers%20--%20web%20programming,%20JS+TS.png)

Transpiling from TypeScript code into JavaScript code, even though with the transpiler being re-written in [Go](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/60%20-%20the%20future%20of%20transpiling#microsofts-efforts-with-transpilation) some day, comes with an overhead, which takes more time to process - even with this little example obviously.

By the way: you can switch to a specific node.js version like this:

```
$ nvm use 22.21.0
Now using node v22.21.0 (npm v10.9.4)
$ node -v
22.21.0
$
```

I checked both script versions with node.js versions 22.21.0, 24.13.0 and 25.5.0. Program execution times are about the same:

- JavaScript, mean in milliseconds (one batch with 20 runs): 42, 39, 41
- TypeScript, mean in milliseconds (one batch with 20 runs): 64, 64, 66

<br/>

Otherwise this should hold:

> While TypeScript introduces an additional compilation step, its impact on runtime performance is negligible, as both TypeScript and JavaScript execute similarly in modern engines.

from: Performance Benchmarking: TypeScript vs. JavaScript in Modern Web Development, Juliana George, Date: 03/20: https://www.researchgate.net/publication/389555848_Performance_Benchmarking_TypeScript_vs_JavaScript_in_Modern_Web_Development

<br/>

## The WebAssembly (Wasm) virtual machine

There's another (cheap) possibility to run the microbenchmark program on node.js, and that is compiling it into a WebAssembly binary file (~.wasm), which is then being called from a machine generated JavaScript file: [Node.js with WebAssembly](https://nodejs.org/en/learn/getting-started/nodejs-with-webassembly)

I took the [C version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) of the microbenchmark program ("speed part" only) **unchanged**, and compiled it to a WebAssembly binary file with the help of the [Emscripten](https://emscripten.org/) compiler toolchain, which is using the LLVM compiler infrastructure.

The Emscripten compiler is being called from the emcc (Emscripten Compiler Frontend), and "is effectively a drop-in replacement for a standard compiler like gcc or clang": [Emscripten Compiler Frontend (emcc)](https://emscripten.org/docs/tools_reference/emcc.html)

For convenience, I installed the Emscripten compiler with the Homebrew package manager (again):

```
$ brew install emscripten
...
$ emcc -v
emcc (Emscripten gcc/clang-like replacement + linker emulating GNU ld) 4.0.24-git
clang version 23.0.0git
Target: wasm32-unknown-emscripten
Thread model: posix
InstalledDir: /home/linuxbrew/.linuxbrew/Cellar/emscripten/5.0.0/libexec/llvm/bin
$
```

You may have a view at the [Emscripten Compiler Settings](https://emscripten.org/docs/tools_reference/settings_reference.html).

One of the settings is essential here according to my experiments, that is _-s STACK_SIZE=2048000_ (for example) to have a big enough stack size.

<br/>

Then, in the _~/.bashrc_ configuration file I commented out any possible paths to programs that have been installed with Homebrew, because they may block nvm (see above) from being found in your paths:

```
# eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
```

At this point, you may have to **re-start** the Bash shell to make the changed configuration becoming fully effective.

Now, emcc is not visible anymore, but I will call it with an absolute path (see below).

<br/>

With source code file [random_streams_for_perf_stats.c](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.c) being located in a WebAssembly working directory, it can now be compiled like this, including clang compiler optimization switch _-O3_:

```
$ /home/linuxbrew/.linuxbrew/bin/emcc -O3 random_streams_for_perf_stats.c -s WASM=1 \
-o random_streams_for_perf_stats_wasm.js -s STACK_SIZE=2048000
...
$
```

This command has hopefully created two files:

- _random_streams_for_perf_stats_wasm.js_, and
- _random_streams_for_perf_stats_wasm.wasm_

(output file name _random_streams_for_perf_stats_wasm.js_ was just chosen here to not mix it with the native JavaScript microbenchmark program _random_streams_for_perf_stats.js_, see above)

Now, we can run this WebAssembly binary file, which is being called from the generated JavaScript file, which serves as the "glue" code:

```
$ node ./random_streams_for_perf_stats_wasm.js

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$ 
```

As seen in the diagram above, a compilation to a WebAssembly binary file can make a speedier program, but with an execution time of around 30 milliseconds it's not a quantum leap into the league of super-fast programming languages with this microbenchmark program.

A reason for this may be the fact that by default WebAssembly binary files are not ahead-of-time (AOT) compiled; see **Wasmtime** for compiling to "compiled wasm" (~.cwasm) files: https://docs.wasmtime.dev/cli-options.html#compile

<br/>

A WebAssembly binary file can also be compiled from other sources, like:

- C++, also with the emscripten compiler, or
- [Rust](https://rust-lang.org/what/wasm/), or
- [AssemblyScript](https://www.assemblyscript.org/)

<br/>

Actually, the "WebAssembly binary file" does not represent native, binary machine code (like compiled from C source code for example). It is a standardized bytecode format, which is being
executed on a stack-based virtual machine inside a web browser or a runtime environment like node.js: https://webassembly.org/

Though, WebAssembly apparently seems to avoid terms "bytecode" and "virtual machine", see for example at pages [Overview](https://webassembly.github.io/spec/core/intro/overview.html) and [Conventions](https://webassembly.github.io/spec/core/binary/conventions.html).

<br/>

## The Wasmtime runtime

Since I mentioned [Wasmtime](https://wasmtime.dev/) above, this question emerged after some experimentation:

> Can this microbenchmark program be compiled into a WebAssembly binary file, which can then be executed in the Wasmtime runtime?

Yes!

But it took me a while to find a solution, and not with the emcc (Emscripten Compiler Frontend), but with LLVM's clang compiler directly and - again - a little source code change in the original C program.

<br/>

Here's a guide for compiling and running the speed part of the microbenchmark program in its adapted C version with Wasmtime:

First, make sure that the Homebrew installed programs and environments are available again (see above):

```
$ clang -v  # make sure that LLVM's clang compiler is accessible
Homebrew clang version 21.1.8
...
$
```

In the [modified C program](./random_streams_for_perf_stats_wasmtime.c) I had to use a user defined function for the conversion of the random integer numbers into their binary representation as a string, same like in the [Checked C version](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/eb055d399cdd454982c7cb4b93c65c6e2e18f4ea/03%20-%20source%20code/01%20-%20imperative%20languages/C/random_streams_for_perf_stats.checked_c.c#L62).

It looks like that this funtionality, that is the notorious format specifier _"%016b"_, is not yet implemented in the WASI libc implementation for WebAssembly: https://github.com/WebAssembly/wasi-libc

```
    ...
    // sprintf(bits_x_str, "%016b", x[i]);  // this is not working (yet) for Wasmtime!
    ...
    integer_to_bin_string(x[i], bits_x_str);  // fixed version from random_streams_for_perf_stats.checked_c.c
    ...
```

<br/>

WASI = WebAssembly System Interface, a group of standards-track API specifications for software compiled to the WebAssembly standard: https://wasi.dev/

<br/>

Then install Wasmtime, WASI C-header files for LLVM and WASI runtimes for LLVM:

```
$ curl https://wasmtime.dev/install.sh -sSf | bash
...
$ wasmtime -V  # just check version
wasmtime 41.0.1 (c30fce86b 2026-01-26)
$ brew install wasi-libc  # install WASI C-header files for LLVM
...
$ brew install wasi-runtimes  # install WASI runtimes for LLVM
...
$
```

With hopefully all pre-requisites being fulfilled by now, program [random_streams_for_perf_stats_wasmtime.c](./random_streams_for_perf_stats_wasmtime.c) is to be compiled into a WebAssembly binary file _random_streams_for_perf_stats.wasm_:

```
$ clang -O3 --target=wasm32-wasi random_streams_for_perf_stats_wasmtime.c \
-o random_streams_for_perf_stats.wasm \
-Wl,--export=main -Wl,-z,stack-size=2048000
$
```

This resulting WebAssembly file is then being executed like this:

```
$ wasmtime --dir=. random_streams_for_perf_stats.wasm
```

I discovered the essential _--dir=._ parameter in this page: [Executing in Wasmtime](https://github.com/bytecodealliance/wasmtime/blob/main/docs/WASI-tutorial.md#executing-in-wasmtime).

<br/>

Here's the updated execution speeds diagram with the results from WebAssembly and Wasmtime, both being competitively fast in comparison to other [natively compiled to machine code languages](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments):

![plot](./mean_stddev_err_whiskers%20--%20web%20programming,%20full.png)

"Standard ML to JS / node.js" = transpiled from Standard ML to JavaScript with the [LunarML transpiler](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/02%20-%20functional%20languages/Standard%20ML#transpiling-from-standard-ml-to-lua-and-javascript-with-lunarml), where the JavaScript code is then being executed and time measured on node.js

<br/>

##_end
