2026-02-11: work in progress

I discovered these rather new node.js replacements here: [Languages](https://github.com/bddicken/languages?tab=readme-ov-file#languages) and here: https://benjdd.com/languages2/

..and just wanted to compare their execution times with of one of [TypeScript](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/05%20-%20web%20programming/05a%20-%20web%20languages%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime/random_streams_for_perf_stats.ts) on node.js: [Complete execution speeds diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05a%20-%20web%20languages%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#complete-execution-speeds-diagram)

<br/>

# Deno

https://deno.com/, which uses [Safari browser's](https://www.apple.com/safari/) JavaScriptCore engine

I installed Deno with: _$ curl -fsSL https://deno.land/install.sh | sh_, and added to _~/.bashrc_ command: _. $HOME/.deno/env_

After a Bash restart, I can run the microbenchmark program ("speed part") in its TypeScript variant like this: _$ deno run --unstable-detect-cjs --allow-write ./random_streams_for_perf_stats.ts_

The script execution time is about 45 milliseconds (_$ multitime -n 20 ..._), which is faster then running it on node.js: [Why is the TypeScript variant slower than the equivalent JavaScript variant?](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20web%20programming/05a%20-%20web%20languages%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#why-is-the-typescript-variant-slower-than-the-equivalent-javascript-variant)

As advertised, I can compile this TypeScript script into a standalone executable like this:

```
$ deno compile --unstable-detect-cjs --allow-write --no-check \
-o random_streams_for_perf_stats_ts_deno ./random_streams_for_perf_stats.ts
...
$ ./random_streams_for_perf_stats_ts_deno
...
$
```

Switch _--unstable-detect-cjs_ is needed to suppress this error:

```
error: Uncaught (in promise) ReferenceError: require is not defined
const fs = require('fs');
...
```

Switch _--allow-write_ is needed to automatically allow writing to files, and switch _--no-check_ is needed to suppress this error:

```
...
TS18046 [ERROR]: 'ex' is of type 'unknown'.
            console.error(`could not write to file: ${file_bits_x} ! -- ${ex.message}`);
...
```

As usual, this executable has numerous Linux dependencies, which may hinder good portability:

```
$ ldd ./random_streams_for_perf_stats_ts_deno
	linux-vdso.so.1 (0x000072b0cc8ec000)
	libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x000072b0cc8ca000)
	libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x000072b0cc89c000)
	librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x000072b0cc897000)
	libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x000072b0cc892000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x000072b0c6f17000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x000072b0c6c00000)
	/lib64/ld-linux-x86-64.so.2 (0x000072b0cc8ee000)
$ 
```

With about 43 milliseconds, the execution time of the standalone executable is a little bit faster compared to the _$ deno run_ command with around 45 milliseconds.

It's file size is big with 92,477,439 bytes.

<br/>

# Bun

https://bun.com/, which uses the V8 JavaScript engine, same like node.js

Let's do the same with Bun. Install like this: _$ curl -fsSL https://bun.sh/install | bash_, add this to your _~/.bashrc_ (and restart the shell then):

```
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
```

Now, let's run the script on Bun: 

```
$ bun ./random_streams_for_perf_stats.ts

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte
$
```

The script works out of the box! And execution time is excellent:

```
$ time bun ./random_streams_for_perf_stats.ts
...
real	0m0.024s
...
$
```

But it's getting even better. I manually set output file _random_bitstring.bin_ to "read-only", and see how exception handling works here.

The error message is written in red color in the Bash shell:

$\textcolor{red}{could\ not\ write\ to\ file:\ random\\_bitstring.bin\ !\ --\ EACCES:\ permission\ denied,\ open\ 'random\\_bitstring.bin'}$

Deno is not doing this, node.js is not doing this, and none of the other programming languages I've tested so far!

Let's compile this script also into a standalone executable. See from: [Optimization -- Bytecode Caching](https://bun.com/docs/bundler/bytecode#with-standalone-executables), Speed up JavaScript execution with bytecode caching in Bun’s bundler:

```
$ bun build ./random_streams_for_perf_stats.ts --compile --bytecode --outfile=random_streams_for_perf_stats_ts_bun
...
$ ./random_streams_for_perf_stats_ts_bun
...
$
```

There are some less dependencies:

```
$ ldd ./random_streams_for_perf_stats_ts_bun
	linux-vdso.so.1 (0x00007ffdae15c000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007915fcc00000)
	/lib64/ld-linux-x86-64.so.2 (0x00007915fce89000)
	libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007915fce70000)
	libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007915fce6b000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007915fcb17000)
$ 
```

With about 22 milliseconds, the execution time of the standalone executable is also a little bit compared to the _$ bun_ command with around 23 milliseconds.

It's file size is also big with 102,377,912 bytes.

<br/>

## Microbenchmark program in TypeScript: execution speeds on Bun, Deno and node.js diagram

TBD

<br/>

##_end
