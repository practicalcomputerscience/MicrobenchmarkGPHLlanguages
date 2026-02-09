2026-02-09: work in progress

# PHP

https://www.php.net/

---

<br/>

Using modern PHP on the [Zend Engine](https://www.zend.com/), a stack-based virtual machine
(same like [WebAssembly](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/05%20-%20%22web%20languages%22%20on%20node.js%2C%20WebAssembly%20and%20Wasmtime#the-webassembly-wasm-virtual-machine))
has become a real hit:

- the "speed part" of the microbenchmark program is executed in around just 15 milliseconds, which is just a little bit slower than ahead-of-time compiled Dart: [Dart execution speeds diagram](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Dart#dart-execution-speeds-diagram)

Sime 2020, PHP can also employ just-in-time (JIT) compilation; see here at: [PHP 8.0: JIT](https://php.watch/versions/8.0/JIT)

Remember from facebook's [Hack](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/30%20-%20languages%20that%20didn't%20make%20it%20to%20my%20list/README.md#hack):

> ...also this language didn't survive, because PHP had its comeback...

<br/>

However, with this microbenchmark program, practically an "on-off script", JIT compilation doesn't move the needle in terms of execution speed:

```
$ time php -d opcache.enable_cli=1 -d opcache.jit_buffer_size=100M -d opcache.jit=1255 ./random_streams_for_perf_stats.php

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.015s
...
$ 
```

TBD

##_end
