# Crystal

https://crystal-lang.org/

https://github.com/crystal-lang/crystal

<br/>

I call Crystal "Compiled Ruby": this language can be **very fast**: [Master diagram with most program environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/02%20-%20execution%20times#master-diagram-with-most-program-environments)

Although, Crytal cannot be used as a system programming language because a standalone executable includes a **runtime** with garbage collection, that is automatic memory management, and other services.

Key for success in execution speed is using Crystal's _**IO::Memory**_ class (https://crystal-lang.org/api/1.18.2/IO/Memory.html) for fast string building:

```
...
bits_x   = IO::Memory.new  # https://crystal-lang.org/reference/1.16/guides/performance.html
...
  bits_x_str = x[i].to_s(2, precision: 16)
  ...
  bits_x << bits_x_str
...
```

<br/>

## Static linking in Crystal

With rare exceptions, compiling source code makes executables (in Linux), which are dynamically linked by default, which means that these executables depend on other libraries during runtime, often in a different Linux system, often only in their right versions: [Portability of programs](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/70%20-%20portability%20from%20Linux%20to%20Linux#portability-of-programs)

Crystal also supports [Static Linking](https://crystal-lang.org/reference/1.20/guides/static_linking.html#static-linking).

First, let's check the (usual) dynamic dependencies:

```
$ crystal build random_streams_for_perf_stats_cr.cr --release  # my usual building command so far with dynamic linking
$ ldd ./random_streams_for_perf_stats_cr
	linux-vdso.so.1 (0x00007ce5ede57000)
	libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007ce5edc68000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007ce5eda00000)
	/lib64/ld-linux-x86-64.so.2 (0x00007ce5ede59000)
$
```

Now, let's build for static linking: 

```
$ crystal build --release --static random_streams_for_perf_stats_cr.cr -o random_streams_for_perf_stats_cr_stat
$ ldd ./random_streams_for_perf_stats_cr_stat
	not a dynamic executable
$ 
```

Sizes of executables in bytes:

- random_streams_for_perf_stats_cr: 1155480
- random_streams_for_perf_stats_cr_stat: 2121320

Execution speed of executables with command: _$ sudo perf stat -r 20 ./random_streams_for_perf_stats_cr..._: (at 32°C ambient temperature!)

- random_streams_for_perf_stats_cr: _0.00855 +- 0.00102 seconds time elapsed  ( +- 11.96% )_
- random_streams_for_perf_stats_cr_stat: _0.007466 +- 0.000159 seconds time elapsed  ( +-  2.13% )_, that's more than -12% 

These results are typical for static linking from my point of view: bigger executable sizes, but lower execution speeds and lower variance of execution speeds in average.

<br/>

##_end
