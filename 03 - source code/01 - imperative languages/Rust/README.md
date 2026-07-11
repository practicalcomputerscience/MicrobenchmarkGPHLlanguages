# Rust

https://www.rust-lang.org/

See the related language description slide [Part 1](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/01%20-%20presentation%20slides#part-1) for what things I say about Rust.

<br/>

I installed and shortly tested Rust in Ubuntu 24 LTS like this:

```
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
...
$ source "$HOME/.cargo/env"
$ rustc --version
rustc 1.92.0 (ded5c06cf 2025-12-08)
$
```

<br/>

## Static linking in Rust

Static linking in Rust is not too complicated.

At first, install target _x86_64-unknown-linux-musl_ in your project:

```
$ cd random_streams_for_perf_stats  # change into Rust project
$ rustup target add x86_64-unknown-linux-musl  # install this target
info: downloading component 'rust-std' for 'x86_64-unknown-linux-musl'
info: installing component 'rust-std' for 'x86_64-unknown-linux-musl'
 36.6 MiB /  36.6 MiB (100 %)  20.8 MiB/s in  1s
$ cargo build -r -v --target=x86_64-unknown-linux-musl
...
$ ldd ./target/x86_64-unknown-linux-musl/release/random_streams_for_perf_stats  # check dependencies
	statically linked
$ time ./target/x86_64-unknown-linux-musl/release/random_streams_for_perf_stats  # make a first exe speed check

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.046s
...
$
```

46 milliseconds is a dud! (even at 31°C ambient temperature as of 2026-06-29!). Here's the established and dynamically linked version:

```
$ time ./target/release/random_streams_for_perf_stats

generating a random bit stream...
Bit stream has been written to disk under name:  random_bitstring.bin
Byte stream has been written to disk under name: random_bitstring.byte

real	0m0.010s
...
$
```

Both versions of executable _random_streams_for_perf_stats_ passed the portability test on a different target Linux system. The dynamically linked version doesn't depend on exotic shared objects:

```
$ ldd ./target/release/random_streams_for_perf_stats
	linux-vdso.so.1 (0x000077ff16804000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x000077ff16400000)
	/lib64/ld-linux-x86-64.so.2 (0x000077ff16806000)
	libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x000077ff16756000)
$
```

<br/>

## Criticism of Rust

Rust may not be well suitable for programming of high-performance computer games, even though a couple of frameworks and engines exist:

**2024: Leaving Rust gamedev after 3 years**

https://loglog.games/blog/leaving-rust-gamedev/

<br/>

**2022: Minimalism in Programming Language Design**

https://pointersgonewild.com/2022/05/23/minimalism-in-programming-language-design/

> Both at the syntactic and semantic level, Rust is a very complex language. The syntax can get very verbose, and there's a lot to know, a lot of rules and unintuitive subtleties about what you can and can't do where. The learning curve is steep and the cognitive load is high.

<br/>

##_end
