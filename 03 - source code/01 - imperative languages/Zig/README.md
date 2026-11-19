# Zig

https://ziglang.org/

https://codeberg.org/ziglang/zig.git

<br/>

> [!NOTE]
> Zig still makes breaking changes, like with _ArrayList_ for a buffer that can change in size in 2025: https://codeberg.org/ziglang/zig/src/branch/master/lib/std/array_list.zig

---

<br/>

## Installation and compilation tips and Intel Advanced Vector Extensions 512 (AVX-512)

I installed Zig like this: _$ brew install zig_

<br/>

On my testing system (![On configuring building and execution environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments)), I'm compiling Zig programs like this:

```
$ zig build-exe random_streams_for_perf_stats.zig -mcpu=native-avx512f -O ReleaseFast
```

..where switch _-O ReleaseFast_ is essential to build a fast program.

Notice that compiler switch _-mcpu=native-avx512f_ is activated here. Background is this:

- I test the compiled programs with the memory tester program [Valgrind](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/15%20-%20memory%20leak%20detection%20with%20Valgrind#memory-leak-detection-with-valgrind), and if the Zig program would have been compiled without this switch, it would crash immediately.

Compiler switch _-mcpu=native-avx512f_ subtracts (-) the _avx512f_ feature. Because every other AVX-512 extension (_avx512vl_, _avx512bw_, _avx512dq_, etc.) depends on _avx512f_, disabling _avx512f_ automatically disables the entire AVX-512 instruction family in LLVM (Google AI). This also allows good portability of such a Zig executable to Linux systems with an Intel cpu without AVX-512 support.

The Intel Core i7-11700K @ 3.6GHz desktop cpu of my testing system still features these 512-bit extensions (great!) to some Single Instruction, Multiple Data (SIMD) instructions: https://en.wikipedia.org/wiki/AVX-512

To not be misunderstood: compiling without this switch, just like _$ zig build-exe random_streams_for_perf_stats.zig -O ReleaseFast_, also compiles to a correctly working executable, also on my testing system, but not for _Valgrind_.

At Chapel, I've written more background about these AVX-512 instructions: [x86-64 microarchitecture levels](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Chapel#x86-64-microarchitecture-levels)

<br/>

## On error handling 

Initially I had source code like this when writing the strings to their files:

```
...
    const file1 = try std.fs.cwd().createFile(file_bits_x, .{ .read = true },);
    file1.writeAll(bits_x.items) catch |err| {
        std.debug.print("could not write to file: {s} ! -- {any}\n", .{file_bits_x, err});
        return;
    };
    std.debug.print("Bit stream has been written to disk under name:  {s}\n", .{file_bits_x});
    file1.close();
...
```

However, this code only works correctly when everything is OK. It fails when there are problems with writing a string to a file. 

After some experimentation, including "AI prompt engineering", I gave up on _try-catch_ constructs and turned to conservative _if-then-else_ constructs:

```
...
    if (std.fs.cwd().createFile(file_bits_x, .{},)) |file| {
        defer file.close();
        if (file.writeAll( bits_x.items)) {
            std.debug.print("Bit stream has been written to disk under name:  {s}\n", .{file_bits_x});
        } else |err| {
            std.debug.print("could not write to file: {s} ! -- {any}\n", .{file_bits_x, err});
        }
    } else |err| {
        std.debug.print("could not create file: {s} ! -- {any}\n", .{file_bits_x, err});
    }
...
```

It was this comment which showed me the way to success: https://github.com/ziglang/zig/issues/5421#issuecomment-633720665

<br/>

#### Regular Expressions in Zig

At the moment (as of 2026-06-12), Zig doesn't support regular expressions natively. Though, some 3rd party libraries have been implemented, like for example:

- [zig-regex](https://github.com/zig-utils/zig-regex)
- [zig-regex](https://github.com/tiehuis/zig-regex)

Similar to [Regular expressions in Mojo](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Mojo#regular-expressions-in-mojo), also here I've decided against using them.

<br/>

##_end
