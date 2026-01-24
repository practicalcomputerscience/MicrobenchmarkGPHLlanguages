# Zig

https://ziglang.org/

https://codeberg.org/ziglang/zig.git

<br/>

> [!NOTE]
> Zig still makes breaking changes, like with _ArrayList_ for a buffer that can change in size in 2025: https://codeberg.org/ziglang/zig/src/branch/master/lib/std/array_list.zig

---

<br/>

## Installation and compilation tips

I installed Zig like this: _$ brew install zig_

<br/>

On my target system (![On configuring building and execution environments](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main#on-configuring-building-and-execution-environments)), I'm compiling Zig programs like this:

```
$ zig build-exe random_streams_for_perf_stats.zig -mcpu=native-avx512f -O ReleaseFast
```

..where switch _-O ReleaseFast_ is essential to build a fast program.

Notice that compiler switch _-mcpu=native-avx512f_ is activated here. Background is this: I test the compiled programs with the memory tester program [Valgrind](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/15%20-%20memory%20leak%20detection%20with%20Valgrind#memory-leak-detection-with-valgrind), and if the Zig program would have been compiled without this switch, it would crash immediately.

My Intel Core i7-11700K @ 3.6GHz desktop CPU still features these 512-bit extensions (great!) to some Single Instruction, Multiple Data (SIMD) instructions: https://en.wikipedia.org/wiki/AVX-512

To not be misunderstood: compiling without this switch, just like _$ zig build-exe random_streams_for_perf_stats.zig_, also compiles to a correctly working program, also on my system, but not for _Valgrind_.

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

##_end
