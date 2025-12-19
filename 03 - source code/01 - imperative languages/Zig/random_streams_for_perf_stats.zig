// random_streams_for_perf_stats.zig
//
// 2025-06-01, 2025-07-22: fixing error handling when writing to files without a try-catch construct
// 2025-12-18: see below
//
// build on Ubuntu 24 LTS: $ zig build-exe random_streams_for_perf_stats.zig -mcpu=native-avx512f -O ReleaseFast
//                           switch -mcpu=native-avx512f is absolutely needed at the test system,
//                           otherwise and even with an "empty" program, running valgrind would crash immediately!
//
// run on Ubuntu 24 LTS:   $ sudo perf stat -r 20 ./random_streams_for_perf_stats
//
// $ zig version
// 0.15.2
// $


const std = @import("std");

pub fn main() !void {  // ! --> error handling: allow this function to return an error.
                       // So, there's some "try" in the source code below which wants to return a potential error.

    const END: usize = 62501;  // 62501 for exactly 1M binary digits; usize is same like the Rust solution
    // const END: usize = 12;  // for testing

    // const M1: usize = 1_000_000;  // 1_000_000 / 62500 = 16
    // const M1: usize = (END-1) * 16;
    // const K250: usize = 250_000;
    // const K250: usize = (END-1) * 4;

    const m: usize = 65521;  // = 2^16 - 15
    const a: usize = 17364;
    const c: usize = 0;

    const file_bits_x   = "random_bitstring.bin";
    const file_bits_hex = "random_bitstring.byte";


    var x: [END]usize = undefined;  // allocate memory
    // also needed for the password


    // OK:
    // const seed = blk: {
    //     var seed: u64 = undefined;
    //     try std.posix.getrandom(std.mem.asBytes(&seed));
    //     break :blk seed;
    // };
    // var prng = std.Random.DefaultPrng.init(seed);  // OK!!
    // https://matklad.github.io/2025/03/19/comptime-zig-orm.html
    // https://zig.guide/master/standard-library/random-numbers/
    // x[0] = prng.random().uintLessThan(usize, m);  // OK!!
    // std.debug.print("x[0] = {d}\n", .{x[0]});  // for testing

    // so, why not using std.posix.getrandom(std.mem.asBytes(&seed)) directly as a random seed for x[0]? =>
    const seed = blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    };

    // bringing seed/u64 into usize with maximum value m: type casting
    //   https://zig.guide/language-basics/floats/
    // 2025-12-18:
    const seed2 = @as(f32, @floatFromInt(seed)) / @as(f32, @floatFromInt(std.math.maxInt(u64))) * @as(f32, @floatFromInt(m-1)) + @as(f32, @floatFromInt(1));

    x[0] = @intFromFloat(seed2);
    // std.debug.print("x[0] = {d}\n", .{x[0]});  // for testing

    var buf16: [16]u8 = undefined;  // for several use cases in this program to hold a "string" of 16 "bits"


    const allocator = std.heap.page_allocator;  // for several use cases in this program, not just here

    // var bits_x   = std.array_list.Managed(u8).init(allocator);  // 2025-12-18:
    //                                                             // 3.83 milliseconds from 3 * 20 runs
    // var bits_hex = std.array_list.Managed(u8).init(allocator);  // 2025-12-18
    // extremely high variability, for example: 0.00509 +- 0.00175 seconds time elapsed  ( +- 34.39% )
    
    var bits_x:   std.ArrayList(u8) = .empty;  // 2025-12-18: 4.00 milliseconds from 3 * 20 runs: take this version for lower variability
    defer bits_x.deinit(allocator);            // 2025-12-18
    var bits_hex: std.ArrayList(u8) = .empty;  // 2025-12-18
    defer bits_hex.deinit(allocator);          // 2025-12-18
    

    std.debug.print("\ngenerating a random bit stream...\n", .{});
    var i: usize = 1;
    while (i < END) : (i += 1) {
        x[i] = (a*x[i-1] + c) % m;
        // std.debug.print("  {d:5}\n", .{x[i]});  // for testing; formatted printing
        // https://zig.guide/standard-library/advanced-formatting/

        const bits_x_str = try std.fmt.bufPrint(&buf16, "{b:0>16}", .{x[i]});  // with padding: 0001100110101101 --> 16 bits; type: []u8
        // https://github.com/ziglang/zig/issues/20078
        // std.debug.print("@typeName(bits_x_str) = {s}\n", .{@typeName(@TypeOf(bits_x_str))}); // for testing
        // https://ziggit.dev/t/how-typeof-chooses-the-resulting-type-when-there-are-more-than-one-arguments/3265
        // std.debug.print("bits_x_str = {s}\n", .{bits_x_str});  // for testing
        
        try bits_x.appendSlice(allocator, bits_x_str);  // 2025-12-18
        // try bits_x.appendSlice(bits_x_str);  // 2025-12-18


        const bits_hex_str = try std.fmt.bufPrint(&buf16, "{x:0>4}", .{x[i]});  // with padding: a544 --> 2 bytes; type: []u8
        // std.debug.print("bits_hex_str = {s}\n", .{bits_hex_str});  // for testing
        
        try bits_hex.appendSlice(allocator, bits_hex_str);  // 2025-12-18
        // try bits_hex.appendSlice(bits_hex_str);  // 2025-12-18

    }
    // std.debug.print("\nbits_x = {s}\n", .{bits_x.items});  // for testing
    // std.debug.print("bits_hex = {s}\n", .{bits_hex.items});  // for testing

    // for testing:
    // for (bits_x) |value| {
    //     std.debug.print("{any}", .{value});
    // }
    // std.debug.print("\n", .{});
    // for (bits_hex) |value| {
    //     std.debug.print("{any}", .{value});
    // }

    // write bit stream to disk:
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
    // https://github.com/ziglang/zig/issues/5421#issuecomment-633720665

    // write byte stream to disk:
    if (std.fs.cwd().createFile(file_bits_hex, .{},)) |file| {
        defer file.close();
        if (file.writeAll( bits_hex.items)) {
            std.debug.print("Byte stream has been written to disk under name: {s}\n", .{file_bits_hex});
        } else |err| {
            std.debug.print("could not write to file: {s} ! -- {any}\n", .{file_bits_hex, err});
        }
    } else |err| {
        std.debug.print("could not create file: {s} ! -- {any}\n", .{file_bits_hex, err});
    }

}

// end of random_streams_for_perf_stats.zig
