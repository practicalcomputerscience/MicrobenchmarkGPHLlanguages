// random_bitstring_and_flexible_password_generator.zig
//
// 2025-05-14/15/16/22, 2025-06-01, 2025-07-22: fixing error handling when writing to files without a try-catch construct
//
// build on Ubuntu 24 LTS: $ zig build-exe random_bitstring_and_flexible_password_generator.zig -mcpu=native-avx512f
//                           switch -mcpu=native-avx512f is absolutely needed at the test system,
//                           otherwise and even with an "empty" program, running valgrind would crash immediately!
//                           $ valgrind ./random_bitstring_and_flexible_password_generator
//
// run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator
//
// $ zig version
// 0.14.0
// $
//
//
// lessons:
//   1) "var bits_x: [M1]u8 = undefined;" is not faster than "var bits_x = std.ArrayList(u8).init(allocator);" !!
//   2) experiments on std.heap. ... brought no exe speed improvement: https://zig.guide/standard-library/allocators/
//   3) tests with: zig build-exe -Doptimize=ReleaseFast -fno-single-threaded switches <... .zig>: no faster executables!


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
    const seed2 = @as(f32, @floatFromInt(seed)) / @as(f32, @floatFromInt(std.math.maxInt(u64))) * @as(f32, @floatFromInt(m));
    x[0] = @intFromFloat(seed2);
    // std.debug.print("x[0] = {d}\n", .{x[0]});  // for testing

    var buf16: [16]u8 = undefined;  // for several use cases in this program to hold a "string" of 16 "bits"
    var buf99: [99]u8 = undefined;  // for several use cases in this program to hold a "string" of 99 characters


    const allocator = std.heap.page_allocator;  // for several use cases in this program, not just here

    // this even seems slower:
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer {
    //     const deinit_status = gpa.deinit();
    //     //fail test; can't try in defer as defer is executed after we return
    //     if (deinit_status == .leak) expect(false) catch @panic("TEST FAIL");
    // }

    // not faster either:
    // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    // const allocator = arena.allocator();

    // not faster either:
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // https://www.openmymind.net/learning_zig/heap_memory/
    //
    // build with: $ ... -lc to link with libc
    // const allocator = std.heap.c_allocator;  // for several use cases in this program, not just here
    // => not faster than: const allocator = std.heap.page_allocator;

    var bits_x = std.ArrayList(u8).init(allocator);
    var bits_hex = std.ArrayList(u8).init(allocator);

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
        try bits_x.appendSlice(bits_x_str);

        const bits_hex_str = try std.fmt.bufPrint(&buf16, "{x:0>4}", .{x[i]});  // with padding: a544 --> 2 bytes; type: []u8
        // std.debug.print("bits_hex_str = {s}\n", .{bits_hex_str});  // for testing
        try bits_hex.appendSlice(bits_hex_str);

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



    // make a password of N_CHAR printable chars: user input requested here
    var N_CHAR: i32 = 12;
    var answer: bool = false;
    const stdin = std.io.getStdIn().reader();

    while (!answer) {
        N_CHAR = 12;
        const q1_str = try std.fmt.bufPrint(&buf99, "{}", .{N_CHAR});
        std.debug.print("\nPassword of {s} printable chars OK? 'y' or another integer number >= 8: ", .{q1_str});

        if (try stdin.readUntilDelimiterOrEof(buf99[0..], '\n')) |answer_str| {
            // std.debug.print("\nanswer_str = {s}", .{answer_str});  // for testing
            if ( std.mem.eql(u8, answer_str, "y")) {
                answer = true;
            } else {
                // const int_input = try std.fmt.parseInt(i32, answer_str, 10);  // this crashes on inputs like: 6.6, sadkjg, ...
                // https://github.com/ziglang/zig/blob/0.6.0/test/standalone/guess_number/main.zig
                const int_input = std.fmt.parseInt(i32, answer_str, 10) catch {
                    std.debug.print("enter an integer number >= 8 or 'y'\n", .{});
                    continue;  // this is the trick here!
                };

                // std.debug.print("\nnumber = {d}\n", .{int_input});  // for testing
                N_CHAR = int_input;
                if (N_CHAR < 8) {
                    std.debug.print("enter an integer number >= 8 or 'y'\n", .{});
                } else {
                    answer = true;
                }
            }
        } else {
           std.debug.print("enter an integer number >= 8 or 'y'\n", .{});
        }
    }
    // std.debug.print("\nN_CHAR ={d}---\n", .{N_CHAR});  // for testing

    var WITH_SPECIAL_CHARS = true;
    answer = false;
    while (!answer) {
      std.debug.print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ", .{});
        if (try stdin.readUntilDelimiterOrEof(buf99[0..], '\n')) |answer_str| {

            if ( std.mem.eql(u8, answer_str, "y")) {
                answer = true;
            } else {
                WITH_SPECIAL_CHARS = false;
                answer = true;
            }
        } else {
           std.debug.print("user intput failed...\n", .{});
        }
    }


    // building char_set:
    //   https://zig.guide/standard-library/arraylist
    var char_set = std.ArrayList(u8).init(allocator);

    var utf8_buffer = try allocator.alloc(u8, 1);  // allocate 1 byte for one UTF-8 character
    // https://character-encoding-decoding.mojoauth.com/utf-32-encoding--zig/

    if (WITH_SPECIAL_CHARS) {
      var codepoint: usize = 33;
      while (codepoint <= 127) : (codepoint += 1) {
        utf8_buffer[0] = @intCast(codepoint);

        try char_set.appendSlice(utf8_buffer);
      }
    } else {
      try char_set.appendSlice("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
    }
    // std.debug.print("char_set = {s}\n", .{char_set.items});  // for testing
    // https://stackoverflow.com/questions/77290888/can-i-sprintf-to-an-arraylist-in-zig


    i = 0;             // char counter for the password
    var j: usize = 0;  // char counter for x
    var pw_chars = std.ArrayList(u8).init(allocator);
    var char0_buffer_pw = try allocator.alloc(u8, 1);  // allocate 1 byte for 1 UTF-8 character
    var char1_buffer_pw = try allocator.alloc(u8, 1);
    const char_set1 = try char_set.toOwnedSlice();  // char_set is of type std.ArrayList(u8) --> convert to: []const u8
    // https://ziggit.dev/t/how-to-use-toownedslice-and-alternatives/2465

    while (i < N_CHAR) {
      const bin0 = try std.fmt.bufPrint(&buf16, "{b:0>16}", .{x[j]});  // with padding: 0001100110101101 --> 16 bits; type: []u8
      // std.debug.print("\nbin0 = {s}", .{bin0});  // for testing

      const bin0_0 = try std.fmt.parseInt(u8, bin0[0..8], 2);
      const bin0_1 = try std.fmt.parseInt(u8, bin0[8..16], 2);
      // https://ziggit.dev/t/how-to-convert-numerical-values-to-string-and-vice-versa-from-redit/719/2
      // std.debug.print("\nbin0_0 = {any}, bin0_1 = {any}", .{bin0_0, bin0_1});  // for testing: "bin0_0 = 180, bin0_1 = 5"

      char0_buffer_pw[0] = @as(u8, bin0_0);
      const char0_test = std.mem.containsAtLeast(u8, char_set1, 1, char0_buffer_pw);
      char1_buffer_pw[0] = @as(u8, bin0_1);
      const char1_test = std.mem.containsAtLeast(u8, char_set1, 1, char1_buffer_pw);
      // std.debug.print("\nchar0_test = {any}, char1_test = {any}\n", .{char0_test, char1_test});  // for testing

      if (char0_test) {
        try pw_chars.appendSlice(char0_buffer_pw);
        i += 1;
      }

      if (char1_test and i < N_CHAR) {
        try pw_chars.appendSlice(char1_buffer_pw);
        i += 1;
      }

      j += 1;
    }

    std.debug.print("\nYour password of {d} characters is: {s}\n", .{N_CHAR, pw_chars.items});
}

// end of random_bitstring_and_flexible_password_generator.zig
