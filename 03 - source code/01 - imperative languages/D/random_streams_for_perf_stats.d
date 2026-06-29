/*
random_streams_for_perf_stats.d

2026-01-17
2026-06-29: switching from gdc to ldc2 compiler for a faster executable

build on Ubuntu 24 LTS: $ ldc2 ./random_streams_for_perf_stats.d -of=random_streams_for_perf_stats_ldc2  # for development
                        $ ldc2 ./random_streams_for_perf_stats.d -of=random_streams_for_perf_stats_ldc2 --O3  # for production

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_ldc2


$ ldc2 --version
LDC - the LLVM D compiler (1.36.0):
  based on DMD v2.106.1 and LLVM 17.0.6
  built with LDC - the LLVM D compiler (1.36.0)
  Default target: x86_64-pc-linux-gnu
  Host CPU: rocketlake
...
$


partly translated from random_streams_for_perf_stats.cpp with Big AI,
though manual fine-tuning was needed

*/


import std.stdio;
import std.format;
import std.random;
import std.file;
import std.array : appender;


// Constants and Macros
enum END = 62501;  // Equivalent to #define END
// enum END = 10;  // for testing
// enum M1   = END * 16;
// enum K250 = END * 4;

enum m = 65521;
enum a = 17364;
enum c = 0;

const string file_bits_x = "random_bitstring.bin";
const string file_bits_hex = "random_bitstring.byte";


int main() {
    int main_return_val = 0;

    int[] x = new int[](END);  // use dynamic array (equivalent to std::vector)

    auto rnd = Random(unpredictableSeed);  // seed the random number generator
    x[0] = uniform(1, m, rnd); // random number between 1 and m-1

    auto bits_x = appender!string();
    char[] bits_x_str = new char[16];

    auto bits_hex = appender!string();
    char[] bits_hex_str = new char[4];

    writeln("\ngenerating a random bit stream...");
    for (int i = 1; i < END; i++) {
        x[i] = ((a * x[i - 1]) + c) % m;
        // writeln("\nx[i] = ", x[i]);  // for testing

        bits_x_str[0 .. 16] = format("%016b", x[i]);  // formatting: %016b for 16-bit binary
        // writeln("bits_x_str = ", bits_x_str);  // for testing
        bits_x.put(bits_x_str);

        bits_hex_str[0 .. 4] = format("%04x", x[i]);  // formatting: %04x for 4-digit hex
        // writeln("bits_hex_str = ", bits_hex_str);  // for testing
        bits_hex.put(bits_hex_str);
    }


    // writeln("\nbits_x = ", bits_x);  // for testing
    // writeln("bits_hex = ", bits_hex);  // for testing
    string bits_x_str_total = bits_x.data;
    string bits_hex_str_total = bits_hex.data;


    // Write bit stream to disk:
    try {
        std.file.write(file_bits_x, bits_x_str_total);
        writeln("Bit stream has been written to disk under name:  ", file_bits_x);
    } catch (Exception e) {
        stderr.writeln("could not write to file: ", file_bits_x, " !");
        main_return_val = 1;
    }

    // Write byte stream to disk:
    try {
        std.file.write(file_bits_hex, bits_hex_str_total);
        writeln("Byte stream has been written to disk under name: ", file_bits_hex);
    } catch (Exception e) {
        stderr.writeln("could not write to file: ", file_bits_hex, " !");
        main_return_val = 1;
    }

    return main_return_val;
}

// end of random_streams_for_perf_stats.d
