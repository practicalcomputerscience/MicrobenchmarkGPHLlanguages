/*
random_bitstring_and_flexible_password_generator.d

2026-01-21
2026-06-13: refactored from char_set to pattern (for regular expressions)
2026-06-29: switching from gdc to ldc2 compiler for a faster executable


build on Ubuntu 24 LTS: $ ldc2 ./random_bitstring_and_flexible_password_generator.d -of=random_bitstring_and_flexible_password_generator_ldc2  # for development
                        $ ldc2 ./random_bitstring_and_flexible_password_generator.d -of=random_bitstring_and_flexible_password_generator_ldc2 --O3  # for production

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator_ldc2


$ ldc2 --version
LDC - the LLVM D compiler (1.36.0):
  based on DMD v2.106.1 and LLVM 17.0.6
  built with LDC - the LLVM D compiler (1.36.0)
  Default target: x86_64-pc-linux-gnu
  Host CPU: rocketlake
...
$


partly translated from random_bitstring_and_flexible_password_generator.cpp with Big AI,
though manual fine-tuning was needed

*/


import std.stdio;
import std.format;
import std.random;
import std.file;
import std.array : appender;
import std.string;
import std.conv;
import std.algorithm;
import std.regex;  // 2026-06-13


// Constants and Macros
enum END = 62501;  // Equivalent to #define END
// enum END = 20;  // for testing
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


    // make a password of N_CHAR printable chars: user input requested here
    int N_CHAR = 12;
    bool answer = false;
    string answer_str;

    while (!answer) {
        writef("\nPassword of %d printable chars OK? 'y' or another integer number >= 8: ", N_CHAR);
        answer_str = readln().strip();

        if (answer_str == "y") {
            answer = true;
        } else {
            try {
                N_CHAR = to!int(answer_str);
                if (N_CHAR < 8) {
                    write("enter an integer number >= 8 or 'y'\n");  // 2026-06-13
                } else {
                    answer = true;
                }
            } catch (ConvException e) {
                write("enter an integer number >= 8 or 'y'\n");  // 2026-06-13
            }
        }
    }
    // writeln("\nN_CHAR = ", N_CHAR);  // for testing


    bool WITH_SPECIAL_CHARS = true;
    answer = false;
    while (!answer) {
        write("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
        answer_str = readln().strip();

        if (answer_str == "y") {
            answer = true;
        } else {
            WITH_SPECIAL_CHARS = false;
            answer = true;
        }
    }
    // writeln("\nWITH_SPECIAL_CHARS = ",WITH_SPECIAL_CHARS);  // for testing


    // 2026-06-13: old solution:
    //   string char_set;
    //   if (WITH_SPECIAL_CHARS) {
    //       foreach (i; 33 .. 127) {
    //           char_set ~= cast(char)i;
    //       }
    //   } else {
    //       char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    //   }
    // writeln("\nchar_set = ", char_set);  // for testing


    // 2026-06-13: new solution with regular expressions:
    auto print_re = regex(r"[!-~]");
    auto alnum_re = regex(r"[A-Za-z0-9]");
    auto pattern  = WITH_SPECIAL_CHARS ? print_re : alnum_re;  // using the ternary operator (Google AI)


    string pw_chars;
    char[] bin0   = new char[16];
    char[] bin0_0 = new char[8];
    char[] bin0_1 = new char[8];

    int i = 0;  // char counter for the password
    int j = 0;  // counter for x

    while (i < N_CHAR) {
        // writeln("\nx[j] = ", x[j]);  // for testing
        bin0[0 .. 16] = format("%016b", x[j]);  // formatting: %016b for 16-bit binary
        // writeln("bin0 = ", bin0);  // for testing

        bin0_0[0 .. 8] = bin0[0 .. 8];
        bin0_1[0 .. 8] = bin0[8 .. 16];
        // writeln("bin0_0 = ", bin0_0);  // for testing
        // writeln("bin0_1 = ", bin0_1);  // for testing

        int char0a = to!int(bin0_0, 2);
        int char1a = to!int(bin0_1, 2);
        // writeln("char0a = ", char0a);  // for testing
        // writeln("char1a = ", char1a);  // for testing

        char char0b = cast(char)char0a;
        char char1b = cast(char)char1a;
        // writeln("char0b = ", char0b);  // for testing
        // writeln("char1b = ", char1b);  // for testing

        // 2026-06-13: new solution with regular expressions:
        if (matchFirst([char0b], pattern)) {
            pw_chars ~= char0b;
            i++;
        }

        if (matchFirst([char1b], pattern) && i < N_CHAR) {
            pw_chars ~= char1b;
            i++;
        }

        j++;
    }

    writefln("\nYour password of %d characters is: %s", N_CHAR, pw_chars);

    return main_return_val;
}

// end of random_bitstring_and_flexible_password_generator.d
