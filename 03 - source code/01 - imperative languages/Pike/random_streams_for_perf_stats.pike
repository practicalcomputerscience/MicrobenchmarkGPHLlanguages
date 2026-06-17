/*
random_streams_for_perf_stats.pike

2026-06-18

run on Ubuntu 24 LTS: $ pike random_streams_for_perf_stats.pike

                      $ time pike random_streams_for_perf_stats.pike => real	0m0.067s


$ pike -v
Pike v8.0 release 1738 Copyright © 1994-2022 Linköping University
...
$

Transpiled from random_streams_for_perf_stats.groovy with Google AI.

*/


int main(int argc, array(string) args) {
    int END = 62501;  // 62501 for exactly 1M binary digits

    int m = 65521;  // = 2^16 - 15
    int a = 17364;
    int c = 0;

    string file_bits_x   = "random_bitstring.bin";
    string file_bits_hex = "random_bitstring.byte";

    // Allocate integer array of size END
    array(int) x = allocate(END);

    // Initial random value bounded by (m - 1) + 1
    x[0] = random(m - 1) + 1;

    // String builders in Pike are efficiently handled via String.Buffer
    object bits_x   = String.Buffer();
    object bits_hex = String.Buffer();

    write("\ngenerating a random bit stream...\n");
    for (int i = 1; i < END; i++) {
        x[i] = (a * x[i - 1] + c) % m;

        // sprintf("%016b") formats to binary and pads left with '0' up to 16 chars
        string bits_x_str = sprintf("%016b", x[i]);
        bits_x->add(bits_x_str);

        // sprintf("%04x") formats to hex and pads left with '0' up to 4 chars
        string bits_hex_str = sprintf("%04x", x[i]);
        bits_hex->add(bits_hex_str);
    }


    // write bit stream to disk:
    mixed ex;
    ex = catch {
        object file = Stdio.File();
        if (file->open(file_bits_x, "wct")) {
            file->write((string)bits_x);
            file->close();
            write("Bit stream has been written to disk under name:  %s\n", file_bits_x);
        } else {
            error("Could not open file descriptor.\n");
        }
    };
    if (ex) {
        write("could not write to file: %s ! -- %s\n", file_bits_x, describe_backtrace(ex));
    }

    // write byte stream to disk:
    ex = catch {
        object file = Stdio.File();
        if (file->open(file_bits_hex, "wct")) {
            file->write((string)bits_hex);
            file->close();
            write("Byte stream has been written to disk under name: %s\n", file_bits_hex);
        } else {
            error("Could not open file descriptor.\n");
        }
    };
    if (ex) {
        write("could not write to file: %s ! -- %s\n", file_bits_hex, describe_backtrace(ex));
    }

    return 0;
}

// end of random_streams_for_perf_stats.pike
