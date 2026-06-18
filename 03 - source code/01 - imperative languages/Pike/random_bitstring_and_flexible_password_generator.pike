/*
random_bitstring_and_flexible_password_generator.pike

2026-06-18

run on Ubuntu 24 LTS: $ pike random_bitstring_and_flexible_password_generator.pike


$ pike -v
Pike v8.0 release 1738 Copyright © 1994-2022 Linköping University
...
$

Mostly transpiled from random_streams_for_perf_stats.groovy with Google AI.

*/


// user defined functions:

int is_all_digits(string s) {
    string matched;
    // Returns 1 only if the entire string consists of characters 0-9:
    return sscanf(s, "%[0-9]", matched) == 1 && sizeof(matched) == sizeof(s);
    // https://pike.lysator.liu.se/generated/manual/modref/ex/predef_3A_3A/sscanf.html
}

// end of user defined functions


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
        write("could not write to file: %s ! -- %s", file_bits_x, describe_backtrace(ex));
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
        write("could not write to file: %s ! -- %s", file_bits_hex, describe_backtrace(ex));
    }


    // make a password of N_CHAR printable chars: user input requested here
    int N_CHAR = 12;
    int answer = 0;

    while (!answer) {
        write(sprintf("\nPassword of %d printable chars OK? 'y' or another integer number >= 8: ", N_CHAR));
        // Stdin.gets() reads from standard input and strips trailing newlines
        string answer_str = Stdio.stdin->gets();

        if (answer_str == "y") {
            answer = 1;
        } else {
            int parsed_val = (int)answer_str;  // Pike evaluates non-numeric strings to 0
            // write(sprintf("parsed_val = %d\n", parsed_val));  // for testing
            // write(sprintf("is_all_digits = %d\n", is_all_digits(answer_str)));  // for testing
            // (int)answer_str: '66 ggg' evaluates to 66
            //    => strictly check all characters with user defined function is_all_digits()
            if (is_all_digits(answer_str) && parsed_val >= 8) {
                N_CHAR = parsed_val;
                answer = 1;
            } else {
                write("enter an integer number >= 8 or 'y'\n");
            }
        }
    }
    // write(sprintf("N_CHAR = %d\n", N_CHAR));  // for testing

    int WITH_SPECIAL_CHARS = 1;
    answer = 0;
    while (!answer) {
        write("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
        string answer_str = Stdio.stdin->gets();

        if (answer_str == "y") {
            answer = 1;
        } else {
            WITH_SPECIAL_CHARS = 0;
            answer = 1;
        }
    }

    object alnum_re = Regexp("^[A-Za-z0-9]$");
    object print_re = Regexp("^[!-~]$");
    // Pike v8.0 installed "out of the box" comes without support of Regexp.PCRE:
    //   import Regexp;
    //   write("%O\n", indices(Regexp));
    //   ({ /* 3 elements */
    //       "`()",
    //       "SimpleRegexp",
    //       "_SimpleRegexp"
    //   })
    object pattern = WITH_SPECIAL_CHARS ? print_re : alnum_re;


    int i = 0;  // char counter for the password
    int j = 0;  // counter for x
    string pw_chars = "";

    while (i < N_CHAR) {
        // Extract 8-bit pieces using bitwise shifts instead of heavy string parsing
        int val0 = (x[j] >> 8) & 0xFF;
        int val1 = x[j] & 0xFF;

        // Convert integers directly to character strings
        string char0 = sprintf("%c", val0);
        string char1 = sprintf("%c", val1);

        if (pattern->match(char0)) {
            pw_chars += char0;
            i++;
        }

        if (pattern->match(char1) && i < N_CHAR) {
            pw_chars += char1;
            i++;
        }

        j++;
    }

    write(sprintf("\nYour password of %d characters is: %s\n", N_CHAR, pw_chars));

    return 0;
}

// end of random_streams_for_perf_stats.pike
