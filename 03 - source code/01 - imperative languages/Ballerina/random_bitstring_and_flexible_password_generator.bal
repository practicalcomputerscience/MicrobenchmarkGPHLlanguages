// random_bitstring_and_flexible_password_generator.bal as main.bal
//
// 2026-05-08/11
// 2026-06-12: refactored from char_set to pattern (for regular expressions)
// 2026-06-18: introduced ternary operator at pattern
//
// build on Ubuntu 24 LTS: do this only once:
//                         $ bal new random_bitstring_and_flexible_password_generator
//                         $ cd random_bitstring_and_flexible_password_generator
//                         fix source code file name to: main.bal from random_bitstring_and_flexible_password_generator.bal
//
//                         do this after every code change:
//                         $ bal build
//
// run on Ubuntu 24 LTS:   $ bal run  # shows more output
//                         $ bal run ./target/bin/random_bitstring_and_flexible_password_generator.jar
//
//                         or like this with right JRE version (must be 21 as of 2026-05-07) for no warnings:
//                         $ sudo update-alternatives --config java  # switch to JRE version 21, if already installed
//                         # check a potential Homebrew/Linuxbrew JDK version and mask it in the ~/.bashrc file!!
//                         $ java --version  # openjdk 21.0.10 2026-01-20
//                         $ java -jar ./target/bin/random_bitstring_and_flexible_password_generator.jar
//
//
// 2026-06-12: language upgrade with: $ bal dist pull 2201.13.4
// $ bal version
// Ballerina 2201.13.4 (Swan Lake Update 13)
// Language specification 2024R1
// Update Tool 1.5.1
// $ java --version
// openjdk 21.0.10 2026-01-20
// OpenJDK Runtime Environment (build 21.0.10+7-Ubuntu-124.04)
// OpenJDK 64-Bit Server VM (build 21.0.10+7-Ubuntu-124.04, mixed mode, sharing)
// $
//
//
// On efficient string building in Ballerina:
//   using the Java StringBuilder Class for potential efficiency would cause some real extra code
//   to define the needed Java bindings and to convert from Ballerina strings into Java string handles:
//
//   Interface to external code
//   https://ballerina.io/learn/by-example/interface-to-external-code/
//   ..The handle type is basically an opaque handle that can be passed to and from external functions.
//   There is no typing for handle and it can be added as a private member of a Ballerina class for better type safety...
//
// So, I decided to stick with simple joining of many little strings, being stored in a dynamically-sized array of strings,
// into one big string.
//

import ballerina/io;
import ballerina/random;
import ballerina/lang.'string;  // 2026-06-12


public function main() returns error? {
    final int END = 62501;  // 62501 for exactly 1M binary digits
    // const values can only be located at the module level!
    // final int END = 50;  // for testing

    final int m = 65521;  // = 2^16 - 15
    final int a = 17364;
    final int c = 0;

    string file_bits_x   = "random_bitstring.bin";
    string file_bits_hex = "random_bitstring.byte";

    int[] x = [];  // Use a dynamically-sized integer array

    int rnd = check random:createIntInRange(1, m);
    // https://central.ballerina.io/ballerina/random/1.7.0
    // Generates a random number between the given start(inclusive) and end(exclusive) values.
    x.push(rnd);

    string[] bits_x   = [];  // Google AI: Ballerina has no inbuilt string builder
    string[] bits_hex = [];  // => using dynamically-sized string arrays

    io:println("\ngenerating a random bit stream...");
    foreach int i in 1 ... END - 1 {
        int xi = ((a * x[i - 1]) + c) % m;
        x.push(xi);

        string bits_x_str = toBinaryString(xi);  // calling a user defined function
        bits_x.push(bits_x_str);

        string bits_hex_str = xi.toHexString().padZero(4);  // calling inbuilt functions
        bits_hex.push(bits_hex_str);
    }

    string bits_x_str_total   = string:'join("", ...bits_x);
    string bits_hex_str_total = string:'join("", ...bits_hex);

    // io:println("\nbits_x_str_total = " + bits_x_str_total);  // for testing
    // io:println("bits_hex_str_total = " + bits_hex_str_total);  // for testing


    // write bit stream to disk
    // do...on fail allows you to catch and handle the error locally: Google AI
    do {
        // 'check' will jump to the 'on fail' block if an error occurs
        check io:fileWriteString(file_bits_x, bits_x_str_total);
        io:println("Bit stream has been written to disk under name:  " + file_bits_x);
    } on fail error e {
        // Handle specific errors like "File not found" or "Permission denied"
        io:println("could not write to file: " + file_bits_x + " ! -- " + e.message());
    }

    // write byte stream to disk
    do {
        check io:fileWriteString(file_bits_hex, bits_hex_str_total);
        io:println("Byte stream has been written to disk under name: " + file_bits_hex);
    } on fail error e {
        io:println("could not write to file: " + file_bits_hex + " ! -- " + e.message());
    }


    // make a password of N_CHAR printable chars: user input requested here
    // Google AI transpilation from random_bitstring_and_flexible_password_generator.groovy
    int N_CHAR = 12;
    boolean answer = false;
    while (!answer) {
        // io:readln is used to capture user input
        string answer_str = io:readln(string:concat("\nPassword of ", N_CHAR.toString(),
                                      " printable chars OK? 'y' or another integer number >= 8: "));

        if (answer_str == "y") {
            answer = true;
        } else {
            // Attempt to parse string to int
            int|error result = int:fromString(answer_str);

            if (result is int) {
                if (result < 8) {
                    io:println("enter an integer number >= 8 or 'y'");
                } else {
                    N_CHAR = result;
                    answer = true;
                }
            } else {
                // This block executes if parsing fails (NumberFormatException equivalent)
                io:println("enter an integer number >= 8 or 'y'");
            }
        }
    }
    // io:println("N_CHAR = " + N_CHAR.toString());  // for testing


    boolean WITH_SPECIAL_CHARS = true;
    answer = false;
        while (!answer) {
        string answer_str = io:readln("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");  // 2026-05-11

        if (answer_str == "y") {
            answer = true;
        } else {
            WITH_SPECIAL_CHARS = false;
            answer = true;
        }
    }
    // io:println("WITH_SPECIAL_CHARS = " + WITH_SPECIAL_CHARS.toString());  // for testing


    // 2026-06-12: old solution:
    // In Ballerina, we use a map with null values to simulate a Set
    //   map<()> char_set = {};
    //
    //   if (WITH_SPECIAL_CHARS) {
    //       // Range from '!' (33) to '~' (126)
    //       foreach int i in 33 ... 126 {
    //           // 'check' handles the potential error
    //           char_set[check string:fromCodePointInt(i)] = ();
    //       }
    //   } else {
    //       // Lowercase a-z (97-122)
    //       foreach int i in 97 ... 122 {
    //           char_set[check string:fromCodePointInt(i)] = ();
    //       }
    //       // Uppercase A-Z (65-90)
    //       foreach int i in 65 ... 90 {
    //           char_set[check string:fromCodePointInt(i)] = ();
    //       }
    //       // Digits 0-9 (48-57)
    //       foreach int i in 48 ... 57 {
    //           char_set[check string:fromCodePointInt(i)] = ();
    //       }
    //   }
    // io:println(string `char_set = ${char_set.keys().toString()}`);  // for testing

    // 2026-06-12: new solution with regular expressions:
    //             https://ballerina.io/learn/advanced-general-purpose-language-features/#regular-expressions
    string:RegExp print_re = re `[!-~]+`;
    string:RegExp alnum_re = re `[A-Za-z0-9]+`;
    // 2026-06-18:
    string:RegExp pattern = WITH_SPECIAL_CHARS ? print_re : alnum_re;


    int i = 0;  // char counter for the password
    int j = 0;  // counter for x
    string pw_chars = "";

    while (i < N_CHAR) {
        string bin0 = toBinaryString(x[j]);
        // io:println("\nbin0 = " + bin0);  // for testing

        string bin0_0 = bin0.substring(0, 8);
        string bin0_1 = bin0.substring(8, 16);
        // io:println(bin0_0 + " + " + bin0_1);  // for testing

        int char0a = check fromBinaryString(bin0_0);
        string char0b = check string:fromCodePointInt(char0a);
        int char1a = check fromBinaryString(bin0_1);
        string char1b = check string:fromCodePointInt(char1a);
        // io:println(char0b + " + " + char1b);  // for testing

        // 2026-06-12: new solution with regular expressions:
        //             https://central.ballerina.io/ballerina/lang.regexp/latest
        if pattern.isFullMatch(char0b) {
          pw_chars += char0b;
          i += 1;
        }

        if pattern.isFullMatch(char1b) && i < N_CHAR {
          pw_chars += char1b;
          i += 1;
        }

        j += 1;
    }

    io:println("\nYour password of " + N_CHAR.toString() + " characters is: " + pw_chars);
}


//////////////////////////////////////////////////////////////////////////////////////
//
// user defined functions

// Converts an integer to its binary and padded string representation.
// + n - The integer to convert.
// + return - The binary string.
function toBinaryString(int n) returns string {
    if (n == 0) {
        return "0000000000000000";
    }

    string binary = "";
    int temp = n;

    while (temp > 0) {
        // Use modulo to get the remainder (bit)
        binary = (temp % 2).toString() + binary;
        temp = temp / 2;
    }

    return binary.padZero(16);
}

// from Google AI:
function fromBinaryString(string s) returns int|error {
    int res = 0;
    foreach var c in s {
        int bit = check int:fromString(c);
        // if bit > 1 { return error("Not a binary string"); }
        res = (res << 1) | bit;
    }
    return res;
}

//
// end user defined functions
//
//////////////////////////////////////////////////////////////////////////////////////

// end of random_bitstring_and_flexible_password_generator.bal
