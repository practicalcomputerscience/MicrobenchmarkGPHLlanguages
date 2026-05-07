// random_streams_for_perf_stats.bal as main.bal
//
// 2026-05-07
//
// build on Ubuntu 24 LTS: $ bal new random_streams_for_perf_stats
//                         $ cd random_streams_for_perf_stats
//                         fix main.bal <-- random_streams_for_perf_stats.bal
//                         $ bal build
//
// run on Ubuntu 24 LTS:   ($ bal run  # shows more output: this command also takes significantly longer: real	0m1.152s)
//                         $ bal run ./target/bin/random_streams_for_perf_stats.jar
//                         $ time bal run ./target/bin/random_streams_for_perf_stats.jar => real	0m0.540s
//
//                         or like this with right JRE version (must be 21 as of 2026-05-07) for no warnings:
//                         $ sudo update-alternatives --config java  # switch to JRE version 21, if already installed
//                         $ java --version  # openjdk 21.0.10 2026-01-20
//                         $ java -jar ./target/bin/random_streams_for_perf_stats.jar
//                         $ time java -jar ./target/bin/random_streams_for_perf_stats.jar => real	0m0.450s <<<<<<<<<<<<<
//                         $ multitime -n 20 java -jar ./target/bin/random_streams_for_perf_stats.jar
//                         =>             Mean        Std.Dev.
//                            real        0.454       0.007
//
//
// $ bal version
// Ballerina 2201.13.3 (Swan Lake Update 13)
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
import ballerina/lang.'string;


public function main(string[] args) returns error? {
    final int END = 62501;  // 62501 for exactly 1M binary digits
    // const values can only be located at the module level!
    // final int END = 10;  // for testing

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

//
// end user defined functions
//
//////////////////////////////////////////////////////////////////////////////////////

// end of random_streams_for_perf_stats.bal
