/*
random_streams_for_perf_stats.cpp

2026-01-14

build on Ubuntu 24 LTS: $ g++ -std=c++20 random_streams_for_perf_stats.cpp -o random_streams_for_perf_stats_g++  # for development
                        $ g++ -O3 -std=c++20 random_streams_for_perf_stats.cpp -o random_streams_for_perf_stats_g++  # for production

                        $ clang++ -std=c++20 -stdlib=libstdc++ random_streams_for_perf_stats.cpp -o random_streams_for_perf_stats_clang  # for development
                        $ clang++ -O3 -std=c++20 -stdlib=libstdc++ random_streams_for_perf_stats.cpp -o random_streams_for_perf_stats_clang  # for production

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_g++
                        $ time ./random_streams_for_perf_stats_g++ => real	0m0.006s <<<<<<<<<<<

                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats_g++
                        => 0.0058596 +- 0.0000762 seconds time elapsed  ( +-  1.30% ) <<<<<<<<<<<

                        $ ./random_streams_for_perf_stats_clang
                        $ time ./random_streams_for_perf_stats_clang => real	0m0.008s

                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats_clang
                        => 0.0073083 +- 0.0000801 seconds time elapsed  ( +-  1.10% )


$ g++ --version
g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
...
$

$ clang++ --version
Homebrew clang version 21.1.7
Target: x86_64-unknown-linux-gnu
Thread model: posix
$ 


partly translated from random_streams_for_perf_stats.c with Big AI,
though much manual fine-tuning was needed

*/


#include <iostream> // cout
#include <fstream>  // ifstream, ofstream
#include <ctime>    // time()
#include <cstdlib>  // srand()
#include <vector>
// #include <string>
#include <format>   // format_to()


using namespace std;


#define END  62501  // 62501 for exactly 1M binary digits; val is immutable
// #define END  10     // for testing
#define M1    END * 16
#define K250  END * 4

#define m 65521  // = 2^16 - 15
#define a 17364
#define c 0

const string file_bits_x = "random_bitstring.bin";
const string file_bits_hex = "random_bitstring.byte";


int main() {
    int main_return_val = 0;

    vector<int> x(END);  // Use vector for dynamic sizing

    srand(static_cast<unsigned int>(time(nullptr))); // https://en.cppreference.com/w/cpp/numeric/random/srand.html

    x[0] = rand() % (m - 1) + 1;   // rand(): random number between 0 and RAND_MAX
    // cout << x[0] << endl;  // for testing

    char bits_x[M1];
    char bits_x_str[17];
    int byte_nbr;

    char bits_hex[K250];
    char bits_hex_str[5];

    cout << "\ngenerating a random bit stream...";
    for (int i = 1; i < END; i++) {
        x[i] = ((a * x[i - 1]) + c) % m;
        // cout << "\n" << x[i] << endl;  // for testing

        format_to(bits_x_str, "{:016b}", x[i]);
        // cout << bits_x_str << endl;  // for testing
        byte_nbr = (i - 1) * 16;
        copy(bits_x_str, bits_x_str + 16, bits_x + byte_nbr);
        bits_x[byte_nbr + 16] = '\0'; // Null-terminate the bin string

        format_to(bits_hex_str, "{:04x}", x[i]);
        // cout << bits_hex_str << endl;  // for testing
        byte_nbr = (i - 1) * 4;
        copy(bits_hex_str, bits_hex_str + 4, bits_hex + byte_nbr);
        bits_hex[byte_nbr + 4] = '\0'; // Null-terminate the hex string
    }

    // cout << "\n" << bits_x << endl;  // for testing
    // cout << "\n" << bits_hex << endl;  // for testing


    // Write bit stream to disk:
    ofstream f1(file_bits_x);
    if (!f1) {
        cerr << "\ncould not write to file: " << file_bits_x << " !";
        main_return_val = 1;
    } else {
        f1 << bits_x;
        cout << "\nBit stream has been written to disk under name:  " << file_bits_x;
        f1.close();
    }

    // Write byte stream to disk:
    ofstream f2(file_bits_hex);
    if (!f2) {
        cerr << "\ncould not write to file: " << file_bits_hex << " !";
        main_return_val = 1;
    } else {
        f2 << bits_hex;
        cout << "\nByte stream has been written to disk under name: " << file_bits_hex;
        f2.close();
    }

    cout << "\n";
    return main_return_val;
}

// end of random_streams_for_perf_stats.cpp
