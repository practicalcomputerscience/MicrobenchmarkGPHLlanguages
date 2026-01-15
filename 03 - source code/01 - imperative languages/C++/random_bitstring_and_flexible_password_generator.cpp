/*
random_streams_for_perf_stats.cpp

2026-01-15

build on Ubuntu 24 LTS: $ g++ -std=c++20 random_bitstring_and_flexible_password_generator.cpp -o random_bitstring_and_flexible_password_generator  # for development
                        $ g++ -O3 -std=c++20 random_bitstring_and_flexible_password_generator.cpp -o random_bitstring_and_flexible_password_generator  # for production

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator



$ g++ --version
g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
...
$


partly translated from random_bitstring_and_flexible_password_generator.c with Big AI,
though much manual fine-tuning was needed

*/


#include <iostream> // cout
#include <fstream>  // ifstream, ofstream
#include <ctime>    // time()
#include <cstdlib>  // srand(), strtol()
#include <vector>
#include <string>
#include <format>   // format_to()
#include <cstring> // for strcspn
#include <stdbool.h>  // For boolean data type (bool, true, false)


using namespace std;


#define END  62501  // 62501 for exactly 1M binary digits; val is immutable
// #define END  15     // for testing
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


    // make a password of N_CHAR printable chars: user input requested here
    int N_CHAR = 12;
    bool answer = false;
    string answer_str;
    char* endptr;

    while (!answer) {
        N_CHAR = 12;
        cout << "\n\nPassword of " << N_CHAR << " printable chars OK? 'y' or another integer number >= 8: ";
        getline(cin, answer_str);  // safe stdin
        // cout << "answer_str = " << answer_str << "---";  // for testing

        if (answer_str == "y") {
            answer = true;
        } else {
            // char* endptr;
            N_CHAR = strtol(answer_str.c_str(), &endptr, 10);
            if (*endptr != '\0') {
                cout << "enter an integer number >= 8 or 'y'";
            } else {
                if (N_CHAR < 8) {
                    cout << "enter an integer number >= 8 or 'y'";
                } else {
                    answer = true;
                }
            }
        }
    }
    // cout << "\nN_CHAR = " << N_CHAR;  // for testing


    bool WITH_SPECIAL_CHARS = true;  // "true" default
    answer = false;
    while (answer == 0) {
        cout << "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ";
        getline(cin, answer_str);  // safe stdin

        if (answer_str == "y") {
          answer = true;
        } else {
          WITH_SPECIAL_CHARS = false;
          answer = true;
        }
    }
    // cout << "\nWITH_SPECIAL_CHARS = " << WITH_SPECIAL_CHARS;  // for testing

    string char_set = "";
    if (WITH_SPECIAL_CHARS) {
        for (int i = 33; i < 127; i++) {
            char_set += static_cast<char>(i);  // appends another character
        }
    } else {
        char_set += "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    }
    // cout << "\nchar_set = " << char_set;  // for testing


    string pw_chars = "";
    char bin0[17];
    char bin0_0[9];
    char bin0_1[9];

    int i = 0;  // char counter for the password
    int j = 0;  // counter for x

    while (i < N_CHAR) {
        // cout << "\n" << x[j] << endl;  // for testing
        format_to(bin0, "{:016b}", x[j]);
        bin0[16] = '\0';  // Null-terminate the string
        // cout << bin0 << endl;  // for testing

        strncpy(bin0_0, bin0, 8);
        bin0_0[8] = '\0';  // Null-terminate the string
        strncpy(bin0_1, bin0 + 8, 8);
        bin0_1[8] = '\0';  // Null-terminate the string
        // cout << "bin0_0 = " << bin0_0 << " -- bin0_1 = " << bin0_1 << endl;  // for testing

        int char0a = strtol(bin0_0, &endptr, 2);
        int char1a = strtol(bin0_1, &endptr, 2);
        // cout << "char0a = " << char0a << " -- char1a = " << char1a << endl;  // for testing

        char char0b = static_cast<char>(char0a);
        char char1b = static_cast<char>(char1a);

        if (char_set.find(char0b) != std::string::npos) {  // universally understood in C++
            pw_chars += char0b;
            i++;
            // cout << "i = " << i << " -- " << char0b << endl;  // for testing
        }

        if (char_set.find(char1b) != std::string::npos && i < N_CHAR) {
            pw_chars += char1b;
            i++;
            // cout << "i = " << i << " -- " << char1b << endl;  // for testing
        }

        j++;
    }

    cout << "\nYour password of " << N_CHAR << " characters is: " << pw_chars << endl;

    return main_return_val;
}

// end of random_bitstring_and_flexible_password_generator.cpp
