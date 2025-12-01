/*
random_streams_for_perf_stats.checked_c.c

2025-12-01

base version for Checked C of C program: random_streams_for_perf_stats.c


a/
convert from C to Checked C source code with 3c tool:
    $ ~/scripts/Checked_C/CheckedC-Clang-12.0.0git-Linux/bin/3c -addcr -alltypes -output-postfix=checked random_streams_for_perf_stats.c --

b/
fix this warning with a user defined function (see below):
    "... warning: invalid conversion specifier 'b' [-Wformat-invalid-specifier] sprintf(bits_x_str, "%016b", x[i]); ..."

c/
build on Ubuntu 24 LTS with modified Clang compiler:
    $ ~/scripts/Checked_C/CheckedC-Clang-12.0.0git-Linux/bin/clang ./random_streams_for_perf_stats.checked_c.c -O3 -ffast-math -o random_streams_for_perf_stats.checked_c
    # -ffast-math without speed influence here

d/
run on Ubuntu 24 LTS:
    $ sudo perf stat -r 20 ./random_streams_for_perf_stats.checked_c
    => 0,003159 +- 0,000176 seconds time elapsed  ( +-  5,58% ) !!!


$ ~/scripts/Checked_C/CheckedC-Clang-12.0.0git-Linux/bin/clang --version
clang version 12.0.0 (https://github.com/checkedc/checkedc-llvm-project 9d329e1130ccab7bc045722a3c911da273459293)
Target: x86_64-unknown-linux-gnu
Thread model: posix
...
$

*/


#include <time.h>    // srand(time(NULL))
#include <stdio.h>   // printf()
#include <stdlib.h>
#include <string.h>  // strncpy()
#include <ctype.h>   //
#include <stdint.h>


#define END  62501  // 62501 for exactly 1M binary digits; val is immutable
// #define END  20     // for testing
#define M1   END * 16
#define K250 END * 4

#define m 65521  // = 2^16 - 15
#define a 17364
#define c 0

#define file_bits_x   "random_bitstring.bin"
#define file_bits_hex "random_bitstring.byte"


// user defined functions:

// Duck.ai answer:
void integer_to_bin_string(int n, char *binary_str) {
    binary_str[16] = '\0'; // Null-terminate the string
    for (int i = 15; i >= 0; i--) {
        binary_str[i] = (n & 1) ? '1' : '0'; // Set the bit accordingly
        n >>= 1; // Right shift by 1
    }
}

// ChatGPT answer for translation from the Ada solution:
char *integer_to_hex_string(uint16_t N, char hex_str[5])
{
    const int STR_LENGTH_HEX = 4;
    int j = STR_LENGTH_HEX - 1;   // C uses 0-based indexing
    uint16_t k = N;

    // Initialize output as "0000"
    for (int i = 0; i < STR_LENGTH_HEX; i++) {
        hex_str[i] = '0';
    }
    hex_str[4] = '\0';  // Null terminator

    while (k > 0 && j >= 0) {
        uint16_t remainder = k % 16;

        if (remainder < 10)
            hex_str[j] = '0' + remainder;
        else
            hex_str[j] = 'a' + (remainder - 10);

        k /= 16;
        j--;
    }

    return hex_str;
}

// end of user defined functions


int main()
{
  int main_return_val = 0;

  int x _Checked[END];  // set aside a space on the stack big enough for END integers

  srand(time(NULL));      // Initialize RNG seed.
  // printf("%i\n", rand()); // Make one draw.
  x[0] = rand() % (m);    //Generate a random number between 0 and N
  // printf("x[0] = %d\n", x[0]);  // for testing

  char bits_x _Nt_checked[M1];
  char bits_x_str[17];
  int  byte_nbr;

  char bits_hex _Nt_checked[K250+1];
  char bits_hex_str[5];


  printf("\ngenerating a random bit stream...");
  for (int i = 1; i < END; i++) {
    x[i] = ((a * x[i-1]) + c) % m;
    // printf("\n%d\n", x[i]);      // for testing

    // sprintf(bits_x_str, "%016b", x[i]);  // original version
    integer_to_bin_string(x[i], bits_x_str);  // fixed version: https://libc.llvm.org/dev/printf_behavior.html
    // printf("%s\n", bits_x_str);  // for testing

    byte_nbr = (i-1)*16;
    bits_x[byte_nbr]     = bits_x_str[0];
    bits_x[byte_nbr+1]   = bits_x_str[1];
    bits_x[byte_nbr+2]   = bits_x_str[2];
    bits_x[byte_nbr+3]   = bits_x_str[3];
    bits_x[byte_nbr+4]   = bits_x_str[4];
    bits_x[byte_nbr+5]   = bits_x_str[5];
    bits_x[byte_nbr+6]   = bits_x_str[6];
    bits_x[byte_nbr+7]   = bits_x_str[7];
    bits_x[byte_nbr+8]   = bits_x_str[8];
    bits_x[byte_nbr+9]   = bits_x_str[9];
    bits_x[byte_nbr+10]  = bits_x_str[10];
    bits_x[byte_nbr+11]  = bits_x_str[11];
    bits_x[byte_nbr+12]  = bits_x_str[12];
    bits_x[byte_nbr+13]  = bits_x_str[13];
    bits_x[byte_nbr+14]  = bits_x_str[14];
    bits_x[byte_nbr+15]  = bits_x_str[15];


    // sprintf(bits_hex_str, "%04x", x[i]);
    // https://en.cppreference.com/w/c/io/fprintf
    integer_to_hex_string(x[i], bits_hex_str);  // test for speed: 
    // printf("%s\n", bits_hex_str);  // for testing

    byte_nbr = (i-1)*4;
    bits_hex[byte_nbr]     = bits_hex_str[0];
    bits_hex[byte_nbr+1]   = bits_hex_str[1];
    bits_hex[byte_nbr+2]   = bits_hex_str[2];
    bits_hex[byte_nbr+3]   = bits_hex_str[3];

    bits_hex[byte_nbr+4]   = '\0';  // there's trash at the last char's/bytes
  }
  // printf("%s\n", bits_x);  // for testing
  // printf("%s\n", bits_hex);  // for testing


  // write bit stream to disk:
  _Ptr<FILE> f1 = fopen(file_bits_x, "w");
  if (f1 == NULL)
  _Checked {
    _Unchecked { printf("\ncould not write to file: %s !", file_bits_x); };
    main_return_val = 1;
  } else {
    fprintf(f1, "%s", bits_x);
    printf("\nBit stream has been written to disk under name:  %s", file_bits_x);
    fclose(f1);
  }

  // write byte stream to disk:
  _Ptr<FILE> f2 = fopen(file_bits_hex, "w");
  if (f2 == NULL)
  _Checked {
    _Unchecked { printf("\ncould not write to file: %s !", file_bits_hex); };
    main_return_val = 1;
  } else {
    fprintf(f2, "%s", bits_hex);
    printf("\nByte stream has been written to disk under name: %s", file_bits_hex);
    fclose(f2);
  }

  printf("\n");
  return main_return_val;
}

// end of random_streams_for_perf_stats.checked_c.c
