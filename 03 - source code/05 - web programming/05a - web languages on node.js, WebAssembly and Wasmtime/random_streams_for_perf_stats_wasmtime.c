/*
random_streams_for_perf_stats_wasmtime.c

2026-02-01

build on Ubuntu 24 LTS:
  $ clang -O3 --target=wasm32-wasi random_streams_for_perf_stats_wasmtime.c \
  -o random_streams_for_perf_stats.wasm \
  -Wl,--export=main -Wl,-z,stack-size=2048000

run on Ubuntu 24 LTS:
  $ wasmtime --dir=. random_streams_for_perf_stats.wasm
  # --dir=. is essential to allow access to the current directory of the file system

  $ time wasmtime --dir=. random_streams_for_perf_stats.wasm => real	0m0.012s
  $ multitime -n 20 wasmtime --dir=. random_streams_for_perf_stats.wasm
  =>
                Mean        Std.Dev.
    real        0.011       0.000


$ clang -v
Homebrew clang version 21.1.8
...
$ wasmtime -V
wasmtime 41.0.1 (c30fce86b 2026-01-26)
$

check clang version to support the C23 standard:
$ clang -std=c23 -dM -E - < /dev/null | grep __STDC_VERSION__
#define __STDC_VERSION__ 202311L
$


*/


#include <time.h>    // srand(time(NULL))
#include <stdio.h>   // printf()
#include <stdlib.h>  // srand(), rand()


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

// end of user defined functions


int main()
{
  int main_return_val = 0;

  int x[END];  // set aside a space on the stack big enough for END integers

  srand(time(NULL));      // Initialize RNG seed.
  // printf("%i\n", rand()); // Make one draw.

  x[0] = rand() % (m-1) + 1;  // rand(): random number between 0 and RAND_MAX, both included; 2025-12-17
  // printf("x[0] = %d\n", x[0]);  // for testing

  char bits_x[M1];
  char bits_x_str[17];
  int  byte_nbr;

  char bits_hex[K250+1];
  char bits_hex_str[5];


  printf("\ngenerating a random bit stream...");
  for (int i = 1; i < END; i++) {
    x[i] = ((a * x[i-1]) + c) % m;
    // printf("\n%d\n", x[i]);      // for testing

    // sprintf(bits_x_str, "%016b", x[i]);  // this is not working (yet) for Wasmtime!
    // it may be that the WASI libc implementation for WebAssembly
    // is not ready yet for "%016b": https://github.com/WebAssembly/wasi-libc
    integer_to_bin_string(x[i], bits_x_str);  // fixed version from random_streams_for_perf_stats.checked_c.c
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


    sprintf(bits_hex_str, "%04x", x[i]);
    // https://en.cppreference.com/w/c/io/fprintf
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
  FILE *f1 = fopen(file_bits_x, "w");
  if (f1 == NULL)
  {
    printf("\ncould not write to file: %s !", file_bits_x);
    main_return_val = 1;
  } else {
    fprintf(f1, "%s", bits_x);
    printf("\nBit stream has been written to disk under name:  %s", file_bits_x);
    fclose(f1);
  }

  // write byte stream to disk:
  FILE *f2 = fopen(file_bits_hex, "w");
  if (f2 == NULL)
  {
    printf("\ncould not write to file: %s !", file_bits_hex);
    main_return_val = 1;
  } else {
    fprintf(f2, "%s", bits_hex);
    printf("\nByte stream has been written to disk under name: %s", file_bits_hex);
    fclose(f2);
  }

  printf("\n");
  return main_return_val;
}

// end of random_streams_for_perf_stats_wasmtime.c
