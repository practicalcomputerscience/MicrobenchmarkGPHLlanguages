/*
random_streams_for_perf_stats.c

2025-05-31, 2025-07-10 (compiling with clang)
2025-07-15: repaired Exception Handling when writing to files => program must not stop at an exception here!

test on Ubuntu 24 LTS: OK

build on Ubuntu 24 LTS: $ gcc -Wall -Ofast -faggressive-loop-optimizations random_streams_for_perf_stats.c -o random_streams_for_perf_stats_c

run on Ubuntu 24 LTS:   $ sudo perf stat -r 20 ./random_streams_for_perf_stats_c
                        => 0,008218 +- 0,000159 seconds time elapsed  ( +-  1,93% )

---
Compiling with clang:   $ clang random_streams_for_perf_stats.c -O3 -o random_streams_for_perf_stats_clang
                          -ffast-math is not improving exe speed here

                        $ time ./random_streams_for_perf_stats_clang => real	0m0,013s
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats_clang
                        => 0,008383 +- 0,000491 seconds time elapsed  ( +-  5,86% )


$ gcc --version --> gcc version 13.3.0 (Ubuntu 13.3.0-6ubuntu2~24.04)

$  clang -v
clang version 17.0.0 (https://github.com/swiftlang/llvm-project.git 901f89886dcd5d1eaf07c8504d58c90f37b0cfdf)
Target: x86_64-unknown-linux-gnu
Thread model: posix....
$


*/


#include <time.h>    // srand(time(NULL))
#include <stdio.h>   // printf()
#include <stdlib.h>
#include <string.h>  // strncpy()
#include <ctype.h>   //


#define END  62501  // 62501 for exactly 1M binary digits; val is immutable
// #define END  20     // for testing
#define M1   END * 16
#define K250 END * 4

#define m 65521  // = 2^16 - 15
#define a 17364
#define c 0

#define file_bits_x   "random_bitstring.bin"
#define file_bits_hex "random_bitstring.byte"

#define nanosec_to_millisec 1000000


int main()
{
  int main_return_val = 0;

  int x[END];  // set aside a space on the stack big enough for END integers

  srand(time(NULL));      // Initialize RNG seed.
  // printf("%i\n", rand()); // Make one draw.
  x[0] = rand() % (m);    //Generate a random number between 0 and N
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

    sprintf(bits_x_str, "%016b", x[i]);
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

// end of random_streams_for_perf_stats.c
