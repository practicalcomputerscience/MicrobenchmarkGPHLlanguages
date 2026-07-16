/*
random_streams_for_perf_stats2a.c

2025-05-31, 2025-07-10 (compiling with clang)
2025-07-15: repaired Exception Handling when writing to files => program must not stop at an exception here!
2025-12-01: learning from Checked C version: replacing sprintf(bits_x_str, "%016b", x[i]); with a user defined function
2026-07-16: bits_x is wrong on disk in Xubuntu 18.04.5 32-bit


OS: xubuntu-18.04.5-desktop-i386 as a virtual machine (Oracle VirtualBox) on a Ubuntu 24 LTS host (Intel Core Ultra 7 270K Plus)


build on Xubuntu 18.04.5 32-bit: $ gcc -Wall random_streams_for_perf_stats2a.c -o random_streams_for_perf_stats2a  # development
                                 $ gcc -Wall -O3 random_streams_for_perf_stats2a.c -o random_streams_for_perf_stats2a  # production


run on Xubuntu 18.04.5 32-bit:   $ time ./random_streams_for_perf_stats2a
				 $ multitime -n 10 ./random_streams_for_perf_stats2a
                                 1: ./random_streams_for_perf_stats2a
                                             Mean        Std.Dev.    Min         Median      Max
                                 real        0.005       0.001       0.004       0.005       0.007    


$ gcc --version
gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0
...
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
  x[0] = rand() % (m);    //Generate a random number between 0 and N
  // printf("x[0] = %d\n", x[0]);  // for testing

  char bits_x[M1+1];  // 2026-07-16
  char bits_x_str[17];
  int  byte_nbr;

  char bits_hex[K250+1];
  char bits_hex_str[5];


  printf("\ngenerating a random bit stream...");
  for (int i = 1; i < END; i++) {
    x[i] = ((a * x[i-1]) + c) % m;
    // printf("\n%d\n", x[i]);      // for testing

    // sprintf(bits_x_str, "%016b", x[i]);   // doesn't work with the used versions, which are too old
    integer_to_bin_string(x[i], bits_x_str);
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
    
    bits_x[byte_nbr+16]   = '\0';  // there's trash at the last char's/bytes; 2026-07-16


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

// end of random_streams_for_perf_stats2a.c
