/*
random_bitstring_and_flexible_password_generator.c

2025-05-29
2025-07-15: repaired Exception Handling when writing to files => program must not stop at an exception here!
2025-12-17: see below
2026-01-11: deleted one outdated definition for nanosec_to_millisec
2026-02-01: deleted one oudated import ctype.h
2026-05-25: refactored from char_set to pattern (for regular expressions)

build on Ubuntu 24 LTS: $ make  # see make file below

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
makefile with tabs:

all: random_bitstring_and_flexible_password_generator

random_bitstring_and_flexible_password_generator: random_bitstring_and_flexible_password_generator.o
	clang -o random_bitstring_and_flexible_password_generator random_bitstring_and_flexible_password_generator.o

random_bitstring_and_flexible_password_generator.o: random_bitstring_and_flexible_password_generator.c
	clang -O3 -c random_bitstring_and_flexible_password_generator.c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$ clang -v
Homebrew clang version 22.1.5
...
$

*/


#include <stdio.h>   // printf()
#include <stdlib.h>  // srand(), rand(); 2026-02-01
#include <string.h>  // strncpy()
#include <time.h>    // srand(time(NULL))
#include <regex.h>   // 2026-05-25

#define END  62501  // 62501 for exactly 1M binary digits; val is immutable
// #define END  25    // for testing
#define M1   END * 16
#define K250 END * 4

#define m 65521  // = 2^16 - 15
#define a 17364
#define c 0

#define file_bits_x   "random_bitstring.bin"
#define file_bits_hex "random_bitstring.byte"

const char *print_re = "^[!-~]+$";
const char *alnum_re = "^[[:alnum:]]\+$";  // POSIX based solution


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



  // make a password of N_CHAR printable chars: user input requested here
  int N_CHAR = 12;
  int answer = 0;  // no fancy #include <stdbool.h> here
  char answer_str[256];
  char* endptr; // for using strtol()
  // https://www.geeksforgeeks.org/convert-string-to-int-in-c/#
  // https://en.cppreference.com/w/c/string/byte/strtol
  // The atoi function is also considered obsolete; use strtol instead.
  // https://www.gnu.org/software/libc/manual/html_node/Parsing-of-Integers.html
  // https://stackoverflow.com/questions/59237084/converting-char-to-int-with-strtol-only-if-its-a-valid-number
  while (answer == 0) {
    N_CHAR = 12;
    printf("\n\nPassword of %d printable chars OK? 'y' or another integer number >= 8: ", N_CHAR);
    endptr = fgets(answer_str, sizeof(answer_str), stdin);  // safe stdin
    // https://en.cppreference.com/w/c/io/fgets
    // https://stackoverflow.com/questions/7709452/how-to-read-string-from-keyboard-using-c

    // truncate new_line char:
    answer_str[strcspn(answer_str, "\n")] = 0;
    // printf("answer_str = %s---", answer_str);

    if (strcmp("y", answer_str) == 0) {
      answer = 1;
    } else {
      N_CHAR = strtol(answer_str, &endptr, 10);
      if (*endptr != NULL) {  // 89sad is not accepted
        printf("enter an integer number >= 8 or 'y'");
      } else {
        if (N_CHAR < 8) {
          printf("enter an integer number >= 8 or 'y'");
        } else {
          answer = 1;
        }
      }
    }
  }
  // printf("\nN_CHAR = %d", N_CHAR);  // for testing


  int WITH_SPECIAL_CHARS = 1;  // "true" default
  answer = 0;
  while (answer == 0) {
    printf("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
    endptr = fgets(answer_str, sizeof(answer_str), stdin);

    answer_str[strcspn(answer_str, "\n")] = 0;
    if (strcmp("y", answer_str) == 0) {
      answer = 1;
    } else {
      WITH_SPECIAL_CHARS = 0;
      answer = 1;
    }
  }


  // 2026-05-25: old solution:
  //   char char_set[95] = {'\0'};  // 127 - 33 + 1 for \0; see also below at pw_chars
  //   if (WITH_SPECIAL_CHARS) {
  //     for (char chr = 33; chr < 127; chr++) {
  //       char_set[chr - 33] = chr;
  //     }
  //   } else {
  //     strcat(char_set, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
  //   }
  //   printf("\nchar_set = %s", char_set);  // for testing
  //   printf("\nlen of char_set = %ld\n", strlen(char_set));  // for testing



  // 2026-05-25: new solution with regular expressions (Duck.ai)
  // type definitions for regexpr's:
  regex_t re_printable, re_alphanum;
  // compile both regexpr's:
  regcomp(&re_printable, print_re, REG_EXTENDED);  // REG_EXTENDED is essential with "^[!-~]+$" for example
  regcomp(&re_alphanum,  alnum_re, REG_EXTENDED);
  // select the chosen regexpr:
  regex_t *pattern = WITH_SPECIAL_CHARS ? &re_printable : &re_alphanum;


  char pw_chars[128] = {'\0'};  // get a really empty string here!
  // if not, this string is usually not terminated correctly
  // 128 a fixed size at compile time, which is needed if done this static way
  char bin0[17];
  char bin0_0[9];
  char bin0_1[9];

  int i = 0;  // char counter for the password
  int j = 0;  // counter for x

  while (i < N_CHAR) {
    // printf("%d\n", x[j]);      // for testing
    sprintf(bin0, "%016b", x[j]);
    // printf("%s\n", bin0);  // for testing

    strncpy(bin0_0, bin0, 8);
    strncpy(bin0_1, bin0 + 8, 8);
    // printf("bin0_0 = %s -- bin0_1 = %s\n", bin0_0, bin0_1);  // for testing

    int char0a = strtol(bin0_0, &endptr, 2);
    int char1a = strtol(bin0_1, &endptr, 2);
    // printf("char0a = %d -- char1a = %d\n", char0a, char1a);  // for testing

    // 2026-05-25: for new solution with regular expressions: build strings:
    char char0b[2];
    char0b[0] = (char)char0a;
    char0b[1] = '\0';
    char char1b[2];
    char1b[0] = (char)char1a;
    char1b[1] = '\0';
    // printf("char0b = %s -- char1b = %s\n", char0b, char1b);  // for testing

    // const char* pos1 = strchr(char_set, char0b);  // old solution

    if (regexec(pattern, char0b, 0, NULL, 0) == 0) {
      pw_chars[i] = char0a;  // char needed here, not string!
      i++;
      // printf("i = %d -- %s\n", i, char0b);  // for testing
    }

    if (regexec(pattern, char1b, 0, NULL, 0) == 0 && i < N_CHAR) {
      pw_chars[i] = char1a;
      i++;
      // printf("i = %d -- %s\n", i, char1b);  // for testing
    }

    j++;
  }

  printf("\nYour password of %d characters is: %s\n", N_CHAR, pw_chars);

  return main_return_val;
}

// end of random_bitstring_and_flexible_password_generator.c

