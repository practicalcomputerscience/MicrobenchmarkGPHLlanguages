/*
random_streams_for_perf_stats.c3

2025-05-31; 2025-12-17: see below; 2026-01-09: deleted one outdated definition
2026-01-25: fixing the error of last missing digits in bits_x and bits_hex
2026-01-25: copying from little strings into big strings to have more abstraction here like in Fortran, C++ or Eiffel

build on Ubuntu 24 LTS: $ c3c compile random_streams_for_perf_stats.c3 -o random_streams_for_perf_stats_c3 --max-stack-object-size 262144  # for development
                        $ c3c compile -O3 random_streams_for_perf_stats.c3 -o random_streams_for_perf_stats_c3 --max-stack-object-size 262144  # for production

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_c3 => real	0m0.015s
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats_c3
                        => 0.015396 +- 0.000116 seconds time elapsed  ( +-  0.75% ) => a little bit faster than the old, C-like solution!


$ c3c --version
C3 Compiler Version:       0.7.8
...
LLVM version:              19.1.7
...
$

*/


import std::io;
import std::math::random;
// import std::time;  // now()


const int END = 62501;  // 62501 for exactly 1M binary digits; val is immutable
// const int END  = 50;     // for testing
const int M1   = END * 16 - 16;  // 2026-01-25
const int K250 = END * 4 - 4;  // 2026-01-25

const int M = 65521;  // = 2^16 - 15
const int A = 17364;
const int C = 0;

const FILE_BITS_X   = "random_bitstring.bin";
const FILE_BITS_HEX = "random_bitstring.byte";


fn int main()
{
  int main_return_val = 0;

  int[END] x;

  x[0] = rand(M-1) + 1;  // rand(): random number between 1 and M-1, both included; 2025-12-17
  // io::printfn("%d", x[0]);  // for testing: the n variants of the print functions will add a newline after printing

  char[M1] bits_x;
  char[16] bits_x_str;
  int byte_nbr;

  char[K250] bits_hex;
  char[4] bits_hex_str;


  io::printn("\ngenerating a random bit stream...");
  for (int i = 1; i < END; i++) {
    x[i] = ((A * x[i-1]) + C) % M;
    // io::printfn("\n%d", x[i]);      // for testing

    io::bprintf(&bits_x_str, "%016b", x[i]);
    // io::printfn("%s", (String)&bits_x_str);  // for testing
    // https://c3-lang.org/standard-library/#char-bprintfchar-buffer-string-format-args-maydiscard
    // https://c3-lang.org/language-fundamentals/basic-types-and-values/#printing-values

    byte_nbr = (i-1)*16;
    bits_x[byte_nbr..byte_nbr+15] = bits_x_str[0..15];  // 2026-01-25

    io::bprintf(&bits_hex_str, "%04x", x[i]);
    // io::printfn("%s", (String)&bits_hex_str);  // for testing

    byte_nbr = (i-1)*4;
    bits_hex[byte_nbr..byte_nbr+3] = bits_hex_str[0..3];  // 2026-01-25
  }
  // io::printfn("%s", (String)&bits_x);  // for testing
  // io::printfn("%s", (String)&bits_hex);  // for testing


  // write bit stream to disk:
  File? f1 = file::open(FILE_BITS_X, "w");
  if (catch excuse1 = f1) {
    io::printfn("could not open file for writing: %s -- %s", FILE_BITS_X, excuse1);
    main_return_val = 1;
  } else {
    usz? f1a = io::fprintf(&f1, "%s", (String)&bits_x);
    // https://c3-lang.org/faq/#patterns
    // https://c3-lang.org/standard-library/#usz-fprintfoutstream-out-string-format-args
    if (catch excuse1a = f1a) {
      io::printfn("could not write to file: %s -- %s", FILE_BITS_X, excuse1a);
      main_return_val = 2;
    } else {
      io::printfn("Bit stream has been written to disk under name:  %s", FILE_BITS_X);
    }
  }
  (void)f1.close();

  // write byte stream to disk:
  File? f2 = file::open(FILE_BITS_HEX, "w");
  if (catch excuse2 = f2) {
    io::printfn("could not open file for writing: %s -- %s", FILE_BITS_HEX, excuse2);
    main_return_val = 1;
  } else {
    usz? f2a = io::fprintf(&f2, "%s", (String)&bits_hex);
    if (catch excuse2a = f2a) {
      io::printfn("could not write to file: %s -- %s", FILE_BITS_HEX, excuse2a);
      main_return_val = 2;
    } else {
      io::printfn("Byte stream has been written to disk under name: %s", FILE_BITS_HEX);
    }
  }
  (void)f2.close();

  return main_return_val;
}

// end of random_streams_for_perf_stats.c3
