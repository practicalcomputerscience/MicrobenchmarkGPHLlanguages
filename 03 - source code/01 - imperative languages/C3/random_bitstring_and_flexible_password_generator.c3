/*
random_bitstring_and_flexible_password_generator.c3

2025-05-30; 2025-12-17: see below; 2026-01-09: deleted one outdated definition

build on Ubuntu 24 LTS: $ c3c compile -O3 random_bitstring_and_flexible_password_generator.c3 --max-stack-object-size 262144

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-O0  - Safe, no optimizations, emit debug info.
-O1  - Safe, high optimization, emit debug info.
-O2  - Unsafe, high optimization, emit debug info.
-O3  - Unsafe, high optimization, single module, emit debug info. <<<<<<< more stable exe times and lower exe times ("wall clock")
-O4  - Unsafe, highest optimization, relaxed maths, single module, emit debug info, no panic messages.
-O5  - Unsafe, highest optimization, fast maths, single module, emit debug info, no panic messages, no backtrace.
-Os  - Unsafe, high optimization, small code, single module, no debug info, no panic messages.
-Oz  - Unsafe, high optimization, tiny code, single module, no debug info, no panic messages, no backtrace.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$ c3c -V
C3 Compiler Version:       0.7.8
Installed directory:       ~/scripts/C3/c3-ubuntu-22/c3
Git Hash:                  5961bcaf866a1d866747c12426436864ebaa4cb1
Backends:                  LLVM
LLVM version:              19.1.7
LLVM default target:       x86_64-pc-linux-gnu
$

*/


import std::io;
import std::math::random;
import std::time;  // now()


const int END = 62500;  // 62501 for exactly 1M binary digits; val is immutable
// const int END  = 50;     // for testing
const int M1   = END * 16;
const int K250 = END * 4;

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


  io::printn("generating a random bit stream...");
  for (int i = 1; i < END; i++) {
    x[i] = ((A * x[i-1]) + C) % M;
    // io::printfn("\n%d", x[i]);      // for testing

    io::bprintf(&bits_x_str, "%016b", x[i]);
    // io::printfn("%s", (String)&bits_x_str);  // for testing
    // https://c3-lang.org/standard-library/#char-bprintfchar-buffer-string-format-args-maydiscard
    // https://c3-lang.org/language-fundamentals/basic-types-and-values/#printing-values

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

    io::bprintf(&bits_hex_str, "%04x", x[i]);
    // io::printfn("%s", (String)&bits_hex_str);  // for testing

    byte_nbr = (i-1)*4;
    bits_hex[byte_nbr]     = bits_hex_str[0];
    bits_hex[byte_nbr+1]   = bits_hex_str[1];
    bits_hex[byte_nbr+2]   = bits_hex_str[2];
    bits_hex[byte_nbr+3]   = bits_hex_str[3];
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



  // make a password of n_char printable chars: user input requested here
  int n_char = 12;
  bool answer = false;

  while (answer == false) {
    io::printf("\nPassword of %d printable chars OK? 'y' or another integer number >= 8: ", n_char);
    // printf => no terminal new line here

    String? answer_str = io::treadline();
    // \r will be filtered from the String.
    // https://c3-lang.org/standard-library/#string-treadlinestream--iostdin
    if (catch err1 = answer_str) {
       io::printfn("enter an integer number >= 8 or 'y'");
    } else {
      if (answer_str == "y") {
        answer = true;
      } else {
        int? n_char_ = String.to_int(answer_str, 10);
        if (catch err2 = n_char_) {
          io::printfn("enter an integer number >= 8 or 'y'");
        } else {
          if (n_char_ < 8) {
            io::printfn("enter an integer number >= 8 or 'y'");
          } else {
            n_char = n_char_;
            answer = true;
          }
        }
      }
    }
  }
  // io::printfn("\nn_char = %d", n_char);  // for testing


  bool with_special_chars = true;  // "true" default
  answer = false;
  while (answer == false) {
    io::printf("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");

    String? answer_str = io::treadline();

    if (catch err3 = answer_str) {
       io::printfn("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
    } else {
      if (answer_str == "y") {
        answer = true;
      } else {
        with_special_chars = false;
        answer = true;
      }
    }
  }


  // DString: dynamic string builder that supports various string operations at runtime,
  // allowing for flexible manipulation without the need for manual memory allocation.
  DString char_set;

  if (with_special_chars) {
    for (char chr = 33; chr < 127; chr++) {
      DString.appendf(&char_set, "%c", chr);
    }
  } else {
    DString.appendf(&char_set, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
  }
  // io::printfn("char_set = %s", char_set);  // for testing
  // io::printfn("len of char_set = %d\n", DString.len(&char_set));  // for testing


  DString pw_chars;
  char[16] bin0;
  char[8] bin0_0;
  char[8] bin0_1;

  int i = 0;  // char counter for the password
  int j = 0;  // char counter for x

  String char_set_str = DString.str_view(char_set);

  while (i < n_char) {
    // io::printfn("\n%d", x[j]);  // for testing
    io::bprintf(&bin0, "%016b", x[j]);
    // io::printfn("%s", (String)&bin0);  // for testing

    bin0_0 = bin0[0 .. 7];   // char[8]
    bin0_1 = bin0[8 .. 15];
    // io::printfn("bin0_0 = %s -- bin0_1 = %s", (String)&bin0_0, (String)&bin0_1);  // for testing

    int char0a = String.to_int((String)&bin0_0, 2)!!;  // Rethrow `!`
    int char1a = String.to_int((String)&bin0_1, 2)!!;
    // io::printfn("char0a = %d -- char1a = %d", char0a, char1a);  // for testing

    char char0b = (char)char0a;
    char char1b = (char)char1a;
    // io::printfn("char0b = %c -- char1b = %c", char0b, char1b);  // for testing

    ulong? pos1 = String.index_of_char(char_set_str, char0b);
    if (catch excuse11 = pos1) {
      ;
    } else {
      DString.appendf(&pw_chars, "%c", char0b);
      i++;
    }


    ulong? pos2 = String.index_of_char(char_set_str, char1b);
    if (catch excuse12 = pos2) {
      ;
    } else {
      if (i < n_char) {
        DString.appendf(&pw_chars, "%c", char1b);
        i++;
      }
    }

    j++;
  }

  io::printfn("\nYour password of %d characters is: %s\n", n_char, pw_chars);  // for testing

  return main_return_val;
}

// end of random_bitstring_and_flexible_password_generator.c3
