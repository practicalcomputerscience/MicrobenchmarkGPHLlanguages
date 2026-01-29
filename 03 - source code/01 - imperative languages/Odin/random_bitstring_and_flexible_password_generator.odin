/*
random_bitstring_and_flexible_password_generator.odin

2026-01-07/08

build on Ubuntu 24 LTS: $ odin build random_bitstring_and_flexible_password_generator.odin -file           # for developing
                        $ odin build random_bitstring_and_flexible_password_generator.odin -file -o:speed  # for release

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ odin version
odin version dev-2026-01-nightly
$

*/

package main

import "core:os"       // write_entire_file(), os.read()
import "core:fmt"      // println(), eprintln(), printf()
import "core:math/rand"
import "core:strings"  // builder_make(), builder_destroy()
import "core:strconv"  // parse_int()


main :: proc() {
	END : int : 62501  // 62501 for exactly 1M binary digits: constant
  // END  : int : 20  // for testing

  M1   : int : END * 16
  K250 : int : END * 4

  m : int : 65521  // = 2^16 - 15
  a : int : 17364
  c : int : 0

  file_bits_x   :: "random_bitstring.bin"
  file_bits_hex :: "random_bitstring.byte"

  // x: [END]int
  // => Warning: Declaration of 'x' may cause a stack overflow due
  //    to its type '[62501]int' having a size of 500008 bytes
  // =>
  x := make([]int, END)
  defer delete(x)

  x[0] = int(rand.int_max(m - 1))  // range is: [1...max]
  // fmt.eprintln("x[0] =", x[0])  // for testing

  bits_x   := strings.builder_make()  // strings.builder_make(0, M1) is not faster
  defer strings.builder_destroy(&bits_x)
  // see: https://github.com/odin-lang/Odin/blob/f9d9166ff11f3b6eeedb4355dfa930d69c40be8a/core/flags/usage.odin#L91

  bits_hex := strings.builder_make()
  defer strings.builder_destroy(&bits_hex)

  bits_x_str   := "0000000000000000"
  bits_hex_str := "0000"


  fmt.println("\ngenerating a random bit stream...")
  for i in 1..<END {  // i is a variable
	    // fmt.eprintln("\ni =", i)  // for testing
      x[i] = ((a * x[i-1]) + c) % m
      // fmt.eprintln("x[i] =", x[i])  // for testing

      bits_x_str = fmt.tprintf("%016b", x[i])   // t for temporary
      // fmt.eprintln("bits_x_str =", bits_x_str)  // for testing
      strings.write_string(&bits_x, bits_x_str)

      bits_hex_str = fmt.tprintf("%04x", x[i])  // t for temporary
      // fmt.eprintln("bits_hex_str =", bits_hex_str)  // for testing
      strings.write_string(&bits_hex, bits_hex_str)
  }

  bits_x_str_total := strings.to_string(bits_x)  // convert string builder into a string
  // fmt.eprintln("bits_x_str_total =", bits_x_str_total)  // for testing
  bits_hex_str_total := strings.to_string(bits_hex)
  // fmt.eprintln("bits_hex_str_total =", bits_hex_str_total)  // for testing

  // try to write the string to the file with error handling:
  // https://github.com/odin-lang/Odin/blob/f9d9166ff11f3b6eeedb4355dfa930d69c40be8a/core/unicode/tools/generate_entity_table.odin#L217
  written := os.write_entire_file(file_bits_x, transmute([]byte)bits_x_str_total)
	if written {
		fmt.printf("Bit stream has been written to disk under name:  %v", file_bits_x)
	} else {
		fmt.printf("could not write to file: %v", file_bits_x)
	}

  written = os.write_entire_file(file_bits_hex, transmute([]byte)bits_hex_str_total)
	if written {
		fmt.printf("\nByte stream has been written to disk under name: %v", file_bits_hex)
	} else {
		fmt.printf("\ncould not write to file: %v", file_bits_hex)
	}
  fmt.printf("\n")


  // make a password of N_CHAR printable chars: user input requested here
  N_CHAR : int
  answer := false
  buf    : [256]byte  // [52, 53, 10, 0, 0, 0, ..., 0] --> "45"
  for answer == false {
      N_CHAR = 12
      fmt.printf("\nPassword of %v printable chars OK? 'y' or another integer number >= 8: ", N_CHAR)
      n, err := os.read(os.stdin, buf[:])
      // https://github.com/odin-lang/examples/blob/master/console/read_console_input/read_console_input.odin
      if err != nil {
		      fmt.println("enter an integer number >= 8 or 'y'")
      }
      answer_str := string(buf[:n - 1])  // chump final new line
      // fmt.eprint("answer_str =", answer_str, "--")  // for testing

      if answer_str == "y" {
          answer = true
      } else {
          if input_nbr, parse_ok := strconv.parse_int(answer_str); parse_ok {
          // https://github.com/odin-lang/Odin/blob/f9d9166ff11f3b6eeedb4355dfa930d69c40be8a/core/strconv/strconv.odin#L351
          // https://github.com/odin-lang/Odin/blob/f9d9166ff11f3b6eeedb4355dfa930d69c40be8a/core/flags/usage.odin#L115
              if input_nbr < 8 {
                  fmt.println("enter an integer number >= 8 or 'y'")
              } else {
                  N_CHAR = input_nbr
                  answer = true
              }
          } else {
              fmt.println("enter an integer number >= 8 or 'y'")
          }
      }
	}
  // fmt.eprintln("\nN_CHAR =", N_CHAR)  // for testing


  WITH_SPECIAL_CHARS := true  // "true" default
  answer = false
  for answer == false {
      fmt.printf("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
      n, err := os.read(os.stdin, buf[:])
      if err != nil {
		      answer = false
      }
      answer_str := string(buf[:n - 1])  // chump final new line

      if answer_str == "y" {
          answer = true
      } else {
          WITH_SPECIAL_CHARS = false
          answer = true
      }
	}
  // fmt.eprintln("\nWITH_SPECIAL_CHARS =", WITH_SPECIAL_CHARS)  // for testing


  // char_set :=  [95]u8
  // => instead, also using a dynamic string builder here:
  char_set_sb := strings.builder_make()
  defer strings.builder_destroy(&char_set_sb)
  if WITH_SPECIAL_CHARS {
      for codepoint in 33..< 127 {
          char: rune = rune(codepoint)  // first, convert a codepoint into a rune: this is the trick here!!
          // this is not featured in the https://odin-lang.org/docs/overview page,
          // but can be found in the GitHub repository! For example here:
          // https://github.com/odin-lang/Odin/blob/f9d9166ff11f3b6eeedb4355dfa930d69c40be8a/core/text/scanner/scanner.odin#L296
          s := fmt.tprintf("%r", char)  // then, convert this rune into a string
          strings.write_string(&char_set_sb, s)  // finally, append this string to a string builder
      }
  } else {
      strings.write_string(&char_set_sb, "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
  }
  char_set := strings.to_string(char_set_sb)  // convert string builder into a string
  // fmt.eprintln("\nchar_set =", char_set)  // for testing


  // again, using dynamic string builder:
  pw_chars_sb := strings.builder_make()
  defer strings.builder_destroy(&pw_chars_sb)
  i := 0  // char counter for the password
  j := 0  // counter for x

  for i < N_CHAR {
      bin0 := fmt.tprintf("%016b", x[j])
      // fmt.eprintln("\nbin0 =", bin0)  // for testing

      bin0_0 := bin0[0:9]
      bin0_1 := bin0[8:16]
      // fmt.eprintln("bin0_0 =", bin0_0)  // for testing
      // fmt.eprintln("bin0_1 =", bin0_1)  // for testing

      char0a, parse_ok_0a := strconv.parse_int(bin0_0, 2)
      char1a, parse_ok_1a := strconv.parse_int(bin0_1, 2)
      // fmt.eprintln("char0a =", char0a)  // for testing
      // fmt.eprintln("char1a =", char1a)  // for testing

      char0 : rune = rune(char0a)  // convert int into rune
      char1 : rune = rune(char1a)
      // fmt.eprintln("char0 =", char0)  // for testing
      // fmt.eprintln("char1 =", char1)  // for testing

      if strings.contains_rune(char_set, char0) {
          s := fmt.tprintf("%r", char0)  // convert this rune into a string
          strings.write_string(&pw_chars_sb, s)  // append this string to a string builder
          i += 1
      }

      if strings.contains_rune(char_set, char1) && i < N_CHAR {
          s := fmt.tprintf("%r", char1)  // convert this rune into a string
          strings.write_string(&pw_chars_sb, s)  // append this string to a string builder
          i += 1
      }

      j += 1
  }

  pw_chars := strings.to_string(pw_chars_sb)  // convert string builder into a string
  fmt.printf("\nYour password of %v characters is: %v\n", N_CHAR, pw_chars)
}

// end of random_bitstring_and_flexible_password_generator.odin


