/*
random_streams_for_perf_stats.odin

2026-01-07/08

build on Ubuntu 24 LTS: $ odin build random_streams_for_perf_stats.odin -file           # for developing
                        $ odin build random_streams_for_perf_stats.odin -file -o:speed  # for release
                        $ (odin build random_streams_for_perf_stats.odin -file -o:aggressive => no better speed)

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats # -o:speed      => real	0m0.014s
                        ($ time ./random_streams_for_perf_stats # -o:aggressive => real	0m0.014s)

                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats


$ odin version
odin version dev-2026-01-nightly
$

*/

package main

import "core:os"   // write_entire_file
import "core:fmt"  // println, eprintln, printf
import "core:math/rand"
import "core:strings"  // builder_make, builder_destroy


main :: proc() {
	END : int : 62501  // 62501 for exactly 1M binary digits: constant
  // END  : int : 10  // for testing

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
      x[i] = ((a * x[i-1]) + c) % m;
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
}

// end of random_streams_for_perf_stats.odin
