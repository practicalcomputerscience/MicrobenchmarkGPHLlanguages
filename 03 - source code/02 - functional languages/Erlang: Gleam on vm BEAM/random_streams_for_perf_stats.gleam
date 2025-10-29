// random_streams_for_perf_stats.gleam
//
// 2025-10-26/27
//
// install this package:    $ gleam add simplifile
//
// build on Ubuntu 24 LTS:  $ gleam new random_streams_for_perf_stats
//                          $ cd random_streams_for_perf_stats
//                          $ gleam test
//
// run on Ubuntu 24 LTS:    $ gleam run --no-print-progress
//                          $ time gleam run --no-print-progress --> real	0m0,234s
//                          $ ./exe_times_statistics_for_one_test_case_in_cwd2 "gleam run --no-print-progress"    
//
// $ gleam -V
// gleam 1.13.0
// $


import gleam/io
import gleam/string
import gleam/int  // for random
import gleam/list

import simplifile
// for writing to files: https://github.com/bcpeinhardt/simplifile/blob/d8e69974b2bfe9382e7c2ebeaf374950e0d4d6d0/src/simplifile.gleam


const end: Int = 62500  // 62500 for exactly 1M binary digits
// const end: Int = 10  // for testing

const m: Int = 65521  // = 2^16 - 15
const a: Int = 17364
const c: Int = 0

const file_bits_x:   String = "random_bitstring.bin"
const file_bits_hex: String = "random_bitstring.byte"


/////////////////////////////////////////////////////////////////////////////
//
// user defined functions

fn integer_to_bin_string(n: Int) -> String {
  let binary_string = int.to_base2(n)

  string.pad_start(binary_string, to: 16, with: "0")
  // https://hexdocs.pm/gleam_stdlib/gleam/string.html#pad_start
}

fn integer_to_hex_string(n: Int) -> String {
  let hex_string = int.to_base16(n)

  string.pad_start(hex_string, to: 4, with: "0") |> string.lowercase  // convert also to lower case
}


fn write_to_file(filename: String, content: String, file_type: String) {
  case file_type {
    "bit"  -> {
      let bit_file  = content |> simplifile.write(to: filename)  // using the pipe operator: |>
      // let assert Ok(_) is crashing the program, if not OK:+
      //   https://github.com/gleam-lang/cookbook/blob/55ed1da52a92db758cf8bb7d7459e59fdf3a3078/universal/test/file_system/work_with_files.gleam#L24
      // echo bit_file  // for testing: good case return value: Ok(Nil)
      case bit_file {
        Ok(Nil) -> {
          let _ = io.println("Bit stream has been written to disk under name:  " <> filename)
        }
        Error(e) -> {  // e is of type simplifile.FileError
          io.print("could not write to file: " <> filename)
          io.print(" -- " <> simplifile.describe_error(e) <> "\n")
        }
        // double error case demo (2 x no write access): $ gleam run --no-print-progress
        //
        // generating a random bit stream...
        // could not write to file: random_bitstring.bin -- Permission denied
        // could not write to file: random_bitstring.byte -- Permission denied
        // $
      }
    }

    "byte" -> {
      let byte_file = content |> simplifile.write(to: filename)
      case byte_file {
        Ok(Nil) -> {
          io.println("Byte stream has been written to disk under name: " <> filename)
        }
        Error(e) -> {
          io.print("could not write to file: " <> filename)
          io.print(" -- " <> simplifile.describe_error(e) <> "\n")
        }
      }
    }

    _ -> {
      let _empty_file = "" |> simplifile.write(to: "empty_file")  // this case should never happen
      io.println("no file has been written to disk")
    }
  }
}


// recursive master loop
fn masterloop(n: Int, seed: Int, x: List(Int), bits_x: List(String), bits_hex: List(String)) {
  // echo n  // for testing: debug print something with the echo keyword
  // echo seed  // for testing

  let bits_x_str   = integer_to_bin_string(seed)
  let bits_hex_str = integer_to_hex_string(seed)

  // echo bits_x_str    // for testing
  // echo bits_hex_str  // for testing

  let new_seed = { a * seed + c } % m  // use curly brackets to group expressions together

  case n {
    // return bits_x + bits_hex lists as a tuple
    // 0 -> #(x, bits_x, bits_hex)  // for testing
    0 -> #(bits_x, bits_hex)

    _ -> masterloop(n - 1, new_seed, [seed, ..x], [bits_x_str, ..bits_x], [bits_hex_str, ..bits_hex])
                                                            // .. is the cons operator
    // $ gleam test on [..x, new_seed]:
    //   Lists are immutable and singly-linked, so to append items to them
    //   all the elements of a list would need to be copied into a new list.
    //   This would be slow, so there is no built-in syntax for it.
    //   Hint: Prepend items to the list and then reverse it once you are done.
  }
}

//
// end of user defined functions
//
/////////////////////////////////////////////////////////////////////////////


pub fn main() {  // be careful here with: "pub fn main() -> Nil {"; look at the last return type!

  let start_seed = int.random(m)
  // m is exclusive: https://hexdocs.pm/gleam_stdlib/gleam/int.html#random
  // echo start_seed  // for testing

  io.println("\ngenerating a random bit stream...")

  // counting down from end; pass lists:
  // let #(random_numbers, bits_x, bits_hex) = masterloop (end, start_seed, [], [], [])  // for testing
  let #(bits_x, bits_hex) = masterloop (end, start_seed, [], [], [])  // for testing

  // echo random_numbers  // for testing
  // reverse this list due to prepending: https://hexdocs.pm/gleam_stdlib/gleam/list.html#reverse
  // let random_numbers_rev = list.reverse(random_numbers)
  // echo random_numbers_rev  // for testing

  // echo list.reverse(bits_x)  // for testing
  let bits_x   = string.concat(list.reverse(bits_x))
  // echo bits_x  // for testing

  // echo list.reverse(bits_hex)  // for testing
  let bits_hex = string.concat(list.reverse(bits_hex))
  // echo bits_hex  // for testing

  let _ = write_to_file(file_bits_x, bits_x, "bit")
  let _ = write_to_file(file_bits_hex, bits_hex, "byte")
}

// end of random_streams_for_perf_stats.gleam
