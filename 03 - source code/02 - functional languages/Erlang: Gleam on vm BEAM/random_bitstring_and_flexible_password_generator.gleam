// random_bitstring_and_flexible_password_generator.gleam
//
// 2025-10-27/28/29
//
// install these packages:  $ gleam add simplifile
//
// build on Ubuntu 24 LTS:  $ gleam new random_bitstring_and_flexible_password_generator
//                          $ cd random_bitstring_and_flexible_password_generator
//                          $ gleam test
//
// run on Ubuntu 24 LTS:    $ gleam run --no-print-progress
//
// $ gleam -V
// gleam 1.13.0
// $

import gleam/io
import gleam/string
import gleam/int  // for random, parse (string to integer)
import gleam/list
import gleam/result  // for unwrap
// import gleam/option

import simplifile
// for writing to files: https://github.com/bcpeinhardt/simplifile/blob/d8e69974b2bfe9382e7c2ebeaf374950e0d4d6d0/src/simplifile.gleam


const end: Int = 62500  // 62500 for exactly 1M binary digits
// const end: Int = 25  // for testing

const m: Int = 65521  // = 2^16 - 15
const a: Int = 17364
const c: Int = 0

const n_char_default = 12

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


// Declare an external function binding to Erlang's function: io:get_line/1
@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> String

fn input_a_valid_number(n_char: Int) -> Int {
  io.print("Password of " <> int.to_string(n_char) <> " printable chars OK? 'y' or another integer number >= 8: ")
  // from user input from console:
  let answer_str = string.trim(get_line(""))
  // echo answer_str  // for testing

  case answer_str {
    "y" -> 12
    _   -> {
      let int_of_string_opt = int.parse(answer_str)
      case int_of_string_opt {
        // Ok(value) -> result.unwrap(int_of_string_opt, 0)  // https://hexdocs.pm/gleam_stdlib/gleam/result.html#unwrap
        Ok(value) -> {
          case value {
            value if value >= 8 -> value
            _ -> {
              io.println("enter an integer number >= 8 or 'y'")
              input_a_valid_number(n_char_default)
            }
          }
        }
        Error(_)  -> {
          io.println("enter an integer number >= 8 or 'y'")
          input_a_valid_number(n_char_default)
        }
      }
    }
  }
}


fn answer_yes_or_no () -> Bool {
  io.print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  let answer_str = string.trim(get_line(""))

  case answer_str {
    "y" -> True
    _   -> False
  }
}


// recursive password loop
//   this solution is based on the MLton Standard ML solution
//   j: char counter for x
fn pw_generator (j: Int, pw_str: String, n: Int, x: List(Int), char_set: String) -> String {

  let next_elem_x = result.unwrap(list.first(x), 0)  // type of next_elem_x: Result(Int, Nil)
  // echo next_elem_x  // for testing
  let next_x      = result.unwrap(list.rest(x),[])

  let bin0 = integer_to_bin_string(next_elem_x)
  // echo bin0  // for testing

  let bin0_0 = string.slice(from: bin0, at_index: 0, length: 8)
  let bin0_1 = string.slice(from: bin0, at_index: 8, length: 8)
  // echo bin0_0  // for testing
  // echo bin0_1  // for testing

  let char0 = result.unwrap(int.base_parse(bin0_0, 2), 0)  // "00111001" --> 57
  let char1 = result.unwrap(int.base_parse(bin0_1, 2), 0)
  // echo char0  // for testing
  // echo char1  // for testing

  let codepoints0 = list.filter_map([char0], fn(n) { string.utf_codepoint(n) })  // ... --> [57]
  let codepoints1 = list.filter_map([char1], fn(n) { string.utf_codepoint(n) })
  // echo codepoints0  // for testing
  // echo codepoints1  // for testing

  let char0a = string.from_utf_codepoints(codepoints0)  // ... --> "9"
  let char1a = string.from_utf_codepoints(codepoints1)
  // echo char0a  // for testing
  // echo char1a  // for testing

  let char0_add = {
    case string.contains(does: char_set, contain: char0a) {
      True  -> char0a
      False -> ""
    }
  }
  // echo char0_add  // for testing

  let char1_add = {
    case string.contains(does: char_set, contain: char1a) {
      True  -> char1a
      False -> ""
    }
  }
  // echo char0_add  // for testing


  let new_pw_str  = pw_str <> char0_add <> char1_add
  let new_pw_size = string.length(new_pw_str)
  
  case new_pw_size >= n {
    True  -> new_pw_str
    False -> pw_generator (j+1, new_pw_str, n, next_x, char_set)  // recursion
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
    0 -> #(x, bits_x, bits_hex)  // for testing

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
  let #(x, bits_x, bits_hex) = masterloop (end, start_seed, [], [], [])
  // echo x  // for testing
  // reverse this list due to prepending: https://hexdocs.pm/gleam_stdlib/gleam/list.html#reverse
  let x_rev = list.reverse(x)
  // echo x_rev  // for testing

  // echo list.reverse(bits_x)  // for testing
  let bits_x   = string.concat(list.reverse(bits_x))
  // echo bits_x  // for testing

  // echo list.reverse(bits_hex)  // for testing
  let bits_hex = string.concat(list.reverse(bits_hex))
  // echo bits_hex  // for testing

  let _ = write_to_file(file_bits_x, bits_x, "bit")
  let _ = write_to_file(file_bits_hex, bits_hex, "byte")
  io.println("")


  let n_char = input_a_valid_number(n_char_default)
  // echo n_char  // for testing

  let with_special_chars = answer_yes_or_no ()
  // echo with_special_chars  // for testing

  let char_set = {
    case with_special_chars {
      True ->  // char_range '!' '~'
        {let int_numbers = list.range(33, 126)  // make a list of integers of Unicode codepoints

         // convert integers to codepoints --> [Ok(33), Ok(34), Ok(35), Ok(36), ...]
         let codepoints  = list.filter_map(int_numbers, fn(n) { string.utf_codepoint(n) })
         // echo codepoints  // for testing
         string.from_utf_codepoints(codepoints)  // convert codepoints to strings

        }

      False ->
        {let int_numbers1 = list.range(48, 57)
         let codepoints1  = list.filter_map(int_numbers1, fn(n) { string.utf_codepoint(n) })
         // echo codepoints1  // for testing
         let int_numbers2 = list.range(65, 90)
         let codepoints2  = list.filter_map(int_numbers2, fn(n) { string.utf_codepoint(n) })

         let int_numbers3 = list.range(97, 122)
         let codepoints3  = list.filter_map(int_numbers3, fn(n) { string.utf_codepoint(n) })

         string.from_utf_codepoints(codepoints1) <> string.from_utf_codepoints(codepoints2) <>
         string.from_utf_codepoints(codepoints3)
        }
    }
  }
  // echo char_set  // for testing


  let pw_chars = pw_generator(0, "", n_char, x_rev, char_set)
  io.println("\nYour password of " <> int.to_string(n_char) <> " characters is: " <> pw_chars)
}

// end of random_bitstring_and_flexible_password_generator.gleam
