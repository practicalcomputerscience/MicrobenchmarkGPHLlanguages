/*
random_bitstring_and_flexible_password_generator.v

2025-05-20/21/31

build on Ubuntu 24 LTS: $ v random_bitstring_and_flexible_password_generator.v
  build for production: $ v -prod random_bitstring_and_flexible_password_generator.v

run on Ubuntu 24 LTS:   $ v run random_bitstring_and_flexible_password_generator.v
or the compiled binary: $ ./random_bitstring_and_flexible_password_generator

Old version with string concatenation: mut bits_x   := ""; bits_x += bits_x_str1 + inbuilt number to string conversion functions
    generating a random bit stream...
    Bit stream has been written to disk under name:  random_bitstring.bin
    Byte stream has been written to disk under name: random_bitstring.byte
    this took 2213ms to run

$ v version
V 0.4.10 ddfedc7
$

*/

module main

import os
import rand        // https://modules.vlang.io/rand.html
import rand.seed
import rand.pcg32
import strconv     // provides functions for converting strings to numbers and numbers to strings
                   // https://modules.vlang.io/strconv.html#format_uint
import strings     // strings.Builder


// const can only be defined at the top level (outside of functions):
const upper_limit := 62501   // 62501 for exactly 1M binary digits; type must be int for later array use
// const upper_limit := 50   // for testing
const str_length_bin := 16
const str_length_hex := 4


fn main() {
  m1   := (upper_limit - 1) * 16
  k250 := (upper_limit - 1) * 4

  m := 65521  // = 2^16 - 15
  a := 17364
  c := 0

  file_bits_x   := "random_bitstring.bin"
  file_bits_hex := "random_bitstring.byte"


  mut x := []int{cap: upper_limit}
  // Setting the capacity improves performance of pushing elements to the array as reallocations can be avoided
  // https://docs.vlang.io/v-types.html#array-initialization

  // mut bits_x   := ""
  // mut bits_hex := ""  // needed for program ENT - A Pseudorandom Number Sequence Test Program
  // strings.Builder is used to efficiently append many strings to a large dynamically growing buffer,
  // then use the resulting large string. Using a string builder is much better for performance/memory
  // usage than doing constantly string concatenation.

  mut bits_x   := strings.new_builder(m1)
  mut bits_hex := strings.new_builder(k250)

  // Initialise the generator struct (note the `mut`):
  mut rng := &rand.PRNG(pcg32.PCG32RNG{})  // https://modules.vlang.io/rand.html
  // Optionally seed the generator:
  rng.seed(seed.time_seed_array(pcg32.seed_len))

  // x[0] := ... is not working in V!
  // !!you cannot **update** an individual array element, be it a fixed size array or not!! => this smells like functional programming
  // https://modules.vlang.io/rand.html#PRNG.u32n
  x << int(rand.u32n(u32(m))!)

  println("\ngenerating a random bit stream...")
  for i := 1; i < upper_limit; i++ {
    x_now := (a*x[i-1] + c) % m  // x[i] := ... is not working in V!
    // println(x_now)  // for testing
    x << x_now

    // bit stream:
    //
    // https://modules.vlang.io/strconv.html#format_int
    // bits_x_str0 := strconv.format_int(i64(x_now), 2)   // non padded return string: 13892 =>   11011001000100
    // bits_x_str1 := strconv.format_str(bits_x_str0, b)  // padding to "16 bits":    13892  => 0011011001000100
    // mut b := strconv.BF_param{}  // https://modules.vlang.io/strconv.html#BF_param -- BF_param Struct
    // https://github.com/vlang/v/blob/b21dbbb420d25c3274b76642f407d9dd3ba56094/vlib/strconv/format.md?plain=1#L241
    // b.pad_ch = u8(`0`)  // https://modules.vlang.io/strconv.html#BF_param
    // b.len0 = 16
    // !!it's not working if b is outside of this loop!!
    //   cf. https://github.com/vlang/v/blob/4dc34650466779e00b037ed0984cdb543440ab6f/vlib/strconv/format.v#L80
    //
    // => writing my own functions based on the Inko code:
    bits_x_str0 := integer_to_bin_string(x_now)
    // println(bits_x_str0)   // for testing
    bits_x.write_string(bits_x_str0)


    // byte stream for program ENT:
    bits_hex_str0 := integer_to_hex_string(x_now)
    // println(bits_hex_str0)   // for testing
    bits_hex.write_string(bits_hex_str0)
  }
  // println(bits_x)    // for testing
  // println(bits_hex)  // for testing

  // writing streams to files:
  // due to return requirement:
  //   return for: error: `or` block must provide a default value of type `int`,
  //   or return/continue/break or call a @[noreturn] function like panic(err) or exit(1)
  // put writing to files in its own function:
  string_to_file (file_bits_x, bits_x, "Bit")
  string_to_file (file_bits_hex, bits_hex, "Byte")


  // make a password of n_char printable chars: user input requested here
  mut n_char := 12
  mut answer := false
  mut answer_str := ""
  for !answer {
    n_char = 12
    answer_str = os.input("\nPassword of ${n_char} printable chars OK? 'y' or another integer number >= 8: ")
    // println("answer_str = ${answer_str}---")  // for testing
    if answer_str == "y" {
      answer = true
    } else {
      n_char_i64 := strconv.parse_int(answer_str, 10, 32) or {
        println("enter an integer number >= 8 or 'y'")
        continue
      }
      n_char = int(n_char_i64)
      if n_char < 8 {
        println("enter an integer number >= 8 or 'y'")
      } else {
        answer = true
      }
    }
  }
  // println("n_char = ${n_char}")  // for testing


  mut with_special_chars := true
  answer = false
  answer_str = ""
  for !answer {
    answer_str = os.input("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
    if answer_str == "y" {
      answer = true
    } else {
      with_special_chars = false
      answer = true
    }
  }


  mut char_set := ""
  if with_special_chars {
    for i := 33; i < 127; i++ {
     char_set += utf32_to_str(u32(i))
    }
  } else {
    char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  }
  // println("char_set = ${char_set}")  // for testing


  mut i := 0  // char counter for the password
  mut j := 0  // char counter for x
  mut pw_chars := ""

  for i < n_char {  // char counter for the password
    bin0  := integer_to_bin_string(x[j])

    bin0_0 := bin0[0..8]
    bin0_1 := bin0[8..16]

    char0a, _ := strconv.common_parse_uint2(bin0_0, 2, 16)
    char0b, _ := strconv.common_parse_uint2(bin0_1, 2, 16)
    // print("char0a = ${char0a}  --  char0b = ${char0b}\n")  // for testing

    char1a := utf32_to_str(u32(char0a))
    char1b := utf32_to_str(u32(char0b))
    // print("char1a = ${char1a}  --  char1b = ${char1b}\n")  // for testing

    if char_set.contains(char1a) {
      pw_chars += char1a
      i += 1
    }

    if char_set.contains(char1b) && i < n_char {
      pw_chars += char1b
      i += 1
    }

    j += 1
  }

  println("\nYour password of $n_char characters is: ${pw_chars}")
}



///////////////////////////////////////////////////////
//
// user defined functions

fn integer_to_bin_string (n int) string {
  mut bin_str_raw := strings.new_builder(str_length_bin)
  mut bin_str_ret := strings.new_builder(str_length_bin)

  mut j := str_length_bin - 1
  mut k := n
  mut l := 0

  for k > 0 && j >= 0 {
    if k % 2 == 0 {
      bin_str_raw.write_u8(`0`)
    } else {
      bin_str_raw.write_u8(`1`)
    }
    k /= 2  // no remainder here
    j -= 1
    l += 1
  }

  diff := str_length_bin - l
  // diff > 0 ==> padding on the right needed:
  if diff > 0 {
     for k = 0; k < diff; k++ {
       bin_str_raw.write_u8(`0`)
     }
  }

  // reverse char order:
  for k = str_length_bin - 1; k >= 0; k-- {
    bin_str_ret.write_u8(bin_str_raw[k])
  }

  return bin_str_ret.str()
}


fn integer_to_hex_string (n int) string {
  mut hex_str_raw := strings.new_builder(str_length_hex)
  mut hex_str_ret := strings.new_builder(str_length_hex)

  mut j := str_length_hex - 1
  mut k := n
  mut remainder := 0

  for k > 0 && j >= 0 {
    remainder = k % 16
    match remainder {
      0   {hex_str_raw.write_u8(`0`)}
      1   {hex_str_raw.write_u8(`1`)}
      2   {hex_str_raw.write_u8(`2`)}
      3   {hex_str_raw.write_u8(`3`)}
      4   {hex_str_raw.write_u8(`4`)}
      5   {hex_str_raw.write_u8(`5`)}
      6   {hex_str_raw.write_u8(`6`)}
      7   {hex_str_raw.write_u8(`7`)}
      8   {hex_str_raw.write_u8(`8`)}
      9   {hex_str_raw.write_u8(`9`)}
      10  {hex_str_raw.write_u8(`a`)}
      11  {hex_str_raw.write_u8(`b`)}
      12  {hex_str_raw.write_u8(`c`)}
      13  {hex_str_raw.write_u8(`d`)}
      14  {hex_str_raw.write_u8(`e`)}
      15  {hex_str_raw.write_u8(`f`)}
      else  {hex_str_raw.write_u8(`_`)}
    }
    k /= 16  // no remainder here
    j -= 1
  }

  diff := str_length_hex - 1
  // diff > 0 ==> padding on the right needed:
  if diff > 0 {
     for k = 0; k < diff; k++ {
       hex_str_raw.write_u8(`0`)
     }
  }

  // reverse char order:
  for k = str_length_hex - 1; k >= 0; k-- {
    hex_str_ret.write_u8(hex_str_raw[k])
  }

  return hex_str_ret.str()
}


fn string_to_file (file_name string, str_b strings.Builder, stream string ) {
  mut f := os.create(file_name) or {
    println("could not write to file: ${file_name} !")
    return
  }

  f.write(str_b) or {
    println("could not write to file: ${file_name} !")
    return
  }
  f.close()

  println("${stream} stream has been written to disk under name: ${file_name}")
}

// end of user defined functions
//
///////////////////////////////////////////////////////

// end of random_bitstring_and_flexible_password_generator.v
