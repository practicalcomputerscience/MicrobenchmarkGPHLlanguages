/* random_bitstring_and_flexible_password_generator.kk

2025-06-23

test on Ubuntu 24 LTS: OK

build on Ubuntu 24 LTS: $ koka -O2 -o random_bitstring_and_flexible_password_generator random_bitstring_and_flexible_password_generator.kk

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


Koka is an open source programming language created in 2012 by Daan Leijen
https://pldb.io/concepts/koka.html


$ koka --version
Koka 3.2.2, 20:30:51 Jul 22 2025 (ghc release version)
...
$

*/


module random_bitstring_and_flexible_password_generator


import std/num/random
import std/num/int32
import std/os/path
import std/os/file
import std/os/readline


fun main()  // fun: named function
  val upper_limit = 62501  // 62501 for exactly 1M binary digits; val is immutable
  // val upper_limit = 50  // for testing

  val m = 65521  // = 2^16 - 15
  val a = 17364
  val c = 0

  val file_bits_x   = "random_bitstring.bin"
  val file_bits_hex = "random_bitstring.byte"

  val x0 = srandom-int32-range(1.int32, m.int32).int  // strong random int32 uniformly distributed in the range [lo,hi).
  // println("\nx0 = " ++ x0.show)  // for testing

  // x: for implementing the OCaml solution with a Mutable array --> Todo:
  //   https://github.com/koka-lang/koka/blob/c9c313f09cc10c24386f97f3d80a22f3f132baed/lib/std/data/array.kk#L12
  //
  // though list append and string append exist:
  //   v2.1.1, 2021-03-08: bug fixes, use right-associative (++) for string- and list append (instead of (+)),
  //   improved internal string handling.
  //   https://github.com/koka-lang/koka/tree/c9c313f09cc10c24386f97f3d80a22f3f132baed

  var bits_x   := ""
  var bits_hex := ""

  val n_char_default = 12


  println("\ngenerating a random bit stream...")

  //----------------------  recursive master loop  -----------------------------
  //
  fun masterloop(i : int, seed : int, x_ : list<int>) : <div|_e> list<int>
    // <div|_e> for needed effect typing:
    //   div: may not terminate (diverge)
    //   _e:  wildcard effect type

    if i == upper_limit then
      x_
    else
      // println("\ni = " ++ i.show)

      val next-seed = (a * seed + c) % m
      // println("  next-seed = " ++ next-seed.show)  // for testing

      val bits_x_str = integer_to_bin_string(next-seed)
      // println("  bits_x_str = " ++ bits_x_str)  // for testing
      bits_x := bits_x ++ bits_x_str

      val bits_hex_str = integer_to_hex_string(next-seed)
      // println("  bits_hex_str = " ++ bits_hex_str)  // for testing
      bits_hex := bits_hex ++ bits_hex_str

      masterloop(i + 1, next-seed, x_ ++ [next-seed])  // recursion

  val x = masterloop(1, x0, [])

  // println("\nbits_x = " ++ bits_x)  // for testing
  // println("bits_hex = " ++ bits_hex)  // for testing

  // write bit stream to disk:
  //   https://github.com/koka-lang/koka/blob/c9c313f09cc10c24386f97f3d80a22f3f132baed/lib/std/os/file.kk#L28
  //   https://github.com/koka-lang/koka/blob/c9c313f09cc10c24386f97f3d80a22f3f132baed/test/mask/inject-behind1.kk
  try {
    write-text-file(file_bits_x.path, bits_x)  // this function has exception handling
    println("Bit stream has been written to disk under name:  " ++ file_bits_x)
  } fn(exn) {
    println("could not write to file: " ++ file_bits_x ++ " !")
  }

  // write byte stream to disk:
  try {
    write-text-file(file_bits_hex.path, bits_hex)  // this function has exception handling
    println("Byte stream has been written to disk under name: " ++ file_bits_hex)
  } fn(exn) {
    println("could not write to file: " ++ file_bits_hex ++ " !")
  }


  // make a password of n_char printable chars: user input requested here
  val n_char = input_a_valid_number(n_char_default)
  // println("\nmain: n_char = " ++ n_char.show)  // for testing

  val with_special_chars = answer_yes_or_no()
  // println("\nmain: with_special_chars = " ++ with_special_chars.show)  // for testing

  var char_set := ""
  if with_special_chars then
    char_set := cp_string(33, 126)  // both inclusive
  else
    char_set := cp_string(48, 57)  // both inclusive
    char_set := char_set ++ cp_string(65, 90)
    char_set := char_set ++ cp_string(97, 122)
  // println("\nchar_set = " ++ char_set)  // for testing


  // so far (2025-06-23) only outsourced in this programming language:
  val pw_chars = pw_generator(n_char, x, char_set)

  println("\nYour password of " ++ n_char.show ++ " characters is: " ++ pw_chars ++ "\n")
  // Last statement in a block must be an expression



///////////////////////  user defined functions  ////////////////////////////
//

fun integer_to_bin_string(nbr : int) : <div|_e> string  // <div|_e> for effect typing
// this function is done according to the function of same name in the OCaml solution
  var buffer   := ""
  var aux_char := "0"  // this is a literal string

  fun aux (n : int, j : int)
    if n >= 0 && j >= 1 then
      aux (n / 2, j - 1)

      if n % 2 == 0 then
        aux_char := "0"
      else
        aux_char := "1"

      buffer := buffer ++ aux_char  // string concatenation

  if nbr == 0 then
    "0000000000000000"
  else
    aux(nbr, 16)
    buffer


fun integer_to_hex_string(nbr : int) : <div|_e> string  // <div|_e> for effect typing
// this function is needed because of lack of formatted printing in Koka
// it's different from user defined functions hex_str_generate + fix_hex_zeros in the Roc solution
  var buffer   := ""
  var aux_char := "0"  // this is a literal string

  fun aux (n : int, j : int)
    if n >= 0 && j >= 1 then
      aux (n / 16, j - 1)

      val mod_ = n % 16
      if mod_ == 0 then
        aux_char := "0"
      else
        if mod_ == 1 then
          aux_char := "1"
        else
          if mod_ == 2 then
            aux_char := "2"
          else
            if mod_ == 3 then
              aux_char := "3"
            else
              if mod_ == 4 then
                aux_char := "4"
              else
                if mod_ == 5 then
                  aux_char := "5"
                else
                  if mod_ == 6 then
                    aux_char := "6"
                  else
                    if mod_ == 7 then
                      aux_char := "7"
                    else
                      if mod_ == 8 then
                        aux_char := "8"
                      else
                        if mod_ == 9 then
                          aux_char := "9"
                        else
                          if mod_ == 10 then
                            aux_char := "a"
                          else
                            if mod_ == 11 then
                              aux_char := "b"
                            else
                              if mod_ == 12 then
                                aux_char := "c"
                              else
                                if mod_ == 13 then
                                  aux_char := "d"
                                else
                                  if mod_ == 14 then
                                    aux_char := "e"
                                  else
                                    if mod_ == 15 then
                                      aux_char := "f"
                                    else
                                      aux_char := " "

      buffer := buffer ++ aux_char  // string concatenation

  if nbr == 0 then
    "0000"
  else
    aux(nbr, 4)
    buffer


fun input_a_valid_number(n_char : int) : <console|_e> int
  print("\nPassword of " ++ n_char.show ++ " printable chars OK? 'y' or another integer number >= 8: ")  // not println here
  val answer_str = readline()

  if answer_str == "y" then
    n_char

  // https://github.com/koka-lang/koka/blob/c9c313f09cc10c24386f97f3d80a22f3f132baed/lib/std/core/int.kk#L275
  else
    val n_char_ = parse-int(answer_str).default(0)
    // returned type: maybe<int>; with .default(0) illegal integer numbers will be defaulted to 0

    if n_char_ < 8 then
      println("enter an integer number >= 8 or 'y'")
      input_a_valid_number(n_char)
    else
      n_char_


fun answer_yes_or_no(): <console|_e> bool
  print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
  val answer_str = readline()
  // println(answer_str)  // for testing

  if answer_str == "y" then
    True
  else
    False


fun cp_string(start : int, end : int) : <div|_e> string
  var i := start
  var str := ""
  while { i <= end }
    str       := str ++ char/string(int/char(i))
    i         := i + 1
  return str


fun binary_str_to_unsigned_int(inp_str : string) : <div|_e> int
// following: random_bitstring_and_flexible_password_generator.inko
  var out_nbr := 0  // result
  var power := 2
  var j := 0
  val char_v = inp_str.vector  // Convert a string to a vector of characters.
  var pos := char_v.length - 1

  while { j < char_v.length }
    val digit = @index(char_v, pos)

    if digit == '1' then
      out_nbr := out_nbr + pow(power, j)  // 48 --> '0', 49 --> '1'

    pos := pos - 1
    j   := j + 1

  return out_nbr



//------------------  iterative password creation  ---------------------------
//
// similar to the Racket solution
fun pw_generator(n_char_ : int, x_ : list<int>, char_set_ : string)
  var pw_chars_ := ""
  var j         := 0  // counter for x
  val x_v        = x_.vector  // convert list into vector since vector allows access to individual elements
                              // like in binary_str_to_unsigned_int
  var char0_add := ""
  var char1_add := ""

  while { pw_chars_.count < n_char_ }
    val x_ind =  @index(x_v, j)

    // type error: abstract type(s) escape(s) into the context
    // inferred type: () -> <local<$h>,console/console,div,exn|_e> ()
    // => put function pw_generator() out of main into an extra user defined function to get rid of these type problems!
    //    2025-06-23: so far, this is true for the first programming language


    // println("\nx_ind = " ++ x_ind.show)  // for testing
    val bin0 = integer_to_bin_string(x_ind)
    // println("bin0 = " ++ bin0)  // for testing

    val bin0_0 = first(bin0, 8).string  // first(bin0, 8) is of type sslice --> string needed
    val bin0_1 = last(bin0, 8).string
    // println("bin0_0 = " ++ bin0_0 ++ " -- bin0_1 = " ++ bin0_1)  // for testing

    // "no batteries included": binary string to integer, binary string to unsigned integer (tags)
    val char0_nbr = binary_str_to_unsigned_int(bin0_0)
    val char1_nbr = binary_str_to_unsigned_int(bin0_1)
    // println("char0_nbr = " ++ char0_nbr.show ++ " -- char1_nbr = " ++ char1_nbr.show)  // for testing

    val char0 = char/string(int/char(char0_nbr))
    val char1 = char/string(int/char(char1_nbr))
    // println("char0 = " ++ char0 ++ " -- char1 = " ++ char1)  // for testing

    // is string char0 element of a string? --> contains( s : string, sub : string ) : bool
    // https://koka-lang.github.io/koka/doc/std_core_string.html#contains

    if contains(char_set_, char0) then
      char0_add := char0
    else
      char0_add := ""

    if contains(char_set_, char1) && (pw_chars_.count + 1) < n_char_ then
      char1_add := char1
    else
      char1_add := ""

    pw_chars_ := pw_chars_ ++ char0_add ++ char1_add

    j := j + 1

  return pw_chars_

//
////////////////////  end of user defined functions  ////////////////////////


// end of random_bitstring_and_flexible_password_generator.kk

