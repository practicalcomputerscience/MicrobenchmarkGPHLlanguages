/* random_streams_for_perf_stats.kk

2025-06-22/23

test on Ubuntu 24 LTS: OK, but slow!

build on Ubuntu 24 LTS: $ koka -o random_streams_for_perf_stats random_streams_for_perf_stats.kk
                        $ chmod 755 random_streams_for_perf_stats
                        $
                        $ koka -O2 -o random_streams_for_perf_stats random_streams_for_perf_stats.kk

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats with -O2: real	0m3,221s <<<<<<<<<<<<<<<<<<<<<<<<<


Koka is an open source programming language created in 2012 by Daan Leijen
https://pldb.io/concepts/koka.html


$ koka --version
Koka 3.1.2, 17:56:43 May 30 2024 (ghc release version)
$

*/


module random_streams_for_perf_stats


import std/num/random
import std/num/int32
import std/os/path
import std/os/file


fun main()  // fun: named function
  val upper_limit = 62501  // 62501 for exactly 1M binary digits; val is immutable
  // val upper_limit = 10  // for testing

  val m = 65521  // = 2^16 - 15
  val a = 17364
  val c = 0

  val file_bits_x   = "random_bitstring.bin"
  val file_bits_hex = "random_bitstring.byte"

  val x0 = srandom-int32-range(1.int32, m.int32).int
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

//
////////////////////  end of user defined functions  ////////////////////////


// end of random_streams_for_perf_stats.kk
