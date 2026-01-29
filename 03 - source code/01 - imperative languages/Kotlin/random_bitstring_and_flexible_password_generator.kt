/*
random_bitstring_and_flexible_password_generator.kt

2025-06-03/18

build on Ubuntu 24 LTS for the JVM:
$ kotlinc random_bitstring_and_flexible_password_generator.kt -include-runtime -d random_bitstring_and_flexible_password_generator.jar -opt-in=kotlin.ExperimentalStdlibApi

run on Ubuntu 24 LTS on the JVM:
$ java -jar random_bitstring_and_flexible_password_generator.jar


exe time measurement:
$ ./exe_times_statistics_for_one_test_case_in_cwd2a java -jar random_bitstring_and_flexible_password_generator.jar

=> StringBuilder() needs only 55% of the time of var bits_x = Array<Char>(M1) {'0'}

*/


import java.io.File
import java.io.IOException


fun main() {

  val END: Int = 62501  // 62501 for exactly 1M binary digits; val is immutable
  // val END:  Int = 100  // for testing
  // val M1:   Int = END*16
  // val K250: Int = END*4

  val m:  Int = 65521  // = 2^16 - 15
  val a:  Int = 17364
  val c : Int = 0

  val file_bits_x:   String = "random_bitstring.bin"
  val file_bits_hex: String = "random_bitstring.byte"


  // var x: Array<Int?> = arrayOfNulls(END)  // OK
  // https://www.w3schools.com/kotlin/kotlin_arrays.php
  var x = Array<Int>(END) { 0 }  // ini needed
  // https://kotlinlang.org/docs/arrays.html#create-arrays
  // also needed for the password

  x[0] = kotlin.random.Random.nextInt(0, m)  // https://kotlinlang.org/api/core/kotlin-stdlib/kotlin.random/-random/
  // https://stackoverflow.com/questions/54340057/first-app-random-nextint-unresolved-reference
  // println(x[0])  // for testing

  // var bits_x = Array<Char>(M1) {'0'}   // needed for bit stream
  val bits_x = StringBuilder()
  // https://kotlinlang.org/api/core/kotlin-stdlib/kotlin.text/-string-builder/

  var bits_x_str: String = ""  // needs initialization
  var byte_nbr: Int = 0

  // var bits_hex = Array<Char>(K250) {'0'}  // needed for program ENT - A Pseudorandom Number Sequence Test Program
  val bits_hex = StringBuilder()


  println("\ngenerating a random bit stream...")
  for (i in 1..END-1) {
    x[i] = (a*x[i-1] + c) % m
    // println(x[i])  // for testing

    bits_x_str = Integer.toBinaryString(x[i]).padStart(Int.SIZE_BITS, '0')
    // 7832 --> 00000000000000000001111010011000
    bits_x_str = bits_x_str.substring(16, 32)
    // println(bits_x_str)  // for testing
    bits_x.append(bits_x_str)

    bits_x_str = x[i].toHexString(numberHexFormat)
    // println(bits_x_str)  // for testing
    bits_hex.append(bits_x_str)
  }
  // println(bits_x.toString())  // for testing
  // println(bits_hex.toString())  // for testing

  // write bit stream to disk:
  try {
      val file = File(file_bits_x)
      file.writeText(bits_x.toString())
      println("Bit stream has been written to disk under name:  ${file_bits_x}")
  } catch (e: IOException) {
      println("could not write to file: ${file_bits_x} -- ${e.message}")
  }

  // write byte stream to disk:
  try {
      val file = File(file_bits_hex)
      file.writeText(bits_hex.toString())
      println("Byte stream has been written to disk under name: ${file_bits_hex}")
  } catch (e: IOException) {
      println("could not write to file: ${file_bits_hex} -- ${e.message}")
  }



  // make a password of N_CHAR printable chars: user input requested here
  var N_CHAR: Int = 12
  var answer: Boolean = false
  while (!answer) {
    N_CHAR = 12
    print("\nPassword of $N_CHAR printable chars OK? 'y' or another integer number >= 8: ")

    val answer_str = readLine()
    if (answer_str == "y") {
      answer = true
    } else {
      // print("$answer_str---")  // for testing
      try {
        val N_CHAR_ = answer_str.toString().toInt()
        // https://stackoverflow.com/questions/43876910/how-to-convert-any-to-int-in-kotlin
        if (N_CHAR_ < 8) {
          println("enter an integer number >= 8 or 'y'")
        } else {
          N_CHAR =  N_CHAR_
          answer = true
        }
      } catch (e: NumberFormatException) {
        println("enter an integer number >= 8 or 'y'")
      }
    }
  }
  // println(N_CHAR)  // for testing


  var WITH_SPECIAL_CHARS = true
  answer = false
  while (!answer) {
    print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")

    val answer_str = readLine()
    if (answer_str == "y") {
      answer = true
    } else {
      WITH_SPECIAL_CHARS = false
      answer = true
    }
  }

  var char_set = ('a'..'z').toSet() + ('A'..'Z').toSet() + ('0'..'9').toSet()
  if (WITH_SPECIAL_CHARS) {
    char_set = ('!'..'~').toSet()
  }
  // println(char_set)  // for testing



  var i: Int = 0  // char counter for the password
  var j: Int = 0  // counter for x
  var pw_chars: String = ""
  var bin0: String = ""

  while (i < N_CHAR) {
    bin0 = Integer.toBinaryString(x[j]).padStart(Int.SIZE_BITS, '0')
    bin0 = bin0.substring(16, 32)

    val bin0_0 = bin0.substring(0, 8)
    val bin0_1 = bin0.substring(8, 16)

    val char0 = bin0_0!!.toIntOrNull(2)  // https://kotlinlang.org/docs/null-safety.html#not-null-assertion-operator
    val char1 = bin0_1!!.toIntOrNull(2)
    // println(char0) // for testing
    // println(char1) // for testing

    val char0a = char0!!.toChar()
    val char1a = char1!!.toChar()
    // println(char0a) // for testing
    // println(char1a) // for testing

    if (char0a in char_set) {
      pw_chars = pw_chars + char0a
      i += 1
    }

    if (char1a in char_set && i < N_CHAR) {
      pw_chars = pw_chars + char1a
      i += 1
    }

    j += 1
  }

  println("\nYour password of $N_CHAR characters is: $pw_chars")

}


// user-defined functions:
// https://kotlinlang.org/api/core/kotlin-stdlib/kotlin.text/-hex-format/-number-hex-format/
val numberHexFormat = HexFormat {
    upperCase = false
    number {
        removeLeadingZeros = true
        minLength = 4
        prefix = ""
    }
}

// end of random_bitstring_and_flexible_password_generator.kt

