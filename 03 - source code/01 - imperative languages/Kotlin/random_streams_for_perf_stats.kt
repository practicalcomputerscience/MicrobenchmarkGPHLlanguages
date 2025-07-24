/*
random_streams_for_perf_stats.kt

2025-06-03/10/18

build on Ubuntu 24 LTS for the JVM:
$ kotlinc random_streams_for_perf_stats.kt -include-runtime -d random_streams_for_perf_stats.jar -opt-in=kotlin.ExperimentalStdlibApi

run on Ubuntu 24 LTS:
$ java -jar random_streams_for_perf_stats.jar

exe time measurement:
$ ./exe_times_statistics_for_one_test_case_in_cwd2a java -jar random_streams_for_perf_stats.jar

---
2025-06-10
Build standalone binary executable with the GraalVM:
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar random_streams_for_perf_stats.jar
...
$ ./random_streams_for_perf_stats
$ sudo perf stat -r 20 ./random_streams_for_perf_stats

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

// end of random_streams_for_perf_stats.kt
