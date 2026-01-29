/*
random_bitstring_and_flexible_password_generator.scala

2025-04-05/06, 2025-05-05/19, 2025-06-01/02/03/18
2025-12-21: see below

build on Ubuntu 24 LTS: use sbt to make one "fat-Jar" (uberjar):
                      $ sbt new
                      take option d to start with a Scala 3 seed template
                      set project name to: random_bitstring_and_flexible_password_generator
                      $ cd ./random_bitstring_and_flexible_password_generator  # change to the project root dir
                      fix config files build.sbt in project root dir and plugins.sbt in ./project subdir
                      copy source code file _random_bitstring_and_flexible_password_generator.scala_ as Main.scala into subdir _./src/main/scala/Main.scala
                      $ sbt assembly

run on Ubuntu 24 LTS: $ java -jar ./target/scala-3.7.4/random_bitstring_and_flexible_password_generator-assembly-0.1.0-SNAPSHOT.jar


---
$ sbt --version
sbt runner version: 1.11.7
...
$ scala --version
Scala code runner version: 1.9.1
Scala version (default): 3.7.4
$ java --version
openjdk 25.0.1 2025-10-21
OpenJDK Runtime Environment (build 25.0.1+8-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 25.0.1+8-Ubuntu-124.04, mixed mode, sharing)
$

*/


import java.io._          // for FileWriter etc.
import scala.util.{Try}


object random_bitstring_and_flexible_password_generator {
  def main(args: Array[String]): Unit = { // keep main() to keep easier track with the Java tool chain

    val END: Int = 62501  // 62501 for exactly 1M binary digits; val is immutable
    // val END: Int = 25  // for testing
    // val M1: Int = END*16
    // val K250: Int = END*4

    val m: Int = 65521  // = 2^16 - 15
    val a: Int = 17364
    val c: Int = 0

    val file_bits_x: String   = "random_bitstring.bin"
    val file_bits_hex: String = "random_bitstring.byte"


    var x = new Array[Int](END)  // allocate memory --> automatic garbage collection with Scala; var is mutable
    // also needed for the password

    val rnd = new scala.util.Random
    x(0) = rnd.nextInt(m - 1) + 1  // end is exclusive; 2025-12-21
    // https://www.scala-lang.org/api/2.12.4/scala/util/Random.html

    var bits_x_str:   String = ""  // needs initialization
    var bits_hex_str: String = ""  // needs initialization

    val bits_x   = new StringBuilder()
    val bits_hex = new StringBuilder()
    // https://www.scala-lang.org/api/current/scala/collection/mutable/StringBuilder.html


    print("\ngenerating a random bit stream...")
    for (i <- 1 to END-1) {
      x(i) = (a*x(i - 1) + c) % m

      bits_x_str = "0000000000000000" + x(i).toBinaryString takeRight 16  // needed for bit stream
      // https://stackoverflow.com/questions/9442381/formatting-binary-values-in-scala/54950845#54950845

      bits_x.append(bits_x_str)
      // https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#stringbuilders

      bits_hex_str = "0000" + x(i).toHexString takeRight 4  // needed for program ENT
      bits_hex.append(bits_hex_str)
    }
    // print(f"\n$bits_x%s\n")  // for testing
    // print(f"$bits_hex%s\n")  // for testing

    // write bit stream to disk using Java
    // val path1: os.Path = os.pwd / file_bits_x  // '/' is part of path!
    // os.Path is a OS-Lib thing which is not part of official Scala!
    // https://www.baeldung.com/scala/file-io
    // https://alvinalexander.com/scala/how-to-write-text-files-in-scala-printwriter-filewriter/
    Try {
      val fileWriter1 = new FileWriter(new File(file_bits_x))
      fileWriter1.write(bits_x.toString)
      fileWriter1.close()
    }.toEither match {
      case Left (ex) =>
        print(f"could not write to file: $file_bits_x%s !")
      case Right(_) =>
        print(f"\nBit stream has been written to disk under name:  $file_bits_x")
    }

    // write byte stream to disk:
    Try {
      val fileWriter2 = new FileWriter(new File(file_bits_hex))
      fileWriter2.write(bits_hex.toString)
      fileWriter2.close()
    }.toEither match {
      case Left (ex) =>
        print(f"could not write to file: $file_bits_hex%s !")
      case Right(_) =>
        print(f"\nByte stream has been written to disk under name: $file_bits_hex\n")
    }



    // make a password of N_CHAR printable chars: user input requested here
    var N_CHAR = 12
    var answer = false
    while !answer do {
      N_CHAR = 12
      print(f"\n\nPassword of $N_CHAR%s printable chars OK? 'y' or another integer number >= 8: ")

      val answer_str = scala.io.StdIn.readLine()
      if answer_str == "y" then
        answer = true
      else
        try
          N_CHAR = answer_str.toInt
          if N_CHAR < 8 then
            print("enter an integer number >= 8 or 'y'")
          else
            answer = true

        catch
          // Exception in thread "main" java.lang.NumberFormatException: For input string: "sdaf"
          case ex: NumberFormatException =>
            print("enter an integer number >= 8 or 'y'")
    }

    var WITH_SPECIAL_CHARS = true
    answer = false
    while !answer do {
      print(f"\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")

      val answer_str = scala.io.StdIn.readLine()
      if answer_str == "y" then
        answer = true
      else
        WITH_SPECIAL_CHARS = false
        answer = true
    }

    var char_set = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
    // https://www.baeldung.com/scala/string-character-check-alphanumeric
    if WITH_SPECIAL_CHARS then
      char_set = (('!' to '~')).toSet


    var i = 0  // char counter for the password
    var j = 0  // counter for x
    var pw_chars = ""

    while i < N_CHAR do {
      // convert an integer number into a string of '0' and '1' characters:
      var bin0 = "0000000000000000" + x(j).toBinaryString takeRight 16
      // bin0 could be for example ' 111001001100101'
      // --> padding needed with leading zeros

      var bin0_0 = bin0.substring(0, 8)   // end index is exclusive
      var bin0_1 = bin0.substring(8, 16)

      // convert a string of '0' and '1' characters into a character:
      var char0 = Integer.parseUnsignedInt(bin0_0, 2).toChar
      var char1 = Integer.parseUnsignedInt(bin0_1, 2).toChar

      if char_set.contains(char0) then
        pw_chars = pw_chars + char0
        i += 1

      if char_set.contains(char1) && i < N_CHAR then
        pw_chars = pw_chars + char1
        i += 1

      j += 1
    }

    print(f"\nYour password of $N_CHAR%d characters is: $pw_chars%s\n")
  }
}

// end of random_bitstring_and_flexible_password_generator.scala

