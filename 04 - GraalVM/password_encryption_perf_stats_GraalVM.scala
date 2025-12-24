/*
password_encryption_perf_stats_GraalVM.scala

2025-06-09/10
2025-12-24: see below

Way #1:
  build on Ubuntu 34 LTS: $ sbt
    - fix build.sbt + plugin.sbt + .bashrc (to activate the Runtime Environment Oracle GraalVM and shadow the OpenJDK)
                          sbt:password_encryption_perf_stats_GraalVM> compile
                          sbt:password_encryption_perf_stats_GraalVM> run
                          sbt:password_encryption_perf_stats_GraalVM> exit

  run on Ubuntu 24 LTS:   $ ./target/scala-3.6.4/password_encryption_perf_stats_graalvm
                          $ sudo perf stat -r 20 ./target/scala-3.6.4/password_encryption_perf_stats_graalvm

Way #2 which makes faster code: 1/ compile to uberjar with OpenJDK
                                2/ compile this uberjar with GraalVM's native-image:
                                $ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar

*/


import java.io._          // for FileWriter etc.
import scala.util.{Try}


@main def random_streams_for_perf_stats(): Unit = {
  val END: Int = 62501  // 62501 for exactly 1M binary digits; val is immutable
  // val END: Int = 25  // for testing
  val M1: Int = END*16
  val K250: Int = END*4

  val m: Int = 65521  // = 2^16 - 15
  val a: Int = 17364
  val c: Int = 0

  val file_bits_x: String   = "random_bitstring.bin"
  val file_bits_hex: String = "random_bitstring.byte"


  var x = new Array[Int](END)  // allocate memory --> automatic garbage collection with Scala; var is mutable
  // also needed for the password

  val rnd = new scala.util.Random
  x(0) = rnd.nextInt(m - 1) + 1  // end is exclusive; 2025-12-24
  

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
    // no speed up with: BufferedWriter()
    // https://stackoverflow.com/questions/31751348/which-solution-has-better-performance-stringbuilder-or-string-interpolation-con
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
}

// end of password_encryption_perf_stats_GraalVM.scala
