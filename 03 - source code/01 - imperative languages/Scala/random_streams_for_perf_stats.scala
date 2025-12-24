/*
random_streams_for_perf_stats.scala

2025-06-03/08/09/10/18
2025-12-21: see below

build on Ubuntu 24 LTS: use sbt to make one "fat-Jar" (uberjar):
                      $ sbt new
                      take option d to start with a Scala 3 seed template
                      set project name to: random_streams_for_perf_stats
                      $ cd ./random_streams_for_perf_stats  # change to the project root dir
                      fix config files build.sbt in project root dir and plugins.sbt in ./project subdir
                      copy source code file _random_streams_for_perf_stats.scala_ as _Main.scala into subdir ./src/main/scala/Main.scala
                      $ sbt assembly

run on Ubuntu 24 LTS: $ java -jar ./target/scala-3.7.4/random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT.jar

exe time measurement:
$ multitime -n 20 java -jar ./target/scala-3.7.4/random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT.jar

see:
Collections -- Performance Characteristics
https://docs.scala-lang.org/overviews/collections-2.13/performance-characteristics.html


---
building a binary executable with GraalVM:
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar ./target/scala-3.7.4/random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT.jar
$ ./random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT
$ sudo perf stat -r 20 ./random_streams_for_perf_stats-assembly-0.1.0-SNAPSHOT


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


object random_streams_for_perf_stats {
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
}

// end of random_streams_for_perf_stats.scala

