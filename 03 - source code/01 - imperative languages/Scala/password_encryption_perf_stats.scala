/*
password_encryption_perf_stats.scala

2025-06-03/08/09/10/18

build on Ubuntu 24 LTS: use sbt in project dir to make one "fat-Jar" (uberjar):
                      $ sbt assembly

run on Ubuntu 24 LTS: $ java -jar ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar

exe time measurement:
$ ./exe_times_statistics_for_one_test_case_in_cwd2a java -jar ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar
=> 2025-06-09: mean = 143 [milliseconds] OpenJDK <=> GraalVM: mean = 168 [milliseconds]

building a binary executable with GraalVM
$ $HOME/.sdkman/candidates/java/24-graal/lib/svm/bin/native-image -jar ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar
$ ./password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT
$ sudo perf stat -r 20 ./password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT

----
2025-06-09
  with GraalVM installed: $ java -- version
  java 24 2025-03-18
  Java(TM) SE Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01)
  Java HotSpot(TM) 64-Bit Server VM Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01, mixed mode, sharing)
  at $HOME/.sdkman/candidates/java/current/bin
  
  => mean = 168 [milliseconds] => go back to OpenJDK => now works faster again
----


----
mean = 216 [milliseconds] with var bits_x = new Array[Char](M1) + var bits_hex = new Array[Char](K250)

=> refactor with StringBuilder:
  => mean = 189 [milliseconds] with val bits_x = new StringBuilder() + val bits_hex = new StringBuilder()
     => -12% less exe speed => this is not impressive!
  bits_x += bits_x_str doesn't change a thing versus append()
  
=> refactor with array buffer for x => mean = 193 [milliseconds] also doesn't change a thing versus var x = new Array[Int](END)

see:
Collections -- Performance Characteristics
https://docs.scala-lang.org/overviews/collections-2.13/performance-characteristics.html


---
Scala code runner: $ ./exe_times_statistics_for_one_test_case_in_cwd2a scala ./src/main/scala/Main.scala
  => mean = 519 [milliseconds] <-- re-affirmed on 2025-06-09
  => this includes compilation time apparently for: ./target/scala-3.6.4/password_encryption_perf_stats-assembly-0.1.0-SNAPSHOT.jar

---
$ sbt
welcome to sbt 1.11.0 (Ubuntu Java 21.0.7)
...
> exit
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
    x(0) = rnd.nextInt(m)

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

// end of password_encryption_perf_stats.scala
