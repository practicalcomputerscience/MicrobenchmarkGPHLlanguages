/*
random_streams_for_perf_stats.groovy

2026-01-28/29/31

build on Ubuntu 24 LTS: <have a build.gradle file for Groovy version 5.0.4 and OpenJDK 25 ready in the project's root dir>
                        there make an uberjar file:
                        $ gradle shadowJar

run on Ubuntu 24 LTS:   $ java -jar ./build/libs/random_streams_for_perf_stats.jar  # run in project root dir
                        $ time java -jar ./build/libs/random_streams_for_perf_stats.jar => 0m0.587s with OpenJDK 25 + Groovy 5
                        $ time java -jar ./build/libs/random_streams_for_perf_stats.jar => 0m0.340s with OpenJDK 25 + Groovy 5 + CompileStatic
                        $ multitime -n 20 java -jar ./build/libs/random_streams_for_perf_stats.jar  # with OpenJDK 25 + Groovy 5 + CompileStatic
                        =>             Mean        Std.Dev.
                           real        0.341       0.004

                        direct execution with groovy with Groovy 5.0.4 and OpenJDK 25 and CompileStatic:
                        $ groovy ./src/main/groovy/random_streams_for_perf_stats.groovy
                        $ time groovy ./src/main/groovy/random_streams_for_perf_stats.groovy => real	0m0.925s


building a binary executable with GraalVM and Java(TM) SE Runtime Environment Oracle GraalVM 24+36.1 (build 24+36-jvmci-b01)
and Groovy version 4.0.30 for better interaction with the GraalVM:
  I've given up after numerous experiments (to prevent java.lang.NullPointerException's),
  and experiments with in this source code.
  Groovy's dynamic capabilities nature apparently clashes with GraalVM's static nature.


$ groovy --version
Groovy Version: 5.0.4 JVM: 25.0.1 Vendor: Ubuntu OS: Linux
$

$ java --version
openjdk 25.0.1 2025-10-21
OpenJDK Runtime Environment (build 25.0.1+8-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 25.0.1+8-Ubuntu-124.04, mixed mode, sharing)
$

$ gradle -v
...
Gradle 9.3.0
...
$

partly transpiled from Scala's Main.scala with Duck.ai

*/


// these packages and classes are imported by default:
// import java.io.File
// import java.io.FileWriter
// import java.util.Random

import groovy.transform.CompileStatic  // this is a real exe time booster!
                                       // It bypasses Groovy’s dynamic Meta-Object Protocol,
                                       // allowing the JVM to perform standard optimizations.

@CompileStatic
class random_streams_for_perf_stats {
    static void main(String[] args) {
        final int END = 62501  // 62501 for exactly 1M binary digits
        // final int END = 10  // for testing

        final int m = 65521  // = 2^16 - 15
        final int a = 17364
        final int c = 0

        String file_bits_x   = "random_bitstring.bin"
        String file_bits_hex = "random_bitstring.byte"  // 2026-01-31

        int[] x = new int[END]

        Random rnd = new Random()
        x[0] = rnd.nextInt(m - 1) + 1

        StringBuilder bits_x   = new StringBuilder()
        StringBuilder bits_hex = new StringBuilder()

        println "\ngenerating a random bit stream..."
        for (int i = 1; i < END; i++) {
            x[i] = (a * x[i - 1] + c) % m

            String bits_x_str = Integer.toBinaryString(x[i]).padLeft(16, '0')
            bits_x.append(bits_x_str)

            String bits_hex_str = Integer.toHexString(x[i]).padLeft(4, '0')
            bits_hex.append(bits_hex_str)
        }


        // write bit stream to disk:
        try {
            File file = new File(file_bits_x)
            file.text = bits_x.toString()
            println "Bit stream has been written to disk under name:  $file_bits_x"
        } catch (IOException ex) {
            println "could not write to file: $file_bits_x! -- ${ex.message}"
        }

        // write byte stream to disk:
        try {
            File file = new File(file_bits_hex)
            file.text = bits_hex.toString()
            println "Byte stream has been written to disk under name: $file_bits_hex"
        } catch (IOException ex) {
            println "could not write to file: $file_bits_hex! -- ${ex.message}"
        }
    }
}

// end of random_streams_for_perf_stats.groovy

