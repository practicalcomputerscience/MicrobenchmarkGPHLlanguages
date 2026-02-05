/*
random_bitstring_and_flexible_password_generator.groovy

2026-01-29, 2026-02-05

build on Ubuntu 24 LTS: <have a build.gradle file for Groovy version 5.0.4 and OpenJDK 25 ready in the project's root dir>
                        there make an uberjar file:
                        $ gradle shadowJar

run on Ubuntu 24 LTS:   $ java -jar ./build/libs/random_bitstring_and_flexible_password_generator.jar  # run in project root dir


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
class random_bitstring_and_flexible_password_generator {
    static void main(String[] args) {
        final int END = 62501  // 62501 for exactly 1M binary digits
        // final int END = 25  // for testing

        final int m = 65521  // = 2^16 - 15
        final int a = 17364
        final int c = 0

        String file_bits_x   = "random_bitstring.bin"
        String file_bits_hex = "random_bitstring.byte"

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


        // make a password of N_CHAR printable chars: user input requested here
        int N_CHAR = 12
        boolean answer = false
        while (!answer) {
            print "\nPassword of $N_CHAR printable chars OK? 'y' or another integer number >= 8: "
            String answer_str = System.console().readLine()

            if (answer_str.equals("y")) {
                answer = true
            } else {
                try {
                    N_CHAR = Integer.parseInt(answer_str)
                    if (N_CHAR < 8) {
                        println "enter an integer number >= 8 or 'y'"
                    } else {
                        answer = true
                    }
                } catch (NumberFormatException ex) {
                    println "enter an integer number >= 8 or 'y'"
                }
            }
        }
        // println "N_CHAR = $N_CHAR"  // for testing

        boolean WITH_SPECIAL_CHARS = true
        answer = false
        while (!answer) {
            print "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "
            String answer_str = System.console().readLine()

            if (answer_str.equals("y")) {
                answer = true
            } else {
                WITH_SPECIAL_CHARS = false
                answer = true
            }
        }


        // work with a set of chars here like in Scala:
        Set<String> char_set = new HashSet<>()
        if (WITH_SPECIAL_CHARS) {
            ('!'..'~').each { char_set.add(it) }
        } else {
            ('a'..'z').each { char_set.add(it) }
            ('A'..'Z').each { char_set.add(it) }
            ('0'..'9').each { char_set.add(it) }
        }
        // println "char_set = $char_set"  // for testing


        int i = 0  // char counter for the password
        int j = 0  // counter for x
        String pw_chars = ""

        while (i < N_CHAR) {
            String bin0 = Integer.toBinaryString(x[j]).padLeft(16, '0')
            // println "\n$bin0"  // for testing

            String bin0_0 = bin0.substring(0, 8)
            String bin0_1 = bin0.substring(8, 16)
            // println "$bin0_0 + $bin0_1"  // for testing

            String char0 = String.valueOf((char) Integer.parseUnsignedInt(bin0_0, 2))
            String char1 = String.valueOf((char) Integer.parseUnsignedInt(bin0_1, 2))
            // println "$char0 + $char1"  // for testing

            if (char_set.contains(char0)) {
                pw_chars += char0
                i++
            }

            if (char_set.contains(char1) && i < N_CHAR) {
                pw_chars += char1
                i++
            }

            j++
        }

        println("\nYour password of $N_CHAR characters is: $pw_chars")
    }
}

// end of random_bitstring_and_flexible_password_generator.groovy
