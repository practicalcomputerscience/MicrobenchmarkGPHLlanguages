/*
RandomBitstringAndFlexiblePasswordGenerator.java

2026-05-18/19

run on Ubuntu 24 LTS:   $ java RandomBitstringAndFlexiblePasswordGenerator.java

build on Ubuntu 24 LTS: $ javac RandomBitstringAndFlexiblePasswordGenerator.java
                        $ jar cfev RandomBitstringAndFlexiblePasswordGenerator.jar RandomBitstringAndFlexiblePasswordGenerator RandomBitstringAndFlexiblePasswordGenerator.class
                        # switch e is essential, see from here: https://docs.oracle.com/javase/tutorial/deployment/jar/appman.html

                        $ java -jar RandomBitstringAndFlexiblePasswordGenerator.jar


$ java --version
openjdk 25.0.2 2026-01-20
OpenJDK Runtime Environment (build 25.0.2+10-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 25.0.2+10-Ubuntu-124.04, mixed mode, sharing)
$

mostly transpiled from RandomBitstringAndFlexiblePasswordGenerator.hx (Haxe) with Duck.ai (using GPT-5 mini) and Google AI

*/

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.ThreadLocalRandom;
import java.util.Scanner;  // for new Scanner(System.class) for readLine()
import java.util.HashSet;
import java.util.Set;

public class RandomBitstringAndFlexiblePasswordGenerator {
    public static void main() {
        final int END = 62501;  // 62501 for exactly 1M binary digits
        // final int END = 25;  // for testing

        final int m = 65521;  // = 2^16 - 15
        final int a = 17364;
        final int c = 0;

        final String fileBitsX   = "random_bitstring.bin";
        final String fileBitsHex = "random_bitstring.byte";

        int[] x = new int[END];

        // Std.random(m - 1) + 1  -> random in [1, m-1]
        x[0] = ThreadLocalRandom.current().nextInt(1, m);

        StringBuilder bitsX = new StringBuilder();
        StringBuilder bitsHex = new StringBuilder();

        System.out.println("\ngenerating a random bit stream...");
        for (int i = 1; i < END; i++) {
            x[i] = (int)(((long)a * x[i - 1] + c) % m);

            // Convert to binary string and pad with leading zeros up to 16 characters (Google AI)
            String bitsXStr = String.format("%16s", Integer.toBinaryString(x[i])).replace(' ', '0');
            bitsX.append(bitsXStr);

            String bitsHexStr = String.format("%4s", Integer.toHexString(x[i])).replace(' ', '0');
            bitsHex.append(bitsHexStr);
        }

        String bitsXStrTotal = bitsX.toString();
        String bitsHexStrTotal = bitsHex.toString();

        // write bit stream to disk:
        try {
            Files.writeString(Path.of(fileBitsX), bitsXStrTotal, StandardCharsets.UTF_8);
            System.out.println("Bit stream has been written to disk under name:  " + fileBitsX);
        } catch (IOException ex) {
            System.out.println("could not write to file: " + fileBitsX + " ! -- " + ex);
        }

        // write byte stream to disk:
        try {
            Files.writeString(Path.of(fileBitsHex), bitsHexStrTotal, StandardCharsets.UTF_8);
            System.out.println("Byte stream has been written to disk under name: " + fileBitsHex);
        } catch (IOException ex) {
            System.out.println("could not write to file: " + fileBitsHex + " ! -- " + ex);
        }


        // make a password of N_CHAR printable chars: user input requested here
        int N_CHAR = 12;
        boolean answer = false;
        Scanner scanner = new Scanner(System.in);  // Create a Scanner object to read input (Google AI)
        while (!answer) {
            System.out.print("\nPassword of " + N_CHAR + " printable chars OK? 'y' or another integer number >= 8: ");
            String answer_str = scanner.nextLine().trim();  // nextLine() allows the evaluation of the full string

            if (answer_str.equals("y")) {
                answer = true;
            } else {
                if (answer_str.matches("\\d+")) {  // Google AI: checks if the string contains only digits:
                                                   //            this is simple and elegant solution!
                    N_CHAR = Integer.parseInt(answer_str);
                    if (N_CHAR < 8) {
                        System.out.print("enter an integer number >= 8 or 'y'\n");
                    } else {
                        answer = true;
                    }
                } else {
                    System.out.println("enter an integer number >= 8 or 'y'");
                }
            }
        }
        // System.out.println("N_CHAR = " + N_CHAR);  // for testing

        boolean WITH_SPECIAL_CHARS = true;
        answer = false;
        while (!answer) {
            System.out.print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
            String answer_str = scanner.nextLine().trim();

            if (answer_str.equals("y")) {
                answer = true;
            } else {
                WITH_SPECIAL_CHARS = false;
                answer = true;
            }
        }
        // System.out.println("WITH_SPECIAL_CHARS = " + WITH_SPECIAL_CHARS);  // for testing


        // the original implementation, based on the Haxe source code, was not the most efficient.
        // so, a Groovy based one with a HashSet was the design input here with Google AI:
        Set<String> char_set = new HashSet<>();
        if (WITH_SPECIAL_CHARS) {
            // Groovy ('!'..'~') range equivalent in Java
            for (char chr = '!'; chr <= '~'; chr++) {
                char_set.add(String.valueOf(chr));
            }
        } else {
            // Groovy ('a'..'z'), ('A'..'Z'), ('0'..'9') ranges
            for (char chr = 'a'; chr <= 'z'; chr++) char_set.add(String.valueOf(chr));
            for (char chr = 'A'; chr <= 'Z'; chr++) char_set.add(String.valueOf(chr));
            for (char chr = '0'; chr <= '9'; chr++) char_set.add(String.valueOf(chr));
        }
        // System.out.println("char_set = " + char_set);  // for testing


        int i = 0;  // char counter for the password
        int j = 0;  // counter for x
        StringBuilder pw_chars = new StringBuilder();
        // Use StringBuilder for efficient string concatenation inside loops

        while (i < N_CHAR) {
            String bin0 = String.format("%16s", Integer.toBinaryString(x[j])).replace(' ', '0');

            String bin0_0 = bin0.substring(0, 8);
            String bin0_1 = bin0.substring(8, 16);

            String char0 = String.valueOf((char) Integer.parseUnsignedInt(bin0_0, 2));
            String char1 = String.valueOf((char) Integer.parseUnsignedInt(bin0_1, 2));

            if (char_set.contains(char0)) {
                pw_chars.append(char0);
                i++;
            }

            if (char_set.contains(char1) && i < N_CHAR) {
                pw_chars.append(char1);
                i++;
            }

            j++;
        }

        System.out.println("\nYour password of " + N_CHAR + " characters is: " + pw_chars.toString());
    }
}

// end of RandomBitstringAndFlexiblePasswordGenerator.java
