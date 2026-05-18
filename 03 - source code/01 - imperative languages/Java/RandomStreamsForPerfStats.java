/*
RandomStreamsForPerfStats.java

2026-05-14/18

run on Ubuntu 24 LTS:   $ time java RandomStreamsForPerfStats.java

                        generating a random bit stream...
                        Bit stream has been written to disk under name:  random_bitstring.bin
                        Byte stream has been written to disk under name: random_bitstring.byte

                        real	0m0.357s <<<<<<<<<<<<<<<
                        user	0m1.041s
                        sys	0m0.060s
                        $

build on Ubuntu 24 LTS: $ javac RandomStreamsForPerfStats.java
                        $ jar cfev RandomStreamsForPerfStats.jar RandomStreamsForPerfStats RandomStreamsForPerfStats.class
                        # switch e is essential, see from here: https://docs.oracle.com/javase/tutorial/deployment/jar/appman.html

                        $ time java -jar RandomStreamsForPerfStats.jar

                        generating a random bit stream...
                        Bit stream has been written to disk under name:  random_bitstring.bin
                        Byte stream has been written to disk under name: random_bitstring.byte

                        real	0m0.052s <<<<<<<<<<<<<<<
                        user	0m0.103s
                        sys	0m0.023s
                        $


$ java --version
openjdk 25.0.2 2026-01-20
OpenJDK Runtime Environment (build 25.0.2+10-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 25.0.2+10-Ubuntu-124.04, mixed mode, sharing)
$

mostly  transpiled from RandomStreamsForPerfStats.hx (Haxe) with Duck.ai (using GPT-5 mini) and Google AI

*/

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.ThreadLocalRandom;

public class RandomStreamsForPerfStats {
    public static void main() {
        final int END = 62501;

        final int m = 65521;
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
    }
}

// end of RandomStreamsForPerfStats.java
