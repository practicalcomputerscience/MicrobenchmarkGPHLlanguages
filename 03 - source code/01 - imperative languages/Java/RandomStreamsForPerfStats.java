/*
RandomStreamsForPerfStats.java

2026-05-14

run on Ubuntu 24 LTS:   $ time java RandomStreamsForPerfStats.java

                        generating a random bit stream...
                        Bit stream has been written to disk under name:  random_bitstring.bin
                        Byte stream has been written to disk under name: random_bitstring.byte

                        real	0m0.347s <<<<<<<<<<<<<<<
                        user	0m0.988s
                        sys	0m0.064s
                        $

build on Ubuntu 24 LTS: $ javac RandomStreamsForPerfStats.java
                        $ jar cfev RandomStreamsForPerfStats.jar RandomStreamsForPerfStats RandomStreamsForPerfStats.class
                        # switch e is essential, see from here: https://docs.oracle.com/javase/tutorial/deployment/jar/appman.html
                        
                        $ time java -jar RandomStreamsForPerfStats.jar

                        generating a random bit stream...
                        Bit stream has been written to disk under name:  random_bitstring.bin
                        Byte stream has been written to disk under name: random_bitstring.byte

                        real	0m0.052s <<<<<<<<<<<<<<<
                        user	0m0.092s
                        sys	0m0.022s
                        $


$ java --version
openjdk 25.0.2 2026-01-20
OpenJDK Runtime Environment (build 25.0.2+10-Ubuntu-124.04)
OpenJDK 64-Bit Server VM (build 25.0.2+10-Ubuntu-124.04, mixed mode, sharing)
$

(almost) completely transpiled from RandomStreamsForPerfStats.hx (Haxe) with Duck.ai (using GPT-5 mini)

*/

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.ThreadLocalRandom;

public class RandomStreamsForPerfStats {
    public static void main(String[] args) {
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

            String bitsXStr = integerToBinString(x[i]);
            bitsX.append(bitsXStr);

            String bitsHexStr = leftPad(Integer.toHexString(x[i]).toLowerCase(), '0', 4);
            bitsHex.append(bitsHexStr);
        }

        String bitsXStrTotal = bitsX.toString();
        String bitsHexStrTotal = bitsHex.toString();

        try {
            Files.writeString(Path.of(fileBitsX), bitsXStrTotal, StandardCharsets.UTF_8);
            System.out.println("Bit stream has been written to disk under name:  " + fileBitsX);
        } catch (IOException ex) {
            System.out.println("could not write to file: " + fileBitsX + " ! -- " + ex);
        }

        try {
            Files.writeString(Path.of(fileBitsHex), bitsHexStrTotal, StandardCharsets.UTF_8);
            System.out.println("Byte stream has been written to disk under name: " + fileBitsHex);
        } catch (IOException ex) {
            System.out.println("could not write to file: " + fileBitsHex + " ! -- " + ex);
        }
    }

    private static String integerToBinString(int v) {
        if (v == 0) return "0".repeat(16); // match Haxe lpad behavior to 16 chars
        StringBuilder sb = new StringBuilder();
        int n = v;
        while (n > 0) {
            sb.append((n % 2) == 0 ? '0' : '1');
            n = n / 2;
        }
        sb.reverse();
        return leftPad(sb.toString(), '0', 16);
    }

    private static String leftPad(String s, char padChar, int length) {
        if (s.length() >= length) return s;
        StringBuilder sb = new StringBuilder(length);
        for (int i = s.length(); i < length; i++) sb.append(padChar);
        sb.append(s);
        return sb.toString();
    }
}

// end of RandomStreamsForPerfStats.java
