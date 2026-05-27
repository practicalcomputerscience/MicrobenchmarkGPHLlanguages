/**
RandomBitstringAndFlexiblePasswordGenerator.hx  --  no snake case allowed here according to my tests

Also see: "The name of the file containing a Haxe class is the same as the name of the class itself..":
          https://haxe.org/manual/introduction-hello-world.html


2026-05-17/18
2026-05-27: refactored from char_set to pattern (for regular expressions)

On Ubuntu 24 LTS:
  a/ run in Haxe interpreter:              $ haxe --main RandomBitstringAndFlexiblePasswordGenerator --interp  # Class name only with initial uppercase letter
                                           $ haxe --run RandomBitstringAndFlexiblePasswordGenerator

  c/ build for and run on the HashLink VM: $ haxe --hl RandomBitstringAndFlexiblePasswordGenerator.hl -main RandomBitstringAndFlexiblePasswordGenerator
                                           $ hl RandomBitstringAndFlexiblePasswordGenerator.hl


$ haxe --version
4.3.3
$ hl
HL/JIT 1.15.0 (c)2015-2025 Haxe Foundation
  Usage : hl [--debug <port>] [--debug-wait] <file>
$

mainly transpiled from random_streams_for_perf_stats.groovy with Google AI and MS Copilot

**/

import sys.io.File;
import haxe.ds.StringMap;
import EReg;

class RandomBitstringAndFlexiblePasswordGenerator {  // Class name must start with an uppercase letter and also no snake case allowed here according to my tests!
    static public function main():Void {
        final END:Int = 62501;  // 62501 for exactly 1M binary digits
        // final END:Int = 100;  // for testing

        final m:Int = 65521;  // = 2^16 - 15
        final a:Int = 17364;
        final c:Int = 0;

        var file_bits_x:String   = "random_bitstring.bin";
        var file_bits_hex:String = "random_bitstring.byte";

        // Initialize array with specific size
        var x = new haxe.ds.Vector<Int>(END);

        // Seed initial value
        x[0] = Std.random(m - 1) + 1;

        var bits_x   = new StringBuf();
        var bits_hex = new StringBuf();

        Sys.println("\ngenerating a random bit stream...");
        for (i in 1...END) {
            x[i] = (a * x[i - 1] + c) % m;
            // Sys.println('\n${x[i]}');  // for testing

            // Convert to binary and pad
            var bits_x_str = integer_to_bin_string(x[i]);
            // Sys.println(bits_x_str);  // for testing
            bits_x.add(bits_x_str);

            // Convert to hex and pad
            var bits_hex_str = StringTools.lpad(StringTools.hex(x[i]).toLowerCase(), "0", 4);  // have lower case letters
            // Sys.println(bits_hex_str);  // for testing
            bits_hex.add(bits_hex_str);
        }

        var bits_x_str_total   = bits_x.toString();
        var bits_hex_str_total = bits_hex.toString();
        // Sys.println('\nbits_x_str_total = $bits_x_str_total');  // for testing
        // Sys.println('bits_hex_str_total = $bits_hex_str_total');  // for testing

        // write bit stream to disk:
        try {
            File.saveContent(file_bits_x, bits_x_str_total);
            Sys.println('Bit stream has been written to disk under name:  $file_bits_x');
        } catch (ex:Dynamic) {
            Sys.println('could not write to file: $file_bits_x ! -- $ex');
        }

        // write byte stream to disk:
        try {
            File.saveContent(file_bits_hex, bits_hex_str_total);
            Sys.println('Byte stream has been written to disk under name: $file_bits_hex');
        } catch (ex:Dynamic) {
            Sys.println('could not write to file: $file_bits_hex ! -- $ex');
        }


        // make a password of N_CHAR printable chars: user input requested here
        var N_CHAR:Int = 12;
        var answer:Bool = false;

        var digitRegex = ~/^\d+$/;
        // using Haxe's native regular expression class, EReg (Google AI); 2026-05-19
        // 2026-05-27: replace this to here

        while (!answer) {
            Sys.print("\nPassword of " + N_CHAR + " printable chars OK? 'y' or another integer number >= 8: ");
            var answer_str = Sys.stdin().readLine();  // answer_str is just the text, no newline (2026-05-18)

            if (answer_str == "y") {
                answer = true;
            } else {
                if (digitRegex.match(answer_str)) {
                    var parsed = Std.parseInt(answer_str);
                    if (parsed != null && parsed >= 8) {  // 2026-05-27: refactored to more concise logic
                        N_CHAR = parsed;
                        answer = true;
                    } else {
                        Sys.print("enter an integer number >= 8 or 'y'\n");
                    }
                } else {
                    Sys.print("enter an integer number >= 8 or 'y'\n");
                }
            }
        }
        // Sys.println('N_CHAR = $N_CHAR');  // for testing

        var WITH_SPECIAL_CHARS:Bool = true;
        answer = false;
        while (!answer) {
            Sys.print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
            var answer_str = Sys.stdin().readLine();

            if (answer_str == "y") {
                answer = true;
            } else {
                WITH_SPECIAL_CHARS = false;
                answer = true;
            }
        }
        // Sys.println('WITH_SPECIAL_CHARS = $WITH_SPECIAL_CHARS');  // for testing


        // 2026-05-27: old solution:
        /**
        Using a Map for explicit Set behavior across cross-platform targets
        var char_set = new StringMap<Bool>();
        if (WITH_SPECIAL_CHARS) {
            // Range from '!' (33) to '~' (126)
            for (code in 33...127) {
                char_set.set(String.fromCharCode(code), true);  // all values are true
            }
        } else {
            for (code in 97...123) char_set.set(String.fromCharCode(code), true);  // a-z
            for (code in 65...91)  char_set.set(String.fromCharCode(code), true);  // A-Z
            for (code in 48...58)  char_set.set(String.fromCharCode(code), true);  // 0-9
        }
        Sys.println('char_set = $char_set');  // for testing
        WITH_SPECIAL_CHARS = false
        char_set = [z => true,y => true,x => true,w => true,v => true, ...]  // reverse order!
        **/

        // 2026-05-27: new solution with regular expressions ("Big AI"):
        var alnum_re = new EReg("[A-Za-z0-9]", "");  // this is the safe solution
        // The EReg class uses the underlying regular expression engine of the target platform.
        // EReg: https://api.haxe.org/EReg.html, https://haxe.org/manual/std-regex.html
        // POSIX character classes may not work everywhere, like some JavaScript engines
        //
        // var alnum_re = new EReg("[[:alnum:]]", "");  // POSIX character class works too with target HashLink VM
        var print_re = new EReg("[!-~]", "");
        var pattern = WITH_SPECIAL_CHARS ? print_re : alnum_re;


        var i:Int = 0; // char counter for the password
        var j:Int = 0; // counter for x
        var pw_chars:String = "";

        while (i < N_CHAR) {
            var bin0 = integer_to_bin_string(x[j]);

            var bin0_0 = bin0.substring(0, 8);
            var bin0_1 = bin0.substring(8, 16);

            var char0 = String.fromCharCode(parseUnsignedInt(bin0_0));
            var char1 = String.fromCharCode(parseUnsignedInt(bin0_1));

            // 2026-05-27: new solution with regular expressions:
            if (pattern.match(char0)) {
                pw_chars += char0;
                i++;
            }

            if (pattern.match(char1) && i < N_CHAR) {
                pw_chars += char1;
                i++;
            }

            j++;
        }

        Sys.println("\nYour password of " + N_CHAR + " characters is: " + pw_chars);
    }


    ////////////////////////////////////////////////////////////////////////
    //
    // user defined functions
    //
    // Helper to format integer values to 16-bit binary strings
    static function integer_to_bin_string(v:Int):String {
        // Manual binary conversion for cross-platform support
        var s = "";
        var n = v;
        if (n == 0) return "0";
        while (n > 0) {
            s = ((n % 2) == 0 ? "0" : "1") + s;
            n = Std.int(n / 2);
        }
        return StringTools.lpad(s, "0", 16);
    }

    // Helper to parse unsigned binary string back to an integer
    static function parseUnsignedInt(bin:String):Int {
        var value = 0;
        for (i in 0...bin.length) {
            value = (value << 1) | (bin.charAt(i) == "1" ? 1 : 0);
        }
        return value;
    }
    //
    // end of user defined functions
    //
    ////////////////////////////////////////////////////////////////////////
}

// end of RandomBitstringAndFlexiblePasswordGenerator.hx
