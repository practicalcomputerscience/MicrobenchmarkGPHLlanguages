/**
RandomStreamsForPerfStats.hx  --  no snake case allowed here according to my tests of: Random_streams_for_perf_stats.hx !!

Also see: "The name of the file containing a Haxe class is the same as the name of the class itself..":
          https://haxe.org/manual/introduction-hello-world.html


2026-05-12/13

On Ubuntu 24 LTS:
  a/ run in Haxe interpreter:              $ haxe --main RandomStreamsForPerfStats --interp  # Class name only with initial uppercase letter
                                           $ haxe --run RandomStreamsForPerfStats

  b/ build for and run on the Neko VM:     $ haxe --neko RandomStreamsForPerfStats.n --main RandomStreamsForPerfStats
                                           $ neko RandomStreamsForPerfStats.n

  c/ build for and run on the HashLink VM: $ haxe --hl RandomStreamsForPerfStats.hl -main RandomStreamsForPerfStats
                                           $ hl RandomStreamsForPerfStats.hl


$ haxe --version
4.3.3
$ neko
NekoVM 2.3.0 (c)2005-2017 Haxe Foundation
  Usage : neko <file>
$ hl
HL/JIT 1.15.0 (c)2015-2025 Haxe Foundation
  Usage : hl [--debug <port>] [--debug-wait] <file>
$

mainly transpiled from random_streams_for_perf_stats.groovy with Google Ai

**/

import sys.io.File;

class RandomStreamsForPerfStats {  // Class name must start with an uppercase letter and also no snake case allowed here according to my tests!
    static public function main():Void {
        final END:Int = 62501;  // 62501 for exactly 1M binary digits
        // final END:Int = 10;  // for testing

        final m:Int = 65521; // = 2^16 - 15
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
    }


    /**
     Helper to format integer values to 16-bit binary strings
    **/
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
}

// end of RandomStreamsForPerfStats.hx
