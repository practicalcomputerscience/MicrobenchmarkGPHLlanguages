<?php
/*
random_streams_for_perf_stats.php

2026-02-09

run on Ubuntu 24 LTS: $ php ./random_streams_for_perf_stats.php
                      $ time php ./random_streams_for_perf_stats.php => real	0m0.015s
                      $ multitime -n 20 php ./random_streams_for_perf_stats.php
                      =>
                                    Mean        Std.Dev.
                        real        0.015       0.001


$ php --version
PHP 8.3.6 (cli) (built: Jan  7 2026 08:40:32) (NTS)
Copyright (c) The PHP Group
Zend Engine v4.3.6, Copyright (c) Zend Technologies
    with Zend OPcache v8.3.6, Copyright (c), by Zend Technologies
$


mostly transpiled from random_streams_for_perf_stats.js with Duck.ai
with a major modification with the introduction of
user defined function writeToFile() mostly from MS Bing AI

*/


// user defined function:
// see also at: https://docs.zendframework.com/zend-expressive/v3/features/error-handling/
function writeToFile(string $filename, string $content, string $type) {
    try {
        // Attempt to open the file for writing
        $fileHandle = @fopen($filename, 'w');
        if ($fileHandle === false) {
            throw new RuntimeException("Unable to open file: {$filename}");
        }

        // Attempt to write to the file
        $bytesWritten = @fwrite($fileHandle, $content);
        if ($bytesWritten === false) {
            throw new RuntimeException("Failed to write to file: {$filename}");
        }

        // Close the file
        if (!fclose($fileHandle)) {
            throw new RuntimeException("Failed to close file: {$filename}");
        }

        if ($type == "bit") {
            echo "Bit stream has been written to disk under name:  {$filename}\n";
        } else {
            echo "Byte stream has been written to disk under name: {$filename}\n";
        }
    } catch (Throwable $e) {
        echo "could not write to file: {$filename} ! -- {$e->getMessage()}\n";
    }
}
// end of user defined function


class RandomStreamsForPerfStats {
    public static function main() {
        $END = 62501;  // 62501 for exactly 1M binary digits

        $m = 65521;  // = 2^16 - 15
        $a = 17364;
        $c = 0;

        $file_bits_x   = "random_bitstring.bin";
        $file_bits_hex = "random_bitstring.byte";

        $x = array_fill(0, $END, 0);
        $rnd = rand(1, $m - 1);  // min and max are both inclusive
        $x[0] = $rnd;

        $bits_x   = "";
        $bits_hex = "";

        echo "\ngenerating a random bit stream...\n";
        for ($i = 1; $i < $END; $i++) {
            $x[$i] = ($a * $x[$i - 1] + $c) % $m;

            $bits_x_str = str_pad(decbin($x[$i]), 16, '0', STR_PAD_LEFT);
            $bits_x .= $bits_x_str;

            $bits_hex_str = str_pad(dechex($x[$i]), 4, '0', STR_PAD_LEFT);
            $bits_hex .= $bits_hex_str;
        }

        writeToFile($file_bits_x, $bits_x, "bit");
        writeToFile($file_bits_hex, $bits_hex, "byte");
    }
}

// Run the main function
RandomStreamsForPerfStats::main();

// end of random_streams_for_perf_stats.php
?>
