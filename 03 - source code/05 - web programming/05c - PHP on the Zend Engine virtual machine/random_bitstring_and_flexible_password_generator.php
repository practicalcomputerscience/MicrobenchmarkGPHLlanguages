<?php
/*
random_bitstring_and_flexible_password_generator.php

2026-02-09

run on Ubuntu 24 LTS: $ php ./random_bitstring_and_flexible_password_generator.php


$ php --version
PHP 8.3.6 (cli) (built: Jan  7 2026 08:40:32) (NTS)
Copyright (c) The PHP Group
Zend Engine v4.3.6, Copyright (c) Zend Technologies
    with Zend OPcache v8.3.6, Copyright (c), by Zend Technologies
$


User dialog part partly transpiled from random_bitstring_and_flexible_password_generator.dart with Duck.ai

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


        // Make a password of N_CHAR printable chars: user input requested here
        $N_CHAR = 12;
        $answer = false;
        while (!$answer) {
            echo "\nPassword of $N_CHAR printable chars OK? 'y' or another integer number >= 8: ";
            $answerStr = trim(fgets(STDIN));
            // echo "answerStr = $answerStr"; // for testing

            if ($answerStr === 'y') {
                $answer = true;
            } else {
                if (filter_var($answerStr, FILTER_VALIDATE_INT) !== false) {
                  if ($answerStr >= 8) { 
                      $N_CHAR = intval($answerStr);
                      $answer = true;
                  } else {
                    echo "enter an integer number >= 8 or 'y'\n";
                  }
                } else  {
                    echo "enter an integer number >= 8 or 'y'\n";
                }
            }
        }


        $WITH_SPECIAL_CHARS = true;
        $answer = false;
        while (!$answer) {
            echo "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ";
            $answerStr = trim(fgets(STDIN));

            if ($answerStr === 'y') {
                $answer = true;
            } else {
                $WITH_SPECIAL_CHARS = false;
                $answer = true;
            }
        }

        // Work with a set of chars
        $char_set = [];
        if ($WITH_SPECIAL_CHARS) {
            for ($i = 33; $i <= 126; $i++) { // ASCII printable characters
                $char_set[] = chr($i);
            }
        } else {
            for ($i = 97; $i <= 122; $i++) { // 'a' to 'z'
                $char_set[] = chr($i);
            }
            for ($i = 65; $i <= 90; $i++) {  // 'A' to 'Z'
                $char_set[] = chr($i);
            }
            for ($i = 48; $i <= 57; $i++) {  // '0' to '9'
                $char_set[] = chr($i);
            }
        }


        $i = 0;  // char counter for the password
        $j = 0;  // counter for x
        $pw_chars = "";

        while ($i < $N_CHAR) {
            $bin0 = str_pad(decbin($x[$j]), 16, '0', STR_PAD_LEFT);

            $bin0_0 = substr($bin0, 0, 8);
            $bin0_1 = substr($bin0, 8, 8);

            $char0 = chr(bindec($bin0_0));
            $char1 = chr(bindec($bin0_1));

            if (in_array($char0, $char_set)) {
                $pw_chars .= $char0;
                $i++;
            }

            if (in_array($char1, $char_set) && $i < $N_CHAR) {
                $pw_chars .= $char1;
                $i++;
            }

            $j++;
        }

        echo "\nYour password of $N_CHAR characters is: $pw_chars\n";
    }
}

// Run the main function
RandomStreamsForPerfStats::main();

// end of random_bitstring_and_flexible_password_generator.php
?>
