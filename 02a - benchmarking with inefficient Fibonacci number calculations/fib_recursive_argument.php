<?php
/* fib_recursive_argument.php

2026-02-10

test on Ubuntu 24 LTS with PHP 8.3.6 (cli): OK


output:
  1/ executed Zend bytecode:
     $ php ./fib_recursive_argument.php <n>

     n = 44 => fib = 701408733           => Time: 23.659385919571s
     n = 45 => fib = 1134903170          => Time: 38.111607074738s
     n = 46 => fib = 1836311903          => Time: 61.748165845871s
     n = 47 => fib = 2971215073          => Time: 99.819558858871s


  2/ JIT compiled Zend bytecode:
     $ php -d opcache.enable_cli=1 -d opcache.jit_buffer_size=100M -d opcache.jit=1255 ./fib_recursive_argument.php  <n>

     n = 44 => fib = 701408733           => Time:  7.222774028778s
     ...
     n = 47 => fib = 2971215073          => Time: 27.738076210022s

*/


function fib($n) {
    return $n <= 1 ? $n : fib($n - 1) + fib($n - 2);
}

class FibArgument {
  public static function main($args) {
    array_shift($args); // Remove the script name from the arguments

    $n = $args[0];
    echo "argument n = $n\n";

    $start_time = microtime(true);
    echo fib($n) . "\n";
    echo "Time: " . (microtime(true) - $start_time) . "s\n";
  }
}

FibArgument::main($argv);  // $argv is an array of command-line arguments

// end of fib_recursive_argument.php
?>
