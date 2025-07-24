/*
random_streams_for_perf_stats.chpl

2025-06-05/18

build on Ubuntu 24 LTS: $ chpl random_streams_for_perf_stats.chpl
        for production: $ chpl random_streams_for_perf_stats.chpl --fast

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats


string concatenation (bits_x += bits_x_str;):                    0,243631 +- 0,000799 seconds time elapsed  ( +-  0,33% )
final joining of strings from array (bits_x[i-1] = bits_x_str;): 0,257985 +- 0,000505 seconds time elapsed  ( +-  0,20% )


MS Bing AI says on "String builder string concatenate in Chapel?":
  "Chapel does not have a dedicated StringBuilder class like some other languages (e.g., Java),
   but you can efficiently build strings using loops or arrays. ..."


$ chpl --version
chpl version 2.4.0
  built with LLVM version 18.1.8
...
$

*/


use Random;
use IO;
// use String;


const END: int = 62501;  // 62501 for exactly 1M binary digits
// const END:  int = 15;     // for testing
// const M1:   int = END * 16;
// const K250: int = END * 4;

const M: int = 65521;  // = 2^16 - 15
const A: int = 17364;
const C: int = 0;

const FILE_BITS_X   = "random_bitstring.bin";
const FILE_BITS_HEX = "random_bitstring.byte";


var x: [0..END] int;

var randStream = new randomStream(int);
x[0] = randStream.next(0,M-1);  // https://chapel-lang.org/docs/modules/standard/Random.html#random
// writeln(x[0]);  // for testing

var bits_x:       string = "";
var bits_hex:     string = "";


writeln("\ngenerating a random bit stream...");
var i: int  = 1;
while i < END {
  x[i] = ((A * x[i-1]) + C) % M;
  // writeln(x[i]);  // for testing

  var bits_x_str = "%016bu".format(x[i]);  // 1218 --> 0000010011000010
  // writeln(bits_x_str);  // for testing
  bits_x += bits_x_str;


  var bits_hex_str = "%04xu".format(x[i]);  // 1218 --> 04c2
  // writeln(bits_hex_str);  // for testing
  bits_hex += bits_hex_str;

  i += 1;
}
// writeln(bits_x);  // for testing
// writeln(bits_hex);  // for testing

// write bit stream to disk:
// https://chapel-lang.org/docs/modules/standard/IO.html#module-IO
try {
  var file1 = open(FILE_BITS_X, ioMode.cw);
  var file1writer = file1.writer();
  file1writer.write(bits_x);
  writeln("Bit stream has been written to disk under name:  ", FILE_BITS_X);
} catch e: Error {
  writeln("could not write to file: ", FILE_BITS_X, " ! -- ", e);
}

// write byte stream to disk:
try {
  var file2 = open(FILE_BITS_HEX, ioMode.cw);
  var file2writer = file2.writer();
  file2writer.write(bits_hex);
  writeln("Byte stream has been written to disk under name: ", FILE_BITS_HEX);
} catch e: Error {
  writeln("could not write to file: ", FILE_BITS_HEX, " ! -- ", e);
}


// end of random_streams_for_perf_stats.chpl
