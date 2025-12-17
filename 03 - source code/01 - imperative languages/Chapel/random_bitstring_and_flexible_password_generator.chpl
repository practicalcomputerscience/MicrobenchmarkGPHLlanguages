/*
random_bitstring_and_flexible_password_generator.chpl

2025-06-05/06/18; 2025-12-17: see below

build on Ubuntu 24 LTS: $ chpl random_bitstring_and_flexible_password_generator.chpl
        for production: $ chpl random_bitstring_and_flexible_password_generator.chpl --fast

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ chpl --version
chpl version 2.6.0
  built with LLVM version 20.1.8
  available LLVM targets: xcore, x86-64, x86, wasm64, wasm32, ve, systemz, spirv, spirv64, spirv32, sparcel, sparcv9, sparc, riscv64, riscv32, ppc64le, ppc64, ppc32le, ppc32, nvptx64, nvptx, msp430, mips64el, mips64, mipsel, mips, loongarch64, loongarch32, lanai, hexagon, bpfeb, bpfel, bpf, avr, thumbeb, thumb, armeb, arm, amdgcn, r600, aarch64_32, aarch64_be, aarch64, arm64_32, arm64
Copyright 2020-2025 Hewlett Packard Enterprise Development LP
Copyright 2004-2019 Cray Inc.
(See LICENSE file for more details)
$ 

*/


use Random;
use IO;    // stdin, stdout


const END: int = 62501;  // 62501 for exactly 1M binary digits
// const END:  int = 25;     // for testing
// const M1:   int = END * 16;
// const K250: int = END * 4;

const M: int = 65521;  // = 2^16 - 15
const A: int = 17364;
const C: int = 0;

const FILE_BITS_X   = "random_bitstring.bin";
const FILE_BITS_HEX = "random_bitstring.byte";


var x: [0..END] int;

var randStream = new randomStream(int);
x[0] = randStream.next(1,M-1);  // https://chapel-lang.org/docs/modules/standard/Random.html#random; 2025-12-17
// writeln(x[0]);  // for testing

var bits_x:       string = "";
var bits_hex:     string = "";


writeln("\ngenerating a random bit stream...");
var i: int  = 1;
while i < END {
  x[i] = ((A * x[i-1]) + C) % M;
  // writeln(x[i]);  // for testing

  const bits_x_str = "%016bu".format(x[i]);  // 1218 --> 0000010011000010
  // writeln(bits_x_str);  // for testing
  bits_x += bits_x_str;


  const bits_hex_str = "%04xu".format(x[i]);  // 1218 --> 04c2
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



var n_char: int = 12;
var answer: bool = false;
var answer_str: string;

while answer != true {
  n_char = 12;
  write("\nPassword of ", n_char, " printable chars OK? 'y' or another integer number >= 8: ");
  stdout.flush();  // flushing is needed here

  answer_str = readLine(string, stripNewline = true);  // definitely better than read(string) => works great now!
  // https://chapel-lang.org/docs/modules/standard/IO.html#IO.fileReader.readLine

  if answer_str == "y" {
    answer = true;
  } else {
    try {
      n_char = answer_str: int;
      // https://chapel-lang.org/docs/users-guide/base/casts.html#casts-explicit-type-conversions
      if n_char < 8 {
        writeln("enter an integer number >= 8 or 'y'");
      } else {
        answer = true;
      }
    } catch {
      writeln("enter an integer number >= 8 or 'y'");
    }
  }
}
// writeln("n_char = ", n_char);  // for testing



var with_special_chars: bool = true;
answer = false;
while answer != true {
  write("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
  stdout.flush();  // flushing is needed here

  answer_str = readLine(string, stripNewline = true);

  if answer_str == "y" {
    answer = true;
  } else {
    with_special_chars = false;
    answer = true;
  }
}

var char_set: string = "";
if with_special_chars {
  i = 33;
  while i < 127 {
    char_set += codepointToString(i:int(32));  // codepointToString(i: int(32)) : string
    // https://chapel-lang.org/docs/language/spec/strings.html#String.codepointToString
    i += 1;
  }
} else {
  char_set += "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
}
// writeln("char_set = ", char_set);  // for testing


i = 0;           // char counter for the password
var j: int = 0;  // char counter for x
var pw_chars: string = "";

while i < n_char {
  const bin0 = "%016bu".format(x[j]);
  // writeln("\n", bin0);  // for testing

  const bin0_0 = bin0[0..7];   // 7 is inclusive
  const bin0_1 = bin0[8..15];
  // writeln("bin0_0 = ", bin0_0, " -- ", "bin0_1 = ", bin0_1);  // for testing

  const char0a = bin_string_to_int(bin0_0);
  const char1a = bin_string_to_int(bin0_1);
  // writeln("char0a = ", char0a, " -- ", "char1a = ", char1a);  // for testing

  const char0 = codepointToString(char0a);
  const char1 = codepointToString(char1a);
  // writeln("char0 = ", char0, " -- ", "char1 = ", char1);  // for testing

  if char_set.find(char0) != -1 {
    pw_chars += char0;
    i += 1;
  }

  if char_set.find(char1) != -1 && i < n_char {
    pw_chars += char1;
    i += 1;
  }

  j += 1;
}

writeln("\nYour password of ", n_char, " characters is: ", pw_chars, "\n");


///////////////////////////////////////////////
//
// user-defined functions
//
proc bin_string_to_int(str: string): int(32) {
  var result: int(32) = 0;
  var h: int(32) = 0;
  var pow: int(32) = 1;
  const s: int(32) = str.size:int(32);  // .size: the number of codepoints in str

  while h < s {
    if str[s-h-1] == '1' {
      result += pow;
    }
    pow *= 2;
    h += 1;
  }

  return result;
}


// end of random_bitstring_and_flexible_password_generator.chpl
