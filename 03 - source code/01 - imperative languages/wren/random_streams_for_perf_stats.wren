/* random_streams_for_perf_stats.wren

2025-07-15/16

test in Ubuntu 24 LTS:

run in Ubuntu 24 LTS:  $ wren_cli ./random_streams_for_perf_stats.wren
                       $ time wren_cli ./random_streams_for_perf_stats.wren => real	0m34,036s with simple string concatenation


$ wren_cli --version
wren 0.4.0
$

REPL: $ wren_cli

*/


import "random" for Random
// import "io" for Stdin, Stdout // https://github.com/joshgoebel/wren-console/blob/network/example/animals.wren

import "io" for File



//////////////////////////////////////////////////////////////////////////
//
// user defined functions

var Integer_to_bin_string = Fn.new { |N|
  var bin_str = ""  // A string is an immutable array of bytes: https://wren.io/modules/core/string.html
  var j = 15
  var k = N

  while (k > 0 && j >= 0) {
    if (k % 2 == 0) {
      bin_str = "0" + bin_str
    } else {
      bin_str = "1" + bin_str
    }
    k = (k / 2).floor
    j = j - 1
  }

  // padding with leading "0" if needed:
  var diff = 16 - bin_str.count
  if (diff > 0) {
    return "0"*(diff) + bin_str
  } else {
    return bin_str  // not padding needed
  }
}


var Integer_to_hex_string = Fn.new { |N|
  var hex_str = ""
  var j = 3
  var k = N
  var remainder = 0

  while (k > 0 && j >= 0) {
    remainder = k % 16

    if (remainder == 0) {
      hex_str = "0" + hex_str
    } else {
      if (remainder == 1) {
        hex_str = "1" + hex_str
      } else {
        if (remainder == 2) {
          hex_str = "2" + hex_str
        } else {
          if (remainder == 3) {
            hex_str = "3" + hex_str
          } else {
            if (remainder == 4) {
              hex_str = "4" + hex_str
            } else {
              if (remainder == 5) {
                hex_str = "5" + hex_str
              } else {
                if (remainder == 6) {
                  hex_str = "6" + hex_str
                } else {
                  if (remainder == 7) {
                    hex_str = "7" + hex_str
                  } else {
                    if (remainder == 8) {
                      hex_str = "8" + hex_str
                    } else {
                      if (remainder == 9) {
                        hex_str = "9" + hex_str
                      } else {
                        if (remainder == 10) {
                          hex_str = "a" + hex_str
                        } else {
                          if (remainder == 11) {
                            hex_str = "b" + hex_str
                          } else {
                            if (remainder == 12) {
                              hex_str = "c" + hex_str
                            } else {
                              if (remainder == 13) {
                                hex_str = "d" + hex_str
                              } else {
                                if (remainder == 14) {
                                  hex_str = "e" + hex_str
                                } else {
                                  if (remainder == 15) {
                                    hex_str = "f" + hex_str
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    k = (k / 16).floor
    j = j - 1
  }


  // padding with leading "0" if needed:
  var diff = 4 - hex_str.count
  if (diff > 0) {
    return "0"*(diff) + hex_str
  } else {
    return hex_str  // not padding needed
  }
}


// end of user defined functions
//
//////////////////////////////////////////////////////////////////////////



var END = 62500  // 62500 for exactly 1M binary digits
// var END = 10  // for testing

var m = 65521  // = 2^16 - 15
var a = 17364
var c = 0

var file_bits_x   = "random_bitstring.bin"
var file_bits_hex = "random_bitstring.byte"

var x = []  // the basic list, also needed for the password later
// var x = (0..m).toList as an alternative?

var x_new = 0
var random = Random.new()
x_new = random.int(1, m)
x.add(x_new)

var bits_x       = ""
var bits_hex     = ""
var bits_x_str   = ""
var bits_hex_str = ""


System.print("\ngenerating a random bit stream...")

for (i in 1..END) {  // with .. END is exclusive
  x_new = ((a * x[i-1]) + c) % m
  // System.print("\nx_new = %(x_new)")  // for testing

  bits_x_str = Integer_to_bin_string.call(x_new)
  // System.print("  bits_x_str = %(bits_x_str)")  // for testing
  bits_x = bits_x + bits_x_str

  bits_hex_str = Integer_to_hex_string.call(x_new)
  // System.print("  bits_hex_str = %(bits_hex_str)")  // for testing
  bits_hex = bits_hex + bits_hex_str

  x.add(x_new)
}

// System.print("bits_x = %(bits_x)")  // for testing
// System.print("bits_hex = %(bits_hex)")  // for testing


// see fiber idea for error handling from here: https://wren.io/error-handling.html
// write bit stream to disk:
var write_to_file1 = Fiber.new {
  var file = File.create(file_bits_x)
  file.writeBytes(bits_x)
  file.close()
  System.print("Bit stream has been written to disk under name:  %(file_bits_x)")
  return
}
var error1 = write_to_file1.try()
if (error1 != null) {
  System.print("could not write to file: %(file_bits_x) -- %(error1)")
}

// write byte stream to disk:
var write_to_file2 = Fiber.new {
  var file = File.create(file_bits_hex)
  file.writeBytes(bits_hex)
  file.close()
  System.print("Byte stream has been written to disk under name: %(file_bits_hex)")
  return
}
var error2 = write_to_file2.try()
if (error2 != null) {
   System.print("could not write to file: %(file_bits_hex) -- %(error2)")
}

// end of random_streams_for_perf_stats.wren
