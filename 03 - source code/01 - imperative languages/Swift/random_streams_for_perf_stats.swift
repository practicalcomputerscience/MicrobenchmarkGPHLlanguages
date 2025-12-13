/*
random_streams_for_perf_stats.swift

2025-05-31, 2025-06-08, 2025-12-13: see below

build on Ubuntu 24 LTS: $ mkdir random_streams_for_perf_stats
                        $ cd random_streams_for_perf_stats
                        $ swift package init --name random_streams_for_perf_stats --type executable
                        $ swift build -c release
                        
run on Ubuntu 24 LTS:   $ ./.build/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./.build/x86_64-unknown-linux-gnu/release/random_streams_for_perf_stats
                        => pad(string: ...):                   0,032561 +- 0,000296 seconds time elapsed  ( +-  0,91% )
                        => "inlining" (no user-def. function): 0,031861 +- 0,000293 seconds time elapsed  ( +-  0,92% )
                        
                        => String(repeating:...): 0,041059 +- 0,000290 seconds time elapsed  ( +-  0,71% )
                        => bits_x_str.padding():  0,065185 +- 0,000700 seconds time elapsed  ( +-  1,07% )

*/


import Foundation  // essential data types, collections, OS services: https://developer.apple.com/documentation/foundation


let END  = 62501  // 62501 for exactly 1M binary digits; let is immutable
// let END  = 100  // for testing
// let M1   = 1_000_000
// let K250 = 250_000

let m    = 65521  // = 2^16 - 15
let a    = 17364
let c    = 0

let file_bits_x   = "random_bitstring.bin"
let file_bits_hex = "random_bitstring.byte"

var x = [Int]()
// https://www.hackingwithswift.com/articles/128/array-performance-append-vs-reservecapacity

x.append(Int.random(in: 1..<m))  // 2025-12-13: 0..<m --> 1..<m
// print(x[0])  // for testing

var bits_x         = ""
var bits_x_str     = ""
var bits_x_str_pad = ""
var bits_hex       = ""  // needed for program ENT - A Pseudorandom Number Sequence Test Program


print("\ngenerating a random bit stream...", terminator: "")
for i in 1..<END {
    x.append((a*x[i-1] + c) % m)
    // print("x[i]: \(x[i])")                                // for testing

    bits_x_str     = String(x[i], radix: 2)               // no padding
    // bits_x_str_pad = pad(string: bits_x_str, toSize: 16)  // pad: user defined function
    // print("bits_x_str_pad: \(bits_x_str_pad)")            // for testing
    // bits_x_str_pad = String(repeating: "0", count: 16 - bits_x_str.count).appending(bits_x_str)
    // bits_x_str_pad = bits_x_str.padding(toLength: 16, withPad: "0", startingAt: 0)
    
    for _ in 0..<(16 - bits_x_str.count) {
      bits_x_str = "0" + bits_x_str
    }
    
    // bits_x += bits_x_str_pad
    bits_x += bits_x_str

    bits_x_str     = String(x[i], radix: 16)              // no padding
    // bits_x_str_pad = pad(string: bits_x_str, toSize: 4)   // pad: user defined function
    // print("bits_x_str_pad: \(bits_x_str_pad)")            // for testing
    // bits_x_str_pad = String(repeating: "0", count: 4 - bits_x_str.count).appending(bits_x_str)
    // bits_x_str_pad = bits_x_str.padding(toLength: 4, withPad: "0", startingAt: 0)

    for _ in 0..<(4 - bits_x_str.count) {
      bits_x_str = "0" + bits_x_str
    }
    
    // bits_hex += bits_x_str_pad
    bits_hex += bits_x_str
}


// writing streams to files:
do {
    try bits_x.write(toFile: file_bits_x, atomically: false, encoding: .utf8)
    print("\nBit stream has been written to disk under name:  \(file_bits_x)")
} catch {
    print("could not write to file: \(file_bits_x)")
}

do {
    try bits_hex.write(toFile: file_bits_hex, atomically: false, encoding: .utf8)
    print("Byte stream has been written to disk under name: \(file_bits_hex)")
} catch {
    print("could not write to file: \(file_bits_hex)")
}
// https://www.mozzlog.com/blog/how-to-write-the-content-of-string-to-a-file-in-swift
//   The atomically parameter indicates whether the data should be written atomically to avoid data corruption
//   in the event of an interruption. In this case, we set it to true.
// https://developer.apple.com/documentation/foundation/nsdata/write(tofile:atomically:)
//   useAuxiliaryFile
//   If true, the data is written to a backup file, and then—assuming no errors occur—the backup file is
//   renamed to the name specified by path; otherwise, the data is written directly to path.



///////////////////////////////////////////////////////////////////////
//
// user defined functions:

// https://stackoverflow.com/questions/26181221/how-to-convert-a-decimal-number-to-binary-in-swift#26181323

/*
func pad(string : String, toSize: Int) -> String {
  var padded = string
  for _ in 0..<(toSize - string.count) {
    padded = "0" + padded
  }
  return padded
}
*/


// end of random_streams_for_perf_stats.swift
