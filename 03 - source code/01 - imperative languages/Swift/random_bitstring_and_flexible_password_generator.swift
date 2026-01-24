/*
random_bitstring_and_flexible_password_generator.swift

2025-05-08/09/17/31, 2025-06-01, 2025-12-13: see below
2026-01-24: cosmetics at password user dialog and password printing

build on Ubuntu 24 LTS: $ swift package init --name random_bitstring_and_flexible_password_generator --type executable
                        $ swift build  # Building for ***debugging***
                        $ ...
                        # https://www.swift.org/documentation/server/guides/building.html
                        $ swift build -c release  # Building for release <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                        
run on Ubuntu 24 LTS:   $ ./.build/x86_64-unknown-linux-gnu/release/random_bitstring_and_flexible_password_generator


2025-05-17:
  making this program a little bit faster to execute:
    use try bits_x.write(toFile: file_bits_x, atomically: false, encoding: .utf8)
    instead of try bits_x.write(toFile: file_bits_x, atomically: true, encoding: .utf8)
  see below.
    

Package.swift is the manifest file for Swift. It’s where you keep metadata for your project, as well as dependencies.
https://www.swift.org/getting-started/cli-swiftpm/

$ swift --version
Swift version 6.1 (swift-6.1-RELEASE)
Target: x86_64-unknown-linux-gnu
$

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
    bits_x_str_pad = pad(string: bits_x_str, toSize: 16)  // pad: user defined function
    // print("bits_x_str_pad: \(bits_x_str_pad)")            // for testing
    bits_x += bits_x_str_pad

    bits_x_str     = String(x[i], radix: 16)              // no padding
    bits_x_str_pad = pad(string: bits_x_str, toSize: 4)   // pad: user defined function
    // print("bits_x_str_pad: \(bits_x_str_pad)")            // for testing
    bits_hex += bits_x_str_pad
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



// make a password of N_CHAR printable chars: user input requested here
var N_CHAR = 12
var answer = false
var answer_str = ""
while !answer {
  N_CHAR = 12
  print("\nPassword of \(N_CHAR) printable chars OK? 'y' or another integer number >= 8: ", terminator: "")
  // terminator: "": print without newline at the end
  answer_str = readLine(strippingNewline: true)!
  // https://www.tutorialspoint.com/swift-program-to-get-input-from-the-user

  if answer_str == "y" {
    answer = true
  } else {
    if Int(answer_str) != nil {
      N_CHAR = Int(answer_str)!
      if N_CHAR < 8 {
        print("enter an integer number >= 8 or 'y'\n", terminator: "")
      } else {
        answer = true
      }
    } else {
      print("enter an integer number >= 8 or 'y'\n", terminator: "")
    }
  }
}


var WITH_SPECIAL_CHARS = true
answer = false
answer_str = ""
while !answer {
  print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ", terminator: "")

  answer_str = readLine(strippingNewline: true)!
  if answer_str == "y" {
    answer = true
  } else {
    WITH_SPECIAL_CHARS = false
    answer = true
  }
}

var char_set = ""
let digits          = UInt32("0") ... UInt32("9")
let small_letters   = UInt32("a") ... UInt32("z")
let big_letters     = UInt32("A") ... UInt32("Z")
let printable_chars = UInt32("!") ... UInt32("~")
// https://stackoverflow.com/questions/49808837/initialize-a-string-from-a-range-of-characters-in-swift

if WITH_SPECIAL_CHARS {
  char_set = String(String.UnicodeScalarView(printable_chars.compactMap(UnicodeScalar.init)))
}  else {
  char_set = String(String.UnicodeScalarView(digits.compactMap(UnicodeScalar.init)))
           + String(String.UnicodeScalarView(small_letters.compactMap(UnicodeScalar.init)))
           + String(String.UnicodeScalarView(big_letters.compactMap(UnicodeScalar.init)))
}
// print("char_set = \(char_set)")  // char_set = 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ


var i = 0  // char counter for the password
var j = 0  // char counter for x
var pw_chars = ""

while i < N_CHAR {
  // convert an integer number into a string of '0' and '1' characters:
  bits_x_str     = String(x[j], radix: 2)               // no padding
  bits_x_str_pad = pad(string: bits_x_str, toSize: 16)  // pad: user defined function
  // print("bits_x_str_pad = \(bits_x_str_pad)")  // for testing

  let index0 = bits_x_str_pad.index(bits_x_str_pad.startIndex, offsetBy: 8)
  let bin0_0 = bits_x_str_pad[..<index0]

  let index1 = bits_x_str_pad.index(bits_x_str_pad.endIndex, offsetBy: -8)
  let bin0_1 = bits_x_str_pad[index1...]
  // https://stackoverflow.com/questions/39677330/how-does-string-substring-work-in-swift
  // print("bin0_0 = \(bin0_0), bin0_1 = \(bin0_1)")  // for testing

  let int0_0 = strtoul(String(bin0_0), nil, 2)
  let int0_1 = strtoul(String(bin0_1), nil, 2)
  // print("int0_0 = \(int0_0), int0_1 = \(int0_1)")  // for testing

  let char0 = String(Character(UnicodeScalar(UInt32(int0_0))!))  // https://www.dotnetperls.com/convert-int-character-swift
  let char1 = String(Character(UnicodeScalar(UInt32(int0_1))!))
  // print("char0 = \(char0), char1 = \(char1)")  // for testing

  if char_set.contains(char0) {
      pw_chars = pw_chars + char0
      i += 1
  }

  if char_set.contains(char1) && i < N_CHAR {
      pw_chars = pw_chars + char1
      i += 1
  }

  j += 1
}

print("\nYour password of \(N_CHAR) characters is: \(pw_chars)")



///////////////////////////////////////////////////////////////////////
//
// user defined functions:

// https://stackoverflow.com/questions/26181221/how-to-convert-a-decimal-number-to-binary-in-swift#26181323
func pad(string : String, toSize: Int) -> String {
  var padded = string
  for _ in 0..<(toSize - string.count) {
    padded = "0" + padded
  }
  return padded
}

// end of random_bitstring_and_flexible_password_generator.swift
