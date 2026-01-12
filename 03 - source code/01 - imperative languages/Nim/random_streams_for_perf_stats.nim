#[
random_streams_for_perf_stats.nim

2026-01-12

build on Ubuntu 24 LTS: $ nim c -d:release random_streams_for_perf_stats.nim

run on Ubuntu 24 LTS:   $ time ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats


$ nim -v
Nim Compiler Version 2.2.6 [Linux: amd64]
Compiled at 2025-10-31
Copyright (c) 2006-2025 by Andreas Rumpf
...
$

]#


# basically transpiled from random_streams_for_perf_stats.c by Google AI,
# only changing and adding cosmetics:
import std/[random, strformat, strutils, ropes]

# --- Constants ---
const
  END = 62501  # For exactly 1M binary digits
  # END = 10  # for testing
  # M1       = END * 16
  # K250     = END * 4

  m        = 65521  # 2^16 - 15
  a        = 17364
  c        = 0

  file_bits_x   = "random_bitstring.bin"
  file_bits_hex = "random_bitstring.byte"


proc main(): int =
  var mainReturnVal = 0
  var x = newSeq[int](END)

  randomize()  # Initialize RNG seed (equivalent to srand(time(NULL)))

  # Initial random value
  x[0] = rand(1 .. m-1)  # Nim's rand(max) is inclusive
  # echo &"x[0] = {x[0]}"  # for testing

  var bits_x   = rope("")  # see: https://nim-lang.org/docs/ropes.html
  var bits_hex = rope("")
  var bits_x_str   = "----------------"
  var bits_hex_str = "----"


  echo "\nGenerating a random bit stream..."
  for i in 1 ..< END:
    x[i] = ((a * x[i-1]) + c) mod m  # LCG Calculation
    # echo &"\nx[i] = {x[i]}"  # for testing

    # Formatting using strformat (equivalent to sprintf)
    # {val:016b} formats to 16-bit binary with leading zeros
    # {val:04x} formats to 4-digit hexadecimal with leading zeros
    bits_x_str = fmt"{x[i]:016b}"
    # echo &"bits_x_str = {bits_x_str}"  # for testing
    bits_x.add(bits_x_str)

    bits_hex_str = fmt"{x[i]:04x}"
    # echo &"bits_hex_str = {bits_hex_str}"  # for testing
    bits_hex.add(bits_hex_str)


  let bits_x_str_total   = $bits_x  # convert the rope into a string
  let bits_hex_str_total = $bits_hex


  # Write bit stream to disk
  try:
    writeFile(file_bits_x, bits_x_str_total)
    echo &"Bit stream has been written to disk under name:  {file_bits_x}"
  except IOError:
    echo &"could not write to file: {file_bits_x} !"
    mainReturnVal = 1

  # Write hex stream to disk
  try:
    writeFile(file_bits_hex, bits_hex_str_total)
    echo &"Byte stream has been written to disk under name: {file_bits_hex}"
  except IOError:
    echo &"Could not write to file: {file_bits_hex} !"
    mainReturnVal = 1

  return mainReturnVal

# Program Entry Point
quit(main())

# end of random_streams_for_perf_stats.nim
