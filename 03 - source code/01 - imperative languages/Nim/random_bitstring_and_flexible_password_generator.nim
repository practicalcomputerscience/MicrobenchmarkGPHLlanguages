#[
random_bitstring_and_flexible_password_generator.nim

2026-01-13

build on Ubuntu 24 LTS: $ nim c random_bitstring_and_flexible_password_generator.nim
                        $ nim c -d:release random_bitstring_and_flexible_password_generator.nim  # for production

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ nim -v
Nim Compiler Version 2.2.6 [Linux: amd64]
Compiled at 2025-10-31
Copyright (c) 2006-2025 by Andreas Rumpf
...
$

]#


# basically transpiled from random_bitstring_and_flexible_password_generator.c by Google AI,
# only changing and adding cosmetics:
import std/[random, strformat, strutils, ropes]

# --- Constants ---
const
  END = 62501  # For exactly 1M binary digits
  # END = 20  # for testing
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


  # Password logic
  var N_CHAR = 12
  var answer = false
  var answer_str: string

  while not answer:
    stdout.write &"\nPassword of {N_CHAR} printable chars OK? 'y' or another integer number >= 8: "
    # echo makes a new line at the end
    answer_str = stdin.readLine().strip()

    if answer_str == "y":
      answer = true
    else:
      try:
        N_CHAR = answer_str.parseInt()
        if N_CHAR >= 8:
          answer = true
        else:
          echo "enter an integer number >= 8 or 'y'"
      except ValueError:
        echo "enter an integer number >= 8 or 'y'"

  var WITH_SPECIAL_CHARS = true
  answer = false
  while not answer:
    stdout.write "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "
    answer_str = stdin.readLine().strip()

    if answer_str == "y":
      answer = true
    else:
      WITH_SPECIAL_CHARS = false
      answer = true

  var char_set = ""
  if WITH_SPECIAL_CHARS:
    for c in 33..126:
      char_set.add(chr(c))
  else:
    char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  # echo &"\nchar_set = {char_set}"  # for testing


  var pw_chars = ""
  var char0a, char1a: int

  var i = 0 # char counter for password
  var j = 0 # counter for x array

  while i < N_CHAR:
    let bin0 = fmt"{x[j]:016b}"  # Get 16-bit binary string
    # echo &"\nbin0 = {bin0}"  # for testing

    # Split into two 8-bit segments (equivalent to strncpy in C)
    let bin0_0 = bin0[0..7]
    let bin0_1 = bin0[8..15]
    # echo &"bin0_0 = {bin0_0}"  # for testing
    # echo &"bin0_1 = {bin0_1}"  # for testing

    # Convert binary strings back to integers:
    char0a = bin0_0.parseBinInt()
    char1a = bin0_1.parseBinInt()
    # echo &"char0a = {char0a}"  # for testing
    # echo &"char1a = {char1a}"  # for testing

    let char0b = chr(char0a)
    let char1b = chr(char1a)
    # echo &"char0b = {char0b}"  # for testing
    # echo &"char1b = {char1b}"  # for testing

    # Check if char exists in set (equivalent to strchr)
    if char_set.contains(char0b):
      pw_chars.add(char0b)
      i.inc

    if char_set.contains(char1b) and i < N_CHAR:
      pw_chars.add(char1b)
      i.inc

    j.inc

  echo &"\nYour password of {N_CHAR} characters is: {pw_chars}\n"

  return mainReturnVal


# Program Entry Point
quit(main())

# end of random_bitstring_and_flexible_password_generator.nim
