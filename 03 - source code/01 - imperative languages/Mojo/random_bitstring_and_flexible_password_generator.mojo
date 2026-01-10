# random_bitstring_and_flexible_password_generator.mojo
#
# 2025-05-05/06/07/21/22, 2025-06-18, 2025-12-13, 2025-12-18: see below
# 2026-01-10: Valgrind is also raising now a SIGILL signal even with prior program modifications at var char_set
#             => so, I will re-implement the old version, even though Valgrind will kill this program
#
# build on Ubuntu 24 LTS: $ mkdir password_encryption  # this is just a project directory
#                         $ cd password_encryption
#                         $ pixi shell  # <<<<<<<<<<<<<<<<<<<<<<<<
#                         $ mojo build random_bitstring_and_flexible_password_generator.mojo
#                         $ exit
#
# run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator
#
#
# see from Zig:
#   this is not working:  $ mojo build random_bitstring_and_flexible_password_generator.mojo -mcpu=native-avx512f
#                      => $ mojo: error: failed to create target info: unknown target triple 'unknown-unknown-linux'
#   this is not working:  $ .. -mcpu=native => mojo: error: failed to...
#                         https://docs.modular.com/mojo/cli/build/
#
#
# Valgrind, 2025-05-22 --> Illegal instruction (core dumped): how to fix??
#                      --> solution: not using ***char_set*** in this program,
#                                    but there are still bytes left on the heap at exit
#
#
# $ mojo --version  # do this only in the Pixi shell!!
# Mojo 0.26.1.0.dev2025121217 (3e295ef6)
# $


from random import random_ui64, seed
# from memory.unsafe_pointer import UnsafePointer  # 2025-05-22, this doesn't make a difference


def main():  # def for error handling below at user inputs: https://docs.modular.com/stable/mojo/manual/errors#raise-an-error

    comptime END = 62501  # 62501 for exactly 1M binary digits
    # comptime END  = 12  # for testing
    # comptime M1   = 1_000_000
    # comptime K250 = 250_000

    comptime m    = 65521  # = 2^16 - 15
    comptime a    = 17364
    comptime c    = 0

    file_bits_x   = "random_bitstring.bin"
    file_bits_hex = "random_bitstring.byte"

    # var x = UnsafePointer[UInt32].alloc(END)
    var x = List[UInt32]()  # List is a dynamically-sized array of elements => this has the same speed than UnsafePointer

    var bits_x:     String = ""
    var bits_hex:   String = ""  # needed for program ENT - A Pseudorandom Number Sequence Test Program

    seed()
    x.append(UInt32(random_ui64(1, m - 1)))  # min and max are inclusive

    print("\ngenerating a random bit stream...")
    for i in range(1,END):
        x_now = (a*x[i-1] + c) % m
        x.append(x_now)

        # bit stream:
        var bits_x_str      = bin(x[i-1]).removeprefix('0b').rjust(width = 16, fillchar = "0")
        bits_x             += bits_x_str

        # byte stream for program ENT:
        bits_x_str          = hex(x[i-1]).removeprefix('0x').rjust(width = 4, fillchar = "0")
        bits_hex           += bits_x_str


    # writing streams to files:
    try:
      with open(file_bits_x, "w") as f:
          f.write(bits_x)
      print("\nBit stream has been written to disk under name:", file_bits_x)
    except:
      print("could not write to file:", file_bits_x)

    try:
      with open(file_bits_hex, "w") as f:
          f.write(bits_hex)
      print("Byte stream has been written to disk under name:", file_bits_hex)
    except:
      print("could not write to file:", file_bits_hex)



    # make a password of N_CHAR printable chars: user input requested here
    var N_CHAR: Int = 12
    var answer: Bool = False
    var answer_str: String

    while not answer:
        N_CHAR = 12
        var q1_str = String("\nPassword of ", N_CHAR, " printable chars OK? 'y' or another integer number >= 8: ")
        answer_str = input(q1_str)

        if answer_str == "y":
          answer = True
          # N_CHAR = 12
        else:
          try:
            _ = Int(answer_str)
          except:
            N_CHAR = 12
            print("enter an integer number >= 8 or 'y'")
          else:
            N_CHAR = Int(answer_str)
            if N_CHAR < 8:
              print("enter an integer number >= 8 or 'y'")
            else:
              answer = True



    var WITH_SPECIAL_CHARS: Bool = True
    answer = False

    while not answer:
      var q2_str = String("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
      answer_str = input(q2_str)

      if answer_str == "y":
        answer = True
      else:
        WITH_SPECIAL_CHARS = False
        answer = True


    # 2026-01-10:
    # this is the original definition of char_set, which will make Valgrind kill this program.
    # However, it's not only this definition which will make Valgrind kill this program.
    # So, any changes to make this program 'Valgrind-proof' may be futile,
    # while trying to keep it idiomatic and still compiling it with
    # "$ mojo build random_bitstring_and_flexible_password_generator.mojo",
    # with this fast evolving language.
    var char_set: String = ""
    if WITH_SPECIAL_CHARS == True:
        # add chars dec 33 .. dec 126:
        for i in range(33,127):
          char_set += chr(i)
    else:
        char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


    var i = 0  # char counter for the password
    var j = 0  # char counter for x
    var pw_chars: String = ""

    while i < N_CHAR:
      bin0 = bin(x[j]).removeprefix('0b').rjust(width = 16, fillchar = "0")
      # bin(x[j]): no padding:
      #   0b1000001101100001 --> 16 bits
      #   0b11100110010011
      #   0b101110011010
      # https://docs.modular.com/mojo/stdlib/builtin/format_int/bin
      # https://docs.modular.com/mojo/stdlib/collections/string/string/String#removeprefix
      # https://docs.modular.com/mojo/stdlib/collections/string/string/String/#rjust
      # print(bin0)

      bin0_0 = "0b" + bin0[0:8]
      bin0_1 = "0b" + bin0[8:16]
      # print(bin0_0, bin0_1)

      # atol (--> C/C++): Parses and returns the given string as an integer in the given base:
      # https://docs.modular.com/mojo/stdlib/collections/string/string/atol/
      int0_0 = atol(bin0_0, 0)
      int0_1 = atol(bin0_1, 0)

      var char0: String = chr(int0_0)
      var char1: String = chr(int0_1)

      # https://docs.modular.com/mojo/std/collections/string/string/String/
      if char_set.find(char0) != -1:
          pw_chars += char0
          i += 1
          if i == N_CHAR:
            break

      if char_set.find(char1) != -1 :
          pw_chars += char1
          i += 1

      j += 1

    print("\nYour password of", N_CHAR, "characters is:", pw_chars, "\n")

    # x.free()

# end of random_bitstring_and_flexible_password_generator.mojo
