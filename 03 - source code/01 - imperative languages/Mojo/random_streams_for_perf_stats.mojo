# random_streams_for_perf_stats.mojo
#
# 2025-05-31, 2025-12-13, 2025-12-18: see below
# 2026-05-28: refactor for Mojo v.1.0.0
#
#
# build on Ubuntu 24 LTS: $ mkdir password_encryption  # this is just a project directory
#                         $ cd password_encryption  
#                         $ pixi shell
#                         $ mojo build random_streams_for_perf_stats.mojo
#                         $ exit
#
# run on Ubuntu 24 LTS:   $ sudo perf stat -r 20 ./random_streams_for_perf_stats
#
#
# $ mojo --version  # do this only in the Pixi shell!!
# Mojo 1.0.0b2.dev2026052706 (83444c6d)
# $


from std.random import random_ui64, seed  # 2026-05-28


def main() raises:  # 2026-05-28 
    # def for error handling below at user inputs: https://docs.modular.com/stable/mojo/manual/errors#raise-an-error

    comptime END = 62501  # 62501 for exactly 1M binary digits
    # comptime END  = 12  # for testing
    # comptime M1   = 1_000_000
    # comptime K250 = 250_000

    comptime m    = 65521  # = 2^16 - 15
    comptime a    = 17364
    comptime c    = 0

    file_bits_x   = "random_bitstring.bin"
    file_bits_hex = "random_bitstring.byte"

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
        var bits_x_str      = bin(x_now, prefix = "").ascii_rjust(width = 16, fillchar = "0")  # 2026-05-28
        bits_x             += bits_x_str

        # byte stream for program ENT:
        var bits_hex_str    = hex(x_now, prefix = "").ascii_rjust(width = 4, fillchar = "0")  # 2026-05-28
        bits_hex           += bits_hex_str  # 2026-05-28


    # writing streams to files:
    try:
      with open(file_bits_x, "w") as f:
          f.write(bits_x)
      print("Bit stream has been written to disk under name:", file_bits_x)
    except:
      print("could not write to file:", file_bits_x)

    try:
      with open(file_bits_hex, "w") as f:
          f.write(bits_hex)
      print("Byte stream has been written to disk under name:", file_bits_hex)
    except:
      print("could not write to file:", file_bits_hex)

# end of random_streams_for_perf_stats.mojo
