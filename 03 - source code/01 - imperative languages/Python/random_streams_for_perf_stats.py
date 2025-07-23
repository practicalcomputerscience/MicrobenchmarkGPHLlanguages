# -*- coding: utf-8 -*-
"""
random_streams_for_perf_stats.py

2025-06-01/03/18

run on Ubuntu 24 LTS: $ cd ./scripts/Python/_virtual_envs
                      $ source ./prng_test/bin/activate
                      $ cd ./prng_test
                      $ python3 ./random_bitstring_and_flexible_password_generator.py
                      $ deactivate

                      exe times measurements:
                      $ ./exe_times_statistics_for_one_test_case_in_cwd2 python3 ./random_streams_for_perf_stats.py

new: from io import StringIO => mean = 142 [milliseconds] <==> 237 [milliseconds]

"""

import re
import numpy as np

from io import StringIO  # new on 2025-06-03
# Python doesn’t have a direct equivalent of StringBuilder
# https://www.delftstack.com/howto/python/stringbuilder-in-python/


END = 62501  # 62501 for exactly 1M binary digits: all tests OK!
# END = 50  # for testing
# M1  = END*16 - 16
# K250 = END*4 - 4

m = 65521  # = 2^16 - 15
a = 17364
c = 0
# from: https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html

file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"


x = [0 for i in range(0,END)]  # also needed for the password later
x[0] = np.random.randint(0, m, size=1, dtype=int)[0]

# needed for bit stream:
# bits_x = ''  # original solution with string
# bits_x = bytearray(' '*M1, 'utf-8')  # new solution with bytearray()
                                     # = collection of integers
bits_x = StringIO()  # latest solution

# needed for ENT - A Pseudorandom Number Sequence Test Program:
# bits_hex = ''
# bits_hex = bytearray(' '*K250, 'utf-8')
bits_hex = StringIO()


print("\ngenerating a random bit stream...")
for i in range(1,END):
    x[i] = (a*x[i-1] + c) % m
    # e.g.: x[2] = np.int32(60827)

    # bits_x = bits_x + format(x[i], '016b')  # original solution
    bits_x_str = format(x[i], '016b')  # new solution
    bits_x.write(bits_x_str)

    # bits_hex = bits_hex + "0x{:04x}".format(x[i])[2:]  # original solution
    bits_x_str = format(x[i], '04x')  # needed for program ENT
    bits_hex.write(bits_x_str)

# print(f"{bits_x.getvalue()}")    # for testing
# print(f"{bits_hex.getvalue()}")  # for testing


try:
  with open(file_bits_x, "w", encoding="utf8") as f:
      f.write(bits_x.getvalue())
except Exception as e:
  print(f"could not write to file: {file_bits_x} ! -- {e}")
else:
  print(f"Bit stream has been written to disk under name:  {file_bits_x}")

try:
  with open(file_bits_hex, "w", encoding="utf8") as f:
      f.write(bits_hex.getvalue())
except Exception as e:
  print(f"could not write to file: {file_bits_hex} ! -- {e}")
else:
  print(f"Byte stream has been written to disk under name: {file_bits_hex}")

# end of random_streams_for_perf_stats.py
