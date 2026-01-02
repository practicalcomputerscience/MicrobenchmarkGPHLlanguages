# -*- coding: utf-8 -*-
"""
random_bitstring_and_flexible_password_generator.py

2025-03-09/10/13/15/26/29/30/31, 2025-05-05/28, 2025-06-01/03/18, 2025-12-13: see below
2026-01-03: see below

check the quality of randomness at:
  https://mzsoltmolnar.github.io/random-bitstream-tester/
  --> put exactly 1,000,000 binary digits ("0","1") into its input
      for all tests to pass (no "Error" indication is to be targeted)


run on Ubuntu 24 LTS: $ cd ./scripts/Python/_virtual_envs
                      $ source ./prng_test/bin/activate
                      $ cd ./prng_test
                      $ python3 ./random_bitstring_and_flexible_password_generator.py
                      $ deactivate


----------------------------------------------------------------
PRNG = Pseudo-Random Number Generator
Linear Congruential Generator (LCG)
https://en.wikipedia.org/wiki/Linear_congruential_generator


ideas:
  https://www.codespeedy.com/building-pseudo-random-number-generator-from-scratch-in-javascript/
  https://www.calculatorsoup.com/calculators/math/prime-number-calculator.php
  https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html

"""

import re
# import time
import numpy as np

from io import StringIO  # new on 2025-06-03
# Python doesn’t have a direct equivalent of StringBuilder
# https://www.delftstack.com/howto/python/stringbuilder-in-python/


END = 62501  # 62501 for exactly 1M binary digits: all tests OK!
# M1  = END*16 - 16
# K250 = END*4 - 4

# for testing:
# END = 50
# M1  = END*16 - 16  # OK with END = 10
# K250 = END*4 - 4   # OK with END = 10


m = 65521  # = 2^16 - 15
a = 17364
c = 0
# from: https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html

file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"


x = [0 for i in range(0,END)]  # also needed for the password later
x[0] = np.random.randint(1, m, size=1, dtype=int)[0]  # 2025-12-13: (0, m,.. --> (1, m,): m is exclusive

# needed for bit stream:
# bits_x = ''  # original solution with string
# bits_x = bytearray(' '*M1, 'utf-8')  # new solution with bytearray()
                                     # = collection of integers
# needed for ENT - A Pseudorandom Number Sequence Test Program:
# bits_hex = ''  # original solution with string
# bits_hex = bytearray(' '*K250, 'utf-8')  # new solution with bytearray()

bits_x = StringIO()    # latest solution
bits_hex = StringIO()  # latest solution


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



# make a password of N_CHAR printable chars:
N_CHAR = 12  # base case
answer = False
while answer is False:
    reply = input(f'\nPassword of {N_CHAR} printable chars OK? \
"y" or another integer number >= 8: ')
    if reply == 'y':
        answer = True
    else:
        try:
            N_CHAR = int(reply)
            if N_CHAR >= 8:
                answer = True
            else:
                N_CHAR = 12
                print('enter an integer number >= 8 or "y"')
        except ValueError:
            N_CHAR = 12
            print('enter an integer number >= 8 or "y"')


def binary_to_string(bits):
    """
    bits to string
    """
    return ''.join([chr(int(i, 2)) for i in bits])


WITH_SPECIAL_CHARS = True
answer = False
while answer is False:
    reply = input('\nDo you want me to use special characters like .;,+*... ? "y" or "n": ')
    if reply == 'y':
        answer = True
    else:
        WITH_SPECIAL_CHARS = False
        answer = True


if WITH_SPECIAL_CHARS is True:
    pattern = re.compile(r"[A-Za-z0-9!\"#$%&'()*+,-./:;<=>?@[\]\\^_`{|}~]+")
    # hashcat special characters
    # https://security.stackexchange.com/questions/201931/hashcat-specify-number-of-characters
    # ?s = «space»!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~

else:
    pattern = re.compile(r"[A-Za-z0-9]+")

i = 0  # char counter in password
j = 0  # char counter in bits_char
pw_chars = []

while i < N_CHAR:

    # convert numpy.int32 into a string of '0's and '1's:
    bin0 = f'{x[j]:016b}'
    # bin0 could be for example ' 111001001100101'
    # --> padding needed with leading zeros: 016b

    bin0_0 = bin0[0:8]   # position 8 is exclusive in Python!!
    bin0_1 = bin0[8:16]

    char0 = binary_to_string([bin0_0])
    char1 = binary_to_string([bin0_1])

    if pattern.fullmatch(char0) is not None:
        pw_chars.append(char0)
        i += 1
        if i == N_CHAR:
            break
    if pattern.fullmatch(char1) is not None:
        pw_chars.append(char1)
        i += 1

    j += 1

pw_string = ''.join(pw_chars)
print(f'\nYour password of {N_CHAR} characters is:', pw_string)  # 2026-01-03

# end of random_bitstring_and_flexible_password_generator.py
