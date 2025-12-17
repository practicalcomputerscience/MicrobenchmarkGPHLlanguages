/'
random_streams_for_perf_stats.bas

2025-07-11; 2025-12-17

build on Ubuntu 24 LTS: $ fbc ./random_streams_for_perf_stats.bas

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats

                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats


I'll leave it with the STRING type and not test a program variant with ZSTRING type. This program feels speedy enough.
See also the "freebasic stringbuilder" demo: https://www.freebasic.net/forum/viewtopic.php?t=32964
https://documentation.help/FreeBASIC/CompilerFAQ.html#item5


$ fbc --version
FreeBASIC Compiler - Version 1.10.1 (2023-12-24), built for linux-x86_64 (64bit)
Copyright (C) 2004-2023 The FreeBASIC development team.
$

'/


const upper_limit = 62500  ' 62501 for exactly 1M binary digits
' const upper_limit = 10  ' for testing
const m1          = upper_limit * 16
const k250        = upper_limit * 4

const m = 65521  ' = 2^16 - 15
const a = 17364
const c = 0

const file_bits_x   = "random_bitstring.bin"
const file_bits_hex = "random_bitstring.byte"

dim x(0 to upper_limit) as integer

randomize , 1  ' use C-rand() function
x(0) = int(1 + rnd * (m - 1))  ' 2025-12-17
' print "x(0) = " & x(0)  ' for testing


dim bits_x as string = ""
dim bits_x_str0 as string
dim bits_x_str as string

dim bits_hex as string = ""
dim bits_hex_str0 as string
dim bits_hex_str as string


print !"\r\ngenerating a random bit stream..."  ' !"\r\n for escaping

dim i as integer
for i = 1 to upper_limit
  x(i) = (a * x(i-1) + c) mod m  ' having here x_now to save 2 x x(i) is not a faster solution!
  ' print "x(i) = " & x(i)  ' for testing

  bits_x_str = bin(x(i), 16)  ' convert integer to binary string with padding: https://freebasic.net/wiki/KeyPgBin
  ' print "  bits_x_str = " & bits_x_str  ' for testing
  bits_x += bits_x_str

  bits_hex_str = lcase(hex$(x(i), 4))  ' convert integer to hexadecimal string
  ' print "  bits_hex_str = " & bits_hex_str  ' for testing
  bits_hex += bits_hex_str
next

' print "bits_x = " & bits_x & !"\r"  ' for testing
' print "bits_hex = " & bits_hex & !"\r"  ' for testing


' write bit stream to disk:
dim as long f1 = freefile
if 0 <> open(file_bits_x, for binary, as f1) then
    print
    print "could not open file for writing: " & file_bits_x & !"\r\n"
else
  if 0 <> put(#f1, , bits_x) then
    print "could not write to file: " & file_bits_x & !"\r\n"
  else
    print "Bit stream has been written to disk under name:  " & file_bits_x
    close #f1
  end if
end if


' write byte stream to disk:
dim as long f2 = freefile
if 0 <> open(file_bits_hex, for binary, as f2) then
    print "could not open file for writing: " & file_bits_hex & !"\r\n"
else
  if 0 <> put(#f2, , bits_hex) then
    print "could not write to file: " & file_bits_hex & !"\r\n"
  else
    print "Byte stream has been written to disk under name: " & file_bits_hex
    close #f2
  end if
end if

' end of random_streams_for_perf_stats.bas
