/'
random_bitstring_and_flexible_password_generator.bas

2025-07-12; 2025-12-17

build on Ubuntu 24 LTS: $ fbc ./random_bitstring_and_flexible_password_generator.bas

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ fbc --version
FreeBASIC Compiler - Version 1.10.1 (2023-12-24), built for linux-x86_64 (64bit)
Copyright (C) 2004-2023 The FreeBASIC development team.
$

'/


'------------------------------------------------------------------
' user defined functions

function IsPositiveIntegerNumber(byref s as string) as Boolean
  if s = "" then
    return false  ' Empty string is not an integer
  else
    dim as integer i = 1
    dim as integer codepoint
    for i = i to len(s)
        codepoint = asc(mid(s, i, 1))
        if (codepoint < 48 or codepoint > 57) then  ' character < "0" or > "9"?
          return false
        end if
    next
    return true
  end if
end function

' end of user defined functions
'------------------------------------------------------------------



const upper_limit = 62500  ' 62501 for exactly 1M binary digits
' const upper_limit = 15  ' for testing
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


dim bits_x as string = "",   bits_x_str0 as string,   bits_x_str as string
dim bits_hex as string = "", bits_hex_str0 as string, bits_hex_str as string


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


' make a password of n_char printable chars: user input requested here
dim n_char as integer = 12, n_char_ as integer = 0
dim answer as Boolean = false
dim answer_str as string = ""

while (answer = false)
  line input !"\r\nPassword of " & n_char & " printable chars OK? 'y' or another integer number >= 8: "; answer_str

  if (answer_str = "y") then
    answer = true
  else
    if IsPositiveIntegerNumber(answer_str) Then
      n_char_ = valint(answer_str)
      if (n_char_ < 8) then
        print "enter an integer number >= 8 or 'y'"
      else
        n_char = n_char_
        answer = true
      end if
    else
      print "enter an integer number >= 8 or 'y'"
    end if
  end if
wend
' print "n_char = " & n_char  ' for testing


dim with_special_chars as Boolean = true
answer = false
while (answer = false)
  line input !"\r\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "; answer_str

  if (answer_str = "y") then
    answer = true
  else
    with_special_chars = false
    answer = true
  end if
wend
' print "with_special_chars = " & with_special_chars  ' for testing


dim char_set as string = ""
if with_special_chars then
  for i = 33 to 127
    char_set += chr(i)
  next
else
  char_set = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
end if
' print "char_set = " & char_set  ' for testing


i = 0                 ' char counter for the password
dim j as integer = 0  ' counter for x
dim pw_chars as string = ""
dim bin0 as string, bin0_0 as string, bin0_1 as string
dim char0 as string, char1 as string

while (i < n_char)
  bin0 = bin(x(j), 16)
  ' print !"\r\nx(j) = " &  bin0  ' for testing

  bin0_0 = mid(bin0, 1, 8)
  bin0_1 = mid(bin0, 9, 8)
  ' print "  bin0_0 = " &  bin0_0 & " -- bin0_1 = " &  bin0_1  ' for testing

  char0 = chr(val("&B" + bin0_0))
  char1 = chr(val("&B" + bin0_1))
  ' print "  char0 = " &  char0 & " -- char1 = " &  char1  ' for testing

  if instr(char_set, char0) then
    pw_chars += char0
    i += 1
  end if

  if (instr(char_set, char1) and (i < n_char)) then
    pw_chars += char1
    i += 1
  end if

  j += 1
wend

print !"\r\nYour password of " & n_char & " characters is: " & pw_chars

' end of random_bitstring_and_flexible_password_generator.bas

