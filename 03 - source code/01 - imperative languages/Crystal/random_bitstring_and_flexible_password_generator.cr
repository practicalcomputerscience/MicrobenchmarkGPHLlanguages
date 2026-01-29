# random_bitstring_and_flexible_password_generator.cr
#
# 2025-06-01/02/06/18; 2025-12-17
#
# build on Ubuntu 24 LTS: $ crystal build random_bitstring_and_flexible_password_generator.cr --release
#
# run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator
#                         $ valgrind ./random_bitstring_and_flexible_password_generator


END   = 62501  # 62501 for exactly 1M binary digits; Int32
# END   = 25  # for testing
# M1    = END * 16
# K250  = END * 4

M = 65521  # = 2^16 - 15
A = 17364
C = 0

FILE_BITS_X   = "random_bitstring.bin"
FILE_BITS_HEX = "random_bitstring.byte"


x = [] of Int32
x << Random.rand(M-1) +1  # rand(): Generates a random integer which is greater than or equal to 0 and less than max; 2025-12-17
# p! x  # for testing

bits_x   = IO::Memory.new  # https://crystal-lang.org/reference/1.16/guides/performance.html
bits_hex = IO::Memory.new


puts "\ngenerating a random bit stream..."  # puts = put string; gets = get string
i = 1
while i < END
  x << (A*x[i-1] + C) % M
  # puts "\n#{x[i]}"  # for testing

  bits_x_str = x[i].to_s(2, precision: 16)
  # puts "#{bits_x_str}"  # for testing
  bits_x << bits_x_str

  bits_x_str = x[i].to_s(16, precision: 4)
  # puts "#{bits_x_str}"  # for testing
  bits_hex << bits_x_str

  i += 1
end
# p! bits_x  # for testing
# p! bits_hex  # for testing


# write bit stream to disk:
begin
  file = File.open(FILE_BITS_X, "w") do |file|
    file.print bits_x
  end
  puts "Bit stream has been written to disk under name:  #{FILE_BITS_X}"
rescue ex
  puts "could not write to file: #{FILE_BITS_X} -- #{ex.message}"
end

# write byte stream to disk:
begin
  file = File.open(FILE_BITS_HEX, "w") do |file|
    file.print bits_hex
  end
  puts "Byte stream has been written to disk under name: #{FILE_BITS_HEX}"
rescue ex
  puts "could not write to file: #{FILE_BITS_HEX} -- #{ex.message}"
end



# make a password of n_char printable chars: user input requested here
n_char = 12
answer = false
while !answer
  n_char = 12
  print "\nPassword of #{n_char} printable chars OK? 'y' or another integer number >= 8: "
  # print for no terminal newline char

  answer_str = gets  # you get a union here: def gets(limit : Int, chomp = false) : String | Nil
                     # https://crystal-lang.org/api/1.16.3/IO.html#gets%28limit%3AInt%2Cchomp%3Dfalse%29%3AString%7CNil-instance-method
                     # for this union an extra cast into string: n_char_ = answer_str.to_s is needed below!!
  if answer_str == "y"
    answer = true
  else
    # p! answer_str  # for testing

    begin
      # n_char = answer_str.as(Int32)  # Error: can't cast (String | Nil) to Int32
      n_char_ = answer_str.to_s        # this line with a conversion from the union into a string is the trick here!
      if n_char_.not_nil!
        begin 
          n_char = n_char_.to_i
          if n_char < 8
            puts "enter an integer number >= 8 or 'y'"
          else
            answer = true
          end
        rescue
          puts "enter an integer number >= 8 or 'y'"
        end
      else 
        puts "enter an integer number >= 8 or 'y'"
      end
    rescue
      puts "enter an integer number >= 8 or 'y'"
    end
  end
end
# p! n_char  # for testing


with_special_chars = true
answer = false
while !answer
  print "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "

  answer_str = gets
  if answer_str == "y"
    answer = true
  else
    with_special_chars = false
    answer = true
  end
end
# puts "with_special_chars = #{with_special_chars}"  # for testing


char_set_ = IO::Memory.new
if with_special_chars
  i = 33
  while i < 127
    char_set_ << i.chr  # https://stackoverflow.com/questions/46289803/how-do-you-turn-an-array-of-codepoints-int32-to-a-string
    i += 1
  end
else
  char_set_ << "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
end
char_set = char_set_.to_s  # char_set has type IO::Memory
# puts "char_set = #{char_set}"  # for testing


i = 0  # char counter for the password
j = 0  # counter for x
pw_chars = IO::Memory.new

while i < n_char
  bin0 = x[j].to_s(2, precision: 16)
  # puts  # for testing
  # p! bin0  # for testing
  
  bin0_0 = bin0[0,8]
  bin0_1 = bin0[8,8]
  # p! bin0_0  # for testing
  # p! bin0_1  # for testing
  
  int0_0 = bin0_0.to_i(2)
  int0_1 = bin0_1.to_i(2)
  # p! int0_0  # for testing
  # p! int0_1  # for testing
  
  char0 = int0_0.chr
  char1 = int0_1.chr
  # p! char0  # for testing
  # p! char1  # for testing

  # bin0 # => "0101001001100110"
  # bin0_0 # => "01010010"
  # bin0_1 # => "01100110"
  # int0_0 # => 82
  # int0_1 # => 102
  # char0 # => 'R'
  # char1 # => 'f'
  
  if char_set.includes?(char0)
    pw_chars << char0
    i += 1
  end
  
  if char_set.includes?(char1) && i < n_char
    pw_chars << char1
    i += 1
  end
  
  j += 1
end

puts "\nYour password of #{n_char} characters is: #{pw_chars}\n"

# end of random_bitstring_and_flexible_password_generator.cr

