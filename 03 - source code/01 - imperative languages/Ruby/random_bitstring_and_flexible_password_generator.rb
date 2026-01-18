=begin
random_bitstring_and_flexible_password_generator.rb

2026-01-18

run on Ubuntu 24 LTS: $ ruby ./random_bitstring_and_flexible_password_generator.rb


$ ruby --version
ruby 3.2.3 (2024-01-18 revision 52bb2ac0a6) [x86_64-linux-gnu]
$

translated from random_bitstring_and_flexible_password_generator.py with Big AI;
only cosmetic fine-tuning was needed

=end


# frozen_string_literal: true

require 'stringio'
require 'securerandom'

# Constants
UPPER_LIMIT = 62_501  # 62501 for exactly 1M binary digits
# UPPER_LIMIT = 50  # for testing

m = 65_521    # 2^16 - 15
a = 17_364
c = 0

file_bits_x   = 'random_bitstring.bin'
file_bits_hex = 'random_bitstring.byte'

# Initialize array
x = Array.new(UPPER_LIMIT, 0)

# Random initial value in range [1, m-1]
x[0] = SecureRandom.random_number(m - 1) + 1

# StringIO buffers
bits_x   = StringIO.new
bits_hex = StringIO.new

puts "\ngenerating a random bit stream..."

(1...UPPER_LIMIT).each do |i|
  x[i] = (a * x[i - 1] + c) % m

  # 16-bit binary string
  bits_x_str = format('%016b', x[i])
  bits_x.write(bits_x_str)

  # 4-digit hex string
  bits_hex_str = format('%04x', x[i])
  bits_hex.write(bits_hex_str)
end


# Write binary bit stream to file
begin
  File.open(file_bits_x, 'w', encoding: 'UTF-8') do |f|
    f.write(bits_x.string)
  end
  puts "Bit stream has been written to disk under name:  #{file_bits_x}"
rescue StandardError => e
  puts "could not write to file: #{file_bits_x} ! -- #{e}"
end

# Write hex byte stream to file
begin
  File.open(file_bits_hex, 'w', encoding: 'UTF-8') do |f|
    f.write(bits_hex.string)
  end
  puts "Byte stream has been written to disk under name: #{file_bits_hex}"
rescue StandardError => e
  puts "could not write to file: #{file_bits_hex} ! -- #{e}"
end


# make a password of N_CHAR printable chars:
N_CHAR = [12] # base case; in Ruby, integers are immutable! But array elements can be modified later without a warning.
answer = false

while answer == false
  print "\nPassword of #{N_CHAR} printable chars OK? \"y\" or another integer number >= 8: "
  reply = gets.chomp.downcase

  if reply == 'y'
    answer = true
  else
    begin
      temp_n = Integer(reply)
      if temp_n >= 8
        N_CHAR[0] = temp_n
        answer = true
      else
        puts 'enter an integer number >= 8 or "y"'
      end
    rescue ArgumentError
      puts 'enter an integer number >= 8 or "y"'
    end
  end
end
# print "\nN_CHAR[0] = #{N_CHAR[0]}\n"  # for testing

def binary_to_string(bits_array)
  # Ruby equivalent of ''.join([chr(int(i, 2)) for i in bits])
  bits_array.map { |b| b.to_i(2).chr }.join
end

# WITH_SPECIAL_CHARS = true: because of Ruby's immutability, a slightly different approach is followed:
answer = false
while answer == false
  print "\nDo you want me to use special characters like .;,+*... ? \"y\" or \"n\": "
  reply = gets.chomp.downcase
  if reply == 'y'
    WITH_SPECIAL_CHARS = true
    answer = true
  else
    WITH_SPECIAL_CHARS = false
    answer = true
  end
end
# print "\nWITH_SPECIAL_CHARS = #{WITH_SPECIAL_CHARS}"  # for testing


if WITH_SPECIAL_CHARS == true
  # Ruby Regexp literal
  pattern = /\A[A-Za-z0-9!"#$%&'()*+,\-.\/:;<=>?@\[\\\]^_`{|}~]+\z/
else
  pattern = /\A[A-Za-z0-9]+\z/
end
# print "\npattern = #{pattern}"  # for testing


i = 0 # char counter in password
j = 0 # counter in x[j]
pw_chars = []

while i < N_CHAR[0]
  # Convert to 16-bit binary string with leading zeros
  bin0 = format('%016b', x[j])

  # Ruby slicing: [start, length]
  bin0_0 = bin0[0, 8]
  bin0_1 = bin0[8, 8]

  char0 = binary_to_string([bin0_0])
  char1 = binary_to_string([bin0_1])

  # Pattern matching in Ruby using =~ or .match?
  if char0 =~ pattern
    pw_chars << char0
    i += 1
    break if i == N_CHAR[0]
  end

  if char1 =~ pattern
    pw_chars << char1
    i += 1
  end

  j += 1
end

pw_string = pw_chars.join
puts "\nYour password of #{N_CHAR[0]} characters is: #{pw_string}"

# end of random_bitstring_and_flexible_password_generator.rb
