=begin
main.rb, which is actually random_streams_for_perf_stats_mruby.rb

2026-01-19

build on Ubuntu 24 LTS: $ cd ./mruby  # this is the project dir with main.rb
                        $ mruby ./main.rb  # test for correct interpretation
                        rest see at file: entry.c


$ mruby -v
mruby 3.4.0 (2025-04-20)
$

manually adapted from random_streams_for_perf_stats.rb (for normal CRuby)

=end


# frozen_string_literal: true

# require 'stringio'
# require 'securerandom'

# Constants
UPPER_LIMIT = 62_501  # 62501 for exactly 1M binary digits
# UPPER_LIMIT = 10  # for testing

m = 65_521    # 2^16 - 15
a = 17_364
c = 0

file_bits_x   = 'random_bitstring.bin'
file_bits_hex = 'random_bitstring.byte'

# Initialize array
x = Array.new(UPPER_LIMIT, 0)

# Random initial value in range [1, m-1]
# see from mruby-random:
# x[0] = SecureRandom.random_number(m - 1) + 1
x[0] = rand(1 .. m)  # range = [1..m[
# p x[0]  # for testing; p for short version of puts

# https://github.com/ksss/mruby-stringio
# add: conf.gem :mgem => 'mruby-stringio'
#   to config file: ./build_config/default.rb
#   and compile mruby again: $ rake
# look in dir ./mrbgems
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

# puts "bits_x   = #{bits_x.string}"  # for testing
# puts "bits_hex = #{bits_hex.string}"  # for testing

# Write binary bit stream to file
# https://github.com/ksss/mruby-io
begin
  # File.open(file_bits_x, 'w', encoding: 'UTF-8') do |f|  # => Hash cannot be converted to Integer (TypeError)
  File.open(file_bits_x, 'w') do |f|
    f.write(bits_x.string)
  end
  puts "Bit stream has been written to disk under name:  #{file_bits_x}"
rescue StandardError => e
  puts "could not write to file: #{file_bits_x} ! -- #{e}"
end

# Write hex byte stream to file
begin
  # File.open(file_bits_hex, 'w', encoding: 'UTF-8') do |f|
  File.open(file_bits_hex, 'w') do |f|
    f.write(bits_hex.string)
  end
  puts "Byte stream has been written to disk under name: #{file_bits_hex}"
rescue StandardError => e
  puts "could not write to file: #{file_bits_hex} ! -- #{e}"
end

# end of main.rb
