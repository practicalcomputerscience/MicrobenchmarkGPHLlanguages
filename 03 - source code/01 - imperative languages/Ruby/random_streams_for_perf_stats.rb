=begin
random_streams_for_perf_stats.rb

2026-01-18, 2026-02-10

run on Ubuntu 24 LTS: $ ruby ./random_streams_for_perf_stats.rb        => real	0m0.076s
                      $ ruby --mjit ./random_streams_for_perf_stats.rb => real	0m0.342s (2026-02-10)
                      $ ruby --jit ./random_streams_for_perf_stats.rb  => real	0m0.345s (2026-02-10)
                      $ ruby --yjit ./random_streams_for_perf_stats.rb => ruby: warning: Ruby was built without YJIT support. You may need to install rustc to build Ruby with YJIT.
                        => behaves here like with no switch: real	0m0.076s


                      $ multitime -n 20 ruby ./random_streams_for_perf_stats.rb


$ ruby --version
ruby 3.2.3 (2024-01-18 revision 52bb2ac0a6) [x86_64-linux-gnu]
$

translated from random_streams_for_perf_stats.py with Big AI;
only cosmetic fine-tuning was needed

=end


# frozen_string_literal: true

require 'stringio'
require 'securerandom'

# Constants
UPPER_LIMIT = 62_501  # 62501 for exactly 1M binary digits

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

# end of random_streams_for_perf_stats.rb