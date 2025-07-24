# random_streams_for_perf_stats_cr.cr
#
# 2025-06-02
#
# build on Ubuntu 24 LTS: $ crystal random_streams_for_perf_stats_cr.cr --release
#
# run on Ubuntu 24 LTS:   $ sudo perf stat -r 20 ./random_streams_for_perf_stats_cr
#


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
x << Random.rand(M)
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

# end of random_streams_for_perf_stats_cr.cr
