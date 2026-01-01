#=
random_streams_for_perf_stats.jl

2026-01-01

run on Ubuntu 24 LTS: $ julia ./random_streams_for_perf_stats.jl

                      exe times measurements:
                      $ time julia -O1 ./random_streams_for_perf_stats.jl  => real	0m0.189s

                      $ multitime -n 20 julia -O1 ./random_streams_for_perf_stats.jl
                                  Mean        Std.Dev.    Min         Median      Max
                      real        0.191       0.002       0.186       0.191       0.195

                      $ multitime -n 20 julia ./random_streams_for_perf_stats.jl
                                  Mean        Std.Dev.    Min         Median      Max
                      real        0.202       0.002       0.196       0.202       0.206

                      $ multitime -n 20 julia -O0 ./random_streams_for_perf_stats.jl
                                  Mean        Std.Dev.    Min         Median      Max
                      real        0.168       0.004       0.164       0.167       0.179 <<<<<<<<<<<<<<<<<<<<<<


$ julia --version
julia version 1.12.3
$

=#

using Random

END = 62501  # 62501 for exactly 1M binary digits; const END has no speed effect here
# END = 11  # for testing

m = 65521  # = 2^16 - 15
a = 17364
c = 0

file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"


x = Array{UInt32}(undef, END)
x[1] = rand(1:m-1)  # indexing in Julia is 1-based; m is inclusive
# println("x[1] = ", x[1])  # for testing

# create large arrays of strings:
bits_x   = fill("000000000000", END-1)
bits_hex = fill("0000", END-1)


println("\ngenerating a random bit stream...")
for i in 2:END
    h = i - 1
    # println("h = $h")  # for testing
    x[i] = (a*x[h] + c) % m
    # println("x[i] = ", x[i])  # for testing

    bits_x_str0 = string(x[i], base=2)
    bits_x_str  = lpad(bits_x_str0, 16, "0")  # padding: https://www.jlhub.com/julia/manual/en/function/lpad
    # println("  bits_x_str =  ", bits_x_str)  # for testing
    bits_x[h]   = bits_x_str

    bits_hex_str0 = string(x[i], base=16)
    bits_hex_str  = lpad(bits_hex_str0, 4, "0")
    # println("  bits_hex_str =  ", bits_hex_str)  # for testing
    bits_hex[h]   = bits_hex_str
end


bits_x_str_total   = join(bits_x)
# println("\nbits_x_str_total = ", bits_x_str_total)  # for testing
bits_hex_str_total = join(bits_hex)
# println("\nbits_hex_str_total = ", bits_hex_str_total)  # for testing


# try to write the string to the file with error handling: Duck.ai
try
    # Open the file for writing
    open(file_bits_x, "w") do file
        write(file, bits_x_str_total)
    end
    println("Bit stream has been written to disk under name: ", file_bits_x)
catch e
    println("could not write to file: ", file_bits_x, " -- ", e)
end

try
    # Open the file for writing
    open(file_bits_hex, "w") do file
        write(file, bits_hex_str_total)
    end
    println("Byte stream has been written to disk under name: ", file_bits_hex)
catch e
    println("could not write to file: ", file_bits_hex, " -- ", e)
end


# end of random_streams_for_perf_stats.jl
