#=
random_bitstring_and_flexible_password_generator.jl

2026-01-01

run on Ubuntu 24 LTS: $ julia ./random_bitstring_and_flexible_password_generator.jl


$ julia --version
julia version 1.12.3
$

=#

using Random

END = 62501  # 62501 for exactly 1M binary digits; const END has no speed effect here
# END = 25  # for testing

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


# make a password of N_CHAR printable chars:
global N_CHAR = 12  # base case
global answer = false
while answer != true
  print("\nPassword of ", N_CHAR, " printable chars OK? 'y' or another integer number >= 8: ")
  reply = readline()
  # println("reply =", reply, "--")  # for testing

  if reply == "y"
      global answer = true
  else
      try
          global N_CHAR = parse(Int64, reply)
          if N_CHAR >= 8
              global answer = true
          else
              global N_CHAR = 12
              print("enter an integer number >= 8 or 'y'\n")
          end
      catch
          global N_CHAR = 12
          print("enter an integer number >= 8 or 'y'\n")
      end
  end
end
# println("N_CHAR = ", N_CHAR)  # for testing


global WITH_SPECIAL_CHARS = true
global answer = false
while answer != true
    print("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")
    reply = readline()
    if reply == "y"
        global answer = true
    else
        global WITH_SPECIAL_CHARS = false
        global answer = true
    end
end
# println("WITH_SPECIAL_CHARS = ", WITH_SPECIAL_CHARS)  # for testing


if WITH_SPECIAL_CHARS == true
    global pattern = r"[A-Za-z0-9!\"#$%&'()*+,-./:;<=>?@[\]\\^_`{|}~]+"
else
    global pattern = r"[A-Za-z0-9]+"
end
# println("pattern = ", pattern)  # for testing


global i = 1  # char counter in password
global j = 1  # char counter in bits_char
global pw_chars = []

while i <= N_CHAR
    bin0a = string(x[j], base=2)
    bin0  = lpad(bin0a, 16, "0")
    # println("\nbin0 = ", bin0)  # for testing

    bin0_0 = SubString(bin0, 1, 8)  # 8 is inclusive
    bin0_1 = SubString(bin0, 9, 16)
    # println("bin0_0 = ", bin0_0)  # for testing
    # println("bin0_1 = ", bin0_1)  # for testing

    char0 = string(Char(parse(Int, bin0_0, base=2)))
    char1 = string(Char(parse(Int, bin0_1, base=2)))
    # println("char0 = ", char0)  # for testing
    # println("char1 = ", char1)  # for testing

    is_match0 = occursin(pattern, char0)
    # println("is_match0 = ", is_match0)  # for testing
    if is_match0 == true
        push!(pw_chars, char0)
        global i += 1
        if i > N_CHAR
            break
        end
    end

    is_match1 = occursin(pattern, char1)
    # println("is_match1 = ", is_match1)  # for testing
    if is_match1 == true
        push!(pw_chars, char1)
        global i += 1
    end

    global j += 1
end

pw_string = join(pw_chars)
println("\nYour password of ", N_CHAR, " characters is: ", pw_string)


# end of random_bitstring_and_flexible_password_generator.jl
