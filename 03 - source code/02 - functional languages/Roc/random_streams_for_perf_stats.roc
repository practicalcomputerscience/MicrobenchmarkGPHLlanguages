# random_streams_for_perf_stats.roc
#
# 2025-06-01
# 2025-12-21: see below
#
# build with: $ roc build random_streams_for_perf_stats.roc --optimize  # <<<<<<<<<<<<<<< OPTIMIZE!!
# run with:   $ sudo perf stat -r 20 ./random_streams_for_perf_stats
#
#
# $ roc --version
# roc nightly pre-release, built from commit d73ea109 on Tue 09 Sep 2025 09:02:08 AM UTC
# $
#


app [main!] {
              cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
              rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.5.0/yDUoWipuyNeJ-euaij4w_ozQCWtxCsywj68H0PlJAdE.tar.br",
            }

import cli.Stdout
import cli.Stderr
import cli.Utc      # https://github.com/imclerran/roc-isodate/blob/main/examples/now.roc
import rand.Random
import cli.File


main! = |_args|

  upper_limit = 62500u32   # 62500 for exactly 1M binary digits or 250k hex digits
  # END doesn't work here! --> use: end: also not good because keyword in a context => select: upper_limit

  # upper_limit = 100u32   # for testing: have 100 for longer passwords


  file_bits_x   = "random_bitstring_from_Roc.bin"
  file_bits_hex = "random_bitstring_from_Roc.byte"


  # make a random seed based on current time:
  now = Utc.now!({}) |> Utc.to_millis_since_epoch
  now2 = Num.to_frac(now) / 8.0  # https://www.roc-lang.org/builtins/Num#to_frac
  now3 = Num.round(now2)

  seed = Random.seed(now3)
  # seed = Random.seed(42)  # fixed seed for testing
  generator = Random.bounded_u32(1, m - 1)  # range is inclusive; 2025-12-21
  # https://github.com/kili-ilo/roc-random/blob/main/examples/simple.roc

  random_start = Random.step(seed, generator).value  # U32 return type


  _ = Stdout.line!("\ngenerating a random bit stream...")

  # make a list of random, integer numbers: also needed for the password
  s_int_32 = prng({y : random_start, limit : upper_limit, x : []})
  # print them as a check:
  # List.map(s_int_32, Num.to_str) |> Str.join_with("\n") |> Stdout.line!()?
  # _ = Stdout.line!("")  # just a new line


  # reduce 0b00000000000000001110011010001111 to 16 bits: 0b1110011010001111
  s_int_16 = List.map(s_int_32, (\num -> Num.to_u16(num)))
  # print them as a check:
  # List.map(s_int_16, Num.to_str) |> Str.join_with("\n") |> Stdout.line!()?
  # _ = Stdout.line!("")  # just a new line


  #######################################################
  #
  # bit stream generation
  #
  ini_mask = 0b1000_0000_0000_0000
  # map the list of random integer numbers onto the user defined function and
  # turn them into a common string of "1"'s and "0"'s using lambda syntax with '\':
  s_bin = List.map(s_int_16, (\num -> bin_str_generate({n : num, limit : 16, mask : ini_mask, x : ""})))

  # concatenate the List of strings:
  s_bin_total = s_bin |> Str.join_with("")


  #######################################################
  #
  # hex stream generation
  #
  #
  # taking this solution: https://www.geeksforgeeks.org/program-decimal-hexadecimal-conversion/
  # checking from here:   https://www.convzone.com/decimal-to-hex/
  #
  s_hex_raw = List.map(s_int_16, (\num -> hex_str_generate({n : num, x : ""})))

  # fix missing leading "0"'s:
  s_hex = List.map(s_hex_raw, (\hex_str -> fix_hex_zeros(hex_str)))

  # concatenate the List of strings:
  # s_hex_total = s_hex |> Str.join_with("\n")
  # print string as a check:
  # _ = Stdout.line!(s_hex_total)?

  s_hex_total_file = s_hex |> Str.join_with("")


  #######################################################
  #
  # writing streams to disk
  #
  fw_bin = File.write_utf8!(s_bin_total, file_bits_x)
  _ = when fw_bin is
          Ok({}) ->
              Stdout.line!("Byte stream has been written to disk under name: ${file_bits_x}")
          Err(_) ->
              Stderr.line!("could not write to file: ${file_bits_x} !")

  fw_hex = File.write_utf8!(s_hex_total_file, file_bits_hex)
  # fw_hex = Err(2)  # test
  _ = when fw_hex is
          Ok({}) ->
              Stdout.line!("Byte stream has been written to disk under name: ${file_bits_hex}")
          Err(_) ->
              Stderr.line!("could not write to file: ${file_bits_hex} !")

  Stdout.line!("Bye.")  # for doing a FINAL EXPRESSION



# ===================  more user definitions  ======================================

m = 65521u32  # = 2^16 - 15
a = 17364u32
c = 0u32

# user defined record for state of the random number generation:
State : {
  y : U32,        # prior number
  limit: U32,     # how many numbers still to generate
  x : List (U32)  # list of generated integer numbers: also needed for the password
}

# user defined, pure function: prng = pseudo random number generator
prng : State -> List (U32)
prng = |{y,limit,x}|            # |...| = function arguments
    if limit > 1 then
      list0 = List.append(x,y)
      z = (a * y + c) % m       # calculate next number
      prng({y : z, limit : limit-1, x : list0})  # recursion
    else
      List.append(x,y)


# user defined record for state of an integer number to its string presentation
# with of "1"'s and "0"'s conversion:
State2 : {
  n : U16,       # input integer number to be converted
  limit : U16,   # how many numbers still to generate
  mask :  U16,
  x : Str        # string of "1"'s and "0"'s
}

# user defined, pure function:
bin_str_generate : State2 -> Str
bin_str_generate = |{n, limit, mask, x}|
    masking = Num.bitwise_and(n, mask)
    new_mask = Num.shift_right_zf_by(mask, 1)

    if limit > 1 then
      if masking == 0 then
        list0 = Str.concat(x,"0")
        bin_str_generate({n, limit : limit-1, mask : new_mask, x : list0})  # recursion
      else
        list0 = Str.concat(x,"1")
        bin_str_generate({n, limit : limit-1, mask : new_mask, x : list0})  # recursion

    else
      if masking == 0 then
        Str.concat(x,"0")
      else
        Str.concat(x,"1")



# user defined record for state of an decimal integer number to its hexadecimal string presentation
State3 : {
  n : U16,       # input integer number to be converted
  x : Str        # string of [0..9, a-f] --> [48..57, 97..102]
}

# user defined, pure function:
hex_str_generate : State3 -> Str
hex_str_generate = |{n, x}|
                   if n >= 16 then
                       remainder = n % 16
                       if remainder < 10 then
                           new_char_nbr = Num.to_u8(remainder + 48)
                           new_char = Str.from_utf8_lossy([new_char_nbr])
                           list0 = Str.with_prefix(x, new_char)
                           new_int = n // 16
                           hex_str_generate({n : new_int, x : list0})
                       else
                           new_char_nbr = Num.to_u8(remainder + 87)
                           new_char = Str.from_utf8_lossy([new_char_nbr])
                           list0 = Str.with_prefix(x, new_char)
                           new_int = n // 16
                           hex_str_generate({n : new_int, x : list0})

                   else
                       if n < 10 then
                           new_char_nbr = Num.to_u8(n + 48)
                           new_char = Str.from_utf8_lossy([new_char_nbr])
                           Str.with_prefix(x, new_char)
                       else
                           new_char_nbr = Num.to_u8(n + 87)
                           new_char = Str.from_utf8_lossy([new_char_nbr])
                           Str.with_prefix(x, new_char)


# turning 18e into 018e for example --> always 4 bytes are needed:
fix_hex_zeros : Str -> Str
fix_hex_zeros = |hex_str|
                 # calculate length of hex_str:
                 len_hex_str = Str.count_utf8_bytes(hex_str)
                 when len_hex_str is
                     4 -> hex_str
                     3 -> Str.concat("0", hex_str)
                     2 -> Str.concat("00", hex_str)
                     1 -> Str.concat("000", hex_str)
                     _ -> "0000"


# end of random_streams_for_perf_stats.roc
