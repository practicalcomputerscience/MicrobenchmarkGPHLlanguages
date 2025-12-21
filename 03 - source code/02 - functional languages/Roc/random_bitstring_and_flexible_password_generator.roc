# random_bitstring_and_flexible_password_generator.roc
#
# 2025-04-17/18/27/../30/, 2025-05-01/03/04/05/23, 2025-06-01
# 2025-12-21: see below
#
# check with: $ roc check random_bitstring_and_flexible_password_generator.roc
# build with: $ roc build random_bitstring_and_flexible_password_generator.roc --optimize  # <<<<<<<<<<<<<<< OPTIMIZE!!
# run with:   $ roc random_bitstring_and_flexible_password_generator.roc
# or:         $ ./random_bitstring_and_flexible_password_generator
#
#
# remark on U16: this type doesn't work here; program will crash: "Roc crashed with: Integer addition overflowed! ..."
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
import cli.Stdin
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

  # convert all 16 bit numbers into two 8 bit numbers for the password Str to be built:
  s_int_8 = nbr16_into_nbr8(s_int_16)
  # print them as a check:
  # List.map(s_int_8, Num.to_str) |> Str.join_with("\n") |> Stdout.line!()?


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



  #######################################################
  #
  # make a password of n_char printable chars: user input requested here
  #
  _ = Stdout.write!("\nPassword of 12 printable chars OK? 'y' or another integer number >= 8: ")?

  n_char = input_a_valid_number!(12)  # 12 printable chars is the default pw length
  # n_char_str = Num.to_str(n_char)
  # _ = Stdout.line!("Your choice is: ${n_char_str}")

  _ = Stdout.write!("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ")?
  with_special_chars : Str
  with_special_chars = answer_yes_or_no!({})
  # _ = Stdout.line!("Your choice is: ${with_special_chars}")

  # n_char = 12  # fxied for testing


  # s_int_8 <-- lots of 8 bit numbers => just have a shorter length
  # https://www.ascii-code.com/ --> dec [32...126] is printable => ~37% is printable
  #                             --> dec [48..57], [65..90], [97..122] is alphanumerical 0..9, A-Z, a-z
  s_int_8a = List.sublist(s_int_8, {start: 0, len: Num.to_u64(n_char * 10)})  # 10 is just a safe statistical guess factor

  s_int_8b =
      if with_special_chars == "y" then
          # get only the printable chars out of s_int_8a:
          printable_chars(s_int_8a)  # List (U8) as return type
      else
          # get only the alphanumerical chars out of s_int_8a:
          alphanum_printable_chars(s_int_8a)  # List (U8) as return type

  # shorten s_int_8b to the right length:
  s_int_8c = List.sublist(s_int_8b, {start: 0, len: Num.to_u64(n_char)})
  # turn numbers into chars:
  pw_chars = Str.from_utf8_lossy(s_int_8c)
  n_char_actual = Str.count_utf8_bytes(pw_chars)
  n_char_actual_str = Num.to_str(n_char_actual)


  _ = Stdout.line!("\nYour password of ${n_char_actual_str} characters is: ${pw_chars}\n")


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


# convert all 16 bit numbers into two 8 bit numbers for the password Str to be built:
# user defined, pure function:
nbr16_into_nbr8 : List(U16) -> List (U8)
nbr16_into_nbr8 = |in_16|
                  List.walk(in_16, [], |nums, int16|
                       high_nbr   = Num.shift_right_zf_by(int16, 8)
                       high_nbr_8 = Num.to_u8(high_nbr)

                       low_nbr    = Num.shift_left_by(int16, 8)
                       low_nbr2   = Num.shift_right_zf_by(low_nbr, 8)
                       low_nbr_8  = Num.to_u8(low_nbr2)

                       List.append(nums, high_nbr_8) |> List.append(low_nbr_8)
                      )


printable_chars : List (U8) -> List (U8)
printable_chars = |list_u8|
                  List.walk(list_u8, [], |nums, nbr_u8|
                              # https://www.ascii-code.com/ --> dec [33...126] is printable => no space char (like in scala)
                              if nbr_u8 >= 33 && nbr_u8 <= 126 then
                                List.append(nums, nbr_u8)
                              else
                                nums
                           )

# only alphanumerical chars 0..9, A-Z, a-z = dec [48..57], [65..90], [97..122]
alphanum_printable_chars : List (U8) -> List (U8)
alphanum_printable_chars = |list_u8|
                      List.walk(list_u8, [], |nums, nbr_u8|
                                 if nbr_u8 >= 48 && nbr_u8 <= 57 ||
                                    nbr_u8 >= 65 && nbr_u8 <= 90 ||
                                    nbr_u8 >= 97 && nbr_u8 <= 122 then
                                    List.append(nums, nbr_u8)
                                 else
                                    nums
                               )


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


input_a_valid_number! : U32 => U32  # defining an effectful function
input_a_valid_number! = \nbr ->
    input = Stdin.line!({})
    when input is
        Ok(num) ->
            when Str.to_u32(num) is
                Ok(number) ->
                    if number >= 8 then
                        number
                    else
                        _ = Stdout.write!("enter an integer number >= 8 or 'y': ")
                        input_a_valid_number!(12)

                Err(_) ->
                    if num == "y" then
                        nbr
                    else
                        _ = Stdout.write!("enter an integer number >= 8 or 'y': ")
                        input_a_valid_number!(12)

        Err(_) ->
            _ = Stdout.write!("enter an integer number >= 8 or 'y': ")
            input_a_valid_number!(12)


answer_yes_or_no! : {} => Str  # defining an effectful function
answer_yes_or_no! = \{} ->
    input = Stdin.line!({})
    when input is
      Ok("y") -> "y"
      Ok("n") -> "n"
      _ ->  "n"


# end of random_bitstring_and_flexible_password_generator.roc
