|| random_streams_for_perf_stats.m
||
|| 2026-07-30
||
|| run on Ubuntu 24 LTS: $ echo $RANDOM | mira -heap 10000000 -exec ./random_streams_for_perf_stats.m
||                       $ time echo $RANDOM | mira -heap 10000000 -exec ./random_streams_for_perf_stats.m => real	0m2.000s
||
||                       $RANDOM in Bash generates a pseudo-random integer between 0 and 32767 for each use
||                       (not suitable for security purposes)
||
||
|| $ mira -version
|| 2.066 last revised 31 January 2020
|| $
||
||
|| built manually from scratch with big help from Google AI


|| *********************************************************
|| user defined functions

integer_to_bin_string :: num -> [char]  || return a string [char]
integer_to_bin_string n =
  toBinary 16 n ""
  where
    || Case 1: Force stop exactly at 16 characters
    toBinary 0 any_seed acc = acc

    || Case 2: If the number is fully converted, pad left with '0'
    toBinary count 0 acc = toBinary (count - 1) 0 ('0' : acc)

    || Case 3: Calculate the bit and continue processing
    toBinary count k acc =
      toBinary (count - 1) nextK (bit : acc)
      where
        bit   = '0', if k mod 2 == 0
              = '1', otherwise
        nextK = k div 2


integer_to_hex_string :: num -> [char]
integer_to_hex_string n =
  toHex 4 n ""
  where
    toHex 0 any_seed acc = acc

    toHex count 0 acc = toHex (count - 1) 0 ('0' : acc)

    toHex count k acc =
      toHex (count - 1) nextK (char : acc)
      where
        remainder  = k mod 16
        nextK      = k div 16
        char = "0123456789abcdef" ! remainder


masterloop :: num -> num -> ([num], [[char]], [[char]])  || return a tuple
masterloop n seed =
  masterloop' [] [] [] n seed  || masterloop' is just the name of this helper function
  where
    masterloop' sofar1 sofar2 sofar3 n current_seed
      || recursion with "guard" (if n > 0)
      || use cons : operator for efficiency:
      = masterloop' (current_seed : sofar1) (bits_x_str : sofar2) (bits_hex_str : sofar3) (n-1) new_seed, if n > 0

      || Reverse both accumulated lists when recursion finishes:
      = (reverse sofar1, reverse sofar2, reverse sofar3), otherwise
        where
          new_seed     = (a * current_seed + c) mod m
          bits_x_str   = integer_to_bin_string new_seed
          bits_hex_str = integer_to_hex_string new_seed


|| Defensive write function implementing the logic of the Haskell catch
write_to_file :: [char] -> [char] -> [char] -> [sys_message]
write_to_file filename content fileType
  = [ Stderr ("\ncould not write to file: " ++ filename) ], if ~writable
  = [ Tofile filename content, Stdout success_msg ],        otherwise
    where
      || filemode returns string of permissions e.g. "rw".
      || We look for 'w'. If the file doesn't exist, we must check if the directory allows creation.
      || A standard check for an existing file or a valid new path in Miranda:
      writable = member (filemode filename) 'w' \/ filemode filename == ""  || \/ is logical or

      success_msg
        = "\nBit stream has been written to disk under name:  " ++ filename, if fileType == "bit"
        = "\nByte stream has been written to disk under name: " ++ filename, otherwise

|| end of user defined functions
|| *********************************************************


|| Constants
upper_limit = 62500  || 62500 for exactly 1M binary digits
|| upper_limit = 10  || for testing

m = 65521  || = 2^16 - 15
a = 17364
c = 0

file_bits_x   = "random_bitstring.bin"
file_bits_hex = "random_bitstring.byte"


main :: [sys_message]
main =
  [Stdout "\ngenerating a random bit stream..."]
  ++ write_to_file file_bits_x   bits_x_str_total   "bit"
  ++ write_to_file file_bits_hex bits_hex_str_total "byte"
  ++ [Stdout "\n"]
  where
    os_seed = numval (hd (lines $-)) + 1
    || + 1 to not have a 0 seed!
    || $- = standard symbol for the list of characters typed at the keyboard
    || lines breaks $- into lines, hd takes first line, numval converts it into a number

    (x, bits_x, bits_hex) = masterloop upper_limit os_seed

    bits_x_str_total   = concat bits_x
    bits_hex_str_total = concat bits_hex

|| end of random_streams_for_perf_stats.m
