#!/usr/bin/env zsh
#
# random_streams_for_perf_stats.zsh
#
# 2026-10-24
#
# check the script syntax in Ubuntu 24 LTS: $ zsh -n ./random_streams_for_perf_stats.zsh
#
# run in Ubuntu 24 LTS:                     $ zsh ./random_streams_for_perf_stats.zsh
#                                           $ time zsh ./random_streams_for_perf_stats.zsh => real	0m32.026s!!!
#
#
# $ zsh --version
# zsh 5.9 (x86_64-ubuntu-linux-gnu)
#
#
# partly transpiled from random_streams_for_perf_stats.awk with Duck.ai.
# random_streams_for_perf_stats.tcl may have been the better choice.


# /////////////////////  user defined functions  ////////////////////////////
#

integer_to_bin_string() {
  local n=$1 s="" r bit
  local v=$n
  for (( r=15; r>=0; r-- )); do
    bit=$(( v / (1 << r) ))
    (( bit >= 2 )) && bit=$(( bit % 2 ))
    s+=$(( bit ? 1 : 0 ))
    v=$(( v % (1 << r) ))
  done
  print "$s"
}

#
# //////////////////  end of user defined functions  ////////////////////////


upper_limit=62501  # 62501 for exactly 1M binary digits
# upper_limit=10  # for testing

m=65521  # = 2^16 - 15
a=17364
c=0

file_bits_x="random_bitstring.bin"
file_bits_hex="random_bitstring.byte"

x=()  # define an array

# initialize RNG and first value
x+=$((RANDOM * ($m - 2) / 32767 + 1))  # $RANDOM expands to a random integer in the range [0, 32767]
# echo "x[1] = ${x[1]}"  # for testing

bits_x=""
bits_hex=""

print "\ngenerating a random bit stream..."
for (( i=2; i<=upper_limit; i++ )); do
  prev=${x[i-1]}
  current=$(((a * $prev + c) % m ))
  x+=$current

  bits_x_str=$(integer_to_bin_string "$current")
  bits_x+="$bits_x_str"

  printf -v bits_hex_str '%04x' "$current"
  bits_hex+="$bits_hex_str"
done


# echo "bits_x:   $bits_x"  # for testing
# echo "bits_hex: $bits_hex"  # for testing


# write bit stream to disk:
# 'print -r' preserves backslashes natively in Zsh without unexpected escapes
if print -rn -- "$bits_x" > "$file_bits_x"; then  # the -n flag suppresses the trailing newline
  echo "Bit stream has been written to disk under name:  $file_bits_x"
else
  echo "could not write to file: $file_bits_x" >&2
fi

# write byte stream to disk:
if print -rn -- "$bits_hex" > "$file_bits_hex"; then
  echo "Byte stream has been written to disk under name: $file_bits_hex"
else
  echo "could not write to file: $file_bits_hex" >&2
fi


# end of random_streams_for_perf_stats.zsh
