# random_streams_for_perf_stats.pl
#
# 2025-06-01, 2025-07-21: better error handling when writing to files
#
# run on Ubuntu 24 LTS: $ sudo perf stat -r 20 perl random_streams_for_perf_stats.pl
#
# $ perl --version
# This is perl 5, version 38, subversion 2 (v5.38.2) built for x86_64-linux-gnu-thread-multi
# $


use warnings;
use strict;
use IO::Handle;  # for flushing stdout


my constant $END = 62501;  # 62501 for exactly 1M binary digits
# my constant $END = 100;  # for testing

# my constant $M1   = 1_000_000;
# my constant $K250 =   250_000;

my constant $m = 65521;  # = 2^16 - 15
my constant $a = 17364;
my constant $c = 0;

my constant $file_bits_x   = 'random_bitstring.bin';
my constant $file_bits_hex = 'random_bitstring.byte';


my $random_start = int(rand($m));  # exclusive of m

my @x = ($random_start);  # also needed for the password later
my @bits_x_str;    # array of strings for the bit stream:  "0"'s + "1"'s
my @bytes_x_str;   # array of strings for the byte stream: [0..9][a..f]'s


print('generating a random bit stream...');
STDOUT->flush();

my $i = 1;
while ($i < $END) {
  my $y = ($a * $x[$i-1] + $c) % $m;
  push @x, $y;

  my $bits_x = sprintf('%016b', $y);  # Bin: 0001011111001100
  push @bits_x_str, $bits_x;

  my $bytes_x = sprintf('%04x', $y);  # Hex
  push @bytes_x_str, $bytes_x;

  $i++;
}

my $bits_x_str_total  = join('', @bits_x_str);
my $bytes_x_str_total = join('', @bytes_x_str);

# write bit stream to disk:
my $FH1_flag = 1;  # True = 1
unless(open(FH, '>', $file_bits_x)) {
  warn "\ncan't open file: $file_bits_x -- $!";
  $FH1_flag = 0;
}
unless(print FH $bits_x_str_total) {
  warn "could not write to file: $file_bits_x";
  $FH1_flag = 0;
}
unless(close(FH)) {
  warn "could not close file: $file_bits_x";
  $FH1_flag = 0;
}
STDOUT->flush();
if ($FH1_flag == 1) {
  print "\nBit stream has been written to disk under name:  $file_bits_x";
}

# write byte stream to disk:
my $FH2_flag = 1;  # True = 1
unless(open(FH, '>', $file_bits_hex)) {
  warn "\ncan't open file: $file_bits_hex -- $!";
  $FH2_flag = 0;
}
unless(print FH $bytes_x_str_total) {
  warn "could not write to file: $file_bits_hex";
  $FH2_flag = 0;
}
unless(close(FH)) {
  warn "could not close file: $file_bits_hex";
  $FH2_flag = 0;
}
STDOUT->flush();
if ($FH2_flag == 1) {
  print "\nByte stream has been written to disk under name: $file_bits_hex\n";
}

# end of random_streams_for_perf_stats.pl
