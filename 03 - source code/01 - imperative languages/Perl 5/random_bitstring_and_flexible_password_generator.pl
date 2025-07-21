# random_bitstring_and_flexible_password_generator.pl
#
# 2025-05-05/31, 2025-06-06/18
# 2025-07-21: better error handling when writing to files
#
# run on Ubuntu 24 LTS: $ perl random_bitstring_and_flexible_password_generator.pl
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
  print "\nByte stream has been written to disk under name: $file_bits_hex";
}
STDOUT->flush();



# make a password of N_CHAR printable chars: user input requested here
my $N_CHAR = 12;
my $answer = 0;  # 0 = False
my $answer_str = "";
while (!$answer) {
  $N_CHAR = 12;
  print "\n\nPassword of $N_CHAR printable chars OK? 'y' or another integer number >= 8: ";
  $answer_str = <STDIN>;
  chomp $answer_str;
  if ($answer_str =~ "y") {
    $answer = 1;
  } else {
    if ($answer_str =~ /^\d+\z/) {  # catch inpute like 9.9
      $N_CHAR = int($answer_str);
      if ($N_CHAR < 8) {
        print 'enter an integer number >= 8 or "y"';
      } else {
        $answer = 1;
      }
    } else {
      print 'enter an integer number >= 8 or "y"';
    }
  }
}

my $WITH_SPECIAL_CHARS = 1;  # True = 1
$answer = 0;
$answer_str = "";
while (!$answer) {
  print "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ";
  $answer_str = <STDIN>;
  chomp $answer_str;

  if ($answer_str =~ m/^y$/) {  # exact string match
    $answer = 1;  # True = 1
  } else {
    $WITH_SPECIAL_CHARS = 0;
    $answer = 1;
  }
}


$i = 0;  # char counter for the password
my $j = 0;  # char counter for x
my $pw_chars = "";

while ($i < $N_CHAR) {
  # convert an integer number into a string of '0' and '1' characters:
  my $bin0 = sprintf('%016b', $x[$j]);  # Bin: 0001011111001100

  my $bin0_0 = substr $bin0, 0, 8;   # end index is exclusive
  my $bin0_1 = substr $bin0, 8, 16;

  # convert a string of '0' and '1' characters into a character:
  my $char0 = chr(oct("0b" . $bin0_0));
  my $char1 = chr(oct("0b" . $bin0_1));

  if (!$WITH_SPECIAL_CHARS) {
    if ($char0 =~ /[A-Za-z0-9]/) {
      $pw_chars .= $char0;
      $i++;
    }

    if ($char1 =~ /[A-Za-z0-9]/ && $i < $N_CHAR) {
      $pw_chars .= $char1;
      $i++;
    }
  } else {
    if ($char0 =~ /[!-~]/) {
      $pw_chars .= $char0;
      $i++;
    }

    if ($char1 =~ /[!-~]/ && $i < $N_CHAR) {
      $pw_chars .= $char1;
      $i++;
    }
  }

  $j++;
}

print "\nYour password of $N_CHAR characters is: $pw_chars\n";


# end of random_bitstring_and_flexible_password_generator.pl
