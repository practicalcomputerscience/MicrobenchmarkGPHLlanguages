# random_bitstring_and_flexible_password_generator.raku
#
# 2025-05-04, 2025-06-15/18, 2025-10-05
# 2025-12-19: see below
#
# test in Ubuntu 24 LTS:
#
# run in Ubuntu 24 LTS: $ raku random_bitstring_and_flexible_password_generator.raku
#
#
# $ time raku random_bitstring_and_flexible_password_generator.raku => real	0m5,806s!!!!!
#
# to-do:
#   - speed?
#   - writing to files: QC??
#   - memory allocation of string arrays @bits_x_str + @bits_hex_str??


my constant $END = 62501;  # 62501 for exactly 1M binary digits
# my constant $END = 50;  # for testing

# my constant $M1   = 1_000_000;
# my constant $K250 =   250_000;

my constant $m = 65521;  # = 2^16 - 15
my constant $a = 17364;
my constant $c = 0;

my constant $file_bits_x   = 'random_bitstring.bin';
my constant $file_bits_hex = 'random_bitstring.byte';


# 2025-06-18: this setup: real	0m5,854s
my $random_start = ($m - 1).rand.Int + 1;  # exclusive of m; 2025-12-19
my @x[$END];
@x[0] = ($random_start);  # also needed for the password later

my $bits_x_str = "";
my $bits_hex_str = "";


say 'generating a random bit stream...';

for 1 .. $END-1 -> $i {
  my $y = ($a * @x[$i-1] + $c) % $m;
  @x[$i] = $y;

  my $bits_x = $y.fmt('%016b');  # Bin: 0001011111001100
  $bits_x_str ~= $bits_x;

  my $bits_hex = $y.fmt('%04x');  # Hex
  $bits_hex_str ~= $bits_hex;
}

# say $bits_x_str;  # for testing
# say $bits_hex_str;  # for testing

# writing streams to files:
#   https://rakudocs.gitlab.io/language/io-guide
$file_bits_x.IO.spurt: $bits_x_str;
$file_bits_hex.IO.spurt: $bits_hex_str;



=begin comment

# make a password of N_CHAR printable chars: user input requested here
my $N_CHAR = 12;
my $answer = False;
my $answer_str = "";
while !$answer {
  $N_CHAR = 12;
  print "\nPassword of " ~ $N_CHAR ~ " printable chars OK? 'y' or another integer number >= 8: ";
  $answer_str = prompt();
  if $answer_str ~~ "y" {
    $answer = True;
  }
  else {
    try {
      $N_CHAR = $answer_str.Int.base(10);
      if $N_CHAR < 8 {
        say 'enter an integer number >= 8 or "y"';
      }
      else {
        $answer = True;
      }

      CATCH {
        default {
          say 'enter an integer number >= 8 or "y"';
        }
      }
    }
  }
}

my $WITH_SPECIAL_CHARS = True;
$answer = False;
$answer_str = "";
while !$answer {
  print "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ";
  $answer_str = prompt();
  if $answer_str ~~ "y" {
    $answer = True;
  }
  else {
    $WITH_SPECIAL_CHARS = False;
    $answer = True;
  }
}

my $char_set_rx;
if $WITH_SPECIAL_CHARS {
  $char_set_rx = /<[!..~]>/;
}
else {
  $char_set_rx = /<[0..9a..zA..Z]>/;
}


my $i = 0;  # char counter for the password
my $j = 0;  # char counter for x
my $pw_chars = "";

while $i < $N_CHAR {
  # convert an integer number into a string of '0' and '1' characters:
  my $bin0 = @x[$j].fmt('%016b');  # Bin: 0001011111001100

  my $bin0_0 = $bin0.substr(0, 8);   # end index is exclusive
  my $bin0_1 = $bin0.substr(8, 16);

  # convert a string of '0' and '1' characters into a character:
  my $char0 = "0b$bin0_0".Int.chr;
  my $char1 = "0b$bin0_1".Int.chr;

  if so $char0 ~~ /<$char_set_rx>/ {
    $pw_chars ~= $char0;
    $i++;
  }

  if so $char1 ~~ /<$char_set_rx>/ && $i < $N_CHAR {
    $pw_chars ~= $char1;
    $i++;
  }

  $j++;
}

print "\nYour password of " ~ $N_CHAR ~ " characters is: " ~ $pw_chars ~ "\n";

=end comment

# end of random_bitstring_and_flexible_password_generator.raku

