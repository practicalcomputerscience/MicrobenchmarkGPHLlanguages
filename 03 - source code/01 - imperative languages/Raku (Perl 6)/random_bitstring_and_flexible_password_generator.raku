# random_bitstring_and_flexible_password_generator.raku
#
# 2025-05-04, 2025-06-15/18, 2025-10-05
# 2025-12-19: see below
# 2026-01-24: fix this source code like the cleanup done at Perl 5 program on 2026-01-24
# 2026-05-31: rename char_set_rx to pattern; this solution is already regular expression based!
# 2026-05-31: an important fix of user dialog for N_CHAR printable chars
# 2026-06-18: refactored to a one-line at setting $pattern
#
#
# run in Ubuntu 24 LTS: $ raku random_bitstring_and_flexible_password_generator.raku
#
#
# $ raku --version
# Welcome to Rakudo™ v2026.04.
# Implementing the Raku® Programming Language v6.d.
# Built on MoarVM version 2026.04.
# $
#
#
# to-do:
#   - exe speed:


my constant $END = 62501;  # 62501 for exactly 1M binary digits
# my constant $END = 10;  # for testing
# my $M1   = $END * 16 - 16;
# my $K250 = $END * 4 - 4;

my constant $m = 65521;  # = 2^16 - 15
my constant $a = 17364;
my constant $c = 0;

my constant $file_bits_x   = 'random_bitstring.bin';
my constant $file_bits_hex = 'random_bitstring.byte';


# 2026-05-31: new user defined function:
#  similar name like in Standard ML ("isAllDigits"):
sub is_all_digits(Str $str) {
    return $str ~~ /^\d+$/;
}


my $random_start = ($m - 1).rand.Int + 1;  # exclusive of m; 2025-12-19
my @x[$END];
@x[0] = ($random_start);  # also needed for the password later

# my $bits_x = "";  # + $bits_x ~= $bits_x_str; => real	0m6.290s (2026-01-24)
# my $bits_hex = "";

# my str @bits_x;  # + @bits_x.push: $bits_x_str; => real	0m6.309s (2026-01-24)
# my str @bits_hex;
my @bits_x;  # + @bits_x.push: $bits_x_str; => real	0m5.810s (2026-01-24)
my @bits_hex;

# my @bits_x[$END - 1];  # => real	0m6.279s (2026-01-24)
# my @bits_hex[$END - 1];

# my @bits_x[$END-1]   of str;   # array of strings for the bit stream:  "0"'s + "1"'s => real	0m6.150s
# my @bits_hex[$END-1] of str;   # array of strings for the byte stream: [0..9][a..f]'s

# my @bits_x[$M1];
# + @bits_x[$byte_nbr ..^ $byte_nbr + 16] = $bits_x_str.comb; => real	0m8.463s (2026-01-24)
# my @bits_hex[$K250];

# my @bits_x;
# + @bits_x[$byte_nbr ..^ $byte_nbr + 16] = $bits_x_str.comb; => real	0m7.542s (2026-01-24)
# my @bits_hex;


say "\ngenerating a random bit stream...";

for 1 .. $END-1 -> $i {
  # my $h = $i - 1;

  my $y = ($a * @x[$i-1] + $c) % $m;
  @x[$i] = $y;

  my $bits_x_str = $y.fmt('%016b');  # Bin: 0001011111001100
  # $bits_x ~= $bits_x_str;
  @bits_x.push: $bits_x_str;
  # @bits_x[$h] = $bits_x_str;
  # my $byte_nbr = ($i - 1) * 16;
  # @bits_x[$byte_nbr ..^ $byte_nbr + 16] = $bits_x_str.comb;  # comb splits the string into a char sequence
  # @bits_x[$i-1] = $bits_x_str;


  my $bits_hex_str = $y.fmt('%04x');  # Hex
  # $bits_hex ~= $bits_hex_str;
  @bits_hex.push: $bits_hex_str;
  # @bits_hex[$h] = $bits_hex_str;
  # $byte_nbr = ($i - 1) * 4;
  # @bits_hex[$byte_nbr ..^ $byte_nbr + 4] = $bits_hex_str.comb;
  # @bits_hex[$i-1] = $bits_hex_str;
}

my $bits_x_str_total   = @bits_x.join;
my $bits_hex_str_total = @bits_hex.join;
# say $bits_x_str_total;  # for testing
# say $bits_hex_str_total;  # for testing

# say $bits_x;  # for testing
# say $bits_hex;  # for testing


# writing streams to files:
#   https://rakudocs.gitlab.io/language/io-guide
#
try {
  # $file_bits_x.IO.spurt: $bits_x;
  $file_bits_x.IO.spurt: $bits_x_str_total;
}
if $! {
  say "could not write to file: $file_bits_x -- $!";
} else {
  say "Bit stream has been written to disk under name:  $file_bits_x";
}

try {
  # $file_bits_hex.IO.spurt: $bits_hex;
  # $file_bits_hex.IO.spurt: @bits_hex;
  $file_bits_hex.IO.spurt: $bits_hex_str_total;
}
if $! {
  say "could not write to file: $file_bits_hex -- $!";
} else {
  say "Byte stream has been written to disk under name: $file_bits_hex";
}


# 2026-05-31: stop her for speed testing:
# exit();


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
  } else {
    if is_all_digits($answer_str) {        # 2026-05-31: new user defined function
      $N_CHAR = $answer_str.Int.base(10);  # 2026-05-31: this instruction alone is letting illegal number 8.0 pass through!
      if $N_CHAR < 8 {
        say 'enter an integer number >= 8 or "y"';
      } else {
        $answer = True;
      }
    } else {
      say 'enter an integer number >= 8 or "y"';
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
  } else {
    $WITH_SPECIAL_CHARS = False;
    $answer = True;
  }
}


my $pattern;
# 2026-06-18: have a one-liner here: 
if $WITH_SPECIAL_CHARS { $pattern = /<[!..~]>/ } else { $pattern = /<[0..9a..zA..Z]>/ }


my $i = 0;  # char counter for the password
my $j = 0;  # counter for x
my $pw_chars = "";

while $i < $N_CHAR {
  # convert an integer number into a string of '0' and '1' characters:
  my $bin0 = @x[$j].fmt('%016b');  # Bin: 0001011111001100

  my $bin0_0 = $bin0.substr(0, 8);   # end index is exclusive
  my $bin0_1 = $bin0.substr(8, 16);

  # convert a string of '0' and '1' characters into a character:
  my $char0 = "0b$bin0_0".Int.chr;
  my $char1 = "0b$bin0_1".Int.chr;

  if so $char0 ~~ /<$pattern>/ {
    $pw_chars ~= $char0;
    $i++;
  }

  if so $char1 ~~ /<$pattern>/ && $i < $N_CHAR {
    $pw_chars ~= $char1;
    $i++;
  }

  $j++;
}

print "\nYour password of " ~ $N_CHAR ~ " characters is: " ~ $pw_chars ~ "\n";

# end of random_bitstring_and_flexible_password_generator.raku
