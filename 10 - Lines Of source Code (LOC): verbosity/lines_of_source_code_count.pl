# lines_of_source_code_count.pl
#
# 2025-05-13/14/15/19/21/27/29, 2025-06-01/02/03/06/15/18/27,
# 2025-07-08/12/14, 2025-10-29, 2025-11-16
#
#
# run on Ubuntu 24 LTS: $ perl lines_of_source_code_count.pl random_bitstring_and_flexible_password_generator.<...>
#
#
# $ perl --version
# This is perl 5, version 38, subversion 2 (v5.38.2) built for x86_64-linux-gnu-thread-multi

use warnings;
use strict;

my $file = $ARGV[0];
open( FILE, "$file" ) || die "cannot open $file!\n";

my $line_count = 0;
my $source_code_line_count = 0;

my $line_empty = 0;
my $line_cmt_fwdslash_dbl = 0;
my $line_cmt_hash = 0;
my $line_cmt_minus_dbl = 0;
my $line_cmt_ML_style = 0;  # (*...*)
my $line_cmt_Lisp_style = 0;
my $line_cmt_Basic_style = 0;
my $line_cmt_Mercury_style = 0;  # %


my $fwdslash_star_detected = 0;              # 0 is false --> use strict prevents using true or false
my $star_bwdslash_detected = 0;              # 0 is false
my $triple_dbl_quote_detected = 0;           # 0 is false
my $bracket_star_detected = 0;               # 0 is false
my $star_bracket_detected = 0;               # 0 is false
my $less_than_hash_detected = 0;             # 0 is false
my $hash_greater_than_detected = 0;          # 0 is false
my $hash_bar_detected = 0;                   # 0 is false
my $bar_hash_detected = 0;                   # 0 is false
my $fwdslash_singlequote_detected = 0;       # 0 is false
my $singlequote_fwdslash_detected = 0;       # 0 is false
my $doubleminus_squarebracket_detected = 0;  # 0 is false
my $doublesquarebracket_detected = 0;        # 0 is false


my $language_ext = $file;

my @lang_grp1 = ("rs", "pl", "mojo", "roc", "adb", "zig", "inko", "cr", "gleam");

my @lang_grp2 = ("go", "scala", "swift", "v", "c", "c3", "kt", "chpl");
my @lang_grp3 = ("py");
my @lang_grp4 = ("ml", "sml");
my @lang_grp5 = ("ps");
my @lang_grp6 = ("clj");
my @lang_grp7 = ("lisp", "rkt");
my @lang_grp8 = ("bas");
my @lang_grp9 = ("lua");
my @lang_grp10 = ("m");
my $line_of_block_comment2 = 0;
my $line_of_block_comment3 = 0;
my $line_of_block_comment4 = 0;
my $line_of_block_comment5 = 0;
my $line_of_block_comment6 = 0;
my $line_of_block_comment7 = 0;
my $line_of_block_comment8 = 0;
my $line_of_block_comment9 = 0;


$language_ext =~ s/^\w+\.//;
print "language = ", $language_ext, "\n";


if ( grep(/^$language_ext$/, @lang_grp1)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    # case: empty line or line with white spaces:
    if ($_ =~ /^\s*$/) {
      $line_empty += 1;
    } else {

      # case: // with optional, leading white spaces: Rust, Zig, Gleam
      if ($_ =~ /^\s*\/\//) {
        $line_cmt_fwdslash_dbl += 1;
      } else {

        # case: # with optional, leading white spaces: Perl, Mojo, Roc, Inko
        if ($_ =~ /^\s*#/) {
          $line_cmt_hash += 1;
        } else {

          # case: -- with optional, leading white spaces: Ada
          if ($_ =~ /^\s*--/) {
            $line_cmt_minus_dbl += 1;
          } else {
            $source_code_line_count += 1;
          }
        }
      }
    }
  }
}



if ( grep(/^$language_ext$/, @lang_grp2)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($fwdslash_star_detected) {
      $line_of_block_comment2 += 1;
    }

    # at the moment, only caring about:
    #   beginning /*: with potential leading white spaces and any kind of stuff beyond /*..
    #   ending    */: with potential any kind of stuff before ..*/ and potential trailing white spaces
    if ($_ =~ /^\s*\/\*/ and not $fwdslash_star_detected) {
      $fwdslash_star_detected = 1;
      $star_bwdslash_detected = 0;
      print "  fwdslash_star_detected", "\n";
      $line_of_block_comment2 += 1;
    } else {

      if ($_ =~ /\*\/\s*$/ and $fwdslash_star_detected) {
        $fwdslash_star_detected = 0;
        $star_bwdslash_detected = 1;
        print "  star_bwdslash_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $fwdslash_star_detected) {
          $line_empty += 1;
        } else {
          # case: // with optional, leading white spaces:
          if ($_ =~ /^\s*\/\// and not $fwdslash_star_detected) {
            $line_cmt_fwdslash_dbl += 1;
          } else {
            if (not $fwdslash_star_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}



if ( grep(/^$language_ext$/, @lang_grp3)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($triple_dbl_quote_detected) {
      $line_of_block_comment3 += 1;
    }

    # at the moment, only caring about:
    #   beginning /*: with potential leading white spaces and any kind of stuff beyond /*..
    #   ending    */: with potential any kind of stuff before ..*/ and potential trailing white spaces
    if ($_ =~ /^\s*"""/ and not $triple_dbl_quote_detected) {
      $triple_dbl_quote_detected = 1;
      print "  triple_dbl_quote_detected", "\n";
      $line_of_block_comment3 += 1;
    } else {

      if ($_ =~ /"""\s*$/ and $triple_dbl_quote_detected) {
        $triple_dbl_quote_detected = 0;
        print "  triple_dbl_quote_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $triple_dbl_quote_detected) {
          $line_empty += 1;
        } else {
          # case: # with optional, leading white spaces:
          if ($_ =~ /^\s*#/ and not $triple_dbl_quote_detected) {
            $line_cmt_hash += 1;
          } else {
            if (not $triple_dbl_quote_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}



if ( grep(/^$language_ext$/, @lang_grp4)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($bracket_star_detected) {
      $line_of_block_comment4 += 1;
    }
    # (*..*): with potential leading and trailing white spaces and any kind of stuff in between
    if ($_ =~ /^\s*\(\*.*\*\)\s*$/ and not $bracket_star_detected) {
        $line_cmt_ML_style += 1;
    } else {
      if ($_ =~ /^\s*\(\*/ and not $bracket_star_detected) {
        $bracket_star_detected = 1;
        $star_bracket_detected = 0;
        print "  bracket_star_detected", "\n";
        $line_of_block_comment4 += 1;
      } else {
        if ($_ =~ /\*\)\s*$/ and $bracket_star_detected) {
          $bracket_star_detected = 0;
          $star_bracket_detected = 1;
          print "  star_bracket_detected", "\n";
        } else {
          # case: empty line or line with white spaces:
          if ($_ =~ /^\s*$/ and not $bracket_star_detected) {
            $line_empty += 1;
          } else {
            if (not $bracket_star_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}



if ( grep(/^$language_ext$/, @lang_grp5)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($less_than_hash_detected) {
      $line_of_block_comment5 += 1;
    }

    # at the moment, only caring about:
    #   beginning /*: with potential leading white spaces and any kind of stuff beyond /*..
    #   ending    */: with potential any kind of stuff before ..*/ and potential trailing white spaces
    if ($_ =~ /^\s*\<\#/ and not $less_than_hash_detected) {
      $less_than_hash_detected = 1;
      print "  less_than_hash_detected", "\n";
      $line_of_block_comment5 += 1;
    } else {

      if ($_ =~ /\#\>\s*$/ and $less_than_hash_detected) {
        $less_than_hash_detected = 0;
        print "  hash_greater_than_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $less_than_hash_detected) {
          $line_empty += 1;
        } else {
          # case: # with optional, leading white spaces:
          if ($_ =~ /^\s*#/ and not $less_than_hash_detected) {
            $line_cmt_hash += 1;
          } else {
            if (not $less_than_hash_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}


if ( grep(/^$language_ext$/, @lang_grp6)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    # case: empty line or line with white spaces:
    if ($_ =~ /^\s*$/) {
      $line_empty += 1;
    } else {

      # case: ; with optional, leading white spaces: Clojure
      if ($_ =~ /^\s*;/) {
        $line_cmt_Lisp_style += 1;
      } else {
        $source_code_line_count += 1;
      }
    }
  }
}



if ( grep(/^$language_ext$/, @lang_grp7)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($hash_bar_detected) {
      $line_of_block_comment7 += 1;
    }

    # at the moment, only caring about:
    #   beginning #|: with potential leading white spaces and any kind of stuff beyond /*..
    #   ending    |#: with potential any kind of stuff before ..*/ and potential trailing white spaces
    if ($_ =~ /^\s*\#\|/ and not $hash_bar_detected) {
      $hash_bar_detected = 1;
      $bar_hash_detected = 0;
      print "  hash_bar_detected", "\n";
      $line_of_block_comment7 += 1;
    } else {

      if ($_ =~ /\|\#\s*$/ and $hash_bar_detected) {
        $hash_bar_detected = 0;
        $bar_hash_detected = 1;
        print "  bar_hash_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $hash_bar_detected) {
          $line_empty += 1;
        } else {
          # case: ; with optional, leading white spaces:
          if ($_ =~ /^\s*\;/ and not $hash_bar_detected) {
            $line_cmt_Lisp_style += 1;
          } else {
            if (not $hash_bar_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}


if ( grep(/^$language_ext$/, @lang_grp8)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($fwdslash_singlequote_detected) {
      $line_of_block_comment8 += 1;
    }

    # at the moment, only caring about:
    #   beginning /': with potential leading white spaces and any kind of stuff beyond /'..
    #   ending    '/: with potential any kind of stuff before ..'/ and potential trailing white spaces
    if ($_ =~ /^\s*\/\'/ and not $fwdslash_singlequote_detected) {
      $fwdslash_singlequote_detected = 1;
      $singlequote_fwdslash_detected = 0;
      print "  fwdslash_singlequote_detected", "\n";
      $line_of_block_comment8 += 1;
    } else {

      if ($_ =~ /\'\/\s*$/ and $fwdslash_singlequote_detected) {
        $fwdslash_singlequote_detected = 0;
        $singlequote_fwdslash_detected = 1;
        print "  singlequote_fwdslash_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $fwdslash_singlequote_detected) {
          $line_empty += 1;
        } else {
          # case: ' with optional, leading white spaces:
          if ($_ =~ /^\s*\'/ and not $fwdslash_singlequote_detected) {
            $line_cmt_Basic_style += 1;
          } else {
            if (not $fwdslash_singlequote_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}


if ( grep(/^$language_ext$/, @lang_grp9)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($doubleminus_squarebracket_detected) {
      $line_of_block_comment9 += 1;
    }

    # at the moment, only caring about:
    #   beginning --[[: with potential leading white spaces and any kind of stuff beyond --[[..
    #   ending      ]]: with potential any kind of stuff before ..]] and potential trailing white spaces
    if ($_ =~ /^\s*\-\-\[\[/ and not $doubleminus_squarebracket_detected) {
      $doubleminus_squarebracket_detected = 1;
      $doublesquarebracket_detected = 0;
      print "  doubleminus_squarebracket_detected", "\n";
      $line_of_block_comment9 += 1;
    } else {

      if ($_ =~ /\-\-\s*$/ and $doubleminus_squarebracket_detected) {
        $doubleminus_squarebracket_detected = 0;
        $doublesquarebracket_detected = 1;
        print "  doublesquarebracket_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $doubleminus_squarebracket_detected) {
          $line_empty += 1;
        } else {
          # case: -- with optional, leading white spaces:
          if ($_ =~ /^\s*\-\-/ and not $doubleminus_squarebracket_detected) {
            $line_cmt_minus_dbl += 1;
          } else {
            if (not $doubleminus_squarebracket_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}

if ( grep(/^$language_ext$/, @lang_grp10)) {
  while ( <FILE> ) {
    chomp( $_ );

    $line_count += 1;
    # print $_ , "\n";  # $_ is the current line

    if ($fwdslash_star_detected) {
      $line_of_block_comment2 += 1;
    }

    # at the moment, only caring about:
    #   beginning /*: with potential leading white spaces and any kind of stuff beyond /*..
    #   ending    */: with potential any kind of stuff before ..*/ and potential trailing white spaces
    if ($_ =~ /^\s*\/\*/ and not $fwdslash_star_detected) {
      $fwdslash_star_detected = 1;
      $star_bwdslash_detected = 0;
      print "  fwdslash_star_detected", "\n";
      $line_of_block_comment2 += 1;
    } else {

      if ($_ =~ /\*\/\s*$/ and $fwdslash_star_detected) {
        $fwdslash_star_detected = 0;
        $star_bwdslash_detected = 1;
        print "  star_bwdslash_detected", "\n";
      } else {
        # case: empty line or line with white spaces:
        if ($_ =~ /^\s*$/ and not $fwdslash_star_detected) {
          $line_empty += 1;
        } else {
          # case: % with optional, leading white spaces:
          if ($_ =~ /^\s*\%/ and not $fwdslash_star_detected) {
            $line_cmt_Mercury_style += 1;
          } else {
            if (not $fwdslash_star_detected) {
              $source_code_line_count += 1;
            }
          }
        }
      }
    }
  }
}


close( FILE );

print "\ntotal number of lines = ", $line_count;
print "\nnumber of lines of source code (estimated) = ", $source_code_line_count, "\n";

print "\nnumber of empty lines = ", $line_empty;
print "\nnumber of lines with ___// = ", $line_cmt_fwdslash_dbl;
print "\nnumber of lines with ___# = ", $line_cmt_hash;
print "\nnumber of lines with ___-- = ", $line_cmt_minus_dbl;
print "\nnumber of lines with ___(*..*)___ = ", $line_cmt_ML_style;
print "\nnumber of lines with ___;(;) = ", $line_cmt_Lisp_style;
print "\nnumber of lines with ___' = ", $line_cmt_Basic_style;
print "\nnumber of lines with ___% = ", $line_cmt_Mercury_style;
print "\nnumber of lines in block comment: \/\* ... \*\/ = ", $line_of_block_comment2;
print "\nnumber of lines in block comment: \"\"\" ... \"\"\" = ", $line_of_block_comment3;
print "\nnumber of lines in block comment: \(\* ... \*\) = ", $line_of_block_comment4;
print "\nnumber of lines in block comment: <# ... #> = ", $line_of_block_comment5;
print "\nnumber of lines in block comment: #| ... |# = ", $line_of_block_comment7;
print "\nnumber of lines in block comment: /' ... '/ = ", $line_of_block_comment8;
print "\nnumber of lines in block comment: --[[ ... ]] = ", $line_of_block_comment9;

print "\n";

# end of lines_of_source_code_count.pl
