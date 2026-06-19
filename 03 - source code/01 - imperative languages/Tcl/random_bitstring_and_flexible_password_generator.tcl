# random_bitstring_and_flexible_password_generator.tcl
#
# 2026-06-19
#
# run on Ubuntu 24 LTS: $ tclsh random_bitstring_and_flexible_password_generator.tcl
#
#
# $ echo 'puts $tcl_version;exit 0' | tclsh
# 8.6
# $
#
# mostly transpiled from random_bitstring_and_flexible_password_generator.groovy with Duck.ai and Google AI


# Settings
set END 62501  ;# 62501 for exactly 1M binary digits
# set END 10  ;# for testing

set m 65521    ;# = 2^16 - 15
set a 17364
set c 0

set file_bits_x   "random_bitstring.bin"
set file_bits_hex "random_bitstring.byte"

set x [list]
# list is usually a little bit faster than set in Tcl: 123ms vs 131ms with: array set x {}

lappend x [expr {int(rand() * ($m - 1)) + 1}]

# Accumulators for output strings
set bits_x   ""
set bits_hex ""

puts "\ngenerating a random bit stream..."
for {set i 1} {$i < $END} {incr i} {
    # LCG: x[i] = (a * x[i-1] + c) % m
    set prev [lindex $x [expr {$i - 1}]]
    set current [expr {($a * $prev + $c) % $m}]
    lappend x $current
    # puts "prev = $prev"  ;# for testing

    # binary string padded to 16 bits
    set bits_x_str [format "%016b" $current]
    append bits_x $bits_x_str

    # hexadecimal string padded to 4 hex digits (lowercase)
    set bits_hex_str [format "%04x" $current]
    append bits_hex $bits_hex_str
}


# write bit stream to disk:
if {[catch {
    set fp [open $file_bits_x "w"]
    puts -nonewline $fp $bits_x
    close $fp
} err]} {
    puts "could not write to file: $file_bits_x ! -- $err"
} else {
    puts "Bit stream has been written to disk under name:  $file_bits_x"
}

# write byte stream to disk:
if {[catch {
    set fp2 [open $file_bits_hex "w"]
    puts -nonewline $fp2 $bits_hex
    close $fp2
} err2]} {
    puts "could not write to file: $file_bits_hex ! -- $err2"
} else {
    puts "Byte stream has been written to disk under name: $file_bits_hex"
}


# make a password of N_CHAR printable chars: user input requested here
set N_CHAR 12
set answer 0

while {!$answer} {
    puts -nonewline "\nPassword of $N_CHAR printable chars OK? 'y' or another integer number >= 8: "
    flush stdout
    set answer_str [gets stdin]

    if {$answer_str eq "y"} {
        set answer 1
    } else {
        if {[string is integer -strict $answer_str]} {
            set N_CHAR [expr {int($answer_str)}]
            if {$N_CHAR < 8} {
                puts "enter an integer number >= 8 or 'y'"
            } else {
                set answer 1
            }
        } else {
            puts "enter an integer number >= 8 or 'y'"
        }
    }
}


set WITH_SPECIAL_CHARS 1
set answer 0

while {!$answer} {
    puts -nonewline "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "
    flush stdout
    set answer_str [gets stdin]

    if {$answer_str eq "y"} {
        set answer 1
    } else {
        set WITH_SPECIAL_CHARS 0
        set answer 1
    }
}

# https://wiki.tcl-lang.org/page/Regular+Expressions
# set print_re {([[:graph:]])}  ;# !ÝhÑ$Ý$üÖPGN => not working!
# set alnum_re {([[:alnum:]])}  ;# ðVKnÀãýPßgÚj => not working!
set print_re {([!-~])}
set alnum_re {([A-Za-z0-9])}
set pattern  [expr {$WITH_SPECIAL_CHARS ? $print_re : $alnum_re}]


set i 0  ;# char counter for the password
set j 0  ;# counter for x
set pw_chars ""

while {$i < $N_CHAR} {
    set bin0 [lindex $x $j]

    # Extract the high 8 bits and low 8 bits
    set bin0_0 [expr {($bin0 >> 8) & 0xFF}]
    set bin0_1 [expr {$bin0 & 0xFF}]

    # Convert to characters
    set char0 [format %c $bin0_0]
    set char1 [format %c $bin0_1]

    # Validate character 0
    if {[regexp $pattern $char0]} {
        append pw_chars $char0
        incr i
    }

    # Validate character 1
    if {[regexp $pattern $char1] && $i < $N_CHAR} {
        append pw_chars $char1
        incr i
    }

    incr j
}

puts "\nYour password of $N_CHAR characters is: $pw_chars"

# end of random_bitstring_and_flexible_password_generator.tcl
