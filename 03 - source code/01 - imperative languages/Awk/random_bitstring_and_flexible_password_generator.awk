# random_bitstring_and_flexible_password_generator.awk
#
# 2026-07-23
#
# run in Ubuntu 24 LTS: $ mawk -f random_bitstring_and_flexible_password_generator.awk
#                       $ awk -f random_bitstring_and_flexible_password_generator.awk
#
#
# transpiled from Tcl random_bitstring_and_flexible_password_generator.tcl with Duck.ai with heavy manual corrections
#
#
# $ mawk --version
# mawk 1.3.4 20240123
# Copyright 2008-2023,2024, Thomas E. Dickey
# Copyright 1991-1996,2014, Michael D. Brennan
# ...
# $ awk -V
# GNU Awk 5.2.1, API 3.2, PMA Avon 8-g1, (GNU MPFR 4.2.1, GNU MP 6.3.0)
# Copyright (C) 1989, 1991-2022 Free Software Foundation.
# ...
# $


BEGIN {
    upper_limit = 62501   # 62501 for exactly 1M binary digits
    # upper_limit = 20   # for testing

    m = 65521     # = 2^16 - 15
    a = 17364
    c = 0

    file_bits_x   = "random_bitstring.bin"
    file_bits_hex = "random_bitstring.byte"

    # x list of random integers:

    # initialize RNG and first value
    srand()
    x[0] = int(rand() * (m - 1)) + 1
    # print "x[0] = " x[0]  # for testing

    bits_x = ""
    bits_hex = ""

    print "\ngenerating a random bit stream..."
    for (i = 1; i < upper_limit; i++) {
        x[i] = (a * x[i-1] + c) % m
        # print "\nx[i] = " x[i]  # for testing

        # 16-bit binary string
        bits_x_str   = sprintf("%016d", integer_to_bin_string(x[i]))
        # print "bits_x_str = " bits_x_str  # for testing
        bits_x       = bits_x bits_x_str

        # 4-digit lowercase hex string
        bits_hex_str = sprintf("%04x", x[i])
        # print "bits_hex_str = " bits_hex_str  # for testing
        bits_hex     = bits_hex bits_hex_str
    }


    # write bit stream to disk
    if ((write_to_file(file_bits_x, bits_x)) == 1)
        print "Bit stream has been written to disk under name:  " file_bits_x
    # write byte stream to disk
    if ((write_to_file(file_bits_hex, bits_hex)) == 1)
        print "Byte stream has been written to disk under name: " file_bits_hex


    # make a password of N_CHAR printable chars: user input requested here
    N_CHAR = 12
    answer = 0
    while (!answer) {
        printf "\nPassword of %d printable chars OK? 'y' or another integer number >= 8: ", N_CHAR
        fflush()
        if ((getline answer_str < "/dev/tty") <= 0) exit 1

        if (answer_str == "y") {
            answer = 1
        } else if (answer_str ~ /^[0-9]+$/) {
            N_CHAR = int(answer_str)
            if (N_CHAR < 8) {
                print "enter an integer number >= 8 or 'y'"
            } else {
                answer = 1
            }
        } else {
            print "enter an integer number >= 8 or 'y'"
        }
    }
    # printf "N_CHAR = %d\n", N_CHAR  # for testing


    WITH_SPECIAL_CHARS = 1
    answer = 0
    while (!answer) {
        printf "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "
        fflush()
        if ((getline answer_str < "/dev/tty") <= 0) exit 1

        if (answer_str == "y") {
            answer = 1
        } else {
            WITH_SPECIAL_CHARS = 0
            answer = 1
        }
    }

    if (WITH_SPECIAL_CHARS) {
        pattern = "[!-~]"
    } else {
        pattern = "[A-Za-z0-9]"
    }


    i = 0  # char counter for the password
    j = 0  # counter for x
    pw_chars = ""

    while (i < N_CHAR) {
        # printf "\n\n" x[j]  # for testing
        bin0 = sprintf("%016d", integer_to_bin_string(x[j]))
        # printf "\n" bin0  # for testing

        bin0_0 = substr(bin0, 0 , 8)
        bin0_1 = substr(bin0, 8 , 8)
        # printf "\n" bin0_0 " -- " bin0_1  # for testing

        char0a = bin_string_to_integer(bin0_0)
        char1a = bin_string_to_integer(bin0_1)
        # printf "\n" char0a " -- " char1a  # for testing

        char0b = sprintf("%c", char0a)
        char1b = sprintf("%c", char1a)
        # printf "\n" char0b " -- " char1b  # for testing

        if (char0b ~ pattern) {
            pw_chars = pw_chars char0b
            i++
            # printf "\nmatched char0b!"  # for testing
        }

        if ((char1b ~ pattern) && i < N_CHAR) {
            pw_chars = pw_chars char1b
            i++
            # printf "\nmatched char1b!"  # for testing
        }

        j++
    }

    print "\nYour password of " N_CHAR " characters is: " pw_chars
    exit 0
}


# /////////////////////  user defined functions  ////////////////////////////
#

function integer_to_bin_string(n, s, r) {
    s = ""
    v = n
    for (r = 15; r >= 0; r--) {
        bit = int(v / (2 ^ r))
        if (bit >= 2) bit = bit % 2
        s = s (bit ? "1" : "0")
        v = v % (2 ^ r)
    }
    return s
}


function write_to_file(fname, data,    cmd, is_writable) {
    # Google AI:
    # Use POSIX shell 'test -w' to check write permissions
    # If the file doesn't exist yet, we check if its parent directory is writable
    cmd = "test -w '" fname "' || { [ ! -e '" fname "' ] && [ -w $(dirname '" fname "') ]; }"

    is_writable = (system(cmd) == 0)

    if (!is_writable) {
        print "could not write to file: " fname > "/dev/stderr"
        return 0
    }

    # print data > fname
    printf "%s", data > fname  # no final . at the file on disk
    close(fname)
    return 1
}


function bin_string_to_integer(str,    i, len, val) {
    len = length(str)
    val = 0
    for (i = 1; i <= len; i++) {
        val = (val * 2) + substr(str, i, 1)
    }
    return val
}

#
# //////////////////  end of user defined functions  ////////////////////////

# end of random_bitstring_and_flexible_password_generator.awk
