# random_streams_for_perf_stats.awk
#
# 2026-07-01
#
# run in Ubuntu 24 LTS: $ mawk -f random_streams_for_perf_stats.awk
#
#                       $ time mawk -f random_streams_for_perf_stats.awk => real	0m1.244s
#                       $ time awk -f random_streams_for_perf_stats.awk  => real	0m0.217s <<<< awk is gawk on my system
#
#
# Duck.ai transpiled from Tcl program random_streams_for_perf_stats.tcl
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
    # upper_limit = 10   # for testing

    m = 65521     # = 2^16 - 15
    a = 17364
    c = 0

    file_bits_x   = "random_bitstring.bin"
    file_bits_hex = "random_bitstring.byte"

    # initialize RNG and first value
    srand()
    prev = int(rand() * (m - 1)) + 1
    # print "prev = " prev  # for testing

    bits_x = ""
    bits_hex = ""

    print "\ngenerating a random bit stream..."
    for (i = 1; i < upper_limit; i++) {
        current = (a * prev + c) % m
        prev = current
        # print "\ncurrent = " current  # for testing

        # 16-bit binary string
        bits_x_str   = sprintf("%016d", integer_to_bin_string(current))
        # print "bits_x_str = " bits_x_str  # for testing
        bits_x       = bits_x bits_x_str

        # 4-digit lowercase hex string
        bits_hex_str = sprintf("%04x", current)
        # print "bits_hex_str = " bits_hex_str  # for testing
        bits_hex     = bits_hex bits_hex_str
    }

    # write bit stream to disk
    if ((write_to_file(file_bits_x, bits_x)) == 1)
        print "Bit stream has been written to disk under name:  " file_bits_x
    # write byte stream to disk
    if ((write_to_file(file_bits_hex, bits_hex)) == 1)
        print "Byte stream has been written to disk under name: " file_bits_hex
}


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


# end of random_streams_for_perf_stats.awk
