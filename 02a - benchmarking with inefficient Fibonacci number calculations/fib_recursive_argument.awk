# fib_recursive_argument.awk
#
# 2026-02-11, test on Ubuntu 24 LTS: OK
#
# run in Ubuntu 24 LTS: $ mawk -f fib_recursive_argument.awk 44 => Time: 48s  (result 701408733)
#                       $ mawk -f fib_recursive_argument.awk 47 => Time: 200s (result 2971215073)
#
#
# Duck.ai transpiled from Ruby program fib_recursive_argument.rb
#
#
# $ mawk --version
# mawk 1.3.4 20240123
# Copyright 2008-2023,2024, Thomas E. Dickey
# Copyright 1991-1996,2014, Michael D. Brennan
#
# random-funcs:       arc4random_stir/arc4random
# regex-funcs:        internal
#
# compiled limits:
# sprintf buffer      8192
# maximum-integer     9223372036854775808
# $
#

BEGIN {
    # Parse command line arguments
    n = ARGV[1]
    if (n < 2) {
        exit
    }

    print "argument n = " n

    start_time = systime()  # Record start time

    # Call the Fibonacci function
    result = fib(n)
    print result

    end_time = systime()  # Record end time
    elapsed_time = end_time - start_time
    print "Time: " elapsed_time "s"
}

function fib(n) {
    if (n <= 1) {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

# end of fib_recursive_argument.awk
