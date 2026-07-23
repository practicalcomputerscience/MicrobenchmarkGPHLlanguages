# Awk

Here in the **GNU Awk** ("gawk") implementation: https://www.gnu.org/software/gawk/manual/gawk.html

<br/>

Originally, and like so often before, I didn't have the idea to implement my microbenchmark program also in Awk. 

But then I noticed this:

- compared to GNU Awk, here using (system-default) version GNU Awk 5.2.1 from 2022, using [mawk, an interpreter for the AWK Programming Language](https://invisible-island.net/mawk/), here in version _mawk 1.3.4 20240123_, for inefficiently (recursively) calculating the Fibonacci number of 47 is a speedy thing (on 2026-07-06):

- _$ time mawk -f fib_recursive_argument.awk 47_: _Time: 199s_
- _$ time awk -f fib_recursive_argument.awk 47_: _Time: 659s_, that is 3.3 times slower

However, when doing a benchmark with the ["speed part" of the microbenchmark program](./random_streams_for_perf_stats.awk), the picture flips substantially:

- _$ time mawk -f random_streams_for_perf_stats.awk_ => real	0m1.260s, that's over 5 times slower!
- _$ time awk -f random_streams_for_perf_stats.awk_ => real	0m0.240s

<br/>

Then I just decided to cheaply transpile also the [Tcl implementation of the full microbenchmark program](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/blob/main/03%20-%20source%20code/01%20-%20imperative%20languages/Tcl/random_bitstring_and_flexible_password_generator.tcl) into an Awk version.

Tcl looks like another good source language for transpilation, next to [Groovy](https://github.com/practicalcomputerscience/MicrobenchmarkGPHLlanguages/tree/main/03%20-%20source%20code/01%20-%20imperative%20languages/Groovy#groovy). 

<br/>

However, I cannot really recommend to prefer Awk over, for example, Tcl for general purpose programming. A short look at the source code of writing a string to a file shows why:

Tcl:

```
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
```

Awk:

```
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
```

##_end
