Module: random-streams-for-perf-stats

/*
2026-06-27/28

build on Ubuntu 24 LTS: do this only once:
                        $ deft new application --simple random-streams-for-perf-stats
                        # --simple is used here to make only one random-streams-for-perf-stats executable library,
                        # see from here: https://opendylan.org/getting-started-cli/hello-world.html.
                        # If not, we will get an even more sprawling application, distributed over several subdirectories and files.
                        # Here, "simple" means to have at least 5 shared object files (*.so*) the main executable depends on.
                        $ cd ./random-streams-for-perf-stats  # change into the project root directory
                        # there, fix library.dylan
                        $ deft build --all

                        do this after every source code change:
                        $ deft build

run on Ubuntu 24 LTS:   $ _build/bin/random-streams-for-perf-stats  # run this command from project root dir
                        $ multitime -n 20 ./_build/bin/random-streams-for-perf-stats
                        =>
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.129       0.011       0.121       0.125       0.162


$ deft version
v0.13.0-11-gbb20956 built on 2026-02-19T02:05:17+00:00
$

*/


define constant *END* :: integer = 62501;  // 62501 for exactly 1M binary digits; END is a module constant
// define constant *END* = 10;  // for testing

define constant *m* :: integer = 65521;    // = 2^16 - 15
define constant *a* :: integer = 17364;
define constant *c* :: integer = 0;

define constant *file_bits_x*   :: <string> = "random_bitstring.bin";  // declaring a string constant
define constant *file_bits_hex* :: <string> = "random_bitstring.byte";


define function main ()
    let x = make(<vector>, element-type: <integer>, size: *END*);  // 1-dim <vector> should be faster than a 1-dim <array>

    let bits_x   = make(<string-stream>, direction: #"output");  // this is Dylan's string builder
    let bits_hex = make(<string-stream>, direction: #"output");

    let random-state = make(<random>);  // Google AI
    // Generate a random integer between 0 and *m*, exclusive of *m*:
    x[0] := random(*m* - 1, random: random-state) + 1;
    // format-out("x[0] = %d\n", x[0]);  // for testing

    format-out("\ngenerating a random bit stream...");

    // Loop from index 1 up to (but excluding) the vector size
    // this is a very imperative approach:
    for (i from 1 below x.size)  // this is the for macro
        x[i] := modulo((*a* * x[i - 1] + *c*), *m*);
        // format-out("\n\nx[i] = %d", x[i]);  // for testing

        let bits_x_str :: <string> = integer-to-string(x[i], base: 2, size: 16);
        // format-out("\nbits_x_str = %s", bits_x_str);  // for testing
        write(bits_x, bits_x_str);

        let bits_hex_str :: <string> = integer-to-string(x[i], base: 16, size: 4, lowercase?: #t);
        // format-out("\nbits_hex_str = %s", bits_hex_str);  // for testing
        write(bits_hex, bits_hex_str);
    end for;

    let bits_x_str_total = stream-contents(bits_x);
    close(bits_x);
    let bits_hex_str_total = stream-contents(bits_hex);
    close(bits_hex);
    // format-out("\nbits_x_str_total = %s", bits_x_str_total);  // for testing
    // format-out("\nbits_hex_str_total = %s", bits_hex_str_total);  // for testing


    // write bit stream to disk:
    block ()
      with-open-file (file-stream = *file_bits_x*, direction: #"output")
          write(file-stream, bits_x_str_total);
          format-out("\nBit stream has been written to disk under name:  %s", *file_bits_x*)
      end with-open-file;

    exception (err :: <file-error>)
        format-out("\ncould not write to file: %s -- %s", *file_bits_x*, err);
    end block;

    // write byte stream to disk:
    block ()
        with-open-file (file-stream = *file_bits_hex*, direction: #"output")
            write(file-stream, bits_hex_str_total);
            format-out("\nByte stream has been written to disk under name: %s", *file_bits_hex*)
        end with-open-file;
    exception (err :: <file-error>)
        format-out("\ncould not write to file: %s -- %s", *file_bits_hex*, err);
    end block;

    format-out("\n");
    exit-application(0);
end function;


main();
