Module: random-bitstring-and-flexible-password-generator

/*
2026-06-28

build on Ubuntu 24 LTS: do this only once:
                        $ deft new application --simple random-bitstring-and-flexible-password-generator
                        # --simple is used here to make only one random-bitstring-and-flexible-password-generator executable library,
                        # see from here: https://opendylan.org/getting-started-cli/hello-world.html.
                        # If not, we will get an even more sprawling application, distributed over several subdirectories and files.
                        $ cd ./random-bitstring-and-flexible-password-generator  # change into the project root directory
                        # there, fix: dylan-package.json and library.dylan
                        $ deft build --all

                        do this after every source code change:
                        $ deft build

run on Ubuntu 24 LTS:   $ _build/bin/random-bitstring-and-flexible-password-generator  # run this command from project root dir


$ deft version
v0.13.0-11-gbb20956 built on 2026-02-19T02:05:17+00:00
$
$ dylan-compiler -VERSION
Open Dylan 2026.1
$

*/


define constant *END* = 62501;  // 62501 for exactly 1M binary digits; END is a module constant
// define constant *END* = 50;  // for testing

define constant *m* = 65521;    // = 2^16 - 15
define constant *a* = 17364;
define constant *c* = 0;

define constant *file_bits_x*   :: <string> = "random_bitstring.bin";  // declaring a string constant
define constant *file_bits_hex* :: <string> = "random_bitstring.byte";


////////////////////////////////////////////////////////////////////////
//
// user defined functions
//

// Helper: check if a character is a digit '0'..'9'
// define method digit? (ch :: <character>) => (res :: <boolean>)
//     ch >= '0' & ch <= '9'
// end method digit?;

// MS Bing AI:
define method graphic-no-space? (c :: <character>)
  graphic?(c) & ~whitespace?(c)
end method;

// end of user defined functions
//
////////////////////////////////////////////////////////////////////////


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


    // make a password of N_CHAR printable chars: user input requested here
    let n_char = 12;
    let answer = #f;
    while (~answer)
        format-out("\nPassword of %d printable chars OK? 'y' or another integer number >= 8: ", n_char);
        force-output(*standard-output*); // flush the output buffer
        let answer_str = read-line(*standard-input*);  // returns the line without the trailing newline
        // format-out("answer_str: %s\n", answer_str);  // for testing

        if (answer_str = "y")
          answer := #t;
        else
          block ()
              // if (every?(digit?, answer_str))  // obsolete with strings v.2.0.1 function decimal-digit?()
              if (decimal-digit?(answer_str))
                  let n_char_ = string-to-integer(answer_str);  // 66 ggg evaluates to 66 with inbuilt function string-to-integer
                  if (n_char_ >= 8)
                      n_char := n_char_;
                      answer := #t;
                  else
                      format-out("enter an integer number >= 8 or 'y'\n");
                  end if;
              end if;
          exception (condition :: <error>)
              format-out("enter an integer number >= 8 or 'y'\n");
          end block;
        end if;
    end;
    // format-out("n_char = %d\n", n_char);  // for testing


    let with_special_chars = #t;
    let answer = #f;
    while (~answer)
        format-out("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
        force-output(*standard-output*); // flush the output buffer
        let answer_str = read-line(*standard-input*);  // returns the line without the trailing newline
        // format-out("answer_str = %s\n", answer_str);  // for testing

        if (answer_str = "y")
            answer := #t;
        else
            with_special_chars := #f;
            answer := #t;
        end if;
    end;
    // format-out("with_special_chars = %s\n", with_special_chars);  // for testing


    // New strings Library: https://opendylan.org/proposals/dep-0004-strings-library.html#new-strings-library
    // graphic?
    // alphanumeric?
    let pattern =
        if (with_special_chars)
            // graphic?  // this includes the space char: ' #,0>/BF;h' for 10 pw chars!
            // printable?  // this is also not working since it seems to include control chars!
            // see: https://github.com/dylan-lang/strings/blob/46c047af10d03d10de5b0543ad3b1be3a147e035/strings.dylan#L80
            //      graphic?(char) | whitespace?(char)
            graphic-no-space?
        else
            alphanumeric?
        end if;


    let i = 0;  // char counter for the password
    let j = 0;  // counter for x
    let pw_chars_ = make(<string-stream>, direction: #"output");

    while (i < n_char)
        let bin0 :: <string> = integer-to-string(x[j], base: 2, size: 16);
        // format-out("\nbin0 = %s\n", bin0);  // for testing

        let bin0_0 = copy-sequence(bin0, end: 8);
        let bin0_1 = copy-sequence(bin0, start: 8);
        // format-out("bin0_0 = %s\n", bin0_0);  // for testing
        // format-out("bin0_1 = %s\n", bin0_1);  // for testing

        let char0 = as(<character>, string-to-integer(bin0_0, base: 2));
        let char1 = as(<character>, string-to-integer(bin0_1, base: 2));
        // format-out("char0 = %c\n", char0);  // for testing
        // format-out("char1 = %c\n", char1);  // for testing

        if (pattern(char0) )
            write-element(pw_chars_, char0);
            i := i + 1;
            // format-out("match char0!\n");  // for testing
        end if;

        if (pattern(char1) & i < n_char)
            write-element(pw_chars_, char1);
            i := i + 1;
            // format-out("match char1!\n");  // for testing
        end if;

        j := j + 1;
    end;

    let pw_chars = stream-contents(pw_chars_);
    close(pw_chars_);

    format-out("\nYour password of %d characters is: %s\n", n_char, pw_chars);

    exit-application(0);
end function;


main();
