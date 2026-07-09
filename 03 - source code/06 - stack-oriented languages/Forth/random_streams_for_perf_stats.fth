\ random_streams_for_perf_stats.fth
\
\ 2026-07-06
\ 2026-07-09: introduced (global) variables bits_x_str and bits_hex_str like in the other languages; some code streamlining
\
\
\ build on Ubuntu 24 LTS:  $ ccforth -c ./random_streams_for_perf_stats.fth > random_streams_for_perf_stats_ccforth.c
\                          # allocate more memory, here 8MB:
\                          $ sed -i 's/^#define MEM_SIZE .*/#define MEM_SIZE 8388608/' ./random_streams_for_perf_stats_ccforth.c
\                          $ gcc -O3 random_streams_for_perf_stats_ccforth.c -o random_streams_for_perf_stats_ccforth
\
\ run on Ubuntu 24 LTS:    $ ./random_streams_for_perf_stats_ccforth
\                          $ time ./random_streams_for_perf_stats_ccforth => real	0m0.005s
\                          $ sudo perf stat -r 20 ./random_streams_for_perf_stats_ccforth
\
\
\ use a makefile if possible to automate above program build&run process
\
\
\ piece by piece, I developed from the ground up a little Linear Congruential Generator (LCG)
\ for only generating 10 random integer numbers in ccforth with the help of Google AI + MS Bing AI.


62500 constant END  \ 62500 for exactly 1M binary digits: this needs more memory for safe compilation in C!
\ 10 constant END  \ for testing

65521  constant m  \ = 2^16 - 15
17364  constant a
0      constant c


\ Define your filename strings as constants or inline literals
: file_bits_x   S" random_bitstring.bin" ;
: file_bits_hex S" random_bitstring.byte" ;
\ Forth pushes two distinct values onto the data stack:
\   - f_addr: The raw starting address of the characters in memory
\   - f_len: The exact number of characters in that file name string


\ Declare fixed, persistent global values for our array addresses
0 value x
0 value bits_x
0 value bits_hex
0 value bits_x_str_total
0 value bits_hex_str_total
0 VALUE bits_x_str
0 VALUE bits_hex_str


variable seed


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\ user defined functions

: integer_to_bin_string ( n -- addr len )
  base @ swap 2 base !
  0 <# 16 0 do # loop #>
  rot base ! ;

\ Transpiled Gforth/Ada Lookup Table Solution
\ above idea from integer_to_bin_string is not working here!!
create hex-buf 4 allot
create hex-digits char 0 c, char 1 c, char 2 c, char 3 c, char 4 c, char 5 c,
                  char 6 c, char 7 c, char 8 c, char 9 c, char a c, char b c,
                  char c c, char d c, char e c, char f c,

: integer_to_hex_string ( u -- c-addr u )
    dup $F and hex-digits + c@ hex-buf 3 + c!
    dup 4 rshift $F and hex-digits + c@ hex-buf 2 + c!
    dup 8 rshift $F and hex-digits + c@ hex-buf 1 + c!
    12 rshift $F and hex-digits + c@ hex-buf c!
    hex-buf 4 ;


\ Global value to safely hold our active file handle
0 value output_file_id
: write_to_file ( data_addr data_len file_addr file_len file_type_addr file_type_len -- )
  { d_addr d_len f_addr f_len ft_addr ft_len }  \ Map all cells from the stack into descriptive locals

  \ 1. Try to create the file
  f_addr f_len w/o create-file ( -- file_id ior )
  if
    \ If ior is non-zero, creation failed
    drop  \ clean up uninitialized file_id from stack
    cr S" could not write to file: " type f_addr f_len type
    exit
  then
  to output_file_id  \ Save valid handle

  \ 2. Try to write the data payload
  d_addr d_len output_file_id write-file ( -- ior )
  if
    \ If writing failed, print error message
    cr S" failed to write data to file: " type f_addr f_len type

    \ CRITICAL: Safely close the file handle before exiting so it doesn't lock!
    output_file_id close-file drop
    exit
  then

  \ 3. Try to close the file handle cleanly
  output_file_id close-file ( -- ior )
  if
    cr S" failed to close file handle cleanly: " type f_addr f_len type
    exit
  then

  \ OK message to the console:
  cr ft_addr ft_len type S" stream has been written to disk under name: " type f_addr f_len type ;
  \ cr S" stream has been written to disk under name: " type
  \ file_addr file_len type cr ;



\ end of user defined functions
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


: main
  \ Allocate memory block boundaries safely and map addresses globally (memory linear pools)
  HERE to x                   END cells allot
  HERE to bits_x              END 16 * allot
  HERE to bits_hex            END 4 * allot
  HERE to bits_x_str_total    END 16 * allot
  HERE to bits_hex_str_total  END 4 * allot
  HERE to bits_x_str          16 allot
  HERE to bits_hex_str         4 allot

  \ Fetch the dynamic system time at runtime, convert it, and seed the generator
  UTIME 2DUP XOR 65535 AND seed !  \ initialize the random seed
  seed @ m 2 - mod 1 + seed !      \ limit initial seeds to 1 to m - 1 (both including)
  \ cr ." initial seed = " seed @ . cr  \ for testing

  cr ." generating a random bit stream..."

  END 0 do
    seed @ a * c + m mod dup seed !
    \ cr cr dup .  \ for testing

    \ Store raw integer into x array
    dup i cells x + !

    \ --- Safe Binary Generation and Storage ---
    dup integer_to_bin_string        ( -- n src_addr len )
    over bits_x_str !
    \ cr bits_x_str @ over type  \ for testing

    i 16 * bits_x +                  ( -- n src_addr len dest_addr )
    swap move                        ( -- n ) \ Moves 'len' bytes from src to dest safely

    \ --- Safe Hexadecimal Generation and Storage ---
    dup integer_to_hex_string        ( -- n src_addr len )
    over bits_hex_str !
    \ cr bits_hex_str @ over type  \ for testing

    i 4 * bits_hex +                 ( -- n src_addr len dest_addr )
    swap move                        ( -- n )

    drop                             ( -- )  \ Drop the remaining copy of seed
  loop


  \ Flat Concatenation Block (Zero stack shuffling)
  END 0 do
    i 16 * bits_x +  i 16 * bits_x_str_total   +  16 move
    i 4 * bits_hex + i 4  * bits_hex_str_total +   4 move
  loop

  \ cr cr bits_x_str_total   END 16 * type  \ for testing
  \ cr cr bits_hex_str_total END 4 * type  \ for testing

  \ write bit stream to disk:
  bits_x_str_total   END 16 * file_bits_x   S" Bit "  write_to_file

  \ write byte stream to disk:
  bits_hex_str_total END  4 * file_bits_hex S" Byte " write_to_file

  cr ;


\ main  \ don't call main in case of a transpilation into C language (for compilation into a standalone executable)

\ end of random_streams_for_perf_stats.fth
