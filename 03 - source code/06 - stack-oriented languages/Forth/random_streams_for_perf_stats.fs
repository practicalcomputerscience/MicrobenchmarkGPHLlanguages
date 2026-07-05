\ random_streams_for_perf_stats.fs
\
\ 2026-07-05
\
\ build on Ubuntu 24 LTS: $ gforthmi random_streams_for_perf_stats random_streams_for_perf_stats.fs
\                         ATTENTION: this command is not creating a standalone Linux executable, but a Gforth image file which depends on a Gforth installation!
\
\ run on Ubuntu 24 LTS:   $ time ./random_streams_for_perf_stats => real	0m0.025s <<<<<<<<<<<<<
\
\
\ this program was developed slowly from the ground up with a prompt for MS Bing AI:
\ "Write a simple LCG in gforth Forth, which generates 20 random integer numbers and prints them. Seed should be different with every program run."


62500 CONSTANT END  \ 62500 for exactly 1M binary digits
\ 10 CONSTANT END  \ for testing

16 CONSTANT STR_LENGTH_BIN
4  CONSTANT STR_LENGTH_HEX

END STR_LENGTH_BIN * CONSTANT M1    \ total size in characters of the big bits_x string
END STR_LENGTH_HEX * CONSTANT K250  \ total size in characters of the big bits_hex string

\ LCG parameters:
65521  CONSTANT m  \ = 2^16 - 15
17364  CONSTANT a
0      CONSTANT c


CREATE x         END CELLS ALLOT
CREATE bits_x    END STR_LENGTH_BIN CHARS * ALLOT
CREATE bits_hex  END STR_LENGTH_HEX CHARS * ALLOT

CREATE bits_x_str_total   M1   CHARS ALLOT
CREATE bits_hex_str_total K250 CHARS ALLOT


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\ user defined functions

\ --- initialize seed from system time ---
VARIABLE seed
: init_seed ( -- )
    utime drop seed ! ;   \ use microsecond clock as seed


\ --- next random number ---
: next_rand ( -- u )
    seed @ a * c + m mod dup seed !
    ;


\ --- convert u to 16-bit binary string ---
CREATE binbuf 16 CHARS ALLOT

: integer_to_bin_string ( u -- addr u )
    \ Write bits MSB → LSB into binbuf
    16 0 do
        dup 15 i - rshift 1 and
        IF  [CHAR] 1  ELSE  [CHAR] 0  THEN
        binbuf i + c!
    loop
    drop
    binbuf 16 ;


\ --- convert u to 4-digit hex string ---
\ Ada based solution from random_streams_for_perf_stats.adb with Google AI:
\
\ Create a static 4-byte buffer for the result string
create hex-buf 4 allot

\ Lookup table containing lowercase hex characters
create hex-digits char 0 c, char 1 c, char 2 c, char 3 c, char 4 c, char 5 c,
                  char 6 c, char 7 c, char 8 c, char 9 c, char a c, char b c,
                  char c c, char d c, char e c, char f c,

: integer_to_hex_string ( u -- c-addr u )
    \ $FFFF and ( u )  \ Clamp to 16-bit

    \ Extract digits using masks and shifts, look up characters, and store
    dup $F and hex-digits + c@ hex-buf 3 + c!
    dup 4 rshift $F and hex-digits + c@ hex-buf 2 + c!
    dup 8 rshift $F and hex-digits + c@ hex-buf 1 + c!
    12 rshift $F and hex-digits + c@ hex-buf c!

    hex-buf 4 ;  \ Return address and length


\ for testing:
\  : print-array-int ( addr -- )
\    END 0 DO
\      DUP I CELLS + @ .
\    LOOP
\    DROP ;

\ for testing:
\  : print-array-string ( addr slot-len count -- )
\    locals| count slot-len addr |
\    count 0 DO
\      addr I slot-len * +   slot-len TYPE
\      CR
\    LOOP ;


: build_bits_x_str_total ( -- )
  bits_x_str_total { dst }
  END 0 DO
    bits_x I STR_LENGTH_BIN CHARS * +   \ src
    dst                                 \ src dst
    STR_LENGTH_BIN MOVE                 \ copy STR_LENGTH_BIN chars
    dst STR_LENGTH_BIN CHARS + TO dst
  LOOP ;

: build_bits_hex_str_total ( -- )
  bits_hex_str_total { dst }
  END 0 DO
    bits_hex I STR_LENGTH_HEX CHARS * + \ src
    dst
    STR_LENGTH_HEX MOVE
    dst STR_LENGTH_HEX CHARS + TO dst
  LOOP ;


: byte_nbr_bin  ( i -- addr )  STR_LENGTH_BIN CHARS * bits_x + ;
: byte_nbr_hex  ( i -- addr )  STR_LENGTH_HEX CHARS * bits_hex + ;


\ Helper word to process the operations safely.
\ Primitives return an I/O result (ior) where 0 means success.
\ We use THROW to feed any non-zero ior to our outer CATCH handler.
: (unsafe_write_to_file) ( c-addr u fname-addr fname-u -- )
    W/O CREATE-FILE THROW >R  \ Create/open file. Saves file ID to Return stack

    \ We wrap write-file in its own catch so we can close the file even on failure
    2dup R@ WRITE-FILE ( c-addr u ior )
    ROT ROT 2DROP             \ Clean up string inputs from stack

    R> CLOSE-FILE THROW       \ Always safely close the file here
    THROW ;                   \ If write-file failed, pass that error out

: write_to_file ( c-addr u fname-addr fname-u ftype-addr ftype-u -- )
    locals| ftype-u ftype-addr fname-u fname-addr u c-addr |

    \ Pass the relevant string parameters to the wrapper via CATCH
    c-addr u fname-addr fname-u [ ' (unsafe_write_to_file) ] LITERAL CATCH ( ior )

    IF ( ior is non-zero, something went wrong )
        DROP \ Drop the error number to clean the stack
        S" could not write to file: " TYPE fname-addr fname-u TYPE CR
    ELSE
        \ --- Success Path ---
        ftype-addr ftype-u TYPE
        S"  stream has been written to disk under name: " TYPE
        fname-addr fname-u TYPE CR
    ENDIF ;


\ end of user defined functions
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


: main ( -- )
    init_seed

    cr ." generating a random bit stream..." cr

    \ iterative masterloop:
    \   kept clean with no printing of individual items to prevent a stack overflow:
    END 0 DO
        next_rand dup x I CELLS + !

        dup integer_to_bin_string  \ this is emulating the calculation of bits_x_str
        I byte_nbr_bin swap move   \ move: copy the contents of u consecutive address units at addr1
                                   \ to the u consecutive address units at addr2

        integer_to_hex_string  \ this is emulating the calculation of bits_hex_str
        I byte_nbr_hex swap move
    LOOP

    \ cr x print-array-int cr  \ for testing

    \ build two, big strings to be written to disk:
    build_bits_x_str_total
    build_bits_hex_str_total

    \ bits_x_str_total   M1   TYPE CR  \ for testing
    \ bits_hex_str_total K250 TYPE CR  \ for testing

    \ write bit stream to disk:
    bits_x_str_total   M1   S" random_bitstring.bin"  S" Bit"  write_to_file

    \ write byte stream to disk:
    bits_hex_str_total K250 S" random_bitstring.byte" S" Byte" write_to_file

    bye ;


\ Define the boot sequence
: run-app ( -- )
  main
  bye ;

\ Tell the GForth compiler to map 'run-app' as the main system entry point
' run-app is 'cold

\ end of random_streams_for_perf_stats.fs
