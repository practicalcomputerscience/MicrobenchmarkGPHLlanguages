\ random_streams_for_perf_stats.fs
\
\ 2026-07-05/06/08
\
\ build on Ubuntu 24 LTS: $ gforthmi random_streams_for_perf_stats random_streams_for_perf_stats.fs
\                         ATTENTION: this is not creating a standalone Linux executable, but a Gforth image file which depends on a Gforth installation!
\
\ run on Ubuntu 24 LTS:   $ time ./random_streams_for_perf_stats => real	0m0.025s <<<<<<<<<<<<<
\
\
\ $ gforth --version
\ gforth 0.7.9_20260610 amd64
\ $
\
\
\ this program was developed slowly from the ground up with a prompt for MS Bing AI:
\ "Write a simple LCG in gforth Forth, which generates 20 random integer numbers and prints them. Seed should be different with every program run."
\ Later, the Google AI concepts implemented in random_bitstring_and_flexible_password_generator.fs have been also updated here.


62500 CONSTANT END  \ 62500 for exactly 1M binary digits
\ 10 CONSTANT END  \ for testing

\ LCG parameters:
65521  CONSTANT m  \ = 2^16 - 15
17364  CONSTANT a
0      CONSTANT c

16 CONSTANT STR_LENGTH_BIN
4  CONSTANT STR_LENGTH_HEX

END STR_LENGTH_BIN * CONSTANT M1    \ total size in characters of the big bits_x string
END STR_LENGTH_HEX * CONSTANT K250  \ total size in characters of the big bits_hex string

\ Create POINTER variables to hold our dynamic heap memory addresses
\ 2026-07-08
0 VALUE x
0 VALUE bits_x
0 VALUE bits_hex
0 VALUE bits_x_str_total
0 VALUE bits_hex_str_total

: allocate-large-buffers ( -- )
    \ END CELLS ALLOT               \ 2026-07-08: We keep 'x' in dictionary if small, or heap allocate:
    END CELLS            ALLOCATE THROW TO x
    END STR_LENGTH_BIN * ALLOCATE THROW TO bits_x
    END STR_LENGTH_HEX * ALLOCATE THROW TO bits_hex
    M1                   ALLOCATE THROW TO bits_x_str_total
    K250                 ALLOCATE THROW TO bits_hex_str_total ;


\ 2026-07-06:
CREATE file_bits_x   25 ALLOT  \ Allocate 25 bytes for storage
S" random_bitstring.bin"  file_bits_x   SWAP MOVE
CREATE file_bits_hex 25 ALLOT
S" random_bitstring.byte" file_bits_hex SWAP MOVE



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\ user defined functions

\ --- initialize seed from system time ---
VARIABLE seed
: init_seed ( -- )
    utime drop seed ! ;   \ use microsecond clock as seed

\ --- next random number ---
: next_rand ( -- u )
    seed @ a * c + m mod dup seed ! ;


\ --- convert u to 16-bit binary string ---
: integer_to_bin_string ( u dest-addr -- )
    { dest }
    16 0 do
        dup 15 i - rshift 1 and
        IF  [CHAR] 1  ELSE  [CHAR] 0  THEN
        dest i + c!
    loop
    drop ;


\ --- convert u to 4-digit hex string ---
\ Ada based solution from random_streams_for_perf_stats.adb with Google AI
\
\ Lookup table containing lowercase hex characters
CREATE hex-digits CHAR 0 c, CHAR 1 c, CHAR 2 c, CHAR 3 c, CHAR 4 c, CHAR 5 c,
                  CHAR 6 c, CHAR 7 c, CHAR 8 c, CHAR 9 c, CHAR a c, CHAR b c,
                  CHAR c c, CHAR d c, CHAR e c, CHAR f c,

: integer_to_hex_string ( u dest-addr -- )
    { dest }

    \ Extract digits using masks and shifts, look up characters, and store
    dup $F and hex-digits + c@ dest 3 + c!
    dup 4 rshift $F and hex-digits + c@ dest 2 + c!
    dup 8 rshift $F and hex-digits + c@ dest 1 + c!
    12 rshift $F and hex-digits + c@ dest c! ;


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


\ Helper word to process the operations safely.
\ Primitives return an I/O result (ior) where 0 means success.
\ We use THROW to feed any non-zero ior to our outer CATCH handler.
: (unsafe_write_to_file) ( c-addr u fname-addr fname-u -- )
    W/O CREATE-FILE THROW >R

    \ We wrap write-file in its own catch so we can close the file even on failure
    2dup R@ WRITE-FILE   ( c-addr u ior )
    ROT ROT 2DROP        \ Clean up string inputs from stack
    R> CLOSE-FILE THROW  \ Always safely close the file here
    THROW ;              \ If write-file failed, pass that error out

: write_to_file ( c-addr u fname-addr fname-u ftype-addr ftype-u -- )
    { c-addr u fname-addr fname-u ftype-addr ftype-u }

    \ Pass the relevant string parameters to the wrapper via CATCH
    c-addr u fname-addr fname-u [ ' (unsafe_write_to_file) ] LITERAL CATCH

    IF
        \ --- Error Path ---
        DROP  \ Drop the error number to clean the stack
        S" could not write to file: " TYPE fname-addr fname-u TYPE CR
    ELSE
        \ --- Success Path ---
        ftype-addr ftype-u TYPE
        S"  stream has been written to disk under name: " TYPE
        fname-addr fname-u TYPE CR
    THEN ;


\ other helpers in main:
: build_bits_x_str_total ( -- )
  bits_x_str_total { dst }
  END 0 DO
    bits_x I STR_LENGTH_BIN CHARS * +  \ Push source address
    dst
    STR_LENGTH_BIN MOVE                \ Copy characters
    dst STR_LENGTH_BIN CHARS + TO dst
  LOOP ;

: build_bits_hex_str_total ( -- )
  bits_hex_str_total { dst }
  END 0 DO
    bits_hex I STR_LENGTH_HEX CHARS * +  \ Push source address
    dst
    STR_LENGTH_HEX MOVE                  \ Copy characters
    dst STR_LENGTH_HEX CHARS + TO dst
  LOOP ;


\ end of user defined functions
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


: main ( -- )
    allocate-large-buffers

    init_seed
    
    cr ." generating a random bit stream..." cr

    \ iterative masterloop:
    \   kept clean with no printing of individual items to prevent a stack overflow:
    END 0 DO
        next_rand dup x I CELLS + ! ( u )

        \ 2026-07-08: calculate precise destinations instantly without pointer shifting bugs
        DUP bits_x   I STR_LENGTH_BIN * +   integer_to_bin_string  \ no explicit calculation of bits_x_str!
        DUP bits_hex I STR_LENGTH_HEX * +   integer_to_hex_string  \ no explicit calculation of bits_hex_str!
        DROP
    LOOP

    \ cr x print-array-int cr  \ for testing

    \ build the final, big strings:
    build_bits_x_str_total
    build_bits_hex_str_total

    \ bits_x_str_total   M1   TYPE CR  \ for testing
    \ bits_hex_str_total K250 TYPE CR  \ for testing

    \ write bit stream to disk:
    bits_x_str_total   M1   file_bits_x   20 C" Bit"  COUNT write_to_file
    \ 2026-07-08: C" Bit"  COUNT is for using permanently
    \             dictionary-compiled counted strings unpacked via COUNT at runtime

    \ write byte stream to disk:
    bits_hex_str_total K250 file_bits_hex 21 C" Byte" COUNT write_to_file

    x FREE THROW
    bits_x FREE THROW
    bits_hex FREE THROW
    bits_x_str_total FREE THROW
    bits_hex_str_total FREE THROW ;


\ Define the boot sequence
: run-app ( -- )
  main
  bye ;

\ Tell the GForth compiler to map 'run-app' as the main system entry point
' run-app is 'cold


\ end of random_streams_for_perf_stats.fs
