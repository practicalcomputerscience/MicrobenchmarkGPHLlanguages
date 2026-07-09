\ random_streams_for_perf_stats_swift.f
\
\ this program is only for an evaluation version of SwiftForth. So, building a standalone executable is not possible!
\
\ 2026-07-08
\ 2026-07-09: introduced local variables bits_x_str and bits_hex_str like in the other languages; some code streamlining
\
\
\ run on Ubuntu 24 LTS: $ sf64 ./random_streams_for_perf_stats_swift.f
\                       ...
\                       $ time sf64 ./random_streams_for_perf_stats_swift.f => real	0m0.016s
\
\
\ $ sf64
\ SwiftForth x64-Linux 4.1.8 05-Jul-2026
\ bye
\ $
\
\ this solution is based on random_streams_for_perf_stats.fs for GForth,
\ and has been refactored in several rounds with the help of Google AI.


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
0 VALUE x
0 VALUE bits_x
0 VALUE bits_hex
0 VALUE bits_x_str_total
0 VALUE bits_hex_str_total

: allocate-large-buffers ( -- )
    END CELLS            ALLOCATE THROW TO x
    END STR_LENGTH_BIN * ALLOCATE THROW TO bits_x
    END STR_LENGTH_HEX * ALLOCATE THROW TO bits_hex
    M1                   ALLOCATE THROW TO bits_x_str_total
    K250                 ALLOCATE THROW TO bits_hex_str_total ;


CREATE file_bits_x   25 ALLOT  \ Allocate 25 bytes for storage
S" random_bitstring.bin"  file_bits_x   SWAP MOVE
CREATE file_bits_hex 25 ALLOT
S" random_bitstring.byte" file_bits_hex SWAP MOVE


VARIABLE seed


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\ user defined functions

\ --- convert u to 16-bit binary string ---
: integer_to_bin_string ( u dest-addr -- )
    LOCALS| dest |
    16 0 DO
        DUP 15 I - RSHIFT 1 AND
        IF  [CHAR] 1  ELSE  [CHAR] 0  THEN
        dest I + C!
    LOOP
    DROP ;


\ --- convert u to 4-digit hex string ---
CREATE hex-digits CHAR 0 C, CHAR 1 C, CHAR 2 C, CHAR 3 C, CHAR 4 C, CHAR 5 C,
                  CHAR 6 C, CHAR 7 C, CHAR 8 C, CHAR 9 C, CHAR a C, CHAR b C,
                  CHAR c C, CHAR d C, CHAR e C, CHAR f C,

: integer_to_hex_string ( u dest-addr -- )
    LOCALS| dest |
    DUP $F AND hex-digits + C@ dest 3 + C!
    DUP 4 RSHIFT $F AND hex-digits + C@ dest 2 + C!
    DUP 8 RSHIFT $F AND hex-digits + C@ dest 1 + C!
    12 RSHIFT $F AND hex-digits + C@ dest C! ;


\ Helper word to process the operations safely.
: (unsafe_write_to_file) ( c-addr u fname-addr fname-u -- )
    W/O CREATE-FILE THROW >R
    2DUP R@ WRITE-FILE   ( c-addr u ior )
    ROT ROT 2DROP
    R> CLOSE-FILE THROW
    THROW ;

\ Clean, standard layout with no stack shuffling traps
: write_to_file ( c-addr u fname-addr fname-u ftype-addr ftype-u -- )
    LOCALS| ftype-u ftype-addr fname-u fname-addr u c-addr |

    c-addr u fname-addr fname-u ['] (unsafe_write_to_file) CATCH
    IF
        \ --- Error Path ---
        DROP
        S" could not write to file: " TYPE fname-addr fname-u TYPE CR
    ELSE
        \ --- Success Path ---
        CR ftype-addr ftype-u TYPE
        S"  stream written to disk under name: " TYPE
        fname-addr fname-u TYPE
    THEN ;

\ Block builders using clean pointer increments
: build_bits_x_str_total ( -- )
  bits_x_str_total LOCALS| dst |
  END 0 DO
    bits_x I STR_LENGTH_BIN * +
    dst
    STR_LENGTH_BIN MOVE
    dst STR_LENGTH_BIN + TO dst
  LOOP ;

: build_bits_hex_str_total ( -- )
  bits_hex_str_total LOCALS| dst |
  END 0 DO
    bits_hex I STR_LENGTH_HEX * +
    dst
    STR_LENGTH_HEX MOVE
    dst STR_LENGTH_HEX + TO dst
  LOOP ;

\ end of user defined functions
\
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


: main ( -- )
    LOCALS| bits_x_str bits_hex_str |

    allocate-large-buffers

    COUNTER seed !  \ initialize seed from system time
    \ cr ." raw initial seed = " seed @ . cr  \ for testing

    \ 2026-07-09: seed is initially too high: do this in main
    seed @ m 2 - mod 1 + seed !  \ limit initial seeds to 1 to m - 1 (both including)
    \ cr ." initial seed = " seed @ . cr  \ for testing


    CR ." generating a random bit stream..."

    \ iterative masterloop: this is 1:1 the Gforth solution:
    END 0 DO
        seed @ a * c + m mod dup seed !  \ seed has been duplicated!
        seed @ x I CELLS + ! ( u )  \ write seed to x
        \ cr cr ." seed = " seed @ .  \ for testing
        
        \ 1. Calculate destination address and save to local variable
        bits_x I STR_LENGTH_BIN * + to bits_x_str
        \ 2. Pass the seed (duplicated from stack) and the address to word integer_to_bin_string
        dup bits_x_str integer_to_bin_string
        \ 3. Print the string for debugging using its address and length
        \ cr bits_x_str  16 type  \ for testing
        
        \ 4. Handle hex string processing:
        bits_hex I STR_LENGTH_HEX * + to bits_hex_str
        dup bits_hex_str integer_to_hex_string
        \ cr bits_hex_str 4 type  \ for testing
        
        DROP  \ Drop the remaining copy of seed
    LOOP

    \ cr x print-array-int cr  \ for testing

    \ build the final, big strings:
    build_bits_x_str_total
    build_bits_hex_str_total

    \ cr cr bits_x_str_total   M1   TYPE CR  \ for testing
    \    cr bits_hex_str_total K250 TYPE CR  \ for testing

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
  BYE ;

\ Execute the application immediately upon script evaluation
run-app


\ end of random_streams_for_perf_stats_swift.f
