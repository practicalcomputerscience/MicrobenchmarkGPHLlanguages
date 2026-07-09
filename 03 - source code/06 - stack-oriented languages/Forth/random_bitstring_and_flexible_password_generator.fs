\ random_bitstring_and_flexible_password_generator.fs
\
\ 2026-07-07/08
\ 2026-07-09: introduced (global) variables bits_x_str and bits_hex_str like in the other languages; some code streamlining
\
\
\ build on Ubuntu 24 LTS: this source code is only meant for production, not development:
\                       $ gforthmi random_bitstring_and_flexible_password_generator random_bitstring_and_flexible_password_generator.fs
\                       ATTENTION: this is not creating a standalone Linux executable, but a Gforth image file which depends on a Gforth installation!
\
\ run on Ubuntu 24 LTS: $ ./random_bitstring_and_flexible_password_generator
\
\
\ $ gforth --version
\ gforth 0.7.9_20260610 amd64
\ $
\
\
\ this source code was only possible with massive help from Google AI in many iterations.


62500 CONSTANT END  \ 62500 for exactly 1M binary digits
\ 500 CONSTANT END  \ for testing

\ LCG parameters:
65521  CONSTANT m  \ = 2^16 - 15
17364  CONSTANT a
0      CONSTANT c

16 CONSTANT STR_LENGTH_BIN
4  CONSTANT STR_LENGTH_HEX

END STR_LENGTH_BIN * CONSTANT M1    \ total size in characters of bits_x_str_total
END STR_LENGTH_HEX * CONSTANT K250  \ total size in characters of bits_hex_str_total

\ Create POINTER variables to hold our dynamic heap memory addresses
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


CREATE file_bits_x   25 ALLOT  \ Allocate 25 bytes for storage
S" random_bitstring.bin"  file_bits_x   SWAP MOVE
CREATE file_bits_hex 25 ALLOT
S" random_bitstring.byte" file_bits_hex SWAP MOVE


VARIABLE seed
VARIABLE bits_x_str
VARIABLE bits_hex_str

12 CONSTANT n_char_default

CREATE char_set_buffer 100 ALLOT  \ Buffer to hold the final char_set string

CREATE pw_buffer 128 ALLOT  \ Allocate a safe memory buffer to store the generated password array/string


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\
\ user defined functions

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


CREATE input-buf 64 ALLOT  \ Protects data from terminal interpreter overwrite
\
: read_int_or_y ( -- n ok )
  input-buf 64 ACCEPT

  \ Guard: If the user typed absolutely nothing, exit as false
  dup 0= IF drop 0 false EXIT THEN

  \ Exact string match: check if the input is strictly a single lowercase 'y'
  input-buf over s" y" compare 0= IF
    drop                 \ Discard the input length 'u'
    n_char_default true  \ Push 12 and true to the stack
    EXIT                 \ Bypass number parsing entirely
  THEN

  \ integer number conversion logic:
  input-buf swap s>number?  ( d flag )
  IF
    drop
    DPL @ -1 = IF      \ Valid single-cell integer check
      true             ( n true )
    ELSE
      drop false
    THEN
  ELSE
    2drop false
  THEN ;

: input_a_valid_number ( n -- n )
  cr ." Password of " .
  ." printable chars OK? 'y' or another integer number >= 8: "
  read_int_or_y
  IF
    dup 8 >= IF
      \ cr ." input_a_valid_number: n_char = " dup . \ for testing
      EXIT  \ Success, leaves valid integer on the stack
    THEN
    drop    \ Discard parsed numbers that are too small (< 8)
  THEN
  \ Streamlined bad path loop:
  cr ." enter an integer number >= 8 or 'y'" cr
  n_char_default recurse ;


: answer_yes_or_no ( -- flag )
  cr cr ." Do you want me to use special characters like .;,+*... ? 'y' or 'n': "
  input-buf 64 ACCEPT      ( u )

  \ Check if the string length is exactly 1 AND matches lowercase 'y'
  dup 1 = IF
    drop                   \ Discard the string length
    input-buf c@ 121 = IF  \ 121 is the ASCII value for 'y'
      true EXIT            \ Return true (-1)
    THEN
  ELSE
    drop                   \ Discard lengths that are not equal to 1
  THEN
  false ;                  \ Return false (0) for anything else (like 'n' or accidental strings)


\ create the allowed set of password characters
\
\ pure Forth helper: uses data and return stack, absolutely zero local variables
: append-range ( u-start u-end c-addr -- c-addr-next )
    >R                         ( u-start u-end ) \ Save destination pointer to return stack
    1+ swap                    ( u-end+1 u-start )
    BEGIN
        2dup >                 ( u-end+1 u-start flag ) \ Is current_char < end_val+1?
    WHILE
        \ Strict Guard: Skip space (32) and tab (9) characters
        dup 32 <> over 9 <> and IF
            dup R@ c!          ( u-end+1 u-start )      \ Write current character to destination
            R> 1+ >R           ( u-end+1 u-start )      \ Advance destination pointer on return stack
        THEN
        1+                     ( u-end+1 u-start+1 )    \ Increment current character value
    REPEAT
    2drop R> ;                 ( c-addr-next )          \ Clean stack and return final pointer

\ VARIABLE charset_ptr  \ dead code

: set_char_set ( flag -- char-set-addr char-set-len )
    char_set_buffer                  ( char-set-addr ) \ Keep base address at bottom of stack
    swap IF
        \ 1. with_special_chars path: Full printable ASCII from 33 (!) to 126 (~)
        33 126 char_set_buffer append-range
    ELSE
        \ 2. without_special_chars path: Sequential ranges via stack rotation
        65 90  char_set_buffer append-range
        97 122 rot append-range
        48 57  rot append-range
    THEN                             ( char-set-addr final-ptr )
    \ Calculate length: final-ptr minus char-set-addr
    over -                           ( char-set-addr char-set-len ) ;


\ ********************  iterative password creation  ********************
\
\ doing it here iteratively is a big change from the Factor solution, which does it recursively
\
\ helper to check if a character exists within the char_pool string
: member_of_char_set? ( char pool-addr pool-len -- flag )
    \ CRITICAL FIX: Group into a single brace set to preserve standard Forth stack order.
    \ Stack incoming: char is deepest, pool-addr is middle, pool-len is top.
    { testing-char pool-addr pool-len }
    \ Guard: Instantly reject any byte value outside valid printable ASCII bounds
    testing-char 33 < testing-char 126 > or IF
        false EXIT
    THEN
    \ Pass parameters cleanly to Gforth's native string scan keyword
    pool-addr pool-len testing-char scan ( c-addr' u' )
    nip 0> ; \ Returns true only if it matches an item inside your pool string

VARIABLE pw_i
VARIABLE pw_j

: pw_generator ( length random_nbrs_ptr pool_addr pool_len -- pw-addr pw-len )
    { length random_nbrs_ptr pool_addr pool_len }
    \ Reset internal loop counters cleanly:
    0 pw_i !  \ mutable character counter
    0 pw_j !  \ mutable counter for random numbers index

    BEGIN
        pw_i @ length <
        pw_j @ END < and
    WHILE
        \ 1. Fetch next 16-bit random number from the heap array
        random_nbrs_ptr pw_j @ cells + @ ( bin0 )

        \ 2. Slice bin0 into two clean, positive 8-bit integers on the data stack
        dup 8 rshift 255 and             ( bin0 byte0 )
        swap 255 and                     ( byte0 byte1 )

        \ 3. Process the first byte (byte0) safely using MOD
        pool_len mod                     ( byte1 index0 )
        pool_addr + c@                   ( byte1 char0 )

        \ Write char0 straight to our password buffer
        pw_buffer pw_i @ + c!
        1 pw_i +!                        ( byte1 )

        \ 4. Process the second byte (byte1)
        pw_i @ length < IF
            pool_len mod                 ( index1 )
            pool_addr + c@               ( char1 )

            \ Write char1 straight to our password buffer
            pw_buffer pw_i @ + c!
            1 pw_i +!  \ Increment mutable pw_i
        ELSE
            drop  \ Discard byte1 if password length is already complete
        THEN

        \ 5. Securely advance to next random index
        1 pw_j +!
    REPEAT

    \ Return the password address-length string pair
    pw_buffer pw_i @ ;
\
\ end of password creation
\ ************************************************************************


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
    \ ALL LOCALS MUST BE DECLARED FIRST AT THE VERY TOP inside a SINGLE set of curly braces:
    { | n_char with_special_chars char_set_addr char_set_len pw_addr pw_len }
    \ The '|' character tells Gforth these are INTERNAL variables,
    \ preventing it from trying to pull the values off an empty data stack.

    allocate-large-buffers

    utime drop seed !  \ 2026-07-09: use microsecond clock as seed
                       \             to become more independent from the Forth implementation
                       \ utime returns two values!
    \ 2026-07-09: seed is initially too high: do this in main
    seed @ m 2 - mod 1 + seed !  \ limit initial seeds to 1 to m - 1 (both including)
    
    cr ." generating a random bit stream..." cr

    \ iterative masterloop:
    END 0 DO
        seed @ a * c + m mod dup seed !  \ seed has been duplicated!
        seed @ x I CELLS + ! ( u )  \ write seed to x
        \ cr cr ." seed = " seed @ .  \ for testing

        \ 1. Calculate destination address and save to global variable
        bits_x I STR_LENGTH_BIN * + bits_x_str !
        \ 2. Pass the seed (duplicated from stack) and the address to word integer_to_bin_string
        dup bits_x_str integer_to_bin_string
        \ 3. Print the string for debugging using its address and length
        \ cr bits_x_str  16 type  \ for testing

        \ 4. Handle hex string processing:
        bits_hex I STR_LENGTH_HEX * + bits_hex_str !
        dup bits_hex_str integer_to_hex_string
        \ cr bits_hex_str 4 type  \ for testing

        drop  \ Drop the remaining copy of seed
    LOOP

    \ cr x print-array-int cr  \ for testing

    \ build the final, big strings:
    build_bits_x_str_total
    build_bits_hex_str_total

    \ CR CR bits_x_str_total   M1   TYPE CR  \ for testing
    \ CR bits_hex_str_total K250 TYPE CR  \ for testing

    \ write bit stream to disk:
    bits_x_str_total   M1   file_bits_x   20 C" Bit"  COUNT write_to_file
    \ 2026-07-08: C" Bit"  COUNT is for using permanently
    \             dictionary-compiled counted strings unpacked via COUNT at runtime

    \ write byte stream to disk:
    bits_hex_str_total K250 file_bits_hex 21 C" Byte" COUNT write_to_file


    \ make a password of N_CHAR printable chars: user input requested here
    n_char_default input_a_valid_number to n_char
    \ cr ." main: n_char = " n_char . \ for testing

    answer_yes_or_no to with_special_chars
    \ cr ." main: with_special_chars = " with_special_chars .  \ for testing: false = 0, true = -1

    with_special_chars set_char_set to char_set_len to char_set_addr
    \ cr ." main: char_set = " char_set_addr char_set_len type  \ for testing: printing a GForth string

    n_char x char_set_addr char_set_len pw_generator to pw_len to pw_addr

    cr cr ." Your password of " n_char . ." characters is: " pw_addr pw_len type cr

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


\ end of random_bitstring_and_flexible_password_generator.fs
