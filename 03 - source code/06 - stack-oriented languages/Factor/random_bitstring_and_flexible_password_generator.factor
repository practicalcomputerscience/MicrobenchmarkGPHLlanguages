! random_bitstring_and_flexible_password_generator.factor
!
! for stack-oriented language Factor: this code can be safely compiled into a standalone executable
!                                     because it's not using regular expressions.
!
! 2026-07-05
!
! run on Ubuntu 24 LTS:   $ factor random_bitstring_and_flexible_password_generator.factor
!
! build on Ubuntu 24 LTS: $ mkdir -p ./random_bitstring_and_flexible_password_generator  # -p: no error if existing, make parent directories as needed
!                         $ cp ./random_bitstring_and_flexible_password_generator.factor ./random_bitstring_and_flexible_password_generator
!                         $ factor -e='"extra" "~/scripts/Factor" add-vocab-root "random_bitstring_and_flexible_password_generator" deploy'
!                         # this building takes some time!
!                         ...
!                         Saving final image
!
!                         --- Data stack:
!                         "extra"
!                         $
! run on Ubuntu 24 LTS:  $ ./factor-linux-x86-64-2026-02-11-19-38/factor/random_bitstring_and_flexible_password_generator/random_bitstring_and_flexible_password_generator.out
!
!
! $ factor -version
! Factor 0.102 x86.64 (2301, heads/master-8e54de841b, Feb 11 2026 19:39:11)
! [GCC 11.4.0] on linux
! $
!
! developed with lots of help from Google AI, MS Bing AI, Duck.ai


USING: accessors arrays ascii continuations debugger io io.encodings.utf8 io.files kernel locals
       math math.intervals math.parser prettyprint random ranges regexp sequences splitting
       strings system vectors ;


IN: random_bitstring_and_flexible_password_generator


: END ( -- END ) 62501 ; inline  ! 62501 for exactly 1M binary digits
! : END ( -- END ) 100 ; inline  ! for testing

: m ( -- m ) 65521 ; inline  ! = 2^16 - 15
: a ( -- a ) 17364 ; inline
: c ( -- c ) 0 ; inline

: file_bits_x   ( -- file_bits_x )   "random_bitstring.bin" ; inline
: file_bits_hex ( -- file_bits_hex ) "random_bitstring.byte" ; inline

: seed ( -- seed ) m 1 - random 1 + ; inline

: n_char_default ( -- n_char_default ) 12 ; inline


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! user defined functions

! :: is for using locals, which makes the code dramatically easier to read and maintain
! :: only creates placeholders in the word signature, not things on the stack!
:: masterloop ( start stop initial_seed -- x_array bits_x_array bits_hex_array )
    [let
        ! 1. Initialize the vector accumulators inside a single local scope
        END <vector> :> x_accum
        END <vector> :> bits_x_accum
        END <vector> :> bits_hex_accum
        initial_seed :> seed_var!  ! Mark seed variable as mutable with character !

        ! 2. Calculate exactly how many iterations are needed
        stop start - [
            ! Calculate the next seed
            a seed_var * c + m mod seed_var!  ! Update the mutable seed

            ! Convert next seed to binary string and pad to 16 chars
            seed_var 2 >base 16 CHAR: 0 pad-head :> bits_x_str

            ! Convert next seed to hex string and pad to 4 chars
            seed_var 16 >base 4 CHAR: 0 pad-head :> bits_hex_str

            ! Push entries into our accumulator vectors
            seed_var     x_accum        push
            bits_x_str   bits_x_accum   push
            bits_hex_str bits_hex_accum push
        ] times

        ! 3. Return the populated accumulators as fixed arrays
        x_accum        >array
        bits_x_accum   >array
        bits_hex_accum >array
    ] ;



! very big problems here: "The input quotations to 'recover' do not all leave the stack at the same height"
! then I found a solution from here: https://github.com/factor/factor/blob/main/basis/ftp/server/server.factor
! see at: [ ... ] [ 3drop "File transfer failed" ftp-error ] recover ;
!
:: write_to_file ( path string file_type -- )
    [let
        path :> p
        string :> s
        file_type :> ft
        [
            ! --- success path ---
            s p utf8 <file-writer>          ! open file for writing
            [ write ] with-output-stream    ! write the string
            ft write " stream has been written to disk under name: " write p print
        ]
        [
            ! --- error path ---
            ! Stack layout here: error-obj (locals p and s are handled by let)
            "could not write to file: " write p write " ! -- " write

            ! Extract the failing system word slot (e.g., 'open')
            dup word>> name>> "Unix system call '" write write "' failed and " write

            ! Extract the message and errno slots (e.g., 'Permission denied' (13))
            dup message>> write " (" write
            dup errno>> number>string write ")" print

            drop  ! drop the error object to balance stack
        ] recover
    ] ;


! helper word for input_a_valid_number: Google AI
: is_all_digits? ( str -- ? )
    [ digit? ] all? ;

! see at factorial.factor, plus help from Duck.ai and Google AI:
: input_a_valid_number ( n_char -- new_n_char )
    "Password of " write
    dup number>string write
    " printable chars OK? 'y' or another integer number >= 8: " write flush

    ! see from: factorial.factor
    readln [
        dup "y" = [
            drop  ! Drop the "y" string, leaving input n_char on the stack
        ] [
            ! Strict Guard: Check if the raw string contains only pure digits (0-9)

            dup is_all_digits? [
                string>number    ! Try to convert string to number: 8,0 wrongly evaluates to 80!
                dup integer? [   ! Check if integer
                    dup 8 >=     ! Check if the integer is >= 1
                ] [
                    f            ! error branch
                ] if [
                    ! the only success branch: drop the old default n_char and keep the new valid number:
                    nip
                ] [
                    drop drop                             ! If false: drop the old default n_char and the invalid number/f
                    "enter an integer number >= 8 or 'y'\n" print
                    n_char_default input_a_valid_number  ! Loop: call itself if user just pressed Enter
                ] if
            ] [
                drop drop
                "enter an integer number >= 8 or 'y'\n" print
                n_char_default input_a_valid_number  ! Loop: call itself if user just pressed Enter
            ] if
        ] if
    ] [
        drop  ! Clear the false token from an empty readln
        "enter an integer number >= 8 or 'y'\n" print
        n_char_default input_a_valid_number  ! Loop: call itself if user just pressed Enter
    ] if* ;  ! if*: alternative conditional form that preserves the cond value if it is true.


: answer_yes_or_no ( -- with_special_chars )
    "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': " write flush
    readln [
        "y" = [
          t  ! If true: return true (t)
        ] [
          f  ! If false: return false (f)
        ] if
    ] [
        f  ! return false if user just pressed Enter. Stack is empty.
    ] if* ;


! ********************  recursive password creation  ******************** !
!
:: pw_generator ( length random_nbrs char_pool -- pw_array )
    [let
        length <vector> :> pw_accum
        0               :> i!  ! mutable char counter for the password
        0               :> j!  ! mutable counter for x

        [ i length < ] [  ! do as long as i < length
            j random_nbrs nth 2 >base :> bin0_
            bin0_ 16 CHAR: 0 pad-head :> bin0
            ! nl  ! for testing
            ! bin0 print  ! for testing

            bin0 8 head :> bin0_0
            bin0 8 tail :> bin0_1
            ! bin0_0 print  ! for testing
            ! bin0_1 print  ! for testing

            bin0_0 2 base> 1string :> char0_0
            bin0_1 2 base> 1string :> char0_1
            ! char0_0 print  ! for testing
            ! char0_1 print  ! for testing

            char0_0 first char_pool member? [
                char0_0 pw_accum push
                i 1 + i!
            ] when

            char0_1 first char_pool member? [
                i length < [
                    char0_1 pw_accum push
                    i 1 + i!
                ] when
            ] when

            j 1 + j!
        ] while  ! idea from Google AI instead of if-then-else:
                 ! An if statement can only run once. Because your true branch runs once
                 ! and modifies variables, while your false branch immediately finishes and
                 ! leaves pw_accum >array on the stack, the Factor compiler detects that
                 ! the two paths leave a completely different number of items on the data stack.

        ! loop finished. Return the populated password accumulator as a fixed array:
        pw_accum   >array
    ] ;
!
! end of password creation
! ************************************************************************ !


! end of user defined functions
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


: random_bitstring_and_flexible_password_generator ( -- )

    "\ngenerating a random bit stream...\n" write

    [let  ! open a local variable scope block
        seed :> ini_seed  ! freeze the first random seed; do not generate more seeds

        ! "\nseed = " write ini_seed number>string print  ! for testing

        1 END ini_seed masterloop :> ( x bits_x bits_hex )

        ! nl x .  ! for testing

        bits_x concat :> bits_x_str_total
        ! bits_x_str_total print  ! for testing

        bits_hex concat :> bits_hex_str_total
        ! bits_hex_str_total print  ! for testing

        ! write bit stream to disk:
        file_bits_x bits_x_str_total "Bit" write_to_file

        ! write byte stream to disk:
        file_bits_hex bits_hex_str_total "Byte" write_to_file

        nl


        ! make a password of N_CHAR printable chars: user input requested here
        n_char_default input_a_valid_number :> n_char
        ! n_char number>string print  ! for testing

        answer_yes_or_no :> with_special_chars
        ! with_special_chars .  ! for testing

        with_special_chars [
            33 126 [a..b] >string  ! MS Bing AI: this produces the full printable ASCII range from ! to ~
        ] [
            65 90 [a..b]  >string         ! uppercase A–Z
            97 122 [a..b] >string append  ! lowercase a–z
            48 57 [a..b]  >string append  ! digits 0–9
        ] if :> char_set
        ! char_set .  ! for testing

        n_char x char_set pw_generator :> pw_chars_  ! pw_chars_ is still an array of chars
        pw_chars_ concat :> pw_chars

        "\nYour password of " write n_char number>string write " characters is: " write pw_chars print
    ] ;


MAIN: random_bitstring_and_flexible_password_generator

! end of random_bitstring_and_flexible_password_generator.factor
