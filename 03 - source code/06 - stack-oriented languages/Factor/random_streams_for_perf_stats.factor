! random_streams_for_perf_stats.factor
!
! for stack-oriented language Factor
!
! 2026-07-02/03/04
!
! run on Ubuntu 24 LTS:   $ factor random_streams_for_perf_stats.factor
!                         $ time factor random_streams_for_perf_stats.factor => real	0m0.191s
!
! build on Ubuntu 24 LTS: $ mkdir -p ./random_streams_for_perf_stats  # -p: no error if existing, make parent directories as needed
!                         $ cp ./random_streams_for_perf_stats.factor ./random_streams_for_perf_stats
!                         $ factor -e='"extra" "~/scripts/Factor" add-vocab-root "random_streams_for_perf_stats" deploy'
!                         # this building takes some time!
!                         ...
!                         Saving final image
!
!                         --- Data stack:
!                         "extra"
!                         $
! run on Ubuntu 24 LTS:  $ ./factor-linux-x86-64-2026-02-11-19-38/factor/random_streams_for_perf_stats/random_streams_for_perf_stats.out
!                        $ time ./factor-linux-x86-64-2026-02-11-19-38/factor/random_streams_for_perf_stats/random_streams_for_perf_stats.out
!                          => real	0m0.059s
!
!
! $ factor -version
! Factor 0.102 x86.64 (2301, heads/master-8e54de841b, Feb 11 2026 19:39:11)
! [GCC 11.4.0] on linux
! $
!
! developed with lots of help from Google AI, MS Bing AI


USING: accessors arrays continuations debugger io io.encodings.utf8 io.files kernel locals
       math math.intervals math.parser prettyprint random ranges sequences strings vectors ;

IN: random_streams_for_perf_stats


: END ( -- END ) 62501 ; inline  ! 62501 for exactly 1M binary digits
! : END ( -- END ) 10 ; inline  ! for testing

: m ( -- m ) 65521 ; inline  ! = 2^16 - 15
: a ( -- a ) 17364 ; inline
: c ( -- c ) 0 ; inline

: file_bits_x   ( -- file_bits_x )   "random_bitstring.bin" ; inline
: file_bits_hex ( -- file_bits_hex ) "random_bitstring.byte" ; inline

: seed ( -- seed ) m 1 - random 1 + ; inline


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

            ! extracted from raw error:
            !   T{ unix-system-call-error
            !       { args
            !           {
            !               "~/scripts/Factor/random_bitstring.bin"
            !               577
            !               438
            !           }
            !       }
            !       { errno 13 }
            !       { message "Permission denied" }
            !       { word open }
            !   }

            drop  ! drop the error object to balance stack
        ] recover
    ] ;


! end of user defined functions
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


: random_streams_for_perf_stats ( -- )

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

    ] ;


MAIN: random_streams_for_perf_stats

! end of random_streams_for_perf_stats.factor
