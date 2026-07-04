! factorial.factor
!
! for stack-oriented language Factor
!
! 2026-07-02
!
! run on Ubuntu 24 LTS:  $ factor factorial.factor
!                        Enter an integer n >= 1:
!                        Wrong input!  # user just presses [ENTER]
!
!                        Enter an integer n >= 1: 43
!                        factorial(43) = 60415263063373835637355132068513997507264512000000000
!                        $
!
! $ factor -version
! Factor 0.102 x86.64 (2301, heads/master-8e54de841b, Feb 11 2026 19:39:11)
! [GCC 11.4.0] on linux
! $

USING: io kernel math math.parser prettyprint ranges sequences ;  ! USING: declares external vocabularies to borrow tools from

IN: factorial_with_user_input  ! defines the current vocabulary (home namespace) where all subsequent words created will live

! see https://docs.factorcode.org/content/article-tour-first-word.html
: prod ( {x1,...,xn} -- x1*...*xn ) 1 [ * ] reduce ;  ! (..) documents the stack effect
: fact ( n -- n! ) [1..b] prod ;

! mostly Google AI:
: factorial_with_user_input ( -- )
    "Enter an integer n >= 1: " write flush  ! USER INPUT FROM THE TERMINAL
    readln [                                 ! Reads line from stdin and pushes string to stack
                                             ! https://docs.factorcode.org/content/vocab-io.html
        string>number                        ! Try to convert string to number
        dup integer? [                       ! Check if integer
            dup 1 >=                         ! Check if the integer is >= 1
        ] [ f ] if [
            "factorial(" write               ! If true: calculate factorial and print
            dup number>string write
            ") = " write
            fact number>string print         ! Ends the line with print
        ] [
            drop                             ! If false: drop the invalid number/f
            "Wrong input!\n" print
            factorial_with_user_input       ! Loop: call itself to ask again
        ] if
    ] [ "No input received (EOF)." print
        factorial_with_user_input           ! Loop: call itself if user just pressed Enter
    ] if* ;  ! if*: alternative conditional form that preserves the cond value if it is true.    


MAIN: factorial_with_user_input ! MAIN: declares the entry point

! end of factorial.factor
