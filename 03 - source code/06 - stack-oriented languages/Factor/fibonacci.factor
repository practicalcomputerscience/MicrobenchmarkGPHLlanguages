! fibonacci.factor
!
! for stack-oriented language Factor
!
! 2026-07-02
!
! run on Ubuntu 24 LTS:  $ time factor fibonacci.factor
!                        2971215073
!
!                        real	0m35.218s
!                        ...
!                        $
!
! $ factor -version
! Factor 0.102 x86.64 (2301, heads/master-8e54de841b, Feb 11 2026 19:39:11)
! [GCC 11.4.0] on linux
! $

! USING: io kernel math prettyprint tools.time ;  ! time from tools.time prints big tail information. I do not need that.
USING: io kernel math prettyprint ;  ! USING: declares external vocabularies to borrow tools from

IN: fibonacci  ! defines the current vocabulary (home namespace) where all subsequent words created will live

DEFER: fib-rec  ! DEFER: to define two mutually recursive words:
: fib ( n -- f(n) ) dup 2 < [ ] [ fib-rec ] if ;
: fib-rec ( n -- f(n) ) [ 1 - fib ] [ 2 - fib ] bi + ;

MAIN: [ 47 fib . ]  ! MAIN: declares the entry point

! end of fibonacci.factor
