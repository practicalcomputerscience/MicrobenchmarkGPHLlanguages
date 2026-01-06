! random_streams_for_perf_stats.f90
!
! 2026-01-05/06
!
! build on Ubuntu 24 LTS: $ gfortran -Wall -Wextra -fcheck=all random_streams_for_perf_stats.f90 -o random_streams_for_perf_stats
! run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
!
!
! see: $ gfortran --help
! see also: $ gfortran --help=optimizers
!
!
! $ gfortran --version
! GNU Fortran (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
! Copyright (C) 2023 Free Software Foundation, Inc.
! ...
! $
!
!
! The strings in the files are slightly different than with other programming languages: ...[LF]
! It seems to be practically impossible to get rid off the trailing [LF=line feed] character.


program random_streams_for_perf_stats
  implicit none  ! all variables will be explicitly declared;
                 ! without it variables will be implicitly typed according to the letter they begin with
                 !
                 ! Fortran code is case-insensitive <<<

  integer, parameter :: END = 62501  ! 62501 for exactly 1M binary digits
  ! integer, parameter :: END = 10  ! for testing

  integer, parameter :: M1   = END*16 - 16
  integer, parameter :: K250 = END*4 - 4


  integer, parameter :: m = 65521  ! = 2^16 - 15
  integer, parameter :: a = 17364
  integer, parameter :: c = 0
  ! from: https://statmath.wu.ac.at/software/src/prng-3.0.2/doc/prng.html/Table_LCG.html

  character(len=*), parameter :: file_bits_x   = "random_bitstring.bin"
  character(len=*), parameter :: file_bits_hex = "random_bitstring.byte"

  character(len=16) :: bits_x_str
  character(len=4)  :: bits_hex_str

  real :: ini_random_number  ! https://fortran-lang.org/learn/intrinsics/math/#random-number
  integer :: X0, i, byte_nbr

  integer :: x(END)  ! Arrays in Fortran are one-based by default

  character(:), allocatable :: bits_x, bits_hex, bits_hex_lower
  allocate(character(M1) :: bits_x)
  allocate(character(K250) :: bits_hex)
  allocate(character(K250) :: bits_hex_lower)


  !------------------------------------------------------------------------------------
  ! operations start here:
  call random_number(ini_random_number)
  ! returns a single pseudorandom number from the uniform distribution over the range 0 <= x < 1

  X0 = int(ini_random_number * (m - 1)) + 1
  ! print *, "X0 =", X0  ! for testing; * is for a formatting the compiler thinks is best
  x(1) = X0


  print "(/,A)", "generating a random bit stream..."
  ! print *, ".." prints a leading space char; /,A for two new lines
  ! see from: https://stackoverflow.com/questions/58143647/how-to-break-to-new-line-in-fortran-when-printing
  ! A is for arbitrary length string
  do i = 2, END
      x(i) = modulo(a*x(i-1) + c, m)  ! https://gcc.gnu.org/onlinedocs/gfortran/MODULO.html
      ! print "(A)"  ! for testing
      ! print *, "x(i) =", x(i)  ! for testing

      ! from Google AI:
      write(bits_x_str, '(B16.16)') x(i)  ! x(i) = 404 => bits_x_str = 0000000110010100
      ! print *, "bits_x_str = ", bits_x_str  ! for testing
      byte_nbr = (i-2)*16+1;  ! name byte_nbr from the C solution
      ! +1 for -fcheck=all
      ! without +1 =>
      ! Fortran runtime error: Substring out of bounds: lower bound (0) of 'bits_x' is less than one
      bits_x(byte_nbr:byte_nbr+15) = bits_x_str

      write(bits_hex_str, '(Z4.4)') x(i)  ! x(i) = 1004 => bits_hex_str = 03EC
      ! print *, "bits_hex_str = ", bits_hex_str  ! for testing
      byte_nbr = (i-2)*4+1;
      bits_hex(byte_nbr:byte_nbr+3) = bits_hex_str
  enddo

  ! lowercase bits_hex:
  bits_hex_lower = to_lower(bits_hex)

  ! print *, "bits_x = ", bits_x  ! for testing
  ! print *, "bits_hex = ", bits_hex  ! for testing
  ! print *, "bits_hex_lower = ", bits_hex_lower  ! for testing

  call write_to_file(file_bits_x, bits_x, "bit")
  call write_to_file(file_bits_hex, bits_hex_lower, "byte")

  deallocate(bits_x)
  deallocate(bits_hex)
  deallocate(bits_hex_lower)
  ! valgrind-3.27.0.GIT: HEAP SUMMARY: in use at exit: 40 bytes in 1 blocks
  ! => these deallocations have a very positive effect on heap usage at exit


  contains
  ! user defined functions
  !
  subroutine write_to_file(filename, content, file_type)
    character(len=*), intent(in) :: filename, content, file_type  ! Input arguments

    integer :: unit_num, io_status
    character(len=256) :: io_message

    ! from MS Bing AI:
    ! Assign a free unit number
    unit_num = 10
    ! Open file with error handling
    open(unit=unit_num, file=filename, status="replace", action="write", iostat=io_status, iomsg=io_message)

    if (io_status /= 0) then
        write(*, '(A)') trim(io_message)  ! "Cannot open file 'random_bitstring.byte': Permission denied"
    else
        ! Write string to file with error handling
        write(unit=unit_num, fmt='(A)', iostat=io_status, iomsg=io_message) content
        ! write(unit=unit_num, fmt='(A)', iostat=io_status, iomsg=io_message) "testing"  ! keep the '(A)' to start at position zero!
        ! however, the trailing LF char is still there!!!

        if (io_status /= 0) then
            write(*, '(A, A, A, A)') "could not write to file: ", filename, " -- ", trim(io_message)
        else
            if (file_type .eq. "bit") then
                write(*, '(A, A)') "Bit stream has been written to disk under name:  ", trim(filename)
            else
                write(*, '(A, A)') "Byte stream has been written to disk under name: ", trim(filename)
            end if
        end if

        ! Close file safely
        close(unit_num, iostat=io_status, iomsg=io_message)
        if (io_status /= 0) then
            write(*, '(A, A)') "error closing file: ", trim(io_message)
        end if
    end if
  end subroutine write_to_file


  ! Google AI:
  elemental function to_lower(str) result(res)
  ! marking the function as elemental or pure allows the compiler to optimize
  ! the loop for SIMD instructions or parallel execution
    character(len=*), intent(in) :: str
    character(len=len(str))      :: res
    integer :: i, ic
    res = str
    do i = 1, len(str)
        ic = iachar(str(i:i))
        ! Check if character is between 'A' (65) and 'Z' (90)
        if (ic >= 65 .and. ic <= 90) then
            res(i:i) = achar(ic + 32)
        end if
    end do
  end function to_lower
  !
  ! end of user defined functions

end program random_streams_for_perf_stats


! end of random_streams_for_perf_stats.f90
