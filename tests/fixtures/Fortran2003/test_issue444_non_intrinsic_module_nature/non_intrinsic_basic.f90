! Test Fortran 2003 NON_INTRINSIC module nature (ISO/IEC 1539-1:2004 R1110)
! Basic NON_INTRINSIC usage with a user-defined module

module my_utilities
    implicit none
    integer, parameter :: pi_approx = 314
contains
    subroutine print_pi()
        print *, "Pi approximation: ", pi_approx / 100.0
    end subroutine print_pi
end module my_utilities

program test_non_intrinsic_basic
    use, non_intrinsic :: my_utilities
    implicit none
    call print_pi()
end program test_non_intrinsic_basic
