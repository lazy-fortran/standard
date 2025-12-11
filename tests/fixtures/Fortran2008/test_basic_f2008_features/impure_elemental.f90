! Test Fortran 2008 IMPURE ELEMENTAL procedures (ISO/IEC 1539-1:2010 Section 12.7)
! F2008 adds IMPURE keyword to allow ELEMENTAL procedures with side effects.
! Before F2008, all ELEMENTAL procedures were implicitly PURE.

module impure_elemental_mod
    implicit none
    integer :: global_counter = 0

contains

    ! F2008: IMPURE ELEMENTAL function with side effects
    impure elemental function increment_and_add(x, y) result(z)
        real, intent(in) :: x, y
        real :: z

        ! Side effect: modify module variable
        global_counter = global_counter + 1

        z = x + y
    end function increment_and_add

    ! Explicit PURE ELEMENTAL (default for ELEMENTAL in F2008)
    pure elemental function pure_add(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x + y
    end function pure_add

    ! IMPURE ELEMENTAL subroutine
    impure elemental subroutine scale_and_log(x)
        real, intent(inout) :: x
        x = x * 2.0
        ! Could perform I/O here in F2008
    end subroutine scale_and_log

    ! Implicit PURE ELEMENTAL (no PURE keyword needed)
    elemental function implicit_pure(x) result(y)
        real, intent(in) :: x
        real :: y
        y = sqrt(x)
    end function implicit_pure

end module impure_elemental_mod

program test_impure_elemental
    use impure_elemental_mod
    implicit none
    real, dimension(3) :: a, b, c

    a = [1.0, 2.0, 3.0]
    b = [4.0, 5.0, 6.0]

    ! IMPURE ELEMENTAL function call
    c = increment_and_add(a, b)

    print *, 'Result:', c
    print *, 'Global counter:', global_counter

    ! PURE ELEMENTAL function call
    c = pure_add(a, b)

    ! IMPURE ELEMENTAL subroutine call
    call scale_and_log(a)

end program test_impure_elemental
