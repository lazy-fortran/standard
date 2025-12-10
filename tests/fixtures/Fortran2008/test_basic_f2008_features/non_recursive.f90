! Test Fortran 2008 NON_RECURSIVE procedures (ISO/IEC 1539-1:2010 Section 12.6.2.2)
! Tests the NON_RECURSIVE keyword which explicitly marks procedures as non-recursive,
! enabling compiler optimizations and documenting intent.
!
! R1226: prefix-spec -> ... | RECURSIVE | NON_RECURSIVE

module non_recursive_mod
    implicit none
contains
    ! Explicit NON_RECURSIVE function
    non_recursive function factorial_iterative(n) result(f)
        integer, intent(in) :: n
        integer :: f, i

        f = 1
        do i = 2, n
            f = f * i
        end do
    end function factorial_iterative

    ! Combined with PURE
    pure non_recursive function square(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * x
    end function square

    ! Combined with ELEMENTAL
    non_recursive elemental function dble_it(x) result(y)
        real, intent(in) :: x
        real :: y
        y = 2.0 * x
    end function dble_it

    ! NON_RECURSIVE subroutine
    non_recursive subroutine initialize_array(arr, n)
        integer, intent(in) :: n
        real, intent(out) :: arr(n)
        integer :: i

        do i = 1, n
            arr(i) = real(i)
        end do
    end subroutine initialize_array

    ! Contrast: RECURSIVE function (pre-existing)
    recursive function factorial_recursive(n) result(f)
        integer, intent(in) :: n
        integer :: f

        if (n <= 1) then
            f = 1
        else
            f = n * factorial_recursive(n - 1)
        end if
    end function factorial_recursive

    ! Combined prefix: PURE NON_RECURSIVE ELEMENTAL
    pure non_recursive elemental function negate(x) result(y)
        real, intent(in) :: x
        real :: y
        y = -x
    end function negate
end module non_recursive_mod

program test_non_recursive
    use non_recursive_mod
    implicit none
    integer :: fact
    real :: x, y
    real, dimension(5) :: arr

    ! Test NON_RECURSIVE function
    fact = factorial_iterative(5)
    print *, 'Factorial(5) =', fact

    ! Test PURE NON_RECURSIVE
    x = 3.0
    y = square(x)
    print *, 'Square(3.0) =', y

    ! Test NON_RECURSIVE ELEMENTAL
    arr = dble_it([1.0, 2.0, 3.0, 4.0, 5.0])
    print *, 'Doubled:', arr

    ! Test NON_RECURSIVE subroutine
    call initialize_array(arr, 5)
    print *, 'Initialized:', arr
end program test_non_recursive
