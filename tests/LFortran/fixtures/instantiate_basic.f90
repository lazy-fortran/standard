! Test: INSTANTIATE statement - basic usage
! Reference: J3/24-107r1 INSTANTIATE syntax

program test_instantiate
    use swap_module
    implicit none

    ! Explicit instantiation with rename
    instantiate swap_t(integer), only: swap_int => swap
    instantiate swap_t(real(8)), only: swap_real => swap

    integer :: a, b
    real(8) :: x, y

    a = 1
    b = 2
    call swap_int(a, b)

    x = 1.0d0
    y = 2.0d0
    call swap_real(x, y)
end program test_instantiate
