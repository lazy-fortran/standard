! Test: INSTANTIATE statement without ONLY clause
! Reference: J3/24-107r1 INSTANTIATE syntax

program test_instantiate_no_only
    use swap_module
    implicit none

    ! Instantiation without ONLY clause - imports all template procedures
    instantiate swap_t(integer)
    instantiate swap_t(real(8))

    integer :: a, b
    real(8) :: x, y

    a = 1
    b = 2
    call swap(a, b)

    x = 1.0d0
    y = 2.0d0
    call swap(x, y)
end program test_instantiate_no_only
