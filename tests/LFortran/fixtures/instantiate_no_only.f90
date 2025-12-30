! Test: INSTANTIATE statement without ONLY clause
! Reference: J3/24-107r1 INSTANTIATE syntax
! Note: Uses curly braces {} for instantiation arguments per J3 spec

program test_instantiate_no_only
    use swap_module
    implicit none

    ! Instantiation without ONLY clause - imports all template procedures
    instantiate swap_t{integer}
    instantiate swap_t{real}

    integer :: a, b
    real :: x, y

    a = 1
    b = 2
    call swap(a, b)

    x = 1.0
    y = 2.0
    call swap(x, y)
end program test_instantiate_no_only
