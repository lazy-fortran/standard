! Test: Inline instantiation with curly braces (J3 24-107r1)
! Reference: J3/24-107r1 - Inline instantiation syntax
!
! Syntax: template-name { instantiation-arg-list } (actual-args)
!         Instantiates and calls in one expression

program test_inline_curly
    implicit none

    integer :: a, b, c
    real :: x, y, z

    a = 1
    b = 2
    x = 1.0
    y = 2.0

    ! Inline instantiation with curly braces - subroutine call
    call swap{integer}(a, b)
    call swap{real}(x, y)

    ! Inline instantiation with curly braces - function call
    c = mysum{integer}(a, b)
    z = mysum{real}(x, y)

end program test_inline_curly
