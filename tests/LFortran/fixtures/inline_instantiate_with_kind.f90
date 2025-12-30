! Test: Inline instantiation with kind specifiers
! Reference: J3/24-107r1 - Type specifications as instantiation arguments

program test_inline_kind
    implicit none

    real(8) :: x, y, z
    real(4) :: a, b, c

    x = 1.0d0
    y = 2.0d0
    a = 1.0
    b = 2.0

    ! Curly brace syntax with kind
    call swap{real(8)}(x, y)
    c = mysum{real(4)}(a, b)

    ! Caret syntax with kind
    call swap^(real(8))(x, y)
    z = mysum^(real(8))(x, y)

end program test_inline_kind
