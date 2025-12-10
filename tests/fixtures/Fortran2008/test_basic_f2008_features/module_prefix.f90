! Test Fortran 2008 MODULE prefix combined with other prefixes (ISO/IEC 1539-1:2010 R1226)
! Tests the MODULE keyword as a prefix-spec that can be combined with PURE, ELEMENTAL, etc.
! in submodule separate module procedure definitions.

module parent_module
    implicit none
end module parent_module

! Submodule with separate module procedure definitions using MODULE prefix with PURE/ELEMENTAL
! R1226: MODULE as prefix-spec combined with PURE, ELEMENTAL, NON_RECURSIVE, IMPURE
submodule (parent_module) implementation
contains
    ! R1226: Separate module function with MODULE and PURE prefixes
    module pure function compute(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * 2.0
    end function compute

    ! R1226: Separate module subroutine with MODULE prefix
    module subroutine helper_sub()
    end subroutine helper_sub

    ! R1226: MODULE with ELEMENTAL prefix
    module elemental subroutine scale(x, factor)
        real, intent(inout) :: x
        real, intent(in) :: factor
        x = x * factor
    end subroutine scale

    ! R1226: MODULE with pure and elemental combined
    module pure elemental function double(n) result(m)
        integer, intent(in) :: n
        integer :: m
        m = n * 2
    end function double
end submodule implementation
