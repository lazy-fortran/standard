! ISO/IEC 1539-1:2004 Section 13.7.80 - MOVE_ALLOC intrinsic subroutine
! MOVE_ALLOC with polymorphic types (F2003 OOP)
module move_alloc_poly_mod
    implicit none

    type :: base_type
        integer :: value
    end type base_type

contains
    subroutine swap_polymorphic(obj1, obj2)
        class(base_type), allocatable, intent(inout) :: obj1
        class(base_type), allocatable, intent(out) :: obj2

        call move_alloc(obj1, obj2)
    end subroutine swap_polymorphic
end module move_alloc_poly_mod
