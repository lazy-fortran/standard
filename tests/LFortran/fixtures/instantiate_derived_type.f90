! Test: INSTANTIATE statement with derived type parameter
! Reference: J3/24-107r1 INSTANTIATE syntax
! Note: Uses curly braces {} for instantiation arguments per J3 spec

program test_instantiate_derived
    use container_module
    implicit none

    type :: point
        real :: x, y
    end type point

    ! Instantiation with derived type
    instantiate container_t{type(point)}, only: point_container => container

    type(point_container) :: pc
    type(point) :: p1, p2

    p1%x = 1.0
    p1%y = 2.0

    p2%x = 3.0
    p2%y = 4.0

    call add_item(pc, p1)
    call add_item(pc, p2)
end program test_instantiate_derived
