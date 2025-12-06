module shapes_mod
    implicit none

    ! Abstract base type
    type, abstract :: shape_t
        real :: area
    contains
        procedure(calculate_area), deferred :: calculate_area
        procedure :: print_info
    end type shape_t

    ! Concrete derived type with inheritance
    type, extends(shape_t) :: circle_t
        real :: radius
    contains
        procedure :: calculate_area => circle_calculate_area
    end type circle_t

    ! Abstract interface
    abstract interface
        function calculate_area(this) result(area)
            import :: shape_t
            class(shape_t), intent(in) :: this
            real :: area
        end function calculate_area
    end interface

contains

    subroutine print_info(this)
        class(shape_t), intent(in) :: this
        print *, 'Area:', this%area
    end subroutine print_info

    function circle_calculate_area(this) result(area)
        class(circle_t), intent(in) :: this
        real :: area
        area = 3.14159 * this%radius**2
    end function circle_calculate_area

end module shapes_mod

