module advanced_f2003
    use iso_c_binding
    implicit none

    ! Abstract base class
    type, abstract :: drawable_t
        integer :: id
    contains
        procedure(draw_interface), deferred :: draw
        procedure :: get_id
        final :: cleanup_drawable
    end type drawable_t

    ! Concrete implementation
    type, extends(drawable_t) :: circle_t
        real :: radius
        real :: center(2)
    contains
        procedure :: draw => draw_circle
    end type circle_t

    ! Abstract interface
    abstract interface
        subroutine draw_interface(this)
            import :: drawable_t
            class(drawable_t), intent(in) :: this
        end subroutine draw_interface
    end interface

    ! Procedure pointer
    procedure(draw_interface), pointer :: draw_proc => null()

contains

    function get_id(this) result(id)
        class(drawable_t), intent(in) :: this
        integer :: id
        id = this%id
    end function get_id

    subroutine draw_circle(this)
        class(circle_t), intent(in) :: this

        associate(x => this%center(1), y => this%center(2), r => this%radius)
            print *, 'Drawing circle at (', x, ',', y, ') with radius', r
        end associate
    end subroutine draw_circle

    subroutine cleanup_drawable(this)
        type(drawable_t), intent(inout) :: this
        print *, 'Cleaning up drawable with ID:', this%id
    end subroutine cleanup_drawable

    subroutine demonstrate_features()
        type(circle_t), allocatable :: shapes(:)
        integer :: i
        real :: total_area

        ! Enhanced allocate with source
        allocate(shapes, source=[circle_t(1, 5.0, [0.0, 0.0]), &
                                circle_t(2, 3.0, [1.0, 1.0])])

        ! Polymorphic behavior
        do i = 1, size(shapes)
            call shapes(i)%draw()
        end do

        total_area = 0.0

        do i = 1, size(shapes)
            total_area = total_area + 3.14159 * shapes(i)%radius**2
        end do

        print *, 'Total area:', total_area

        deallocate(shapes)
    end subroutine demonstrate_features

end module advanced_f2003
