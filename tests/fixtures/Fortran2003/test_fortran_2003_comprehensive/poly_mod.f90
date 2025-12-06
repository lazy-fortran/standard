module poly_mod
  implicit none

  type, abstract :: shape_t
  contains
    procedure(shape_draw), deferred :: draw
  end type shape_t

  type, extends(shape_t) :: circle_t
    real :: radius
  contains
    procedure :: draw => draw_circle
  end type circle_t

  abstract interface
    subroutine shape_draw(this)
      import :: shape_t
      class(shape_t), intent(in) :: this
    end subroutine shape_draw
  end interface

contains

  subroutine draw_circle(this)
    class(circle_t), intent(in) :: this
    print *, 'Circle radius', this%radius
  end subroutine draw_circle

  subroutine render(s)
    class(shape_t), intent(in) :: s

    select type (s)
    type is (circle_t)
      call s%draw()
    class default
      print *, 'Unknown shape'
    end select
  end subroutine render

end module poly_mod

