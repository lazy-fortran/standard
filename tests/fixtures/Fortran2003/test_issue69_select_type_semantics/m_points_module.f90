module m_points
  implicit none

  type :: point_t
    real :: x, y
  end type point_t

contains

  subroutine handle_point(obj)
    class(*), intent(in) :: obj

    select type (p => obj)
    type is (point_t)
      print *, 'point:', p%x, p%y
    class default
      print *, 'not a point'
    end select

  end subroutine handle_point

end module m_points

