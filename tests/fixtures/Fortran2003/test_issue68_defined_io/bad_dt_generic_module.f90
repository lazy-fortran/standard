module bad_dt_generic
  implicit none

  interface write(dt=point)
     module procedure write_point
  end interface

contains

  subroutine write_point(x)
    real, intent(in) :: x
  end subroutine write_point

end module bad_dt_generic

