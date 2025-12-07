module c_api
  use iso_c_binding
  implicit none

contains

  ! Function with BIND(C) and NAME=
  integer(c_int) function add(a, b) bind(c, name="c_add")
    integer(c_int), value :: a, b
    add = a + b
  end function add

  ! Subroutine with BIND(C) and NAME=
  subroutine scale_array(n, x, factor) bind(c, name="c_scale_array")
    integer(c_int), value        :: n
    real(c_double)               :: x(n)
    real(c_double), value        :: factor
  end subroutine scale_array

  ! Function with plain BIND(C)
  real(c_double) function length(x, y) bind(c)
    real(c_double), value :: x, y
    length = sqrt(x*x + y*y)
  end function length

end module c_api

