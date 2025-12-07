module c_interfaces
  use iso_c_binding
  implicit none

  interface
    subroutine c_axpy(n, a, x, y) bind(c, name="c_axpy")
      import :: c_int, c_double
      integer(c_int),  value        :: n
      real(c_double),  value        :: a
      real(c_double)                :: x(n), y(n)
    end subroutine c_axpy
  end interface

end module c_interfaces

