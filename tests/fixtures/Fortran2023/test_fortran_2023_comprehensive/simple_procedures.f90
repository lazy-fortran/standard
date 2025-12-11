module simple_procedures_mod
  implicit none

contains

  simple function add_values(x, y) result(z)
    real, intent(in) :: x, y
    real :: z
    z = x + y
  end function add_values

  simple subroutine double_array(values, n)
    real, intent(inout) :: values(n)
    integer, intent(in) :: n
    integer :: idx
    do idx = 1, n
      values(idx) = values(idx) * 2.0
    end do
  end subroutine double_array

end module simple_procedures_mod
