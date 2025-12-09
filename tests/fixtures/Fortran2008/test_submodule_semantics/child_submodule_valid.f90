submodule(math_mod) math_impl
   implicit none
contains
   module subroutine calculate_result(x, y)
      real, intent(in) :: x
      real, intent(out) :: y
      y = x*2.0
   end subroutine calculate_result

   module function compute_value(n) result(val)
      integer, intent(in) :: n
      real :: val
      val = real(n)*3.14
   end function compute_value
end submodule math_impl
