module math_mod
   implicit none

   interface
      subroutine calculate_result(x, y)
         real, intent(in) :: x
         real, intent(out) :: y
      end subroutine calculate_result

      function compute_value(n) result(val)
         integer, intent(in) :: n
         real :: val
      end function compute_value
   end interface

end module math_mod
