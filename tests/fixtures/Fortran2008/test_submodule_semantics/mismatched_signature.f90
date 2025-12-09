submodule(math_mod) wrong_impl
   implicit none
contains
   module function calculate_result(x, y)
      real, intent(in) :: x
      real, intent(out) :: y
      real :: calculate_result
      y = x*2.0
      calculate_result = y
   end function calculate_result
end submodule wrong_impl
