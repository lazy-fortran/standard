submodule(math_mod:math_impl) math_extras
implicit none
contains
module subroutine extra_calc()
   print *, 'Extra calculation'
end subroutine extra_calc
end submodule math_extras
