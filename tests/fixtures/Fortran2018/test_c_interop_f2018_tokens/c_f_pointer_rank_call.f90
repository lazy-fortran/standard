program test_c_f_pointer_rank
   use, intrinsic :: iso_c_binding
   implicit none

   integer(c_int), target :: x(10)
   type(c_ptr) :: ptr
   integer :: ptr_rank

   ptr = c_loc(x)
   ! C_F_POINTER_RANK can be called as a function
   ptr_rank = c_f_pointer_rank(ptr)
   print *, 'Rank:', ptr_rank

end program test_c_f_pointer_rank

