program test_c_interop_combined
   use, intrinsic :: iso_c_binding
   implicit none

   integer(c_int), target :: arr(10)
   type(c_ptr) :: ptr
   integer(c_size_t) :: size_bytes
   integer :: ptr_rank

   ptr = c_loc(arr)

   ! All C interop tokens used as function/subroutine names
   size_bytes = c_sizeof(arr)
   ptr_rank = c_f_pointer_rank(ptr)

   ! CFI procedures can also be used
   call cfi_setpointer(ptr, c_loc(arr))

   print *, 'Size:', size_bytes, 'Rank:', ptr_rank

end program test_c_interop_combined

