program test_cfi_setpointer
   use, intrinsic :: iso_c_binding
   implicit none

   integer(c_int), target :: x
   type(c_ptr) :: cfi_desc, new_ptr

   new_ptr = c_loc(x)

   ! CFI_SETPOINTER can be used as a subroutine name
   call cfi_setpointer(cfi_desc, new_ptr)

end program test_cfi_setpointer

