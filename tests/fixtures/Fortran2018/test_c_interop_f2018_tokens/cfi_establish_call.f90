program test_cfi_establish
   use, intrinsic :: iso_c_binding
   implicit none

   integer(c_int), target :: arr(10, 20)
   type(c_ptr) :: cfi_ptr

   ! CFI_ESTABLISH can be used as a subroutine name
   ! Simplified to use intrinsic types/values
   call cfi_establish(cfi_ptr, c_loc(arr))

end program test_cfi_establish


