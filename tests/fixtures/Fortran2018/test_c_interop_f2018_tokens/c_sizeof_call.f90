program test_c_sizeof
   use, intrinsic :: iso_c_binding
   implicit none

   integer(c_int) :: x
   integer(c_size_t) :: size_bytes

   ! C_SIZEOF can be used as a function name
   size_bytes = c_sizeof(x)
   print *, 'Size of c_int:', size_bytes

end program test_c_sizeof

