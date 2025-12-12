! Valid CHARACTER selector with literal constants
! ISO/IEC 1539:1991 Section 4.3.2 (R406): char-selector is ( [LEN=] length-selector ... )
program test_char_valid
  implicit none
  character(len=10) :: str1
  character(10) :: str2
  character(len=20, kind=1) :: str3

  str1 = "hello"
  str2 = "world"
  str3 = "fortran"
end program test_char_valid
