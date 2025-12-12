! INVALID: CHARACTER KIND selector with variable (non-constant expression)
! ISO/IEC 1539:1991 Section 4.3.2 (R406): kind-selector requires scalar-int-initialization-expr
! This should produce E674-003 error
program test_char_invalid_kind_var
  implicit none
  integer :: k
  k = 1
  ! ERROR: KIND selector requires constant expression, not variable
  character(len=10, kind=k) :: str
  str = "test"
end program test_char_invalid_kind_var
