! INVALID: CHARACTER LEN selector with variable (non-constant expression)
! ISO/IEC 1539:1991 Section 4.3.2 (R406): length-selector requires initialization-expr
! This should produce E674-002 error
program test_char_invalid_len_var
  implicit none
  integer :: n
  n = 10
  ! ERROR: LEN selector requires constant expression, not variable
  character(len=n) :: str
  str = "test"
end program test_char_invalid_len_var
