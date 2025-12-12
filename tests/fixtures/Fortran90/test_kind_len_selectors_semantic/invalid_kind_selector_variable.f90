! INVALID: KIND selector with variable (non-constant expression)
! ISO/IEC 1539:1991 Section 4.3.1 (R404): kind-selector requires scalar-int-initialization-expr
! This should produce E674-001 error
program test_kind_invalid_var
  implicit none
  integer :: k
  k = 4
  ! ERROR: KIND selector requires constant expression, not variable
  integer(kind=k) :: a
  a = 1
end program test_kind_invalid_var
