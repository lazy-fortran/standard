! INVALID: KIND selector with non-constant expression
! ISO/IEC 1539:1991 Section 4.3.1 (R404): kind-selector requires scalar-int-initialization-expr
! This should produce E674-001 error
program test_kind_invalid_expr
  implicit none
  ! ERROR: KIND selector requires constant expression, not computed value
  integer(kind=4+2) :: a
  a = 1
end program test_kind_invalid_expr
