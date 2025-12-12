! Valid KIND selector with integer literal
! ISO/IEC 1539:1991 Section 4.3.1 (R404): kind-selector is ( [KIND=] scalar-int-initialization-expr )
program test_kind_literal
  implicit none
  integer(kind=4) :: a
  integer(4) :: b
  real(kind=8) :: c
  real(8) :: d
  complex(kind=4) :: e
  logical(kind=1) :: f

  a = 1
  b = 2
  c = 3.14d0
  d = 2.71d0
  e = cmplx(1.0, 2.0)
  f = .true.
end program test_kind_literal
