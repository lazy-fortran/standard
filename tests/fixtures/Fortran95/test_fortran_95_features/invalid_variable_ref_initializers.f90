! Valid initialization expressions for reference
! NOTE: To test SEMANTIC violations of Section 7.1.6, a future semantic
! analyzer is needed. This fixture demonstrates syntactically valid code.

program valid_syntax
  implicit none
  integer :: n = 10
  integer :: m = 20

  ! Valid constant expressions
  integer :: x = 1
  integer :: y = 5
  real :: r = 2.5

end program valid_syntax
