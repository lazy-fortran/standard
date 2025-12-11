! Valid initialization expressions for reference
! NOTE: To test SEMANTIC violations of function calls in Section 7.1.6,
! a future semantic analyzer is needed. Only certain intrinsic functions
! are allowed in constant expressions (Section 7.1.6.1).
! This fixture demonstrates syntactically valid code.

program valid_function_initialization
  implicit none

  ! Valid - constant literals
  integer :: x = 42
  integer :: y = -100

contains

  integer function compute()
    compute = 42
  end function

end program valid_function_initialization
