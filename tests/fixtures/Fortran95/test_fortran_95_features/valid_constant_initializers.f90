! Valid initialization expressions (constant expressions)
! ISO/IEC 1539-1:1997 Section 7.1.6 - constant expressions

program valid_initializers
  implicit none
  integer, parameter :: const = 5

  ! Integer constant initialization
  integer :: x = 42

  ! Real constant initialization
  real :: pi = 3.14159

  ! Logical constant initialization
  logical :: flag = .true.

  ! Character constant initialization
  character(len=5) :: name = "hello"

  ! Parameter constant (allowed in initialization)
  integer :: y = const

  ! Arithmetic on constants
  integer :: z = 10 + 20

  ! Negative constant
  integer :: neg = -100

end program valid_initializers
