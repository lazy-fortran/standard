! Fortran 95 enhancements program
! Tests Fortran 95 language compatibility

program fortran95_features
  implicit none

  ! Fortran 90/95 features
  integer, allocatable :: arr(:)
  integer :: i
  real :: x

  ! Allocate array
  allocate(arr(10))

  ! Fill array
  do i = 1, 10
    arr(i) = i
  end do

  ! Test doubling function
  x = double_value(2.0)

  ! Print results
  do i = 1, 10
    print *, "arr(", i, ") = ", arr(i)
  end do

  ! Deallocate
  deallocate(arr)

contains

  ! Function (Fortran 90+ feature)
  real function double_value(x)
    real, intent(in) :: x
    double_value = 2.0 * x
  end function double_value

end program fortran95_features
