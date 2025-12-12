! Valid: assumed-shape array in dummy argument
! ISO/IEC 1539:1991 (WG5 N692) Section 5.1.2.4
! Assumed-shape arrays (with :) can only appear in dummy arguments
!
! This fixture demonstrates correct usage of assumed-shape specification
! in procedure dummy arguments, allowing flexible array size passing.

program test_assumed_shape_valid
  implicit none
  integer :: arr(50)

  call process_array(arr)

contains

  subroutine process_array(x)
    integer, dimension(:), intent(inout) :: x
    x(1) = 42
    x(size(x)) = 99
  end subroutine process_array

end program test_assumed_shape_valid
