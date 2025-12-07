program teams_collectives
  implicit none
  real :: x
  integer :: ierr
  character(len=80) :: msg
  integer :: result_image_var

  interface
    subroutine reducer(x)
      real, intent(inout) :: x
    end subroutine reducer
  end interface

  call co_sum(x)
  call co_min(x, stat = ierr)
  call co_max(x, stat = ierr, errmsg = msg)
  call co_broadcast(x, source_image = 1, stat = ierr)
  call co_reduce(x, reducer, stat = ierr)
end program teams_collectives
