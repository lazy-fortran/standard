module teams_collectives
  implicit none
contains
  subroutine use_collective(x)
    real, intent(inout) :: x(:)
    call co_sum(x)
  end subroutine use_collective
end module teams_collectives

