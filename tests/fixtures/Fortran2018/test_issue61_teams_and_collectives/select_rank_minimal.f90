program select_rank_demo
  implicit none
  integer :: rank_value

  ! Minimal SELECT RANK construct to document current behavior.
  select rank (rank_value)
  end select
end program select_rank_demo

