program select_rank_demo
  implicit none
  integer :: rank_value

  select rank (rank_value)
  rank (0)
     rank_value = 0
  rank (*)
     rank_value = -1
  rank default
     rank_value = -2
  end select
end program select_rank_demo
