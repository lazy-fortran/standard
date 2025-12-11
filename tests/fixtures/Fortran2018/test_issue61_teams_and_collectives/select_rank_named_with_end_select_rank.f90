program select_rank_named_explicit
  implicit none
  integer :: rank_value

  my_rank_select: select rank (rank_value)
  rank (0)
     rank_value = 0
  rank (*)
     rank_value = -1
  rank default
     rank_value = -2
  end select rank my_rank_select
end program select_rank_named_explicit
