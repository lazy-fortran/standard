program select_rank_associate_demo
  implicit none

  ! Assumed-rank dummy array
  integer, allocatable :: arr(:)

  allocate(arr(10))

  ! Test SELECT RANK with associate-name
  select rank (assoc => arr)
  rank (0)
    ! assoc is scalar here
    print *, "Scalar case: ", assoc
  rank (1)
    ! assoc is rank-1 array here
    print *, "Rank-1 case: size = ", size(assoc)
  rank (*)
    ! Assumed rank
    print *, "Assumed rank case"
  rank default
    print *, "Higher rank case"
  end select

  deallocate(arr)
end program select_rank_associate_demo
