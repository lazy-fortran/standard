program sync_team_example
  implicit none
  type(team_type) :: my_team
  integer :: ierr
  character(len=32) :: errmsg

  sync team(my_team)
  sync team(my_team, stat=ierr, errmsg=errmsg)
end program sync_team_example
