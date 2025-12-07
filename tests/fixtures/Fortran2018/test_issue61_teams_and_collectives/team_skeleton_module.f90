program team_event
  implicit none
  integer :: team_handle, stat_var, count_value

  ! Minimal team/event usage intended to exercise the F2018 grammar.
  form team(1, team_handle)
  change team(team_handle)
    event post(team_handle, stat = stat_var)
    event wait(team_handle)
    event query(team_handle, count = count_value)
  end team
end program team_event
