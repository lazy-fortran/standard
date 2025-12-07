module team_skeleton
  implicit none
contains
  subroutine use_team(x)
    integer :: team, me
    me = this_image()
    team = me
    ! NOTE: full TEAM_TYPE and FORM TEAM syntax are beyond the current
    ! grammar subset; this test only documents current behavior.
  end subroutine use_team
end module team_skeleton

