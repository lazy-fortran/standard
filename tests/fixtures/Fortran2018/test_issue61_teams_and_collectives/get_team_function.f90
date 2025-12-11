program get_team_function_test
   ! Test GET_TEAM intrinsic function
   ! ISO/IEC 1539-1:2018 Section 16.9.78
   use, intrinsic :: iso_fortran_env, only: team_type
   implicit none

   type(team_type) :: current_team
   type(team_type) :: level_team
   integer :: level

   ! GET_TEAM with LEVEL argument only
   current_team = get_team()
   level_team = get_team(1)

   ! GET_TEAM with LEVEL and TEAM arguments
   level_team = get_team(2, team=current_team)

   ! GET_TEAM in conditional
   if (get_team(1) /= current_team) then
      print *, "Teams are different"
   end if

   ! GET_TEAM in assignment with expression
   level = 1
   level_team = get_team(level)

end program get_team_function_test
