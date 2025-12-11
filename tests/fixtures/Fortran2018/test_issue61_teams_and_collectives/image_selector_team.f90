program team_image_selector_example
  implicit none
  TYPE(TEAM_TYPE) :: my_team
  integer :: ierr
  integer :: a[*]
  integer :: b

  b = a[5, TEAM=my_team]
  b = a[2, TEAM_NUMBER=1]
  b = a[3, STAT=ierr]
  b = a[4, TEAM=my_team, STAT=ierr]
end program team_image_selector_example
