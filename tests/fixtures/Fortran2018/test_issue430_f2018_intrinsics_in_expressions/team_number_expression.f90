program test_team_number_expression
    ! ISO/IEC 1539-1:2018 Section 16.9.187: TEAM_NUMBER
    ! Test TEAM_NUMBER intrinsic in expression context
    implicit none
    integer :: team_id

    ! TEAM_NUMBER must parse as primary in expression
    team_id = TEAM_NUMBER()

    if (TEAM_NUMBER() == 1) then
        print *, "Running on team 1"
    end if

end program test_team_number_expression
