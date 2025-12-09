program collective_function_intrinsics
    use, intrinsic :: iso_fortran_env, only: team_type
    implicit none
    integer :: codims(2), tnum
    real :: x[*]
    type(team_type) :: t

    codims = coshape(x)
    codims = coshape(x, kind=4)

    tnum = team_number()
    tnum = team_number(team=t)
end program collective_function_intrinsics
