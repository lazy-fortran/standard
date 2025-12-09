program image_status_intrinsics
    use, intrinsic :: iso_fortran_env, only: team_type
    implicit none
    integer :: img, status
    integer, allocatable :: failed(:), stopped(:)
    type(team_type) :: t

    status = image_status(1)
    status = image_status(img, team=t)

    failed = failed_images()
    failed = failed_images(team=t, kind=4)

    stopped = stopped_images()
    stopped = stopped_images(team=t)
end program image_status_intrinsics
