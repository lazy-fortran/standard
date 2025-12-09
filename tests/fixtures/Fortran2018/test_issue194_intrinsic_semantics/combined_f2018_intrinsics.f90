program combined_f2018_intrinsics
    use, intrinsic :: iso_fortran_env, only: team_type
    implicit none

    type(team_type) :: t
    integer :: img_status, tnum
    integer, allocatable :: failed_arr(:)
    real :: arr(10), reduce_result
    logical :: out_check

    interface
        pure function my_add(a, b) result(c)
            real, intent(in) :: a, b
            real :: c
        end function my_add
    end interface

    img_status = image_status(1)
    failed_arr = failed_images()
    tnum = team_number()

    arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    reduce_result = reduce(arr, my_add)

    out_check = out_of_range(1.0e38, 0)

    call random_init(repeatable=.true., image_distinct=.true.)
end program combined_f2018_intrinsics
