program test_string_intrinsics_f2023
    implicit none
    character(len=100) :: line
    character(len=20), allocatable :: tokens(:)
    character(len=1), allocatable :: seps(:)
    integer :: i

    line = "hello,world,foo,bar"

    call split(line, ",", tokens)

    call split(line, ",", tokens, seps)

    call tokenize(line, ",")

    call tokenize(line, ",", quote="'")

end program test_string_intrinsics_f2023
