program labeled_do_test
    implicit none
    integer :: i, sum_result

    ! F77-style labeled DO with CONTINUE terminal statement (issue #440/#463)
    sum_result = 0
    do 100 i = 1, 10
        sum_result = sum_result + i
100 continue

    print *, "Sum:", sum_result

    ! F90-style DO with END DO for comparison
    sum_result = 0
    do i = 1, 10
        sum_result = sum_result + i
    end do

    print *, "Sum F90:", sum_result

end program labeled_do_test
