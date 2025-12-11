program test_block
    implicit none
    integer :: n = 10
    real :: result

    block
        ! Local declarations within block
        real :: temp_array(n)
        integer :: i

        ! Initialize temporary array
        do i = 1, n
            temp_array(i) = real(i)**2
        end do

        ! Calculate result
        result = sum(temp_array)
    end block

    print *, 'Result:', result

end program test_block

