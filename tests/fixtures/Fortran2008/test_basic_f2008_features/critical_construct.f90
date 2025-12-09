program test_critical
    implicit none
    integer :: shared_counter
    shared_counter = 0
    critical
        shared_counter = shared_counter + 1
    end critical
end program test_critical
