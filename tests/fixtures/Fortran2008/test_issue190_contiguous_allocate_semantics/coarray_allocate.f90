program coarray_allocate_test
    implicit none
    integer, allocatable :: shared_counter[:]
    real, allocatable :: partial_sum[:]

    allocate(shared_counter[*])
    allocate(partial_sum[*])

    shared_counter = 0
    partial_sum = 0.0

    sync all

    deallocate(shared_counter)
    deallocate(partial_sum)
end program coarray_allocate_test
