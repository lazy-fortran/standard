program dynamic_arrays
    implicit none
    integer, parameter :: n = 100
    real, allocatable :: dynamic_array(:)
    real, pointer :: ptr_array(:)

    allocate(dynamic_array(n))
    dynamic_array = 1.0
    deallocate(dynamic_array)
end program dynamic_arrays

