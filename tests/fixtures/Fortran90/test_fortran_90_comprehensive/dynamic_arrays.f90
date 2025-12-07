program test_dynamic
    integer, allocatable :: dynamic_array(:)
    integer, pointer :: ptr_array(:)
    integer, target :: target_array(10)

    allocate(dynamic_array(100))
    ptr_array => target_array

    dynamic_array(1) = 42
    ptr_array(1) = 24

    deallocate(dynamic_array)
    nullify(ptr_array)
end program

