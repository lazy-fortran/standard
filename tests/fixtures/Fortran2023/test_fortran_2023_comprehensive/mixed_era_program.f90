program historical_test
    ! F77 CHARACTER type
    character*20 :: name

    ! F90 modules and allocation
    use some_module, only: utility_func
    integer, allocatable :: dynamic_array(:)

    ! F2003 OOP
    class(base_type), allocatable :: obj

    ! F2008 coarrays
    integer :: coarray[*]

    ! F2018 collective operations
    call co_sum(coarray)

    ! F2023 conditional expressions (ISO/IEC 1539-1:2023 Section 10.1.5)
    integer :: x, result
    x = 1
    result = (x > 0 ? x : 0)
end program historical_test

