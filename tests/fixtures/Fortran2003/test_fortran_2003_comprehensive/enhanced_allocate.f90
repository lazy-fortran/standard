program test_enhanced_allocate
    implicit none

    integer, allocatable :: array1(:), array2(:), array3(:)
    integer :: source_array(5) = [1, 2, 3, 4, 5]
    integer :: stat
    character(len=100) :: errmsg

    ! Allocate with SOURCE
    allocate(array1, source=source_array, stat=stat, errmsg=errmsg)
    if (stat /= 0) then
        print *, 'Allocation failed:', trim(errmsg)
        stop
    end if

    ! Allocate with MOLD
    allocate(array2, mold=source_array, stat=stat)
    if (stat == 0) then
        array2 = [10, 20, 30, 40, 50]
    end if

    ! Traditional allocation
    allocate(array3(10))
    array3 = 0

    print *, 'Array1:', array1
    print *, 'Array2:', array2
    print *, 'Array3 size:', size(array3)

    deallocate(array1, array2, array3)

end program test_enhanced_allocate

