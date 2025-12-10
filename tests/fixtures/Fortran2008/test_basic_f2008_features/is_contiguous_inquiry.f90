program test_is_contiguous
    implicit none

    ! Test 1: Contiguous target array
    real, target :: target_array(100)
    real, pointer :: ptr_contiguous(:)
    integer :: i

    ! Initialize array with simple loop
    do i = 1, 100
        target_array(i) = real(i)
    end do

    ! Test contiguous pointer association
    ptr_contiguous => target_array
    if (is_contiguous(ptr_contiguous)) then
        print *, "Test 1 PASS: Contiguous pointer detected"
    else
        print *, "Test 1 FAIL: Should be contiguous"
    end if

    ! Test intrinsic is_contiguous in logical expressions
    if (is_contiguous(target_array)) then
        print *, "Test 2 PASS: Target array is contiguous"
    else
        print *, "Test 2 FAIL: Target should be contiguous"
    end if

    call test_assumed_shape(target_array)

contains

    subroutine test_assumed_shape(arr)
        real, intent(in) :: arr(:)

        if (is_contiguous(arr)) then
            print *, "Test assumed-shape: Array is contiguous"
        else
            print *, "Test assumed-shape: Array is NOT contiguous"
        end if
    end subroutine test_assumed_shape

end program test_is_contiguous
