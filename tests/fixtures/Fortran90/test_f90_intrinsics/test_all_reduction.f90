program test_all_reduction
    implicit none
    logical :: mask(5)
    mask(1) = .true.
    mask(2) = .true.
    mask(3) = .false.
    mask(4) = .true.
    mask(5) = .true.
    if (all(mask)) then
        print *, "All true"
    end if
end program test_all_reduction
