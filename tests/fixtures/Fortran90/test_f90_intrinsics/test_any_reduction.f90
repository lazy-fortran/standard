program test_any_reduction
    implicit none
    logical :: mask(5)
    mask(1) = .false.
    mask(2) = .false.
    mask(3) = .true.
    mask(4) = .false.
    mask(5) = .false.
    if (any(mask)) then
        print *, "At least one true"
    end if
end program test_any_reduction
