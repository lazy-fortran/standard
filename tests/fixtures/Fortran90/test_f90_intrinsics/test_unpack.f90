program test_unpack
    implicit none
    integer :: packed(3), result(5)
    logical :: mask(5)
    packed(1) = 10
    packed(2) = 20
    packed(3) = 30
    mask(1) = .true.
    mask(2) = .false.
    mask(3) = .true.
    mask(4) = .false.
    mask(5) = .true.
    result = unpack(packed, mask, 0)
    print *, "Unpack result"
end program test_unpack
