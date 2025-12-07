module coarray_test
    integer :: data[*]
contains
    subroutine test()
        sync all
        if (this_image() == 1) then
            print *, 'inherited from F2008'
        end if
    end subroutine
end module

