program infer_walrus_struct_02
    implicit none
    type :: pair_t
        integer :: a, b
    end type
    call test()
contains
    function make_pair(x, y) result(res)
        integer, intent(in) :: x, y
        type(pair_t) :: res
        res%a = x
        res%b = y
    end function make_pair

    subroutine test()
        q := make_pair(10, 20)
        print *, q%a, q%b
    end subroutine test
end program infer_walrus_struct_02
