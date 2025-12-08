module pure_procs
    implicit none
contains
    pure function double_val(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * 2
    end function double_val
end module pure_procs
