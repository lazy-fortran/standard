module elemental_procs
    implicit none
contains
    elemental function square(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * x
    end function square
end module elemental_procs
