module array_ops_mod
    implicit none
contains
    subroutine scale_array(a, n, factor)
        integer, intent(in) :: n
        real, intent(inout) :: a(n)
        real, intent(in) :: factor
        integer :: i
        forall (i = 1:n)
            a(i) = a(i) * factor
        end forall
    end subroutine scale_array
end module array_ops_mod
