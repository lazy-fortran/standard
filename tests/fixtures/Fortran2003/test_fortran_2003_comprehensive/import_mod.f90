module import_mod
    implicit none

    integer, parameter :: my_kind = selected_real_kind(15)

    interface
        subroutine external_sub(x, y)
            import :: my_kind  ! Import specific names
            real(my_kind), intent(in) :: x
            real(my_kind), intent(out) :: y
        end subroutine external_sub

        function external_func(a, b)
            import  ! Import all accessible names
            real(my_kind) :: external_func
            real(my_kind), intent(in) :: a, b
        end function external_func
    end interface

end module import_mod

