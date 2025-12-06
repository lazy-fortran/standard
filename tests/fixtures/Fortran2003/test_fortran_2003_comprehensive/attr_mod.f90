module attr_mod
    implicit none

    ! Protected variables (read-only outside module)
    integer, protected :: protected_var = 42

    ! Volatile variables (can be modified by external means)
    integer, volatile :: volatile_var

contains

    subroutine set_protected_var(val)
        integer, intent(in) :: val
        protected_var = val  ! OK within defining module
    end subroutine set_protected_var

    subroutine use_volatile_var()
        ! Compiler cannot optimize access to volatile_var
        if (volatile_var > 0) then
            print *, 'Volatile var is positive:', volatile_var
        end if
    end subroutine use_volatile_var

end module attr_mod

