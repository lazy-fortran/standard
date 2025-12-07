module advanced_features
    implicit none
contains
    subroutine test_select_case(value)
        integer, intent(in) :: value

        select case (value)
        case (1:10)
            write(*,*) 'Small number'
        case (11:100)
            write(*,*) 'Medium number'
        case default
            write(*,*) 'Large number'
        end select
    end subroutine

    subroutine test_where_construct()
        integer, parameter :: n = 100
        real :: array(n), result(n)

        where (array > 0.0)
            result = sqrt(array)
        elsewhere (array < 0.0)
            result = 0.0
        elsewhere
            result = -1.0
        end where
    end subroutine
end module advanced_features

