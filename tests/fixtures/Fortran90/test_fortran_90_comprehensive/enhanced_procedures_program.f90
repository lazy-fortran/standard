program test_procedures
    contains

    recursive function factorial(n) result(fact)
        integer, intent(in) :: n
        integer :: fact

        if (n <= 1) then
            fact = 1
        else
            fact = n * factorial(n - 1)
        end if
    end function factorial

    subroutine process_data(data, size, optional_param)
        real, intent(inout) :: data(:)
        integer, intent(in) :: size
        logical, intent(in), optional :: optional_param

        if (present(optional_param)) then
            print *, "Optional parameter provided"
        end if
    end subroutine process_data
end program

